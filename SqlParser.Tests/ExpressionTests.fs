module SqlParser.Tests.ExpressionTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
// 7.16 <table expression> requires a <from clause>: bare expressions are wrapped
// in "SELECT ... FROM t", and SELECT statements without a top-level FROM get one
// appended (a FROM inside parentheses does not count).
let private hasOuterFrom (s: string) =
    let isIdentChar c =
        System.Char.IsLetterOrDigit c || c = '_'

    let rec go depth i =
        if i >= s.Length then
            false
        elif s.[i] = '(' then
            go (depth + 1) (i + 1)
        elif s.[i] = ')' then
            go (depth - 1) (i + 1)
        elif
            depth = 0
            && i + 4 <= s.Length
            && s.Substring(i, 4).ToUpperInvariant() = "FROM"
            && (i = 0 || s.[i - 1] = ' ')
            && (i + 4 = s.Length || not (isIdentChar s.[i + 4]))
        then
            true
        else
            go depth (i + 1)

    go 0 0

let parseExpr (sql: string) =
    let s = sql.TrimEnd()
    let upper = s.ToUpperInvariant()

    let stmt =
        if upper.StartsWith("SELECT") then
            if hasOuterFrom s then s else s + " FROM t"
        elif
            upper.StartsWith("WITH")
            || upper.StartsWith("VALUES")
            || upper.StartsWith("TABLE")
        then
            s
        else
            "SELECT " + s + " FROM t"

    match SqlParser.parse (stmt + ";") with
    | Ok { Kind = Select(SelectQuery s) } -> s.Columns.[0] |> fun (Column(e, _)) -> e
    | Ok res -> failwithf "Expected Select, got %A" res
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parse sql = (parseExpr sql).Kind

let parseFails (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

// 10.4 — an <SQL argument list> whose arguments are all plain <value expression>s, unwrapped
// for assertions (the second element is the <copartition clause>, usually None).
let (|SqlValueArguments|_|) (arguments: SqlArgumentList) =
    let values =
        arguments.Arguments
        |> List.map (function
            | SqlArgumentValue e -> Some e
            | _ -> None)

    if List.forall Option.isSome values then
        Some(values |> List.map Option.get, arguments.Copartition)
    else
        None

[<Fact>]
let ``Concatenated hex literal verification`` () =
    match parse "SELECT X'0102' '0304'" with
    | Literal(Literal.Binary [| 1uy; 2uy; 3uy; 4uy |]) -> ()
    | res -> Assert.Fail(sprintf "Expected Binary literal, got %A" res)

[<Fact>]
let ``Navigation keywords are still usable as identifiers`` () =
    Assert.Equal(Identifier "FIRST", parse "SELECT first FROM t")
    Assert.Equal(Identifier "NEXT", parse "SELECT next FROM t")
    Assert.Equal(Identifier "PREV", parse "SELECT prev FROM t")
    Assert.Equal(Identifier "LAST", parse "SELECT last FROM t")

[<Fact>]
let ``Literal expressions verification`` () =
    Assert.Equal(Literal(Number 123m), parse "SELECT 123")
    Assert.Equal(Literal(String "hello"), parse "SELECT 'hello'")
    Assert.Equal(Literal(Bool(Some true)), parse "SELECT TRUE")

[<Fact>]
let ``NULL is a null specification, not a literal`` () =
    // 6.5 <null specification> — NULL is NOT a 5.3 <literal>, so it is rejected in
    // plain value-expression positions and accepted only in the contextually-typed slots.
    parseFails "SELECT NULL"
    parseFails "SELECT 1 + NULL"
    parseFails "SELECT ABS(NULL)"
    parseFails "SELECT CASE x WHEN NULL THEN 1 ELSE 0 END"
    parseFails "SELECT x FROM t WHERE x = NULL"

    match parse "SELECT CAST(NULL AS INT)" with
    | Cast({ Kind = Literal Null }, Integer, _) -> ()
    | res -> Assert.Fail(sprintf "Expected CAST(NULL AS INT), got %A" res)

    // 10.4 <SQL argument> admits a <contextually typed value specification>.
    match parse "SELECT my_func(NULL)" with
    | FunctionCall({ Kind = Identifier "MY_FUNC" }, _, SqlValueArguments([ { Kind = Literal Null } ], None), _, _, _) ->
        ()
    | res -> Assert.Fail(sprintf "Expected my_func(NULL), got %A" res)

[<Fact>]
let ``Character and binary string types keep their length model (6.1)`` () =
    // 6.1 <character length> ::= <length> [ <char length units> ]
    match parse "SELECT CAST(x AS CHAR(10))" with
    | Cast(_, Character(Some { Value = 10; Unit = None }), _) -> ()
    | res -> Assert.Fail(sprintf "Expected CHAR(10), got %A" res)

    match parse "SELECT CAST(x AS CHAR(10 CHARACTERS))" with
    | Cast(_, Character(Some { Value = 10; Unit = Some Characters }), _) -> ()
    | res -> Assert.Fail(sprintf "Expected CHAR(10 CHARACTERS), got %A" res)

    match parse "SELECT CAST(x AS CLOB(10 OCTETS))" with
    | Cast(_,
           CharacterLargeObject(Some { Value = 10
                                       Multiplier = None
                                       Unit = Some Octets }),
           _) -> ()
    | res -> Assert.Fail(sprintf "Expected CLOB(10 OCTETS), got %A" res)

    // 6.1 <large object length> ::= <unsigned integer> [ <multiplier> ] | <large object length token>
    match parse "SELECT CAST(x AS CLOB(10M))" with
    | Cast(_,
           CharacterLargeObject(Some { Value = 10
                                       Multiplier = Some Mega
                                       Unit = None }),
           _) -> ()
    | res -> Assert.Fail(sprintf "Expected CLOB(10M), got %A" res)

    match parse "SELECT CAST(x AS NCLOB(4 K))" with
    | Cast(_,
           NationalCharacterLargeObject(Some { Value = 4
                                               Multiplier = Some Kilo
                                               Unit = None }),
           _) -> ()
    | res -> Assert.Fail(sprintf "Expected NCLOB(4 K), got %A" res)

    // 6.1 <binary string type> — <length> is a plain <unsigned integer> (no units, no multiplier).
    match parse "SELECT CAST(x AS BINARY(8))" with
    | Cast(_, Binary(Some 8), _) -> ()
    | res -> Assert.Fail(sprintf "Expected BINARY(8), got %A" res)

    match parse "SELECT CAST(x AS BLOB(1G))" with
    | Cast(_,
           BinaryLargeObject(Some { Value = 1
                                    Multiplier = Some Giga
                                    Unit = None }),
           _) -> ()
    | res -> Assert.Fail(sprintf "Expected BLOB(1G), got %A" res)

[<Fact>]
let ``Varying string types require their length (6.1)`` () =
    // CHARACTER VARYING / VARCHAR / NATIONAL CHARACTER VARYING / NCHAR VARYING /
    // BINARY VARYING / VARBINARY have no length-less alternative.
    parseFails "SELECT CAST(x AS VARCHAR) FROM t"
    parseFails "SELECT CAST(x AS CHARACTER VARYING) FROM t"
    parseFails "SELECT CAST(x AS CHAR VARYING) FROM t"
    parseFails "SELECT CAST(x AS NCHAR VARYING) FROM t"
    parseFails "SELECT CAST(x AS NATIONAL CHARACTER VARYING) FROM t"
    parseFails "SELECT CAST(x AS VARBINARY) FROM t"
    parseFails "SELECT CAST(x AS BINARY VARYING) FROM t"

[<Fact>]
let ``String type modifiers are parsed (6.1)`` () =
    // 6.1 <predefined type> — `[ CHARACTER SET <character set specification> ] [ <collate clause> ]`
    match parse "SELECT CAST(x AS VARCHAR(10) CHARACTER SET utf8)" with
    | Cast(_, CharacterTypeWithModifiers(Varchar { Value = 10; Unit = None }, modifiers), _) ->
        Assert.Equal<ExpressionKind option>(
            Some(Identifier "UTF8"),
            modifiers.CharacterSet |> Option.map (fun e -> e.Kind)
        )

        Assert.True(Option.isNone modifiers.Collation)
    | res -> Assert.Fail(sprintf "Expected a CHARACTER SET modifier, got %A" res)

    match parse "SELECT CAST(x AS VARCHAR(10) CHARACTER SET utf8 COLLATE en_us)" with
    | Cast(_, CharacterTypeWithModifiers(Varchar { Value = 10; Unit = None }, modifiers), _) ->
        Assert.True(Option.isSome modifiers.CharacterSet)

        Assert.Equal<ExpressionKind option>(
            Some(Identifier "EN_US"),
            modifiers.Collation |> Option.map (fun e -> e.Kind)
        )
    | res -> Assert.Fail(sprintf "Expected both type-level modifiers, got %A" res)

    // A <binary string type> takes no modifier …
    parseFails "SELECT CAST(x AS BINARY(8) CHARACTER SET utf8) FROM t"
    // … and a <national character string type> has no CHARACTER SET slot.
    parseFails "SELECT CAST(x AS NCHAR(8) CHARACTER SET utf8) FROM t"

[<Fact>]
let ``Exact numeric type variants are parsed`` () =
    match parse "SELECT CAST(x AS DECIMAL(10,2))" with
    | Cast(_, Decimal(Some 10, Some 2), _) -> ()
    | res -> Assert.Fail(sprintf "Expected DECIMAL(10,2), got %A" res)

    match parse "SELECT CAST(x AS DEC(5))" with
    | Cast(_, Decimal(Some 5, None), _) -> ()
    | res -> Assert.Fail(sprintf "Expected DEC(5), got %A" res)

    match parse "SELECT CAST(x AS DECFLOAT(34))" with
    | Cast(_, DecFloat(Some 34), _) -> ()
    | res -> Assert.Fail(sprintf "Expected DECFLOAT(34), got %A" res)

    match parse "SELECT CAST(x AS NUMERIC)" with
    | Cast(_, Numeric(None, None), _) -> ()
    | res -> Assert.Fail(sprintf "Expected NUMERIC, got %A" res)

[<Fact>]
let ``Approximate numeric type variants are parsed`` () =
    match parse "SELECT CAST(x AS FLOAT)" with
    | Cast(_, Float None, _) -> ()
    | res -> Assert.Fail(sprintf "Expected FLOAT, got %A" res)

    match parse "SELECT CAST(x AS FLOAT(24))" with
    | Cast(_, Float(Some 24), _) -> ()
    | res -> Assert.Fail(sprintf "Expected FLOAT(24), got %A" res)

    match parse "SELECT CAST(x AS REAL)" with
    | Cast(_, Real, _) -> ()
    | res -> Assert.Fail(sprintf "Expected REAL, got %A" res)

    match parse "SELECT CAST(x AS DOUBLE PRECISION)" with
    | Cast(_, DoublePrecision, _) -> ()
    | res -> Assert.Fail(sprintf "Expected DOUBLE PRECISION, got %A" res)

[<Fact>]
let ``Datetime type variants are parsed`` () =
    match parse "SELECT CAST(x AS TIME)" with
    | Cast(_, TimeType(None, false), _) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME, got %A" res)

    match parse "SELECT CAST(x AS TIME(3))" with
    | Cast(_, TimeType(Some 3, false), _) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME(3), got %A" res)

    match parse "SELECT CAST(x AS TIME WITH TIME ZONE)" with
    | Cast(_, TimeType(None, true), _) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME WITH TIME ZONE, got %A" res)

    match parse "SELECT CAST(x AS TIME WITHOUT TIME ZONE)" with
    | Cast(_, TimeType(None, false), _) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME WITHOUT TIME ZONE, got %A" res)

    match parse "SELECT CAST(x AS TIMESTAMP(3) WITH TIME ZONE)" with
    | Cast(_, TimestampType(Some 3, true), _) -> ()
    | res -> Assert.Fail(sprintf "Expected TIMESTAMP(3) WITH TIME ZONE, got %A" res)

    // `pstringCI` returns the keyword as written in the input, so the old mapping
    // (`Some "WITH" -> true | _ -> false`) misread a lower-case `with` as WITHOUT.
    match parse "SELECT CAST(x AS TIME with time zone)" with
    | Cast(_, TimeType(None, true), _) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME with time zone (lower case), got %A" res)

    match parse "SELECT CAST(x AS TIMESTAMP(2) With Time Zone)" with
    | Cast(_, TimestampType(Some 2, true), _) -> ()
    | res -> Assert.Fail(sprintf "Expected TIMESTAMP(2) With Time Zone, got %A" res)

    // 6.1 <with or without time zone> is WITH TIME ZONE | WITHOUT TIME ZONE as a unit —
    // the partial forms are not <datetime type>s.
    parseFails "SELECT CAST(x AS TIME WITH)"
    parseFails "SELECT CAST(x AS TIME WITHOUT)"
    parseFails "SELECT CAST(x AS TIMESTAMP WITHOUT)"
    parseFails "SELECT CAST(x AS TIME WITH TIME)"

[<Fact>]
let ``Interval type keeps its qualifier structure`` () =
    match parse "SELECT CAST(x AS INTERVAL YEAR TO MONTH)" with
    | Cast(_, IntervalType(IntervalQualifier.Range(Year, Month, None)), _) -> ()
    | res -> Assert.Fail(sprintf "Expected structured interval type, got %A" res)

    match parse "SELECT CAST(x AS INTERVAL SECOND(2,3))" with
    | Cast(_, IntervalType(IntervalQualifier.SingleField(Second, Some prec)), _) ->
        Assert.Equal(Some 2, prec.Leading)
        Assert.Equal(Some 3, prec.FractionalSeconds)
    | res -> Assert.Fail(sprintf "Expected fractional precision, got %A" res)

    // A malformed qualifier is now rejected (the old raw-string parser accepted it).
    parseFails "SELECT CAST(x AS INTERVAL FOO BAR)"

[<Fact>]
let ``General value specification keyword forms verification`` () =
    match parse "SELECT CURRENT_USER" with
    | ExpressionKind.CurrentUser -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentUser, got %A" res)

    match parse "SELECT SESSION_USER" with
    | SessionUser -> ()
    | res -> Assert.Fail(sprintf "Expected SessionUser, got %A" res)

    match parse "SELECT SYSTEM_USER" with
    | SystemUser -> ()
    | res -> Assert.Fail(sprintf "Expected SystemUser, got %A" res)

    match parse "SELECT USER" with
    | User -> ()
    | res -> Assert.Fail(sprintf "Expected User, got %A" res)

    match parse "SELECT VALUE" with
    | Value -> ()
    | res -> Assert.Fail(sprintf "Expected Value, got %A" res)

    match parse "SELECT CURRENT_CATALOG" with
    | CurrentCatalog -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentCatalog, got %A" res)

    match parse "SELECT CURRENT_SCHEMA" with
    | CurrentSchema -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentSchema, got %A" res)

    match parse "SELECT CURRENT_PATH" with
    | CurrentPath -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentPath, got %A" res)

    match parse "SELECT CURRENT_ROLE" with
    | ExpressionKind.CurrentRole -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentRole, got %A" res)

    match parse "SELECT CURRENT_DEFAULT_TRANSFORM_GROUP" with
    | CurrentDefaultTransformGroup -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentDefaultTransformGroup, got %A" res)

    match parse "SELECT CURRENT_TRANSFORM_GROUP_FOR_TYPE t" with
    | CurrentTransformGroupForType { Kind = Identifier "T" } -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentTransformGroupForType, got %A" res)

    match parse "SELECT COLLATION FOR ('abc')" with
    | CollationFor { Kind = Literal(String "abc") } -> ()
    | res -> Assert.Fail(sprintf "Expected CollationFor, got %A" res)

[<Fact>]
let ``Routine invocation arity and suffix restrictions (6.10 / 10.9)`` () =
    // 10.9 — the bare `*` argument is only COUNT ( <asterisk> )
    parseFails "SELECT SUM(*)"
    parseFails "SELECT COUNT(*, x)"
    parseFails "SELECT my_func(*)"

    // positive: COUNT(*) parses
    match parse "SELECT COUNT(*)" with
    | FunctionCall({ Kind = Identifier "COUNT" },
                   _,
                   SqlValueArguments([ { Kind = ExpressionKind.Star } ], None),
                   _,
                   _,
                   _) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(*), got %A" res)

    // <rank function type> takes empty parens in the OVER form
    parseFails "SELECT RANK(1) OVER (ORDER BY x)"
    // <general set function> takes exactly one argument
    parseFails "SELECT SUM(a, b)"
    parseFails "SELECT COUNT(*, x)"
    // <binary set function> takes exactly two arguments
    parseFails "SELECT COVAR_POP(a)"
    // <nth value function> takes exactly two arguments
    parseFails "SELECT NTH_VALUE(x) OVER ()"
    // <lead or lag function> offset is an <exact numeric literal>
    parseFails "SELECT LEAD(x, 1 + 2) OVER ()"
    // <listagg separator> is a <character string literal>
    parseFails "SELECT LISTAGG(x, y) WITHIN GROUP (ORDER BY 1)"
    // <inverse distribution function> takes exactly one argument
    parseFails "SELECT PERCENTILE_CONT(1, 2) WITHIN GROUP (ORDER BY x)"
    // OVER / FILTER / WITHIN GROUP are not <routine invocation> suffixes
    parseFails "SELECT my_func(x) OVER (PARTITION BY y)"
    parseFails "SELECT my_func(x) FILTER (WHERE p)"
    parseFails "SELECT my_func(x) WITHIN GROUP (ORDER BY x)"
    // clause order: WITHIN GROUP immediately after the args, then FILTER, then OVER
    parseFails "SELECT SUM(x) OVER (PARTITION BY y) FILTER (WHERE p)"
    parseFails "SELECT RANK() FILTER (WHERE p) WITHIN GROUP (ORDER BY x)"
    // positive: the spec order parses
    match parse "SELECT SUM(x) FILTER (WHERE p) OVER (PARTITION BY y)" with
    | WindowFunction _ -> ()
    | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)

[<Fact>]
let ``COALESCE requires at least two arguments (6.12)`` () = parseFails "SELECT COALESCE(1)"

[<Fact>]
let ``GROUPING takes plain column references (6.9)`` () =
    parseFails "SELECT GROUPING(a COLLATE c)"

[<Fact>]
let ``Dereference right-hand side is a single identifier (6.21)`` () = parseFails "SELECT x -> s.t"

[<Fact>]
let ``Numeric argument slots reject boolean expressions (6.30 / 6.32)`` () =
    parseFails "SELECT ABS(1 = 2)"
    parseFails "SELECT SUBSTRING(s FROM 1 = 2 FOR 3)"
    parseFails "SELECT OVERLAY(s PLACING p FROM 1 = 2)"

[<Fact>]
let ``DEFAULT as a general expression is rejected`` () = parseFails "SELECT DEFAULT"

[<Fact>]
let ``Dotted identifier chain without parens stays a column reference`` () =
    match parse "SELECT a.b.c" with
    | ColumnReference [ "A"; "B"; "C" ] -> ()
    | res -> Assert.Fail(sprintf "Expected ColumnReference, got %A" res)

[<Fact>]
let ``GROUPING operation verification`` () =
    match parse "SELECT GROUPING(a)" with
    | Grouping [ { Kind = Identifier "A" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected Grouping, got %A" res)

    match parse "SELECT GROUPING(a, b)" with
    | Grouping [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected Grouping(a, b), got %A" res)

[<Fact>]
let ``GROUPING operation rejects non column references`` () =
    parseFails "SELECT GROUPING(a + 1)"
    parseFails "SELECT GROUPING(*)"
    parseFails "SELECT GROUPING(1)"

[<Fact>]
let ``Window frame row pattern measures verification`` () =
    match
        parse
            "SELECT SUM(x) OVER (MEASURES y AS m ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW PATTERN (A) DEFINE A AS a > 0)"
    with
    | WindowFunction { Function = { Kind = Identifier "SUM" }
                       Args = [ { Kind = Identifier "X" } ]
                       Window = { Frame = Some frame } } ->
        match frame with
        | { Unit = Rows
            Start = UnboundedPreceding
            End = Some CurrentRow
            Exclusion = None
            Measures = Some [ { Expression = { Kind = Identifier "Y" }
                                Name = { Kind = Identifier "M" } } ]
            RowPattern = Some { AfterMatch = None
                                InitialOrSeek = None
                                Pattern = { Terms = [ { Factors = [ { Primary = RowPatternVariable { Kind = Identifier "A" }
                                                                      Quantifier = None } ] } ] }
                                Subset = []
                                Define = [ { Name = { Kind = Identifier "A" }
                                             Condition = { Kind = BinaryOp(GreaterThan,
                                                                           { Kind = Identifier "A" },
                                                                           { Kind = Literal(Number 0m) }) } } ] } } ->
            ()
        | res -> Assert.Fail(sprintf "Expected window row pattern, got %A" res)
    | res -> Assert.Fail(sprintf "Expected window row pattern, got %A" res)

[<Fact>]
let ``GROUPS window frame verification`` () =
    match parse "SELECT SUM(x) OVER (ORDER BY y GROUPS BETWEEN 1 PRECEDING AND 1 FOLLOWING)" with
    | WindowFunction { Window = { Frame = Some { Unit = Groups
                                                 Start = Preceding { Kind = Literal(Number 1m) }
                                                 End = Some(Following { Kind = Literal(Number 1m) })
                                                 Exclusion = None } } } -> ()
    | res -> Assert.Fail(sprintf "Expected GROUPS frame, got %A" res)

[<Fact>]
let ``Window frame with a leading PRECEDING start is accepted`` () =
    match parse "SELECT SUM(x) OVER (ORDER BY y ROWS 1 PRECEDING)" with
    | WindowFunction { Window = { Frame = Some { Start = Preceding { Kind = Literal(Number 1m) }
                                                 End = None } } } -> ()
    | res -> Assert.Fail(sprintf "Expected ROWS 1 PRECEDING frame, got %A" res)

[<Fact>]
let ``Window frame start cannot be FOLLOWING`` () =
    parseFails "SELECT SUM(x) OVER (ORDER BY y ROWS 1 FOLLOWING)"
    parseFails "SELECT SUM(x) OVER (ORDER BY y ROWS UNBOUNDED FOLLOWING)"
    parseFails "SELECT SUM(x) OVER (ORDER BY y ROWS BETWEEN 1 FOLLOWING AND 2 FOLLOWING)"

[<Fact>]
let ``Window partition accepts a collate clause`` () =
    match parse "SELECT SUM(x) OVER (PARTITION BY a COLLATE c)" with
    | WindowFunction { Window = { PartitionBy = [ { Kind = Collate(_, _) } ] } } -> ()
    | res -> Assert.Fail(sprintf "Expected a COLLATE partition item, got %A" res)

[<Fact>]
let ``Window partition rejects non column references`` () =
    parseFails "SELECT SUM(x) OVER (PARTITION BY a + 1)"
    parseFails "SELECT SUM(x) OVER (PARTITION BY 1)"

[<Fact>]
let ``Window functions verification`` () =
    match parse "SELECT ROW_NUMBER() OVER (ORDER BY id DESC)" with
    | WindowFunction { Function = { Kind = Identifier "ROW_NUMBER" }
                       Args = []
                       IsDistinct = false
                       Window = { ExistingWindowName = None
                                  PartitionBy = []
                                  OrderBy = [ { Kind = Identifier "ID"
                                                Pos = { Line = 1L; Column = 36L } },
                                              false,
                                              None ]
                                  Frame = None } } -> ()
    | res -> Assert.Fail(sprintf "Expected ROW_NUMBER() OVER ..., got %A" res)

    match parse "SELECT SUM(salary) OVER (PARTITION BY dept_id ORDER BY hire_date)" with
    | WindowFunction { Function = { Kind = Identifier "SUM" }
                       Args = [ { Kind = Identifier "SALARY" } ]
                       IsDistinct = false
                       Window = { ExistingWindowName = None
                                  PartitionBy = [ { Kind = Identifier "DEPT_ID" } ]
                                  OrderBy = [ { Kind = Identifier "HIRE_DATE" }, true, None ]
                                  Frame = None } } -> ()
    | res -> Assert.Fail(sprintf "Expected SUM(...) OVER ..., got %A" res)

[<Fact>]
let ``Window-only functions require an OVER clause`` () =
    parseFails "SELECT ROW_NUMBER()"
    parseFails "SELECT RANK()"
    parseFails "SELECT DENSE_RANK()"
    parseFails "SELECT LEAD(x)"
    parseFails "SELECT NTILE(4)"
    parseFails "SELECT PERCENTILE_CONT(0.5)"
    parseFails "SELECT LISTAGG(x, ',')"

[<Fact>]
let ``Set functions that need a suffix are accepted with one`` () =
    match parse "SELECT RANK() OVER (ORDER BY x)" with
    | WindowFunction _ -> ()
    | res -> Assert.Fail(sprintf "Expected RANK OVER, got %A" res)

    match parse "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY x)" with
    | FunctionCall(_, _, _, _, _, Some _) -> ()
    | res -> Assert.Fail(sprintf "Expected PERCENTILE_CONT WITHIN GROUP, got %A" res)

    match parse "SELECT LISTAGG(x, ',') WITHIN GROUP (ORDER BY x)" with
    | FunctionCall(_, _, _, _, _, Some _) -> ()
    | res -> Assert.Fail(sprintf "Expected LISTAGG WITHIN GROUP, got %A" res)

[<Fact>]
let ``Nested row number function verification`` () =
    match parse "SELECT ROW_NUMBER(BEGIN_PARTITION)" with
    | NestedRowNumber RowMarker.BeginPartition -> ()
    | res -> Assert.Fail(sprintf "Expected NestedRowNumber BEGIN_PARTITION, got %A" res)

    match parse "SELECT ROW_NUMBER(END_FRAME)" with
    | NestedRowNumber RowMarker.EndFrame -> ()
    | res -> Assert.Fail(sprintf "Expected NestedRowNumber END_FRAME, got %A" res)

[<Fact>]
let ``Nested window function invalid syntax is rejected`` () =
    parseFails "SELECT ROW_NUMBER(BEGIN)"
    // VALUE_OF is not a whitelisted function keyword, so omitting AT cannot fall through
    // to a generic <routine invocation>.
    parseFails "SELECT VALUE_OF(x)"
    parseFails "SELECT VALUE_OF(x AT 5)"

[<Fact>]
let ``VALUE_OF function verification`` () =
    match parse "SELECT VALUE_OF(x AT CURRENT_ROW)" with
    | ValueOf({ Kind = Identifier "X" },
              { Marker = RowMarker.CurrentRow
                Delta = None },
              None) -> ()
    | res -> Assert.Fail(sprintf "Expected ValueOf CURRENT_ROW, got %A" res)

    match parse "SELECT VALUE_OF(x AT END_FRAME - 1, 0)" with
    | ValueOf({ Kind = Identifier "X" }, marker, Some { Kind = Literal(Number 0m) }) ->
        match marker with
        | { Marker = RowMarker.EndFrame
            Delta = Some(false, ({ Kind = Literal(Number 1m) })) } -> ()
        | res -> Assert.Fail(sprintf "Expected ValueOf delta, got %A" res)
    | res -> Assert.Fail(sprintf "Expected ValueOf with delta and default, got %A" res)

[<Fact>]
let ``Case expression verification`` () =
    match parse "SELECT CASE WHEN a = 1 THEN 'one' ELSE 'other' END" with
    | Case(None,
           [ { Kind = BinaryOp(Equal, { Kind = Identifier "A" }, { Kind = Literal(Number 1m) }) },
             { Kind = Literal(String "one") } ],
           Some { Kind = Literal(String "other") }) -> ()
    | res -> Assert.Fail(sprintf "Expected CASE, got %A" res)

[<Fact>]
let ``Simple CASE expression verification`` () =
    match parse "SELECT CASE x WHEN 1 THEN 'a' WHEN 2, 3 THEN 'b' ELSE 'c' END" with
    | Case(Some { Kind = Identifier "X" },
           [ ({ Kind = Literal(Number 1m) }, { Kind = Literal(String "a") })
             ({ Kind = Literal(Number 2m) }, { Kind = Literal(String "b") })
             ({ Kind = Literal(Number 3m) }, { Kind = Literal(String "b") }) ],
           Some { Kind = Literal(String "c") }) -> ()
    | res -> Assert.Fail(sprintf "Expected simple CASE, got %A" res)

[<Fact>]
let ``CAST FORMAT template verification (6.13)`` () =
    match parse "SELECT CAST(x AS INT FORMAT '999') FROM t" with
    | Cast({ Kind = Identifier "X" }, Integer, Some "999") -> ()
    | res -> Assert.Fail(sprintf "Expected a formatted cast, got %A" res)

    match parse "SELECT CAST(x AS INT)" with
    | Cast(_, Integer, None) -> ()
    | res -> Assert.Fail(sprintf "Expected a cast without FORMAT, got %A" res)

    // <cast target> is a <domain name> or a <data type> — DESCRIPTOR is neither; the
    // CAST ( NULL AS DESCRIPTOR ) form is the 10.4 <descriptor argument> (see below).
    parseFails "SELECT CAST(x AS DESCRIPTOR) FROM t"

[<Fact>]
let ``Descriptor arguments in SQL argument lists (10.4)`` () =
    match parse "SELECT my_func(DESCRIPTOR (a INT, b)) FROM t" with
    | FunctionCall(_, _, arguments, _, _, _) ->
        match arguments.Arguments with
        | [ SqlArgumentDescriptor { Kind = DescriptorValueConstructor [ ({ Kind = Identifier "A" }, Some Integer)
                                                                        ({ Kind = Identifier "B" }, None) ] } ] -> ()
        | res -> Assert.Fail(sprintf "Expected a descriptor value constructor argument, got %A" res)
    | res -> Assert.Fail(sprintf "Expected a descriptor value constructor argument, got %A" res)

    match parse "SELECT my_func(CAST(NULL AS DESCRIPTOR)) FROM t" with
    | FunctionCall(_, _, arguments, _, _, _) ->
        match arguments.Arguments with
        | [ SqlArgumentDescriptor { Kind = DescriptorCast } ] -> ()
        | res -> Assert.Fail(sprintf "Expected a CAST ( NULL AS DESCRIPTOR ) argument, got %A" res)
    | res -> Assert.Fail(sprintf "Expected a CAST ( NULL AS DESCRIPTOR ) argument, got %A" res)

    // The CAST form is a <descriptor argument> only — it is not a <cast specification>.
    parseFails "SELECT CAST(NULL AS DESCRIPTOR) FROM t"

    // A routine named DESCRIPTOR still parses as a plain identifier argument.
    match parse "SELECT my_func(DESCRIPTOR) FROM t" with
    | FunctionCall(_, _, SqlValueArguments([ { Kind = Identifier "DESCRIPTOR" } ], None), _, _, _) -> ()
    | res -> Assert.Fail(sprintf "Expected a plain DESCRIPTOR argument, got %A" res)

[<Fact>]
let ``SQL argument forms (10.4)`` () =
    // <generalized expression> ::= <value expression> AS <path-resolved user-defined type name>
    match parse "SELECT my_func(x AS my_type) FROM t" with
    | FunctionCall(_, _, arguments, _, _, _) ->
        match arguments.Arguments with
        | [ SqlArgumentGeneralized({ Kind = Identifier "X" }, UserDefinedType { Kind = Identifier "MY_TYPE" }) ] -> ()
        | res -> Assert.Fail(sprintf "Expected a generalized expression argument, got %A" res)
    | res -> Assert.Fail(sprintf "Expected my_func, got %A" res)

    // <named argument specification> ::= <SQL parameter name> => <named argument SQL argument>
    match parse "SELECT my_func(a => 1, b => 'x') FROM t" with
    | FunctionCall(_, _, arguments, _, _, _) ->
        match arguments.Arguments with
        | [ SqlArgumentNamed({ Kind = Identifier "A" }, SqlArgumentValue { Kind = Literal(Number 1m) })
            SqlArgumentNamed({ Kind = Identifier "B" }, SqlArgumentValue { Kind = Literal(String "x") }) ] -> ()
        | res -> Assert.Fail(sprintf "Expected named arguments, got %A" res)
    | res -> Assert.Fail(sprintf "Expected my_func, got %A" res)

    parseFails "SELECT my_func(1 => 2) FROM t"

    // <table argument> — `TABLE ( <name> )` is unambiguous, so it needs no clause; its
    // correlation, partitioning, pruning and ordering are kept.
    match parse "SELECT my_ptf(TABLE(t) AS x PARTITION BY a KEEP WHEN EMPTY ORDER BY b DESC) FROM t" with
    | FunctionCall(_, _, arguments, _, _, _) ->
        match arguments.Arguments with
        | [ SqlArgumentTable table ] ->
            match table.Table with
            | TableArgumentName { Kind = Identifier "T" } -> ()
            | res -> Assert.Fail(sprintf "Expected TABLE ( t ), got %A" res)

            match table.Correlation with
            | Some({ Kind = Identifier "X" }, None) -> ()
            | res -> Assert.Fail(sprintf "Expected a correlation, got %A" res)

            match table.PartitionBy with
            | Some [ { Kind = Identifier "A" } ] -> ()
            | res -> Assert.Fail(sprintf "Expected PARTITION BY a, got %A" res)

            Assert.Equal(Some KeepWhenEmpty, table.Pruning)

            match table.OrderBy with
            | Some [ ({ Kind = Identifier "B" }, false, None) ] -> ()
            | res -> Assert.Fail(sprintf "Expected ORDER BY b DESC, got %A" res)
        | res -> Assert.Fail(sprintf "Expected a table argument, got %A" res)
    | res -> Assert.Fail(sprintf "Expected my_ptf, got %A" res)

    // A <table function invocation> proper is also a <value expression>, so it needs a
    // table-argument clause (see docs/trade-off.md).
    match parse "SELECT my_ptf(f(x) PARTITION BY (a, b) PRUNE WHEN EMPTY) FROM t" with
    | FunctionCall(_, _, arguments, _, _, _) ->
        match arguments.Arguments with
        | [ SqlArgumentTable table ] ->
            match table.Table with
            | TableArgumentInvocation { Kind = FunctionCall _ } -> ()
            | res -> Assert.Fail(sprintf "Expected a table function invocation, got %A" res)

            match table.PartitionBy with
            | Some [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ] -> ()
            | res -> Assert.Fail(sprintf "Expected PARTITION BY (a, b), got %A" res)

            Assert.Equal(Some PruneWhenEmpty, table.Pruning)
        | res -> Assert.Fail(sprintf "Expected a table argument, got %A" res)
    | res -> Assert.Fail(sprintf "Expected my_ptf, got %A" res)

    // Without any table-argument clause the same text stays a <value expression>.
    match parse "SELECT my_ptf(TABLE(SELECT x FROM u)) FROM t" with
    | FunctionCall(_, _, arguments, _, _, _) ->
        match arguments.Arguments with
        | [ SqlArgumentValue { Kind = TableQuery _ } ] -> ()
        | res -> Assert.Fail(sprintf "Expected a table query argument, got %A" res)
    | res -> Assert.Fail(sprintf "Expected my_ptf, got %A" res)

    // <copartition clause> ::= COPARTITION <copartition list>
    match parse "SELECT my_ptf(TABLE(t1) PARTITION BY a, TABLE(t2) COPARTITION (t1, t2)) FROM t" with
    | FunctionCall(_, _, arguments, _, _, _) ->
        match arguments.Copartition with
        | Some [ [ { Kind = Identifier "T1" }; { Kind = Identifier "T2" } ] ] -> ()
        | res -> Assert.Fail(sprintf "Expected a copartition clause, got %A" res)
    | res -> Assert.Fail(sprintf "Expected my_ptf, got %A" res)

    // 10.4 — a reserved built-in takes <value expression> arguments only.
    parseFails "SELECT ABS(1, TABLE(t)) FROM t"

[<Fact>]
let ``NEXT VALUE FOR verification`` () =
    match parse "SELECT NEXT VALUE FOR order_seq" with
    | NextValueFor({ Kind = Identifier "ORDER_SEQ" }) -> ()
    | res -> Assert.Fail(sprintf "Expected NextValueFor, got %A" res)

    match parse "SELECT NEXT VALUE FOR app.order_seq" with
    | NextValueFor({ Kind = ColumnReference [ "APP"; "ORDER_SEQ" ] }) -> ()
    | res -> Assert.Fail(sprintf "Expected NextValueFor qualified, got %A" res)

[<Fact>]
let ``TREAT subtype treatment verification`` () =
    match parse "SELECT TREAT(x AS t)" with
    | Treat({ Kind = Identifier "X" }, UserDefinedType { Kind = Identifier "T" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Treat, got %A" res)

[<Fact>]
let ``SPECIFICTYPE method verification`` () =
    match parse "SELECT x.SPECIFICTYPE" with
    | SpecificTypeMethod({ Kind = Identifier "X" }, false) -> ()
    | res -> Assert.Fail(sprintf "Expected SpecificTypeMethod, got %A" res)

    match parse "SELECT x.SPECIFICTYPE()" with
    | SpecificTypeMethod({ Kind = Identifier "X" }, true) -> ()
    | res -> Assert.Fail(sprintf "Expected SpecificTypeMethod with parens, got %A" res)

[<Fact>]
let ``Method invocation verification`` () =
    match parse "SELECT a.obj.prune(x)" with
    | MethodInvocation({ Kind = ColumnReference [ "A"; "OBJ" ] },
                       { Kind = Identifier "PRUNE" },
                       SqlValueArguments([ { Kind = Identifier "X" } ], None)) -> ()
    | res -> Assert.Fail(sprintf "Expected MethodInvocation, got %A" res)

[<Fact>]
let ``Method invocation on parenthesized expression verification`` () =
    match parse "SELECT (a.b).prune(x)" with
    | MethodInvocation({ Kind = Parenthesized { Kind = ColumnReference [ "A"; "B" ] } },
                       { Kind = Identifier "PRUNE" },
                       SqlValueArguments([ { Kind = Identifier "X" } ], None)) -> ()
    | res -> Assert.Fail(sprintf "Expected MethodInvocation on parenthesized, got %A" res)

[<Fact>]
let ``Field reference verification`` () =
    match parse "SELECT a.obj.prune(x).field" with
    | FieldReference({ Kind = MethodInvocation({ Kind = ColumnReference [ "A"; "OBJ" ] },
                                               { Kind = Identifier "PRUNE" },
                                               SqlValueArguments([ { Kind = Identifier "X" } ], None)) },
                     { Kind = Identifier "FIELD" }) -> ()
    | res -> Assert.Fail(sprintf "Expected FieldReference, got %A" res)

[<Fact>]
let ``Method invocation on last chain segment verification`` () =
    match parse "SELECT a.b.c(x)" with
    | MethodInvocation({ Kind = ColumnReference [ "A"; "B" ] },
                       { Kind = Identifier "C" },
                       SqlValueArguments([ { Kind = Identifier "X" } ], None)) -> ()
    | res -> Assert.Fail(sprintf "Expected MethodInvocation on last segment, got %A" res)

    parseFails "SELECT 1 + DEFAULT"

[<Fact>]
let ``Generalized method invocation verification`` () =
    match parse "SELECT (x AS mytype).m()" with
    | GeneralizedInvocation({ Kind = Identifier "X" },
                            UserDefinedType { Kind = Identifier "MYTYPE" },
                            { Kind = Identifier "M" },
                            Some(SqlValueArguments([], None))) -> ()
    | res -> Assert.Fail(sprintf "Expected GeneralizedInvocation, got %A" res)

    match parse "SELECT (x AS mytype).m(1)" with
    | GeneralizedInvocation(_,
                            _,
                            { Kind = Identifier "M" },
                            Some(SqlValueArguments([ { Kind = Literal(Number 1m) } ], None))) -> ()
    | res -> Assert.Fail(sprintf "Expected GeneralizedInvocation with args, got %A" res)

    match parse "SELECT (x AS mytype).m" with
    | GeneralizedInvocation(_, _, { Kind = Identifier "M" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected GeneralizedInvocation without args, got %A" res)

[<Fact>]
let ``Static method invocation verification`` () =
    match parse "SELECT my_type::prune(x)" with
    | StaticMethodInvocation({ Kind = Identifier "MY_TYPE" },
                             { Kind = Identifier "PRUNE" },
                             SqlValueArguments([ { Kind = Identifier "X" } ], None)) -> ()
    | res -> Assert.Fail(sprintf "Expected StaticMethodInvocation, got %A" res)

[<Fact>]
let ``NEW specification verification`` () =
    match parse "SELECT NEW my_type(1, 2)" with
    | NewSpecification({ Kind = Identifier "MY_TYPE" },
                       SqlValueArguments([ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) } ], None)) -> ()
    | res -> Assert.Fail(sprintf "Expected NewSpecification, got %A" res)

[<Fact>]
let ``Dereference operation verification`` () =
    match parse "SELECT x -> attr" with
    | Dereference({ Kind = Identifier "X" }, { Kind = Identifier "ATTR" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Dereference attribute, got %A" res)

    match parse "SELECT x -> m(1)" with
    | Dereference({ Kind = Identifier "X" },
                  { Kind = Identifier "M" },
                  Some(SqlValueArguments([ { Kind = Literal(Number 1m) } ], None))) -> ()
    | res -> Assert.Fail(sprintf "Expected Dereference method, got %A" res)

    match parse "SELECT a.b -> c" with
    | Dereference({ Kind = ColumnReference [ "A"; "B" ] }, { Kind = Identifier "C" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Dereference on column reference, got %A" res)

[<Fact>]
let ``DEREF reference resolution verification`` () =
    match parse "SELECT DEREF(x)" with
    | Deref { Kind = Identifier "X" } -> ()
    | res -> Assert.Fail(sprintf "Expected Deref, got %A" res)

[<Fact>]
let ``Array element reference verification`` () =
    match parse "SELECT arr[1]" with
    | ArrayElement({ Kind = Identifier "ARR" }, { Kind = Literal(Number 1m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected ArrayElement, got %A" res)

    match parse "SELECT arr[i + 1]" with
    | ArrayElement({ Kind = Identifier "ARR" },
                   { Kind = BinaryOp(Add, { Kind = Identifier "I" }, { Kind = Literal(Number 1m) }) }) -> ()
    | res -> Assert.Fail(sprintf "Expected ArrayElement with index expr, got %A" res)

    match parse "SELECT arr??(1??)" with
    | ArrayElement({ Kind = Identifier "ARR" }, { Kind = Literal(Number 1m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected ArrayElement trigraph, got %A" res)

[<Fact>]
let ``ELEMENT multiset element reference verification`` () =
    match parse "SELECT ELEMENT(x)" with
    | Element { Kind = Identifier "X" } -> ()
    | res -> Assert.Fail(sprintf "Expected Element, got %A" res)

[<Fact>]
let ``Row pattern navigation verification`` () =
    match parse "SELECT FIRST(x)" with
    | RowPatternNavigation(RowPatternNavigation.Logical(None, FirstOrLast.First, { Kind = Identifier "X" }, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected FIRST(x), got %A" res)

    match parse "SELECT LAST(x, 1)" with
    | RowPatternNavigation(RowPatternNavigation.Logical(None, FirstOrLast.Last, _, Some { Kind = Literal(Number 1m) })) ->
        ()
    | res -> Assert.Fail(sprintf "Expected LAST(x, 1), got %A" res)

    match parse "SELECT RUNNING FIRST(x)" with
    | RowPatternNavigation(RowPatternNavigation.Logical(Some RunningOrFinal.Running, FirstOrLast.First, _, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected RUNNING FIRST(x), got %A" res)

    match parse "SELECT FINAL LAST(x)" with
    | RowPatternNavigation(RowPatternNavigation.Logical(Some RunningOrFinal.Final, FirstOrLast.Last, _, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected FINAL LAST(x), got %A" res)

    match parse "SELECT PREV(x)" with
    | RowPatternNavigation(RowPatternNavigation.Physical(PrevOrNext.Prev, { Kind = Identifier "X" }, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected PREV(x), got %A" res)

    match parse "SELECT NEXT(x, 2)" with
    | RowPatternNavigation(RowPatternNavigation.Physical(PrevOrNext.Next, _, Some { Kind = Literal(Number 2m) })) -> ()
    | res -> Assert.Fail(sprintf "Expected NEXT(x, 2), got %A" res)

    match parse "SELECT PREV(FIRST(x), 2)" with
    | RowPatternNavigation(RowPatternNavigation.Compound(PrevOrNext.Prev,
                                                         None,
                                                         FirstOrLast.First,
                                                         _,
                                                         None,
                                                         Some { Kind = Literal(Number 2m) })) -> ()
    | res -> Assert.Fail(sprintf "Expected PREV(FIRST(x), 2), got %A" res)

    match parse "SELECT NEXT(RUNNING LAST(x, 1), 2)" with
    | RowPatternNavigation(RowPatternNavigation.Compound(PrevOrNext.Next,
                                                         Some RunningOrFinal.Running,
                                                         FirstOrLast.Last,
                                                         _,
                                                         Some { Kind = Literal(Number 1m) },
                                                         Some { Kind = Literal(Number 2m) })) -> ()
    | res -> Assert.Fail(sprintf "Expected compound navigation, got %A" res)

[<Fact>]
let ``JSON API passing clause verification`` () =
    match parse "SELECT JSON_VALUE(doc, '$.x' PASSING a AS p)" with
    | JsonValue({ Context = { Kind = Identifier "DOC" }
                  Passing = [ { Value = { Kind = Identifier "A" }
                                InputFormat = None
                                Name = { Kind = Identifier "P" } } ] },
                None,
                None,
                None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonValue PASSING, got %A" res)

[<Fact>]
let ``JSON input clause is preserved`` () =
    // 10.14 <JSON context item> ::= <JSON value expression> — the FORMAT clause must survive.
    match parse "SELECT JSON_VALUE(x FORMAT JSON ENCODING UTF16, '$.a')" with
    | JsonValue({ ContextFormat = Some(JsonEncoding(Some Utf16)) }, None, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected context FORMAT kept, got %A" res)

    // 10.14 <JSON passing argument> — per-argument FORMAT clause.
    match parse "SELECT JSON_VALUE(x, '$.a' PASSING y FORMAT JSON AS b)" with
    | JsonValue({ Passing = [ { InputFormat = Some(JsonEncoding None) } ] }, None, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected passing FORMAT kept, got %A" res)

[<Fact>]
let ``JSON_VALUE function verification`` () =
    match parse "SELECT JSON_VALUE(doc, '$.name')" with
    | JsonValue({ Context = { Kind = Identifier "DOC" }
                  Path = "$.name"
                  PathName = None
                  Passing = [] },
                None,
                None,
                None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonValue, got %A" res)

[<Fact>]
let ``JSON path must be a character string literal (10.14)`` () =
    parseFails "SELECT JSON_VALUE(a, b)"

    match parse "SELECT JSON_VALUE(doc, '$.x' RETURNING INT DEFAULT 0 ON EMPTY ERROR ON ERROR)" with
    | JsonValue({ Context = { Kind = Identifier "DOC" } },
                Some Integer,
                Some(JsonDefault { Kind = Literal(Number 0m) }),
                Some JsonError) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonValue with clauses, got %A" res)

[<Fact>]
let ``EXTRACT verification`` () =
    match parse "SELECT EXTRACT(YEAR FROM hire_date)" with
    | Extract({ Kind = Identifier "YEAR" }, { Kind = Identifier "HIRE_DATE" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Extract, got %A" res)

[<Fact>]
let ``POSITION verification`` () =
    match parse "SELECT POSITION('a' IN 'abc')" with
    | Position({ Kind = Literal(String "a") }, { Kind = Literal(String "abc") }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Position, got %A" res)

[<Fact>]
let ``POSITION rejects a non char length unit`` () =
    parseFails "SELECT POSITION('a' IN 'abc' USING JUNK)"

[<Fact>]
let ``CHARACTER length expressions verification`` () =
    match parse "SELECT CHAR_LENGTH(name)" with
    | LengthExpression(LengthFunction.CharLength, { Kind = Identifier "NAME" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected CHAR_LENGTH, got %A" res)

    match parse "SELECT CHARACTER_LENGTH(name USING CHARACTERS)" with
    | LengthExpression(LengthFunction.CharacterLength, { Kind = Identifier "NAME" }, Some "CHARACTERS") -> ()
    | res -> Assert.Fail(sprintf "Expected CHARACTER_LENGTH USING, got %A" res)

    match parse "SELECT OCTET_LENGTH(name)" with
    | LengthExpression(LengthFunction.OctetLength, { Kind = Identifier "NAME" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected OCTET_LENGTH, got %A" res)

[<Fact>]
let ``Length expressions reject a non char length unit`` () =
    parseFails "SELECT CHARACTER_LENGTH(name USING JUNK)"
    // <octet length expression> has no USING slot at all
    parseFails "SELECT OCTET_LENGTH(name USING CHARACTERS)"

[<Fact>]
let ``ABS absolute value verification`` () =
    match parse "SELECT ABS(x)" with
    | NumericValueFunction(NumericFunction.AbsoluteValue, [ { Kind = Identifier "X" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected ABS, got %A" res)

    // 6.38 <interval absolute value function>
    match parse "SELECT ABS(INTERVAL '1' DAY)" with
    | NumericValueFunction(NumericFunction.AbsoluteValue, [ { Kind = Literal(Interval _) } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected ABS(interval), got %A" res)

[<Fact>]
let ``Numeric value function catalog verification`` () =
    let cases =
        [ "SELECT CARDINALITY(c)", NumericFunction.Cardinality
          "SELECT ARRAY_MAX_CARDINALITY(a)", NumericFunction.ArrayMaxCardinality
          "SELECT SIN(x)", NumericFunction.Sin
          "SELECT COS(x)", NumericFunction.Cos
          "SELECT TAN(x)", NumericFunction.Tan
          "SELECT SINH(x)", NumericFunction.Sinh
          "SELECT COSH(x)", NumericFunction.Cosh
          "SELECT TANH(x)", NumericFunction.Tanh
          "SELECT ASIN(x)", NumericFunction.Asin
          "SELECT ACOS(x)", NumericFunction.Acos
          "SELECT ATAN(x)", NumericFunction.Atan
          "SELECT LOG10(x)", NumericFunction.CommonLogarithm
          "SELECT LN(x)", NumericFunction.NaturalLogarithm
          "SELECT EXP(x)", NumericFunction.Exponential
          "SELECT SQRT(x)", NumericFunction.SquareRoot
          "SELECT FLOOR(x)", NumericFunction.Floor
          "SELECT CEIL(x)", NumericFunction.Ceiling
          "SELECT CEILING(x)", NumericFunction.Ceiling ]

    for sql, expected in cases do
        match parse sql with
        | NumericValueFunction(fn, [ { Kind = Identifier "X" } ])
        | NumericValueFunction(fn, [ { Kind = Identifier "A" } ])
        | NumericValueFunction(fn, [ { Kind = Identifier "C" } ]) -> Assert.Equal(expected, fn)
        | res -> Assert.Fail(sprintf "Expected NumericValueFunction %A for %s, got %A" expected sql res)

[<Fact>]
let ``Multi argument numeric value functions verification`` () =
    match parse "SELECT MOD(a, b)" with
    | NumericValueFunction(NumericFunction.Modulus, [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected MOD, got %A" res)

    match parse "SELECT POWER(a, b)" with
    | NumericValueFunction(NumericFunction.Power, [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected POWER, got %A" res)

    match parse "SELECT LOG(b, x)" with
    | NumericValueFunction(NumericFunction.GeneralLogarithm, [ { Kind = Identifier "B" }; { Kind = Identifier "X" } ]) ->
        ()
    | res -> Assert.Fail(sprintf "Expected LOG, got %A" res)

    match parse "SELECT WIDTH_BUCKET(a, b, c, d)" with
    | NumericValueFunction(NumericFunction.WidthBucket, [ _; _; _; _ ]) -> ()
    | res -> Assert.Fail(sprintf "Expected WIDTH_BUCKET, got %A" res)

    match parse "SELECT MATCH_NUMBER()" with
    | NumericValueFunction(NumericFunction.MatchNumber, []) -> ()
    | res -> Assert.Fail(sprintf "Expected MATCH_NUMBER, got %A" res)

[<Fact>]
let ``Regex functions verification`` () =
    match parse "SELECT OCCURRENCES_REGEX('a' IN s)" with
    | RegexOccurrences arg ->
        match arg.Pattern with
        | { Kind = Literal(String "a") } -> ()
        | res -> Assert.Fail(sprintf "Expected pattern 'a', got %A" res)

        match arg.Subject with
        | { Kind = Identifier "S" } -> ()
        | res -> Assert.Fail(sprintf "Expected subject s, got %A" res)

        Assert.Equal(None, arg.Flag)
        Assert.Equal(None, arg.Occurrence)
    | res -> Assert.Fail(sprintf "Expected RegexOccurrences, got %A" res)

    match parse "SELECT SUBSTRING_REGEX('a' FLAG 'i' IN s FROM 2 OCCURRENCE 3 GROUP 1)" with
    | RegexSubstring arg ->
        match arg.Flag with
        | Some { Kind = Literal(String "i") } -> ()
        | res -> Assert.Fail(sprintf "Expected flag 'i', got %A" res)

        match arg.Occurrence with
        | Some(RegexOccurrenceNumber { Kind = Literal(Number 3m) }) -> ()
        | res -> Assert.Fail(sprintf "Expected OCCURRENCE 3, got %A" res)

        match arg.CaptureGroup with
        | Some { Kind = Literal(Number 1m) } -> ()
        | res -> Assert.Fail(sprintf "Expected GROUP 1, got %A" res)
    | res -> Assert.Fail(sprintf "Expected RegexSubstring, got %A" res)

    match parse "SELECT TRANSLATE_REGEX('a' IN s WITH 'b' OCCURRENCE ALL)" with
    | RegexTransliterate arg ->
        match arg.Replacement with
        | Some { Kind = Literal(String "b") } -> ()
        | res -> Assert.Fail(sprintf "Expected replacement 'b', got %A" res)

        Assert.Equal(Some RegexOccurrenceAll, arg.Occurrence)
    | res -> Assert.Fail(sprintf "Expected RegexTransliterate, got %A" res)

[<Fact>]
let ``POSITION_REGEX start verification`` () =
    match parse "SELECT POSITION_REGEX('a' IN s)" with
    | RegexPosition(None, _) -> ()
    | res -> Assert.Fail(sprintf "Expected RegexPosition without start, got %A" res)

    match parse "SELECT POSITION_REGEX(START 'a' IN s)" with
    | RegexPosition(Some RegexStartOfString, _) -> ()
    | res -> Assert.Fail(sprintf "Expected RegexPosition START, got %A" res)

    match parse "SELECT POSITION_REGEX(AFTER 'a' IN s)" with
    | RegexPosition(Some RegexAfterMatch, _) -> ()
    | res -> Assert.Fail(sprintf "Expected RegexPosition AFTER, got %A" res)

[<Fact>]
let ``TRIM verification`` () =
    match parse "SELECT TRIM(BOTH ' ' FROM ' abc ')" with
    | Trim(Some Both, Some { Kind = Literal(String " ") }, { Kind = Literal(String " abc ") }) -> ()
    | res -> Assert.Fail(sprintf "Expected Trim, got %A" res)

    match parse "SELECT TRIM(LEADING FROM name)" with
    | Trim(Some Leading, None, { Kind = Identifier "NAME" }) -> ()
    | res -> Assert.Fail(sprintf "Expected TRIM LEADING, got %A" res)

    // 6.32 — `TRIM ( <trim source> )` is a separate production: no specification, no character.
    match parse "SELECT TRIM(name)" with
    | Trim(None, None, { Kind = Identifier "NAME" }) -> ()
    | res -> Assert.Fail(sprintf "Expected the TRIM shorthand, got %A" res)

    // A specification keyword cannot start the shorthand (`BOTH` is reserved).
    parseFails "SELECT TRIM(BOTH) FROM t"

[<Fact>]
let ``SUBSTRING FROM FOR verification`` () =
    match parse "SELECT SUBSTRING(name FROM 2 FOR 3)" with
    | Substring({ Kind = Identifier "NAME" }, { Kind = Literal(Number 2m) }, Some { Kind = Literal(Number 3m) }, None) ->
        ()
    | res -> Assert.Fail(sprintf "Expected Substring, got %A" res)

[<Fact>]
let ``Regex functions reject a non char length unit`` () =
    parseFails "SELECT OCCURRENCES_REGEX('a' IN s USING JUNK)"

[<Fact>]
let ``SUBSTRING rejects a non char length unit`` () =
    parseFails "SELECT SUBSTRING(name FROM 2 USING JUNK)"

[<Fact>]
let ``OVERLAY PLACING verification`` () =
    match parse "SELECT OVERLAY(name PLACING 'x' FROM 2)" with
    | Overlay({ Kind = Identifier "NAME" }, { Kind = Literal(String "x") }, { Kind = Literal(Number 2m) }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Overlay, got %A" res)

[<Fact>]
let ``SUBSTRING SIMILAR ESCAPE verification`` () =
    match parse "SELECT SUBSTRING(src SIMILAR 'a*' ESCAPE '!')" with
    | SubstringSimilar({ Kind = Identifier "SRC" }, { Kind = Literal(String "a*") }, { Kind = Literal(String "!") }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected SubstringSimilar, got %A" res)

[<Fact>]
let ``UPPER and LOWER fold verification`` () =
    match parse "SELECT UPPER(name)" with
    | Fold(FoldFunction.FoldUpper, { Kind = Identifier "NAME" }) -> ()
    | res -> Assert.Fail(sprintf "Expected UPPER fold, got %A" res)

    match parse "SELECT LOWER(name)" with
    | Fold(FoldFunction.FoldLower, { Kind = Identifier "NAME" }) -> ()
    | res -> Assert.Fail(sprintf "Expected LOWER fold, got %A" res)

[<Fact>]
let ``CONVERT and TRANSLATE transcoding verification`` () =
    match parse "SELECT CONVERT(name USING utf8)" with
    | Transcoding({ Kind = Identifier "NAME" }, { Kind = Identifier "UTF8" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Transcoding, got %A" res)

    match parse "SELECT TRANSLATE(name USING latin)" with
    | CharacterTransliteration({ Kind = Identifier "NAME" }, { Kind = Identifier "LATIN" }) -> ()
    | res -> Assert.Fail(sprintf "Expected CharacterTransliteration, got %A" res)

[<Fact>]
let ``NORMALIZE function verification`` () =
    match parse "SELECT NORMALIZE(name)" with
    | NormalizeFunction({ Kind = Identifier "NAME" }, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected NORMALIZE, got %A" res)

    match parse "SELECT NORMALIZE(name, NFC, CHARACTER_LENGTH(10))" with
    | NormalizeFunction({ Kind = Identifier "NAME" },
                        Some Nfc,
                        Some { Kind = LengthExpression(LengthFunction.CharacterLength, _, _) }) -> ()
    | res -> Assert.Fail(sprintf "Expected NORMALIZE with form and length, got %A" res)

[<Fact>]
let ``CLASSIFIER function verification`` () =
    match parse "SELECT CLASSIFIER()" with
    | Classifier None -> ()
    | res -> Assert.Fail(sprintf "Expected Classifier, got %A" res)

    match parse "SELECT CLASSIFIER(A)" with
    | Classifier(Some { Kind = Identifier "A" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Classifier(A), got %A" res)

[<Fact>]
let ``JSON_OBJECT function verification`` () =
    match parse "SELECT JSON_OBJECT('a' VALUE 1)" with
    | JsonObject([ { Name = { Kind = Literal(String "a") }
                     Value = { Kind = Literal(Number 1m) }
                     Key = false } ],
                 None,
                 None,
                 None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonObject, got %A" res)

    match parse "SELECT JSON_OBJECT(KEY 'a' VALUE 1 ABSENT ON NULL WITH UNIQUE KEYS RETURNING VARCHAR(50))" with
    | JsonObject([ { Name = { Kind = Literal(String "a") }
                     Value = { Kind = Literal(Number 1m) }
                     Key = true } ],
                 Some JsonAbsentOnNull,
                 Some true,
                 Some _) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonObject full, got %A" res)

    match parse "SELECT JSON_OBJECT('a' : 1)" with
    | JsonObject([ { Name = { Kind = Literal(String "a") }
                     Value = { Kind = Literal(Number 1m) }
                     Key = false } ],
                 None,
                 None,
                 None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonObject colon form, got %A" res)

[<Fact>]
let ``JSON_ARRAY function verification`` () =
    match parse "SELECT JSON_ARRAY(1, 2, 3)" with
    | JsonArray([ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) }; { Kind = Literal(Number 3m) } ],
                None,
                None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonArray, got %A" res)

    match parse "SELECT JSON_ARRAY(NULL ON NULL RETURNING VARCHAR(50))" with
    | JsonArray([], Some JsonNullOnNull, Some _) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonArray null clause, got %A" res)

[<Fact>]
let ``JSON_QUERY function verification`` () =
    match parse "SELECT JSON_QUERY(doc, '$.x' WITH WRAPPER)" with
    | JsonQuery({ Context = { Kind = Identifier "DOC" } },
                None,
                Some { WithWrapper = true
                       Conditional = None
                       Array = false },
                None,
                None,
                None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonQuery, got %A" res)

    match
        parse
            "SELECT JSON_QUERY(doc, '$.x' RETURNING VARCHAR(100) FORMAT JSON WITHOUT ARRAY WRAPPER OMIT QUOTES ON SCALAR STRING NULL ON EMPTY ERROR ON ERROR)"
    with
    | JsonQuery({ Context = { Kind = Identifier "DOC" } },
                Some { Returning = Varchar { Value = 100; Unit = None }
                       Format = Some(JsonEncoding None) },
                Some { WithWrapper = false
                       Conditional = None
                       Array = true },
                Some Omit,
                Some JsonQueryNull,
                Some JsonQueryError) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonQuery full, got %A" res)

[<Fact>]
let ``Datetime value functions verification`` () =
    match parse "SELECT CURRENT_DATE" with
    | CurrentDate -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentDate, got %A" res)

    match parse "SELECT CURRENT_TIMESTAMP(3)" with
    | CurrentTimestamp(Some 3) -> ()
    | res -> Assert.Fail(sprintf "Expected CurrentTimestamp(3), got %A" res)

    match parse "SELECT LOCALTIME" with
    | LocalTime None -> ()
    | res -> Assert.Fail(sprintf "Expected LocalTime, got %A" res)

[<Fact>]
let ``Datetime difference interval verification`` () =
    match parse "SELECT (ts1 - ts2) DAY TO SECOND" with
    | DatetimeDifference({ Kind = Identifier "TS1" },
                         { Kind = Identifier "TS2" },
                         IntervalQualifier.Range(Day, Second, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected a datetime difference, got %A" res)

    // Without a trailing <interval qualifier> the parenthesized subtraction is a
    // 6.3 <parenthesized value expression> around the plain subtraction.
    match parse "SELECT (ts1 - ts2)" with
    | Parenthesized { Kind = BinaryOp(Subtract, { Kind = Identifier "TS1" }, { Kind = Identifier "TS2" }) } -> ()
    | res -> Assert.Fail(sprintf "Expected a parenthesized subtraction, got %A" res)

    match parse "SELECT (ts1 - ts2) * 2" with
    | BinaryOp(Multiply, { Kind = Parenthesized _ }, { Kind = Literal(Number 2m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected parenthesized subtraction, got %A" res)

    // Only a difference of two datetimes may carry the qualifier: `(a + b) DAY`
    // is not an <interval value expression> and must be rejected.
    parseFails "SELECT ((a + b) DAY)"

[<Fact>]
let ``TRIM_ARRAY function verification`` () =
    match parse "SELECT TRIM_ARRAY(arr, 2)" with
    | TrimArray({ Kind = Identifier "ARR" }, { Kind = Literal(Number 2m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected TrimArray, got %A" res)

[<Fact>]
let ``ARRAY value constructor verification`` () =
    match parse "SELECT ARRAY[1, 2, 3]" with
    | ArrayConstructor [ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) }; { Kind = Literal(Number 3m) } ] ->
        ()
    | res -> Assert.Fail(sprintf "Expected ArrayConstructor, got %A" res)

    match parse "SELECT ARRAY(SELECT id FROM t)" with
    | ArrayQuery _ -> ()
    | res -> Assert.Fail(sprintf "Expected ArrayQuery, got %A" res)

[<Fact>]
let ``Empty collection specification verification`` () =
    // 6.42/6.45 — the enumeration forms require at least one element; the empty
    // forms are only 6.5 <empty specification>s.
    parseFails "SELECT ARRAY[]"
    parseFails "SELECT MULTISET[]"

    match parse "SELECT ARRAY[1]" with
    | ArrayConstructor [ { Kind = Literal(Number 1m) } ] -> ()
    | res -> Assert.Fail(sprintf "Expected ArrayConstructor [1], got %A" res)

[<Fact>]
let ``SET multiset set function verification`` () =
    match parse "SELECT SET(m)" with
    | MultisetSetFunction { Kind = Identifier "M" } -> ()
    | res -> Assert.Fail(sprintf "Expected MultisetSetFunction, got %A" res)

    match parse "SELECT SET(m1 MULTISET UNION m2)" with
    | MultisetSetFunction { Kind = MultisetSetOperation(MultisetUnion, None, _, _) } -> ()
    | res -> Assert.Fail(sprintf "Expected MultisetSetFunction with union, got %A" res)

[<Fact>]
let ``MULTISET value constructor verification`` () =
    match parse "SELECT MULTISET[1, 2]" with
    | MultisetConstructor [ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) } ] -> ()
    | res -> Assert.Fail(sprintf "Expected MultisetConstructor, got %A" res)

    match parse "SELECT MULTISET(SELECT id FROM t)" with
    | MultisetQuery _ -> ()
    | res -> Assert.Fail(sprintf "Expected MultisetQuery, got %A" res)

[<Fact>]
let ``TABLE table value constructor by query verification`` () =
    match parse "SELECT TABLE (SELECT id FROM t)" with
    | TableQuery _ -> ()
    | res -> Assert.Fail(sprintf "Expected TableQuery, got %A" res)

[<Fact>]
let ``Row value constructor verification`` () =
    // 7.1 <explicit row value constructor> ::= ( <row value constructor element> <comma>
    //     <row value constructor element list> ) | ROW ( <row value constructor element list> )
    match parse "SELECT (1, 2)" with
    | RowValueConstructor [ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) } ] -> ()
    | res -> Assert.Fail(sprintf "Expected (1, 2), got %A" res)

    match parse "SELECT ROW(a, b, 3)" with
    | RowValueConstructor [ { Kind = Identifier "A" }; { Kind = Identifier "B" }; { Kind = Literal(Number 3m) } ] -> ()
    | res -> Assert.Fail(sprintf "Expected ROW(a, b, 3), got %A" res)

    // ROW ( ) takes a one-element list; the parenthesized form needs two elements.
    match parse "SELECT ROW(a)" with
    | RowValueConstructor [ { Kind = Identifier "A" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected ROW(a), got %A" res)

    // A one-element parenthesized form is a 6.3 <parenthesized value expression>,
    // not a <row value constructor> (which needs ≥ 2 elements).
    match parse "SELECT (a)" with
    | Parenthesized { Kind = Identifier "A" } -> ()
    | res -> Assert.Fail(sprintf "Expected a parenthesized identifier, got %A" res)

    // 8.2 <comparison predicate> — <row value predicand> on both sides.
    match parse "SELECT (1, 2) = (3, 4)" with
    | BinaryOp(Equal, { Kind = RowValueConstructor [ _; _ ] }, { Kind = RowValueConstructor [ _; _ ] }) -> ()
    | res -> Assert.Fail(sprintf "Expected row comparison, got %A" res)

    // 8.4 <in predicate> — an <in predicate value list> of row value constructors.
    match parse "SELECT (1, 2) IN ((1, 2), (3, 4))" with
    | InList(_, false, [ { Kind = RowValueConstructor [ _; _ ] }; { Kind = RowValueConstructor [ _; _ ] } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected row IN list, got %A" res)

[<Fact>]
let ``Row value constructor invalid forms are rejected`` () =
    parseFails "SELECT ROW()"
    parseFails "SELECT ROW(a,)"

[<Fact>]
let ``Function call verification`` () =
    match parse "SELECT COUNT(*)" with
    | FunctionCall({ Kind = Identifier "COUNT" },
                   false,
                   SqlValueArguments([ { Kind = ExpressionKind.Star } ], None),
                   None,
                   None,
                   None) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(*), got %A" res)

[<Fact>]
let ``Aggregate functions verification`` () =
    match parse "SELECT COUNT(DISTINCT id)" with
    | FunctionCall({ Kind = Identifier "COUNT" },
                   true,
                   SqlValueArguments([ { Kind = Identifier "ID" } ], None),
                   None,
                   None,
                   None) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(DISTINCT id), got %A" res)

[<Fact>]
let ``FILTER clause verification`` () =
    match parse "SELECT COUNT(*) FILTER (WHERE x > 0)" with
    | FunctionCall({ Kind = Identifier "COUNT" },
                   false,
                   SqlValueArguments([ { Kind = ExpressionKind.Star } ], None),
                   None,
                   Some { Kind = BinaryOp(GreaterThan, { Kind = Identifier "X" }, { Kind = Literal(Number 0m) }) },
                   None) -> ()
    | res -> Assert.Fail(sprintf "Expected FILTER clause, got %A" res)

[<Fact>]
let ``WITHIN GROUP verification`` () =
    match parse "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY x)" with
    | FunctionCall({ Kind = Identifier "PERCENTILE_CONT" },
                   false,
                   SqlValueArguments([ { Kind = Literal(Number 0.5m) } ], None),
                   None,
                   None,
                   Some [ { Kind = Identifier "X" }, true, None ]) -> ()
    | res -> Assert.Fail(sprintf "Expected WITHIN GROUP, got %A" res)

[<Fact>]
let ``Reserved function name cannot be called with an unexpected shape`` () =
    parseFails "SELECT ABS(1, 2)"
    parseFails "SELECT TRIM_ARRAY(1)"
    parseFails "SELECT SET()"

[<Fact>]
let ``Reserved words that are not function keywords are rejected as routine names`` () =
    // 6.3 / 10.4 — the routine name is an explicit whitelist of reserved *function*
    // keywords, so a reserved word that starts a dedicated construct cannot degrade to a
    // generic <routine invocation>.
    parseFails "SELECT EXISTS(x)"
    parseFails "SELECT UNIQUE(x)"
    parseFails "SELECT VALUE_OF(x)"

[<Fact>]
let ``Non reserved names are accepted as routine names`` () =
    match parse "SELECT foo(x)" with
    | FunctionCall({ Kind = Identifier "FOO" },
                   false,
                   SqlValueArguments([ { Kind = Identifier "X" } ], None),
                   None,
                   None,
                   None) -> ()
    | res -> Assert.Fail(sprintf "Expected a FunctionCall for foo, got %A" res)

[<Fact>]
let ``JSON_OBJECTAGG function verification`` () =
    match parse "SELECT JSON_OBJECTAGG('a' VALUE x)" with
    | JsonObjectAgg({ Name = { Kind = Literal(String "a") }
                      Value = { Kind = Identifier "X" } },
                    None,
                    None,
                    None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonObjectAgg, got %A" res)

[<Fact>]
let ``JSON_ARRAYAGG function verification`` () =
    match parse "SELECT JSON_ARRAYAGG(x ORDER BY y)" with
    | JsonArrayAgg({ Kind = Identifier "X" }, Some [ { Kind = Identifier "Y" }, true, None ], None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonArrayAgg, got %A" res)

[<Fact>]
let ``RUNNING and FINAL set function verification`` () =
    match parse "SELECT RUNNING SUM(x)" with
    | SetFunction(Some RunningOrFinal.Running, inner) ->
        match inner.Kind with
        | FunctionCall({ Kind = Identifier "SUM" }, false, arguments, None, None, None) ->
            match arguments.Arguments with
            | [ SqlArgumentValue { Kind = Identifier "X" } ] -> ()
            | res -> Assert.Fail(sprintf "Expected the SUM argument, got %A" res)
        | res -> Assert.Fail(sprintf "Expected the SUM FunctionCall, got %A" res)
    | res -> Assert.Fail(sprintf "Expected RUNNING SUM, got %A" res)

    match parse "SELECT FINAL COUNT(*)" with
    | SetFunction(Some RunningOrFinal.Final, { Kind = FunctionCall({ Kind = Identifier "COUNT" }, _, _, _, _, _) }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected FINAL COUNT, got %A" res)

[<Fact>]
let ``AT TIME ZONE and AT LOCAL verification`` () =
    match parse "SELECT x AT LOCAL" with
    | AtTimeZone({ Kind = Identifier "X" }, TimeZoneSpecifier.TimeZoneLocal) -> ()
    | res -> Assert.Fail(sprintf "Expected AT LOCAL, got %A" res)

    match parse "SELECT x AT TIME ZONE y" with
    | AtTimeZone({ Kind = Identifier "X" }, TimeZoneSpecifier.TimeZoneOffset { Kind = Identifier "Y" }) -> ()
    | res -> Assert.Fail(sprintf "Expected AT TIME ZONE, got %A" res)

    match parse "SELECT x AT TIME ZONE INTERVAL '1' HOUR" with
    | AtTimeZone({ Kind = Identifier "X" }, TimeZoneSpecifier.TimeZoneOffset { Kind = Literal(Interval _) }) -> ()
    | res -> Assert.Fail(sprintf "Expected AT TIME ZONE interval, got %A" res)

    // 6.37 <interval primary> uses pValueExpressionPrimary: §8 predicate atoms
    // and the 7.16 '*' wildcard are not <value expression primary>s.
    parseFails "SELECT x AT TIME ZONE EXISTS (SELECT 1)"
    parseFails "SELECT x AT TIME ZONE *"

[<Fact>]
let ``MULTISET set operations verification`` () =
    match parse "SELECT m1 MULTISET UNION ALL m2" with
    | MultisetSetOperation(MultisetUnion, Some true, { Kind = Identifier "M1" }, { Kind = Identifier "M2" }) -> ()
    | res -> Assert.Fail(sprintf "Expected MULTISET UNION ALL, got %A" res)

    match parse "SELECT m1 MULTISET EXCEPT DISTINCT m2" with
    | MultisetSetOperation(MultisetExcept, Some false, _, _) -> ()
    | res -> Assert.Fail(sprintf "Expected MULTISET EXCEPT DISTINCT, got %A" res)

    match parse "SELECT m1 MULTISET INTERSECT m2" with
    | MultisetSetOperation(MultisetIntersect, None, _, _) -> ()
    | res -> Assert.Fail(sprintf "Expected MULTISET INTERSECT, got %A" res)

    // MULTISET INTERSECT binds tighter than MULTISET UNION
    match parse "SELECT m1 MULTISET UNION m2 MULTISET INTERSECT m3" with
    | MultisetSetOperation(MultisetUnion,
                           None,
                           { Kind = Identifier "M1" },
                           { Kind = MultisetSetOperation(MultisetIntersect, None, _, _) }) -> ()
    | res -> Assert.Fail(sprintf "Expected INTERSECT to bind tighter, got %A" res)

    // 6.43 <multiset primary> uses pValueExpressionPrimary: the right operand is a
    // grammar-shaped <value expression primary>, so predicates and the '*' wildcard are
    // rejected there.
    parseFails "SELECT m1 MULTISET UNION *"
    parseFails "SELECT m1 MULTISET UNION EXISTS (SELECT 1)"

[<Fact>]
let ``Binary operations verification`` () =
    match parse "SELECT 1 + 2" with
    | BinaryOp(Add, { Kind = Literal(Number 1m) }, { Kind = Literal(Number 2m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected 1 + 2, got %A" res)

[<Fact>]
let ``Unary operations verification`` () =
    match parse "SELECT -1" with
    | UnaryOp(Minus, { Kind = Literal(Number 1m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected -1, got %A" res)

[<Fact>]
let ``Boolean NOT factor verification`` () =
    match parse "SELECT NOT a = 1" with
    | UnaryOp(Not, { Kind = BinaryOp(Equal, { Kind = Identifier "A" }, { Kind = Literal(Number 1m) }) }) -> ()
    | res -> Assert.Fail(sprintf "Expected NOT (a = 1), got %A" res)

[<Fact>]
let ``Expression precedence verification`` () =
    match parse "SELECT 1 + 2 * 3" with
    | BinaryOp(Add,
               { Kind = Literal(Number 1m) },
               { Kind = BinaryOp(Multiply, { Kind = Literal(Number 2m) }, { Kind = Literal(Number 3m) }) }) -> ()
    | res -> Assert.Fail(sprintf "Precedence fail: %A" res)

[<Fact>]
let ``Array subscript with invalid expression is rejected`` () =
    // Comparisons/boolean operators are not <numeric value expression>
    parseFails "SELECT a[b = c]"
    parseFails "SELECT a[x OR y]"
    parseFails "SELECT a[EXISTS (SELECT 1)]"

[<Fact>]
let ``EXTRACT with invalid field is rejected`` () =
    parseFails "EXTRACT(FOO FROM ts)"
    parseFails "EXTRACT(a + b FROM ts)"
    parseFails "EXTRACT('YEAR' FROM ts)"

[<Fact>]
let ``TRIM_ARRAY with invalid count is rejected`` () =
    // Comparisons/boolean operators are not <numeric value expression>
    parseFails "SELECT TRIM_ARRAY(arr, a = b)"
    parseFails "SELECT TRIM_ARRAY(arr, x OR y)"
    parseFails "SELECT TRIM_ARRAY(arr, EXISTS (SELECT 1))"
