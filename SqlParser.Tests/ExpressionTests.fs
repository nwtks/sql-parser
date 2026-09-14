module SqlParser.Tests.ExpressionTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
let parseExpr (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok { Kind = Select(SelectQuery s) } -> s.Columns.[0] |> fun (Column(e, _)) -> e
    | Ok res -> failwithf "Expected Select, got %A" res
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parse sql = (parseExpr sql).Kind

let parseFails (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

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
let ``Exact numeric type variants are parsed`` () =
    match parse "SELECT CAST(x AS DECIMAL(10,2))" with
    | Cast(_, Decimal(Some 10, Some 2)) -> ()
    | res -> Assert.Fail(sprintf "Expected DECIMAL(10,2), got %A" res)

    match parse "SELECT CAST(x AS DEC(5))" with
    | Cast(_, Decimal(Some 5, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected DEC(5), got %A" res)

    match parse "SELECT CAST(x AS DECFLOAT(34))" with
    | Cast(_, DecFloat(Some 34)) -> ()
    | res -> Assert.Fail(sprintf "Expected DECFLOAT(34), got %A" res)

    match parse "SELECT CAST(x AS NUMERIC)" with
    | Cast(_, Numeric(None, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected NUMERIC, got %A" res)

[<Fact>]
let ``Approximate numeric type variants are parsed`` () =
    match parse "SELECT CAST(x AS FLOAT)" with
    | Cast(_, Float None) -> ()
    | res -> Assert.Fail(sprintf "Expected FLOAT, got %A" res)

    match parse "SELECT CAST(x AS FLOAT(24))" with
    | Cast(_, Float(Some 24)) -> ()
    | res -> Assert.Fail(sprintf "Expected FLOAT(24), got %A" res)

    match parse "SELECT CAST(x AS REAL)" with
    | Cast(_, Real) -> ()
    | res -> Assert.Fail(sprintf "Expected REAL, got %A" res)

    match parse "SELECT CAST(x AS DOUBLE PRECISION)" with
    | Cast(_, DoublePrecision) -> ()
    | res -> Assert.Fail(sprintf "Expected DOUBLE PRECISION, got %A" res)

[<Fact>]
let ``Datetime type variants are parsed`` () =
    match parse "SELECT CAST(x AS TIME)" with
    | Cast(_, TimeType(None, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME, got %A" res)

    match parse "SELECT CAST(x AS TIME(3))" with
    | Cast(_, TimeType(Some 3, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME(3), got %A" res)

    match parse "SELECT CAST(x AS TIME WITH TIME ZONE)" with
    | Cast(_, TimeType(None, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME WITH TIME ZONE, got %A" res)

    match parse "SELECT CAST(x AS TIME WITHOUT TIME ZONE)" with
    | Cast(_, TimeType(None, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected TIME WITHOUT TIME ZONE, got %A" res)

    match parse "SELECT CAST(x AS TIMESTAMP(3) WITH TIME ZONE)" with
    | Cast(_, TimestampType(Some 3, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected TIMESTAMP(3) WITH TIME ZONE, got %A" res)

[<Fact>]
let ``Literal expressions verification`` () =
    Assert.Equal(Literal(Number 123m), parse "SELECT 123")
    Assert.Equal(Literal(String "hello"), parse "SELECT 'hello'")
    Assert.Equal(Literal(Bool(Some true)), parse "SELECT TRUE")
    Assert.Equal(Literal Null, parse "SELECT NULL")

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
                       [ { Kind = Identifier "X" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected MethodInvocation, got %A" res)

[<Fact>]
let ``Method invocation on parenthesized expression verification`` () =
    match parse "SELECT (a.b).prune(x)" with
    | MethodInvocation({ Kind = ColumnReference [ "A"; "B" ] },
                       { Kind = Identifier "PRUNE" },
                       [ { Kind = Identifier "X" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected MethodInvocation on parenthesized, got %A" res)

[<Fact>]
let ``Field reference verification`` () =
    match parse "SELECT a.obj.prune(x).field" with
    | FieldReference({ Kind = MethodInvocation({ Kind = ColumnReference [ "A"; "OBJ" ] },
                                               { Kind = Identifier "PRUNE" },
                                               [ { Kind = Identifier "X" } ]) },
                     { Kind = Identifier "FIELD" }) -> ()
    | res -> Assert.Fail(sprintf "Expected FieldReference, got %A" res)

[<Fact>]
let ``Method invocation on last chain segment verification`` () =
    match parse "SELECT a.b.c(x)" with
    | MethodInvocation({ Kind = ColumnReference [ "A"; "B" ] }, { Kind = Identifier "C" }, [ { Kind = Identifier "X" } ]) ->
        ()
    | res -> Assert.Fail(sprintf "Expected MethodInvocation on last segment, got %A" res)

    parseFails "SELECT 1 + DEFAULT"

[<Fact>]
let ``Generalized method invocation verification`` () =
    match parse "SELECT (x AS mytype).m()" with
    | GeneralizedInvocation({ Kind = Identifier "X" },
                            UserDefinedType { Kind = Identifier "MYTYPE" },
                            { Kind = Identifier "M" },
                            Some []) -> ()
    | res -> Assert.Fail(sprintf "Expected GeneralizedInvocation, got %A" res)

    match parse "SELECT (x AS mytype).m(1)" with
    | GeneralizedInvocation(_, _, { Kind = Identifier "M" }, Some [ { Kind = Literal(Number 1m) } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected GeneralizedInvocation with args, got %A" res)

    match parse "SELECT (x AS mytype).m" with
    | GeneralizedInvocation(_, _, { Kind = Identifier "M" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected GeneralizedInvocation without args, got %A" res)

[<Fact>]
let ``Static method invocation verification`` () =
    match parse "SELECT my_type::prune(x)" with
    | StaticMethodInvocation({ Kind = Identifier "MY_TYPE" },
                             { Kind = Identifier "PRUNE" },
                             [ { Kind = Identifier "X" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected StaticMethodInvocation, got %A" res)

[<Fact>]
let ``NEW specification verification`` () =
    match parse "SELECT NEW my_type(1, 2)" with
    | NewSpecification({ Kind = Identifier "MY_TYPE" }, [ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) } ]) ->
        ()
    | res -> Assert.Fail(sprintf "Expected NewSpecification, got %A" res)

[<Fact>]
let ``Dereference operation verification`` () =
    match parse "SELECT x -> attr" with
    | Dereference({ Kind = Identifier "X" }, { Kind = Identifier "ATTR" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Dereference attribute, got %A" res)

    match parse "SELECT x -> m(1)" with
    | Dereference({ Kind = Identifier "X" }, { Kind = Identifier "M" }, Some [ { Kind = Literal(Number 1m) } ]) -> ()
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
                  Passing = [ ({ Kind = Identifier "A" }, { Kind = Identifier "P" }) ] },
                None,
                None,
                None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonValue PASSING, got %A" res)

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

[<Fact>]
let ``SUBSTRING FROM FOR verification`` () =
    match parse "SELECT SUBSTRING(name FROM 2 FOR 3)" with
    | Substring({ Kind = Identifier "NAME" }, { Kind = Literal(Number 2m) }, Some { Kind = Literal(Number 3m) }, None) ->
        ()
    | res -> Assert.Fail(sprintf "Expected Substring, got %A" res)

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
                Some { Returning = Varchar(Some 100)
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

    // Without a trailing <interval qualifier> the parenthesized subtraction is unchanged.
    match parse "SELECT (ts1 - ts2)" with
    | BinaryOp(Subtract, { Kind = Identifier "TS1" }, { Kind = Identifier "TS2" }) -> ()
    | res -> Assert.Fail(sprintf "Expected plain subtraction, got %A" res)

    match parse "SELECT (ts1 - ts2) * 2" with
    | BinaryOp(Multiply, { Kind = BinaryOp(Subtract, _, _) }, { Kind = Literal(Number 2m) }) -> ()
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
    match parse "SELECT ARRAY[]" with
    | ArrayConstructor [] -> ()
    | res -> Assert.Fail(sprintf "Expected ArrayConstructor [], got %A" res)

    match parse "SELECT MULTISET[]" with
    | MultisetConstructor [] -> ()
    | res -> Assert.Fail(sprintf "Expected MultisetConstructor [], got %A" res)

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

    // A one-element parenthesized expression is not a row value constructor.
    match parse "SELECT (a)" with
    | Identifier "A" -> ()
    | res -> Assert.Fail(sprintf "Expected a, got %A" res)

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
    | FunctionCall({ Kind = Identifier "COUNT" }, false, [ { Kind = ExpressionKind.Star } ], None, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(*), got %A" res)

[<Fact>]
let ``Aggregate functions verification`` () =
    match parse "SELECT COUNT(DISTINCT id)" with
    | FunctionCall({ Kind = Identifier "COUNT" }, true, [ { Kind = Identifier "ID" } ], None, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(DISTINCT id), got %A" res)

[<Fact>]
let ``FILTER clause verification`` () =
    match parse "SELECT COUNT(*) FILTER (WHERE x > 0)" with
    | FunctionCall({ Kind = Identifier "COUNT" },
                   false,
                   [ { Kind = ExpressionKind.Star } ],
                   None,
                   Some { Kind = BinaryOp(GreaterThan, { Kind = Identifier "X" }, { Kind = Literal(Number 0m) }) },
                   None) -> ()
    | res -> Assert.Fail(sprintf "Expected FILTER clause, got %A" res)

[<Fact>]
let ``WITHIN GROUP verification`` () =
    match parse "SELECT PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY x)" with
    | FunctionCall({ Kind = Identifier "PERCENTILE_CONT" },
                   false,
                   [ { Kind = Literal(Number 0.5m) } ],
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
    | FunctionCall({ Kind = Identifier "FOO" }, false, [ { Kind = Identifier "X" } ], None, None, None) -> ()
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
    | SetFunction(Some RunningOrFinal.Running,
                  { Kind = FunctionCall({ Kind = Identifier "SUM" },
                                        false,
                                        [ { Kind = Identifier "X" } ],
                                        None,
                                        None,
                                        None) }) -> ()
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
