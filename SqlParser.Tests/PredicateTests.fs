module SqlParser.Tests.PredicateTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
// 7.4 <table expression> requires a <from clause>: bare expressions are wrapped
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
let ``Quantified comparison verification`` () =
    match parse "SELECT id = ANY (SELECT id FROM users)" with
    | QuantifiedComparison(Equal, Any, { Kind = Identifier "ID" }, _) -> ()
    | res -> Assert.Fail(sprintf "Expected quantified comparison ANY, got %A" res)

    match parse "SELECT id > ALL (SELECT id FROM users)" with
    | QuantifiedComparison(GreaterThan, All, { Kind = Identifier "ID" }, _) -> ()
    | res -> Assert.Fail(sprintf "Expected quantified comparison ALL, got %A" res)

    match parse "SELECT id = SOME (SELECT id FROM users)" with
    | QuantifiedComparison(Equal, SomeQuantifier, { Kind = Identifier "ID" }, _) -> ()
    | res -> Assert.Fail(sprintf "Expected quantified comparison SOME, got %A" res)

[<Fact>]
let ``Standalone quantified subquery is rejected`` () =
    parseFails "SELECT ANY (SELECT id FROM users)"
    parseFails "SELECT SOME (SELECT id FROM users)"
    parseFails "SELECT x FROM t WHERE ALL (SELECT id FROM users)"

[<Fact>]
let ``ANY and SOME stay usable as routine names`` () =
    // The 8.9 <quantified comparison predicate> term is tried before pRoutineInvocation, so a
    // non-query argument must fall through to the routine-call interpretation, not be rejected.
    match parse "SELECT ANY(x)" with
    | FunctionCall({ Kind = Identifier "ANY" }, false, SqlValueArguments([ { Kind = Identifier "X" } ], None), _, _, _) ->
        ()
    | res -> Assert.Fail(sprintf "Expected ANY(x) routine call, got %A" res)

    match parse "SELECT SOME(x)" with
    | FunctionCall({ Kind = Identifier "SOME" }, false, SqlValueArguments([ { Kind = Identifier "X" } ], None), _, _, _) ->
        ()
    | res -> Assert.Fail(sprintf "Expected SOME(x) routine call, got %A" res)

[<Fact>]
let ``PERIOD value expression verification`` () =
    // 8.20 — PERIOD ( ... ) is a <period predicand>, valid only inside a
    // <period predicate>; it cannot leak as a standalone atom.
    parseFails "SELECT PERIOD (s, e)"

    match parse "SELECT PERIOD (s, e) EQUALS PERIOD (t, u)" with
    | PeriodPredicate(PeriodEquals,
                      { Kind = PeriodValue({ Kind = Identifier "S" }, { Kind = Identifier "E" }) },
                      { Kind = PeriodValue({ Kind = Identifier "T" }, { Kind = Identifier "U" }) }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodEquals, got %A" res)

[<Fact>]
let ``Comparison operands are row value predicands (8.2)`` () =
    // Both operands of a comparison are <row value predicand>s, so the left-associative
    // `opp` must not chain (`a = b = c`) or take a predicate operand (`x = EXISTS (...)`).
    parseFails "SELECT 1 FROM t WHERE a = b = c"
    parseFails "SELECT 1 FROM t WHERE 1 = 2 < 3"
    parseFails "SELECT 1 FROM t WHERE x = EXISTS (SELECT 1 FROM u)"

    // PARENTHESIZED booleans are 6.39 <boolean predicand>s and stay legal.
    match parse "(a = b) = c" with
    | BinaryOp(Equal, { Kind = Parenthesized _ }, { Kind = Identifier "C" }) -> ()
    | res -> Assert.Fail(sprintf "Expected a parenthesized left operand, got %A" res)

    match parse "a = (b = c)" with
    | BinaryOp(Equal, { Kind = Identifier "A" }, { Kind = Parenthesized _ }) -> ()
    | res -> Assert.Fail(sprintf "Expected a parenthesized right operand, got %A" res)

    // The DESUGARED `=` of 6.12 NULLIF and the IsNull of COALESCE must not be caught by
    // the comparison-operand / predicate-left-operand checks.
    parse "NULLIF(1 = 2, 3)" |> ignore
    parse "COALESCE(1 = 2, TRUE)" |> ignore

[<Fact>]
let ``BETWEEN verification`` () =
    match parse "SELECT x BETWEEN 1 AND 10" with
    | ExpressionKind.Between({ Kind = Identifier "X" },
                             false,
                             false,
                             { Kind = Literal(Number 1m) },
                             { Kind = Literal(Number 10m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected Between, got %A" res)

[<Fact>]
let ``Predicate part-2 operands are row value predicands (8.x)`` () =
    // A TOP-LEVEL boolean-producing expression is not a <row value predicand>.
    parseFails "SELECT 1 BETWEEN 1 = 1 AND 2"
    parseFails "SELECT 'a' LIKE 'b' = 'c'"
    parseFails "SELECT 1 IN (1 = 2)"
    parseFails "SELECT x IS DISTINCT FROM 1 = 2"
    parseFails "SELECT x OVERLAPS 1 = 2"

    // 8.5/8.6/8.7 — the pattern/escape slots are *value* expressions: an explicit row value
    // constructor is not one.
    parseFails "SELECT 'a' LIKE (1, 2)"
    parseFails "SELECT 'a' LIKE 'b' ESCAPE (1, 2)"
    parseFails "SELECT 'a' SIMILAR TO (1, 2)"
    parseFails "SELECT 'a' LIKE_REGEX (1, 2)"
    parseFails "SELECT 'a' LIKE_REGEX 'b' FLAG (1, 2)"

    // 8.16/8.17 — the operand is a <multiset value expression>.
    parseFails "SELECT x MEMBER OF (1, 2)"
    parseFails "SELECT x SUBMULTISET OF (1, 2)"
    parseFails "SELECT x MEMBER OF 1 = 2"
    parseFails "SELECT x LIKE_REGEX 'a' FLAG 'i' = 'j'"

    // A PARENTHESIZED boolean expression is a 6.39 <boolean predicand> and stays
    // legal — the AST keeps a Parenthesized node, so the content is not top-level.
    match parse "SELECT x BETWEEN (1 = 1) AND 2" with
    | ExpressionKind.Between({ Kind = Identifier "X" },
                             false,
                             false,
                             { Kind = Parenthesized _ },
                             { Kind = Literal(Number 2m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected Between with a parenthesized operand, got %A" res)

[<Fact>]
let ``IN list verification`` () =
    match parse "SELECT x IN (1, 2, 3)" with
    | InList({ Kind = Identifier "X" },
             false,
             [ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) }; { Kind = Literal(Number 3m) } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected InList, got %A" res)

    // 8.4 <in value list> — an element is a <row value expression>: an explicit row value
    // constructor is one, a term or a parenthesized value expression is not.
    match parse "SELECT x IN (ROW(1, 2), 3)" with
    | InList({ Kind = Identifier "X" },
             false,
             [ { Kind = RowValueConstructor [ _; _ ] }; { Kind = Literal(Number 3m) } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected a row value IN list, got %A" res)

    parseFails "SELECT 1 FROM t WHERE x IN (1 + 1)"
    parseFails "SELECT 1 FROM t WHERE x IN ((1), 2)"
    parseFails "SELECT 1 FROM t WHERE x IN (-1)"

[<Fact>]
let ``IN subquery verification`` () =
    match parse "SELECT x IN (SELECT y FROM t)" with
    | InSubquery({ Kind = Identifier "X" }, false, _) -> ()
    | res -> Assert.Fail(sprintf "Expected InSubquery, got %A" res)

    match parse "SELECT x NOT IN (SELECT y FROM t)" with
    | InSubquery({ Kind = Identifier "X" }, true, _) -> ()
    | res -> Assert.Fail(sprintf "Expected InSubquery NOT, got %A" res)

[<Fact>]
let ``LIKE predicate verification`` () =
    match parse "SELECT x LIKE 'a%'" with
    | Like({ Kind = Identifier "X" }, false, { Kind = Literal(String "a%") }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Like, got %A" res)

    match parse "SELECT x NOT LIKE 'a%' ESCAPE '!'" with
    | Like({ Kind = Identifier "X" }, true, _, Some { Kind = Literal(String "!") }) -> ()
    | res -> Assert.Fail(sprintf "Expected Like NOT ESCAPE, got %A" res)

[<Fact>]
let ``SIMILAR TO predicate verification`` () =
    match parse "SELECT x SIMILAR TO 'a%'" with
    | SimilarTo({ Kind = Identifier "X" }, false, _, None) -> ()
    | res -> Assert.Fail(sprintf "Expected SimilarTo, got %A" res)

    match parse "SELECT x NOT SIMILAR TO 'a%' ESCAPE '!'" with
    | SimilarTo({ Kind = Identifier "X" }, true, _, Some _) -> ()
    | res -> Assert.Fail(sprintf "Expected SimilarTo NOT ESCAPE, got %A" res)

    // Both operands are <character value expression>s, so the part-2 operand check applies
    // (the LIKE branch already used it; SIMILAR TO used the raw expression parser).
    parseFails "SELECT x SIMILAR TO 1 = 1"
    parseFails "SELECT x SIMILAR TO 'a%' ESCAPE 1 = 1"

[<Fact>]
let ``LIKE_REGEX predicate verification`` () =
    match parse "SELECT x LIKE_REGEX 'a.*'" with
    | RegexLike({ Kind = Identifier "X" }, false, { Kind = Literal(String "a.*") }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected RegexLike, got %A" res)

    match parse "SELECT x NOT LIKE_REGEX 'a' FLAG 'i'" with
    | RegexLike({ Kind = Identifier "X" }, true, { Kind = Literal(String "a") }, Some { Kind = Literal(String "i") }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected RegexLike FLAG, got %A" res)

[<Fact>]
let ``IS NULL verification`` () =
    match parse "SELECT x IS NOT NULL" with
    | IsNull({ Kind = Identifier "X" }, true) -> ()
    | res -> Assert.Fail(sprintf "Expected IsNull(true), got %A" res)

[<Fact>]
let ``IS TRUE FALSE UNKNOWN verification`` () =
    match parse "SELECT x IS TRUE" with
    | IsBoolean({ Kind = Identifier "X" }, false, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected IsBoolean TRUE, got %A" res)

    match parse "SELECT x IS NOT FALSE" with
    | IsBoolean({ Kind = Identifier "X" }, true, Some false) -> ()
    | res -> Assert.Fail(sprintf "Expected IsBoolean NOT FALSE, got %A" res)

    match parse "SELECT x IS UNKNOWN" with
    | IsBoolean({ Kind = Identifier "X" }, false, None) -> ()
    | res -> Assert.Fail(sprintf "Expected IsBoolean UNKNOWN, got %A" res)

[<Fact>]
let ``Boolean test requires a boolean primary (6.39)`` () =
    // 6.39 <boolean primary> ::= <predicate> | <boolean predicand>
    //   <boolean predicand> ::= <parenthesized boolean value expression>
    //                         | <nonparenthesized value expression primary>
    // A term is neither, so a boolean test cannot follow it …
    parseFails "SELECT 1 FROM t WHERE 1 + 1 IS TRUE"
    parseFails "SELECT 1 FROM t WHERE -x IS TRUE"
    parseFails "SELECT 1 FROM t WHERE a || b IS TRUE"

    // … and a <boolean test> is not a <boolean primary>, so nothing predicate-shaped follows it.
    parseFails "SELECT 1 FROM t WHERE x IS TRUE IS FALSE"
    parseFails "SELECT 1 FROM t WHERE x IS TRUE IS NULL"

    // A parenthesized boolean value expression is a <boolean predicand>.
    match parse "(a = b) IS TRUE" with
    | IsBoolean({ Kind = Parenthesized _ }, false, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected a parenthesized boolean predicand, got %A" res)

[<Fact>]
let ``IS NORMALIZED predicate verification`` () =
    match parse "SELECT x IS NORMALIZED" with
    | IsNormalized({ Kind = Identifier "X" }, false, None) -> ()
    | res -> Assert.Fail(sprintf "Expected IsNormalized, got %A" res)

    match parse "SELECT x IS NOT NFC NORMALIZED" with
    | IsNormalized({ Kind = Identifier "X" }, true, Some Nfc) -> ()
    | res -> Assert.Fail(sprintf "Expected IsNormalized NFC, got %A" res)

    match parse "SELECT x IS NFKD NORMALIZED" with
    | IsNormalized({ Kind = Identifier "X" }, false, Some Nfkd) -> ()
    | res -> Assert.Fail(sprintf "Expected IsNormalized NFKD, got %A" res)

[<Fact>]
let ``MATCH predicate verification`` () =
    match parse "SELECT x MATCH (SELECT y FROM t)" with
    | Match({ Kind = Identifier "X" }, false, None, _) -> ()
    | res -> Assert.Fail(sprintf "Expected Match, got %A" res)

    match parse "SELECT x MATCH UNIQUE FULL (SELECT y FROM t)" with
    | Match({ Kind = Identifier "X" }, true, Some Full, _) -> ()
    | res -> Assert.Fail(sprintf "Expected Match UNIQUE FULL, got %A" res)

[<Fact>]
let ``OVERLAPS predicate verification`` () =
    match parse "SELECT x OVERLAPS y" with
    | Overlaps({ Kind = Identifier "X" }, { Kind = Identifier "Y" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Overlaps, got %A" res)

[<Fact>]
let ``IS DISTINCT FROM predicate verification`` () =
    match parse "SELECT x IS DISTINCT FROM y FROM t" with
    | IsDistinctFrom({ Kind = Identifier "X" }, false, { Kind = Identifier "Y" }) -> ()
    | res -> Assert.Fail(sprintf "Expected IsDistinctFrom, got %A" res)

    match parse "SELECT x IS NOT DISTINCT FROM y FROM t" with
    | IsDistinctFrom({ Kind = Identifier "X" }, true, { Kind = Identifier "Y" }) -> ()
    | res -> Assert.Fail(sprintf "Expected IsDistinctFrom NOT, got %A" res)

[<Fact>]
let ``MEMBER OF predicate verification`` () =
    match parse "SELECT x MEMBER OF m" with
    | MemberOf({ Kind = Identifier "X" }, false, { Kind = Identifier "M" }) -> ()
    | res -> Assert.Fail(sprintf "Expected MemberOf, got %A" res)

    match parse "SELECT x NOT MEMBER m" with
    | MemberOf({ Kind = Identifier "X" }, true, { Kind = Identifier "M" }) -> ()
    | res -> Assert.Fail(sprintf "Expected MemberOf NOT, got %A" res)

[<Fact>]
let ``SUBMULTISET OF predicate verification`` () =
    match parse "SELECT x SUBMULTISET OF m" with
    | SubmultisetOf({ Kind = Identifier "X" }, false, { Kind = Identifier "M" }) -> ()
    | res -> Assert.Fail(sprintf "Expected SubmultisetOf, got %A" res)

    match parse "SELECT x NOT SUBMULTISET m" with
    | SubmultisetOf({ Kind = Identifier "X" }, true, { Kind = Identifier "M" }) -> ()
    | res -> Assert.Fail(sprintf "Expected SubmultisetOf NOT, got %A" res)

[<Fact>]
let ``IS A SET predicate verification`` () =
    match parse "SELECT x IS A SET" with
    | IsSet({ Kind = Identifier "X" }, false) -> ()
    | res -> Assert.Fail(sprintf "Expected IsSet, got %A" res)

    match parse "SELECT x IS NOT A SET" with
    | IsSet({ Kind = Identifier "X" }, true) -> ()
    | res -> Assert.Fail(sprintf "Expected IsSet NOT, got %A" res)

[<Fact>]
let ``IS OF type predicate verification`` () =
    match parse "SELECT x IS OF (t)" with
    | IsOfType({ Kind = Identifier "X" }, false, [ Inclusive { Kind = Identifier "T" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected IsOfType, got %A" res)

    match parse "SELECT x IS NOT OF (ONLY t1, t2)" with
    | IsOfType({ Kind = Identifier "X" },
               true,
               [ Exclusive { Kind = Identifier "T1" }; Inclusive { Kind = Identifier "T2" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected IsOfType ONLY, got %A" res)

[<Fact>]
let ``Period predicate verification`` () =
    match parse "SELECT p1 EQUALS p2" with
    | PeriodPredicate(PeriodEquals, { Kind = Identifier "P1" }, { Kind = Identifier "P2" }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodEquals, got %A" res)

    match parse "SELECT p1 CONTAINS PERIOD (s, e)" with
    | PeriodPredicate(PeriodContains,
                      { Kind = Identifier "P1" },
                      { Kind = PeriodValue({ Kind = Identifier "S" }, { Kind = Identifier "E" }) }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodContains, got %A" res)

    // 8.20 — the slots are <datetime value expression>s, so the 6.35 chains stay legal.
    match parse "SELECT p1 CONTAINS PERIOD (s + INTERVAL '1' DAY, e)" with
    | PeriodPredicate(PeriodContains, { Kind = Identifier "P1" }, { Kind = PeriodValue _ }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodContains with datetime bounds, got %A" res)

    match parse "SELECT p1 IMMEDIATELY PRECEDES p2" with
    | PeriodPredicate(PeriodImmediatelyPrecedes, { Kind = Identifier "P1" }, { Kind = Identifier "P2" }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodImmediatelyPrecedes, got %A" res)

    match parse "SELECT p1 SUCCEEDS p2" with
    | PeriodPredicate(PeriodSucceeds, { Kind = Identifier "P1" }, { Kind = Identifier "P2" }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodSucceeds, got %A" res)

    // 8.20 <period reference> is a <basic identifier chain> — qualified names
    // are valid on both sides of a non-CONTAINS period predicate.
    match parse "SELECT a.p EQUALS b.p2" with
    | PeriodPredicate(PeriodEquals,
                      { Kind = ColumnReference [ "A"; "P" ] },
                      { Kind = ColumnReference [ "B"; "P2" ] }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodEquals with qualified names, got %A" res)

    // The left operand of a <period predicate> is a <period predicand> — a <period
    // reference> (a plain identifier chain) or PERIOD ( start, end ).
    parseFails "SELECT p1 FROM t WHERE 3 EQUALS PERIOD (s, e)"
    parseFails "SELECT p1 FROM t WHERE 1 + 2 PRECEDES PERIOD (s, e)"
    parseFails "SELECT p1 FROM t WHERE 1 = 2 SUCCEEDS PERIOD (s, e)"

    // 8.20 <period start value>/<period end value> are <datetime value expression>s —
    // boolean predicates and other non-datetime operators are rejected in both slots.
    parseFails "SELECT PERIOD (s = e, f) EQUALS p FROM t"
    parseFails "SELECT p CONTAINS PERIOD (s IS NULL, f) FROM t"

[<Fact>]
let ``IS JSON predicate verification`` () =
    match parse "SELECT x IS JSON" with
    | IsJson({ Kind = Identifier "X" }, None, false, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected IsJson, got %A" res)

    match parse "SELECT x IS NOT JSON VALUE WITH UNIQUE KEYS" with
    | IsJson({ Kind = Identifier "X" }, None, true, Some JsonTypeValue, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected IsJson VALUE, got %A" res)

    match parse "SELECT x IS JSON ARRAY WITHOUT UNIQUE" with
    | IsJson({ Kind = Identifier "X" }, None, false, Some JsonTypeArray, Some false) -> ()
    | res -> Assert.Fail(sprintf "Expected IsJson ARRAY, got %A" res)

    // 8.22 <JSON predicate> ::= <string value expression> [ <JSON input clause> ] IS ...
    match parse "SELECT x FORMAT JSON IS JSON" with
    | IsJson({ Kind = Identifier "X" }, Some(JsonEncoding None), false, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected IsJson with input clause, got %A" res)

    match parse "SELECT x FORMAT JSON ENCODING UTF16 IS NOT JSON SCALAR" with
    | IsJson({ Kind = Identifier "X" }, Some(JsonEncoding(Some Utf16)), true, Some JsonTypeScalar, None) -> ()
    | res -> Assert.Fail(sprintf "Expected IsJson UTF16, got %A" res)

[<Fact>]
let ``COLLATE verification`` () =
    match parse "SELECT name COLLATE \"C\"" with
    | Collate({ Kind = Identifier "NAME" }, { Kind = Identifier "C" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Collate, got %A" res)

[<Fact>]
let ``When operands are row value predicands (6.12)`` () =
    parseFails "SELECT CASE x WHEN 1 AND 2 THEN 1 END"

    // A PARENTHESIZED boolean form is a 6.39 <boolean predicand> and stays legal
    // (the Parenthesized node keeps the parens).
    match parse "SELECT CASE x WHEN (1 = 1) THEN 1 ELSE 0 END" with
    | Case(Some { Kind = Identifier "X" }, [ ({ Kind = Parenthesized _ }, { Kind = Literal(Number 1m) }) ], _) -> ()
    | res -> Assert.Fail(sprintf "Expected simple CASE with a parenthesized when operand, got %A" res)

    // A non-boolean parenthesized operand stays legal.
    match parse "SELECT CASE x WHEN (1 + 2) THEN 1 ELSE 0 END" with
    | Case(Some { Kind = Identifier "X" }, _, _) -> ()
    | res -> Assert.Fail(sprintf "Expected simple CASE, got %A" res)

[<Fact>]
let ``When operand predicate part-2 forms are applied to the case operand (6.12)`` () =
    // 6.12 <when operand> includes the 8.x predicate part-2 forms; the <case operand>
    // supplies the missing part 1, so the case is represented as a searched case.
    match parse "CASE x WHEN = 1 THEN 2 END" with
    | Case(None,
           [ ({ Kind = BinaryOp(Equal, { Kind = Identifier "X" }, { Kind = Literal(Number 1m) }) },
              { Kind = Literal(Number 2m) }) ],
           None) -> ()
    | res -> Assert.Fail(sprintf "Expected the comparison part 2 applied to the case operand, got %A" res)

    match parse "CASE x WHEN IS NULL THEN 2 END" with
    | Case(None, [ ({ Kind = IsNull({ Kind = Identifier "X" }, false) }, { Kind = Literal(Number 2m) }) ], None) -> ()
    | res -> Assert.Fail(sprintf "Expected the null predicate part 2, got %A" res)

    match parse "CASE x WHEN BETWEEN 1 AND 2 THEN 2 END" with
    | Case(None, [ ({ Kind = ExpressionKind.Between({ Kind = Identifier "X" }, false, false, _, _) }, _) ], None) -> ()
    | res -> Assert.Fail(sprintf "Expected the between predicate part 2, got %A" res)

    match parse "CASE x WHEN LIKE 'a%' THEN 2 END" with
    | Case(None, [ ({ Kind = Like({ Kind = Identifier "X" }, false, _, _) }, _) ], None) -> ()
    | res -> Assert.Fail(sprintf "Expected the like predicate part 2, got %A" res)

    match parse "CASE x WHEN = ANY (SELECT 1 FROM u) THEN 2 END" with
    | Case(None, [ ({ Kind = QuantifiedComparison(Equal, Any, { Kind = Identifier "X" }, _) }, _) ], None) -> ()
    | res -> Assert.Fail(sprintf "Expected the quantified comparison part 2, got %A" res)

    // A when operand list distributes: `WHEN 1, = 2` ≡ `WHEN x = 1 OR x = 2`.
    match parse "CASE x WHEN 1, = 2 THEN 3 END" with
    | Case(None,
           [ ({ Kind = BinaryOp(Or, { Kind = BinaryOp(Equal, _, _) }, { Kind = BinaryOp(Equal, _, _) }) }, _) ],
           None) -> ()
    | res -> Assert.Fail(sprintf "Expected the OR of the distributed operands, got %A" res)

    // The alternatives 6.12 does not list stay rejected.
    parseFails "SELECT CASE x WHEN IS TRUE THEN 2 END FROM t"
    parseFails "SELECT CASE x WHEN IS DISTINCT FROM 1 THEN 2 END FROM t"
    parseFails "SELECT CASE x WHEN IS A SET THEN 2 END FROM t"

    // A bare boolean when operand / case operand is not a <row value predicand>.
    parseFails "SELECT CASE x WHEN 1 = 1 THEN 2 END FROM t"
    parseFails "SELECT CASE 1 = 2 WHEN 1 THEN 2 END FROM t"

[<Fact>]
let ``Predicate part-1 left operands are row value predicands (8.x)`` () =
    // Every 8.x predicate's left operand is a <row value predicand>, so a predicate may not
    // sit on a boolean result. 6.39 `IS [NOT] TRUE|FALSE|UNKNOWN` is the exception — its
    // <boolean primary> legitimately includes a predicate.
    parseFails "SELECT 1 FROM t WHERE 1 = 2 IS NULL"
    parseFails "SELECT 1 FROM t WHERE 1 = 2 BETWEEN 1 AND 2"
    parseFails "SELECT 1 FROM t WHERE 1 = 2 LIKE 'a'"
    parseFails "SELECT 1 FROM t WHERE x LIKE 'a' IS NULL"
    parseFails "SELECT 1 FROM t WHERE 1 BETWEEN 1 AND 2 IS NULL"
    parseFails "SELECT 1 FROM t WHERE x IS NULL IS NULL"
    parseFails "SELECT 1 FROM t WHERE EXISTS (SELECT 1 FROM u) IS NULL"
    parseFails "SELECT 1 FROM t WHERE 1 = 2 COLLATE c"
    parseFails "SELECT 1 FROM t WHERE 1 = 2 IS JSON"
    parseFails "SELECT 1 FROM t WHERE 1 = 2 OVERLAPS x"
    parseFails "SELECT 1 FROM t WHERE 1 = 2 MEMBER OF m"
    parseFails "SELECT 1 FROM t WHERE JSON_EXISTS(doc, '$.a') IS NULL"

    match parse "(1 = 2) IS NULL" with
    | IsNull({ Kind = Parenthesized _ }, false) -> ()
    | res -> Assert.Fail(sprintf "Expected a parenthesized boolean left operand, got %A" res)

    match parse "x IS NULL IS TRUE" with
    | IsBoolean({ Kind = IsNull _ }, false, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected a boolean test on a predicate, got %A" res)

    match parse "EXISTS (SELECT 1 FROM u) IS TRUE" with
    | IsBoolean({ Kind = ExpressionKind.Exists _ }, false, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected a boolean test on EXISTS, got %A" res)

    match parse "JSON_EXISTS(doc, '$.a') IS TRUE" with
    | IsBoolean({ Kind = JsonExists _ }, false, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected a boolean test on JSON_EXISTS, got %A" res)

[<Fact>]
let ``EXISTS predicate verification`` () =
    match parse "SELECT EXISTS (SELECT 1 FROM t)" with
    | ExpressionKind.Exists _ -> ()
    | res -> Assert.Fail(sprintf "Expected Exists, got %A" res)

[<Fact>]
let ``UNIQUE predicate verification`` () =
    match parse "SELECT UNIQUE (SELECT 1 FROM t)" with
    | ExpressionKind.Unique _ -> ()
    | res -> Assert.Fail(sprintf "Expected Unique, got %A" res)

[<Fact>]
let ``JSON_EXISTS predicate verification`` () =
    match parse "SELECT JSON_EXISTS(doc, '$.x')" with
    | JsonExists({ Context = { Kind = Identifier "DOC" } }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonExists, got %A" res)

    match parse "SELECT JSON_EXISTS(doc, '$.x' TRUE ON ERROR)" with
    | JsonExists({ Context = { Kind = Identifier "DOC" } }, Some JsonExistsTrue) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonExists ON ERROR, got %A" res)
