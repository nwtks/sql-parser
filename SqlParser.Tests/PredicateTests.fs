module SqlParser.Tests.PredicateTests

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
    | FunctionCall({ Kind = Identifier "ANY" }, false, [ { Kind = Identifier "X" } ], _, _, _) -> ()
    | res -> Assert.Fail(sprintf "Expected ANY(x) routine call, got %A" res)

    match parse "SELECT SOME(x)" with
    | FunctionCall({ Kind = Identifier "SOME" }, false, [ { Kind = Identifier "X" } ], _, _, _) -> ()
    | res -> Assert.Fail(sprintf "Expected SOME(x) routine call, got %A" res)

[<Fact>]
let ``PERIOD value expression verification`` () =
    match parse "SELECT PERIOD (s, e)" with
    | PeriodValue({ Kind = Identifier "S" }, { Kind = Identifier "E" }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodValue, got %A" res)

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
let ``IN list verification`` () =
    match parse "SELECT x IN (1, 2, 3)" with
    | InList({ Kind = Identifier "X" },
             false,
             [ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) }; { Kind = Literal(Number 3m) } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected InList, got %A" res)

[<Fact>]
let ``IN subquery verification`` () =
    match parse "SELECT x IN (SELECT y FROM t)" with
    | InSubquery({ Kind = Identifier "X" }, false, _) -> ()
    | res -> Assert.Fail(sprintf "Expected InSubquery, got %A" res)

    match parse "SELECT x NOT IN (SELECT y FROM t)" with
    | InSubquery({ Kind = Identifier "X" }, true, _) -> ()
    | res -> Assert.Fail(sprintf "Expected InSubquery NOT, got %A" res)

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
let ``IS DISTINCT FROM predicate verification`` () =
    match parse "SELECT x IS DISTINCT FROM y" with
    | IsDistinctFrom({ Kind = Identifier "X" }, false, { Kind = Identifier "Y" }) -> ()
    | res -> Assert.Fail(sprintf "Expected IsDistinctFrom, got %A" res)

    match parse "SELECT x IS NOT DISTINCT FROM y" with
    | IsDistinctFrom({ Kind = Identifier "X" }, true, { Kind = Identifier "Y" }) -> ()
    | res -> Assert.Fail(sprintf "Expected IsDistinctFrom NOT, got %A" res)

[<Fact>]
let ``OVERLAPS predicate verification`` () =
    match parse "SELECT x OVERLAPS y" with
    | Overlaps({ Kind = Identifier "X" }, { Kind = Identifier "Y" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Overlaps, got %A" res)

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

[<Fact>]
let ``COLLATE verification`` () =
    match parse "SELECT name COLLATE \"C\"" with
    | Collate({ Kind = Identifier "NAME" }, { Kind = Identifier "C" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Collate, got %A" res)

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
let ``IS JSON predicate verification`` () =
    match parse "SELECT x IS JSON" with
    | IsJson({ Kind = Identifier "X" }, false, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected IsJson, got %A" res)

    match parse "SELECT x IS NOT JSON VALUE WITH UNIQUE KEYS" with
    | IsJson({ Kind = Identifier "X" }, true, Some JsonTypeValue, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected IsJson VALUE, got %A" res)

    match parse "SELECT x IS JSON ARRAY WITHOUT UNIQUE" with
    | IsJson({ Kind = Identifier "X" }, false, Some JsonTypeArray, Some false) -> ()
    | res -> Assert.Fail(sprintf "Expected IsJson ARRAY, got %A" res)

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
let ``MATCH predicate verification`` () =
    match parse "SELECT x MATCH (SELECT y FROM t)" with
    | Match({ Kind = Identifier "X" }, false, None, _) -> ()
    | res -> Assert.Fail(sprintf "Expected Match, got %A" res)

    match parse "SELECT x MATCH UNIQUE FULL (SELECT y FROM t)" with
    | Match({ Kind = Identifier "X" }, true, Some Full, _) -> ()
    | res -> Assert.Fail(sprintf "Expected Match UNIQUE FULL, got %A" res)

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
let ``Period predicate verification`` () =
    match parse "SELECT p1 EQUALS p2" with
    | PeriodPredicate(PeriodEquals, { Kind = Identifier "P1" }, { Kind = Identifier "P2" }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodEquals, got %A" res)

    match parse "SELECT p1 CONTAINS PERIOD (s, e)" with
    | PeriodPredicate(PeriodContains,
                      { Kind = Identifier "P1" },
                      { Kind = PeriodValue({ Kind = Identifier "S" }, { Kind = Identifier "E" }) }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodContains, got %A" res)

    match parse "SELECT p1 IMMEDIATELY PRECEDES p2" with
    | PeriodPredicate(PeriodImmediatelyPrecedes, { Kind = Identifier "P1" }, { Kind = Identifier "P2" }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodImmediatelyPrecedes, got %A" res)

    match parse "SELECT p1 SUCCEEDS p2" with
    | PeriodPredicate(PeriodSucceeds, { Kind = Identifier "P1" }, { Kind = Identifier "P2" }) -> ()
    | res -> Assert.Fail(sprintf "Expected PeriodSucceeds, got %A" res)

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
