module SqlParser.Tests.QueryTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
let parse (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseFails (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

[<Fact>]
let ``Unicode identifier verification`` () =
    match parse "SELECT U&\"id\" FROM t" with
    | Select(SelectQuery q) ->
        match q.Columns with
        | [ Column({ Kind = Identifier "id" }, _) ] -> ()
        | res -> Assert.Fail(sprintf "Expected Identifier ID, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``ORDER BY NULLS verification`` () =
    let parseOrder sql =
        match parse sql with
        | Select(SelectQuery s) -> s.OrderBy.[0]
        | res -> failwithf "Expected Select, got %A" res

    match parseOrder "SELECT id FROM t ORDER BY id DESC NULLS FIRST" with
    | { Kind = Identifier "ID" }, false, Some NullsFirst -> ()
    | res -> Assert.Fail(sprintf "Expected id DESC NULLS FIRST, got %A" res)

    match parseOrder "SELECT id FROM t ORDER BY name NULLS LAST" with
    | { Kind = Identifier "NAME" }, true, Some NullsLast -> ()
    | res -> Assert.Fail(sprintf "Expected name ASC NULLS LAST, got %A" res)

[<Fact>]
let ``Window functions verification`` () =
    match parse "SELECT SUM(salary) OVER (ORDER BY id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) FROM t" with
    | Select(SelectQuery s) ->
        match s.Columns.[0] with
        | Column({ Kind = WindowFunction { Function = { Kind = Identifier "SUM" }
                                           Args = [ { Kind = Identifier "SALARY" } ]
                                           IsDistinct = false
                                           Window = { ExistingWindowName = None
                                                      PartitionBy = []
                                                      OrderBy = [ ({ Kind = Identifier "ID" }, true, _) ]
                                                      Frame = Some { Unit = Rows
                                                                     Start = UnboundedPreceding
                                                                     End = Some CurrentRow } } } },
                 _) -> ()
        | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT AVG(price) OVER (ORDER BY dt RANGE BETWEEN 1 PRECEDING AND 1 FOLLOWING) FROM t" with
    | Select(SelectQuery s) ->
        match s.Columns.[0] with
        | Column({ Kind = WindowFunction { Function = { Kind = Identifier "AVG" }
                                           Args = [ { Kind = Identifier "PRICE" } ]
                                           IsDistinct = false
                                           Window = { ExistingWindowName = None
                                                      PartitionBy = []
                                                      OrderBy = [ { Kind = Identifier "DT" }, true, None ]
                                                      Frame = Some { Unit = Range
                                                                     Start = Preceding { Kind = Literal(Number 1m) }
                                                                     End = Some(Following { Kind = Literal(Number 1m) }) } } } },
                 _) -> ()
        | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``MATCH_RECOGNIZE row pattern quantifiers verification`` () =
    match parse "SELECT * FROM t MATCH_RECOGNIZE (PATTERN (A{2,3} B{2}) DEFINE A AS a > 0, B AS b > 0)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = MatchRecognize(_, recog, _) } ] ->
            match recog.Common.Pattern.Terms with
            | [ { Factors = [ factorA; factorB ] } ] ->
                match factorA.Primary, factorA.Quantifier with
                | RowPatternVariable { Kind = Identifier "A" },
                  Some(RowPatternQuantifier.Brace(Some lo, Some hi, false)) ->
                    Assert.Equal(Literal(Number 2m), lo.Kind)
                    Assert.Equal(Literal(Number 3m), hi.Kind)
                | res -> Assert.Fail(sprintf "Expected A{2,3}, got %A" res)

                match factorB.Primary, factorB.Quantifier with
                | RowPatternVariable { Kind = Identifier "B" }, Some(RowPatternQuantifier.BraceExact exact) ->
                    Assert.Equal(Literal(Number 2m), exact.Kind)
                | res -> Assert.Fail(sprintf "Expected B{2}, got %A" res)
            | res -> Assert.Fail(sprintf "Expected two factors, got %A" res)
        | res -> Assert.Fail(sprintf "Expected MatchRecognize, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``MATCH_RECOGNIZE plus and question quantifiers verification`` () =
    match
        parse
            "SELECT * FROM t MATCH_RECOGNIZE (PATTERN (A+ B+? C? D??) DEFINE A AS a > 0, B AS b > 0, C AS c > 0, D AS d > 0)"
    with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = MatchRecognize(_, recog, _) } ] ->
            match recog.Common.Pattern.Terms with
            | [ { Factors = [ a; b; c; d ] } ] ->
                Assert.Equal(Some(RowPatternQuantifier.Plus false), a.Quantifier)
                Assert.Equal(Some(RowPatternQuantifier.Plus true), b.Quantifier)
                Assert.Equal(Some(RowPatternQuantifier.Question false), c.Quantifier)
                Assert.Equal(Some(RowPatternQuantifier.Question true), d.Quantifier)
            | res -> Assert.Fail(sprintf "Expected four factors, got %A" res)
        | res -> Assert.Fail(sprintf "Expected MatchRecognize, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``MATCH_RECOGNIZE row pattern anchors verification`` () =
    match parse "SELECT * FROM t MATCH_RECOGNIZE (PATTERN (^ A $) DEFINE A AS a > 0)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = MatchRecognize(_, recog, _) } ] ->
            match recog.Common.Pattern.Terms with
            | [ { Factors = [ startFactor; aFactor; endFactor ] } ] ->
                Assert.Equal(RowPatternAnchorStart, startFactor.Primary)

                match aFactor.Primary with
                | RowPatternVariable { Kind = Identifier "A" } -> ()
                | res -> Assert.Fail(sprintf "Expected variable A, got %A" res)

                Assert.Equal(RowPatternAnchorEnd, endFactor.Primary)
            | res -> Assert.Fail(sprintf "Expected three factors, got %A" res)
        | res -> Assert.Fail(sprintf "Expected MatchRecognize, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``MATCH_RECOGNIZE row pattern alternation verification`` () =
    match parse "SELECT * FROM t MATCH_RECOGNIZE (PATTERN (A | B) DEFINE A AS a > 0, B AS b > 0)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = MatchRecognize(_, recog, _) } ] ->
            match recog.Common.Pattern.Terms with
            | [ { Factors = [ factorA ] }; { Factors = [ factorB ] } ] ->
                match factorA.Primary, factorB.Primary with
                | RowPatternVariable { Kind = Identifier "A" }, RowPatternVariable { Kind = Identifier "B" } -> ()
                | res -> Assert.Fail(sprintf "Expected A | B, got %A" res)
            | res -> Assert.Fail(sprintf "Expected two terms, got %A" res)
        | res -> Assert.Fail(sprintf "Expected MatchRecognize, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``MATCH_RECOGNIZE row pattern exclusion verification`` () =
    match parse "SELECT * FROM t MATCH_RECOGNIZE (PATTERN (A {- B -}) DEFINE A AS a > 0, B AS b > 0)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = MatchRecognize(_, recog, _) } ] ->
            match recog.Common.Pattern.Terms with
            | [ { Factors = [ factorA; excludeFactor ] } ] ->
                match factorA.Primary with
                | RowPatternVariable { Kind = Identifier "A" } -> ()
                | res -> Assert.Fail(sprintf "Expected variable A, got %A" res)

                match excludeFactor.Primary with
                | RowPatternExclude { Terms = [ { Factors = [ { Primary = RowPatternVariable { Kind = Identifier "B" } } ] } ] } ->
                    ()
                | res -> Assert.Fail(sprintf "Expected exclusion of B, got %A" res)
            | res -> Assert.Fail(sprintf "Expected two factors, got %A" res)
        | res -> Assert.Fail(sprintf "Expected MatchRecognize, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``WITH SEARCH clause verification`` () =
    match parse "WITH RECURSIVE t(n) AS (SELECT 1 FROM u) SEARCH DEPTH FIRST BY n SET ord SELECT * FROM t" with
    | WithStatement(true, [ cte ], _) ->
        match cte.SearchClause with
        | Some { IsDepthFirst = true
                 OrderBy = [ { Kind = Identifier "N" } ]
                 SetColumn = { Kind = Identifier "ORD" } } -> ()
        | res -> Assert.Fail(sprintf "Expected WITH SEARCH DEPTH, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "WITH t AS (SELECT 1 FROM u) SEARCH BREADTH FIRST BY n SET ord SELECT * FROM t" with
    | WithStatement(false, [ cte ], _) ->
        match cte.SearchClause with
        | Some { IsDepthFirst = false } -> ()
        | res -> Assert.Fail(sprintf "Expected WITH SEARCH BREADTH, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``WITH CYCLE clause verification`` () =
    match
        parse "WITH RECURSIVE t(n) AS (SELECT 1 FROM u) CYCLE n SET is_cycle TO 1 DEFAULT 0 USING path SELECT * FROM t"
    with
    | WithStatement(true, [ cte ], _) ->
        match cte.CycleClause with
        | Some { CycleColumns = [ { Kind = Identifier "N" } ]
                 SetColumn = { Kind = Identifier "IS_CYCLE" }
                 MarkValue = { Kind = Literal(Number 1m) }
                 DefaultValue = { Kind = Literal(Number 0m) }
                 PathColumn = { Kind = Identifier "PATH" } } -> ()
        | res -> Assert.Fail(sprintf "Expected WITH CYCLE, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``interval term accepts a numeric factor on the right of an operator (6.37)`` () =
    match parse "SELECT * FROM t FOR SYSTEM_TIME AS OF CURRENT_DATE + INTERVAL '1' DAY * 2" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.AsOf { Kind = BinaryOp(Add, _, { Kind = BinaryOp(Multiply, _, _) }) }) } ] ->
            ()
        | res -> Assert.Fail(sprintf "Expected an interval term with a numeric factor, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``interval term accepts an interval factor on the right of an operator (6.37)`` () =
    // The 4th alternative <term> <asterisk> <interval factor> admits a qualifier on the
    // right (<interval primary> ::= <value expression primary> [ <interval qualifier> ]).
    // Interval- vs numeric-valued operands are syntactically indistinguishable, so the
    // distinction is semantic (see docs/trade-off.md).
    match parse "SELECT * FROM t FOR SYSTEM_TIME AS OF CURRENT_DATE + INTERVAL '1' DAY * ? DAY" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.AsOf { Kind = BinaryOp(Add, _, { Kind = BinaryOp(Multiply, _, _) }) }) } ] ->
            ()
        | res -> Assert.Fail(sprintf "Expected an interval term with an interval factor, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Table value constructor as query verification`` () =
    match parse "VALUES (1, 'a'), (2, 'b')" with
    | Select(TableValueConstructor [ [ { Kind = Literal(Number 1m) }; { Kind = Literal(String "a") } ]
                                     [ { Kind = Literal(Number 2m) }; { Kind = Literal(String "b") } ] ]) -> ()
    | res -> Assert.Fail(sprintf "Expected TableValueConstructor, got %A" res)

[<Fact>]
let ``TABLESAMPLE SYSTEM verification`` () =
    match parse "SELECT * FROM users TABLESAMPLE SYSTEM (10)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = TableSample({ Kind = TableSourceKind.Table({ Kind = Identifier "USERS" }, None) },
                                 "SYSTEM",
                                 { Kind = Literal(Number 10m) },
                                 None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected TableSample SYSTEM, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``TABLESAMPLE non-standard method is rejected`` () =
    parseFails "SELECT * FROM users TABLESAMPLE RANDOM (10)"

[<Fact>]
let ``TABLESAMPLE verification`` () =
    match parse "SELECT * FROM users TABLESAMPLE BERNOULLI (10)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = TableSample({ Kind = TableSourceKind.Table({ Kind = Identifier "USERS" }, None) },
                                 "BERNOULLI",
                                 { Kind = Literal(Number 10m) },
                                 None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected TableSample, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``FOR SYSTEM_TIME verification`` () =
    match parse "SELECT * FROM t FOR SYSTEM_TIME AS OF '2020-01-01'" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime({ Kind = TableSourceKind.Table({ Kind = Identifier "T" }, None) }, SystemTimeSpec.AsOf _) } ] ->
            ()
        | res -> Assert.Fail(sprintf "Expected SystemTime AsOf, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    // 6.35 <datetime value expression> — arithmetic on a point in time.
    match parse "SELECT * FROM t FOR SYSTEM_TIME AS OF CURRENT_DATE - INTERVAL '1' DAY" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.AsOf e) } ] ->
            match e.Kind with
            | BinaryOp(Subtract, _, _) -> ()
            | res -> Assert.Fail(sprintf "Expected datetime arithmetic, got %A" res)
        | res -> Assert.Fail(sprintf "Expected SystemTime AsOf, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    // 6.37 <interval primary> — a parameter qualified by an <interval qualifier>.
    match parse "SELECT * FROM t FOR SYSTEM_TIME AS OF CURRENT_DATE - ? DAY" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.AsOf e) } ] ->
            match e.Kind with
            | BinaryOp(Subtract, _, { Kind = IntervalPrimary(_, _) }) -> ()
            | res -> Assert.Fail(sprintf "Expected IntervalPrimary point in time, got %A" res)
        | res -> Assert.Fail(sprintf "Expected SystemTime AsOf, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    // 6.37 <interval factor> ::= [ <sign> ] <interval primary>
    match parse "SELECT * FROM t FOR SYSTEM_TIME AS OF CURRENT_DATE + -INTERVAL '1' DAY" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.AsOf e) } ] ->
            match e.Kind with
            | BinaryOp(Add, _, { Kind = UnaryOp(Minus, _) }) -> ()
            | res -> Assert.Fail(sprintf "Expected signed interval, got %A" res)
        | res -> Assert.Fail(sprintf "Expected SystemTime AsOf, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    // 6.35 <time zone> ::= AT <time zone specifier>
    match parse "SELECT * FROM t FOR SYSTEM_TIME AS OF CURRENT_TIMESTAMP AT TIME ZONE INTERVAL '1' HOUR" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.AsOf e) } ] ->
            match e.Kind with
            | AtTimeZone(_, TimeZoneSpecifier.TimeZoneOffset _) -> ()
            | res -> Assert.Fail(sprintf "Expected AT TIME ZONE point in time, got %A" res)
        | res -> Assert.Fail(sprintf "Expected SystemTime AsOf, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t FOR SYSTEM_TIME BETWEEN '2020-01-01' AND '2020-02-01'" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.Between(_, _, None)) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected SystemTime Between, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t FOR SYSTEM_TIME BETWEEN SYMMETRIC '2020-01-01' AND '2020-02-01'" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.Between(_, _, Some SystemTimeSymmetry.Symmetric)) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected SystemTime Between Symmetric, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t FOR SYSTEM_TIME BETWEEN ASYMMETRIC '2020-01-01' AND '2020-02-01'" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.Between(_, _, Some SystemTimeSymmetry.Asymmetric)) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected SystemTime Between Asymmetric, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t FOR SYSTEM_TIME FROM '2020-01-01' TO '2020-02-01'" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = SystemTime(_, SystemTimeSpec.FromTo(_, _)) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected SystemTime FromTo, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``FOR SYSTEM_TIME point in time rejects non datetime operators`` () =
    parseFails "SELECT * FROM t FOR SYSTEM_TIME AS OF a * b"
    parseFails "SELECT * FROM t FOR SYSTEM_TIME AS OF a || b"
    parseFails "SELECT * FROM t FOR SYSTEM_TIME AS OF a = b"
    parseFails "SELECT * FROM t FOR SYSTEM_TIME AS OF TRUE"
    parseFails "SELECT * FROM t FOR SYSTEM_TIME AS OF FALSE"
    parseFails "SELECT * FROM t FOR SYSTEM_TIME AS OF UNKNOWN"

[<Fact>]
let ``MATCH_RECOGNIZE verification`` () =
    match
        parse
            "SELECT * FROM t MATCH_RECOGNIZE (PARTITION BY a ORDER BY b MEASURES x AS m ALL ROWS PER MATCH PATTERN (A B*) DEFINE A AS a > 0)"
    with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = MatchRecognize(input, recog, output) } ] ->
            match input with
            | Some({ Kind = Identifier "T" }, None) -> ()
            | res -> Assert.Fail(sprintf "Expected input name T, got %A" res)

            Assert.Equal(None, output)

            match recog.PartitionBy with
            | [ { Kind = Identifier "A" } ] -> ()
            | res -> Assert.Fail(sprintf "Expected partition by, got %A" res)

            match recog.OrderBy with
            | [ { Kind = Identifier "B" }, true, None ] -> ()
            | res -> Assert.Fail(sprintf "Expected order by, got %A" res)

            match recog.Measures with
            | [ { Expression = { Kind = Identifier "X" }
                  Name = { Kind = Identifier "M" } } ] -> ()
            | res -> Assert.Fail(sprintf "Expected measures, got %A" res)

            Assert.Equal(Some(AllRowsPerMatch None), recog.RowsPerMatch)

            match recog.Common.Pattern with
            | { Terms = [ { Factors = [ { Primary = RowPatternVariable { Kind = Identifier "A" }
                                          Quantifier = None }
                                        { Primary = RowPatternVariable { Kind = Identifier "B" }
                                          Quantifier = Some(RowPatternQuantifier.Star false) } ] } ] } -> ()
            | res -> Assert.Fail(sprintf "Expected pattern, got %A" res)

            match recog.Common.Define with
            | [ { Name = { Kind = Identifier "A" }
                  Condition = { Kind = BinaryOp(GreaterThan, { Kind = Identifier "A" }, { Kind = Literal(Number 0m) }) } } ] ->
                ()
            | res -> Assert.Fail(sprintf "Expected define, got %A" res)
        | res -> Assert.Fail(sprintf "Expected MatchRecognize, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``MATCH_RECOGNIZE partition rejects non column references`` () =
    parseFails "SELECT * FROM t MATCH_RECOGNIZE (PARTITION BY a + 1 PATTERN (A) DEFINE A AS a > 0)"

[<Fact>]
let ``MATCH_RECOGNIZE full clauses verification`` () =
    match
        parse
            "SELECT * FROM t MATCH_RECOGNIZE (AFTER MATCH SKIP TO NEXT ROW INITIAL PATTERN (A B) SUBSET S = (A, B) DEFINE A AS a > 0, B AS b > 0)"
    with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = MatchRecognize(Some({ Kind = Identifier "T" }, None), recog, None) } ] ->
            match recog.Common with
            | { AfterMatch = Some SkipToNextRow
                InitialOrSeek = Some true
                Pattern = { Terms = [ { Factors = [ { Primary = RowPatternVariable { Kind = Identifier "A" }
                                                      Quantifier = None }
                                                    { Primary = RowPatternVariable { Kind = Identifier "B" }
                                                      Quantifier = None } ] } ] }
                Subset = subset
                Define = define } ->
                match subset with
                | [ { Name = { Kind = Identifier "S" }
                      Variables = [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ] } ] -> ()
                | res -> Assert.Fail(sprintf "Expected subset, got %A" res)

                match define with
                | [ { Name = { Kind = Identifier "A" }
                      Condition = _ }
                    { Name = { Kind = Identifier "B" }
                      Condition = _ } ] -> ()
                | res -> Assert.Fail(sprintf "Expected define, got %A" res)
            | res -> Assert.Fail(sprintf "Expected common, got %A" res)
        | res -> Assert.Fail(sprintf "Expected MatchRecognize, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``MATCH_RECOGNIZE input output names verification`` () =
    // OUT is a reserved word, so the output name must be a non-reserved identifier.
    // <row pattern input name> is a <correlation name> — "t" IS the input name;
    // a separate "t AS inp" table+alias pair is not part of the grammar.
    match parse "SELECT * FROM t MATCH_RECOGNIZE (PATTERN (A) DEFINE A AS a > 0) AS out_t" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = MatchRecognize(input, _, output) } ] ->
            match input with
            | Some(inp, cols) ->
                match inp with
                | { Kind = Identifier "T" } -> ()
                | res -> Assert.Fail(sprintf "Expected input name T, got %A" res)

                Assert.Equal(None, cols)
            | res -> Assert.Fail(sprintf "Expected input name T, got %A" res)

            match output with
            | Some(out, cols) ->
                match out with
                | { Kind = Identifier "OUT_T" } -> ()
                | res -> Assert.Fail(sprintf "Expected output name OUT_T, got %A" res)

                Assert.Equal(None, cols)
            | res -> Assert.Fail(sprintf "Expected output name, got %A" res)
        | res -> Assert.Fail(sprintf "Expected MatchRecognize, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``MATCH_RECOGNIZE missing DEFINE is rejected`` () =
    parseFails "SELECT * FROM t MATCH_RECOGNIZE (PATTERN (A))"

[<Fact>]
let ``JSON_TABLE formatted column verification`` () =
    match parse "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (c VARCHAR(100) FORMAT JSON WITH WRAPPER)) AS jt" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JsonTable(stmt, Some _) } ] ->
            match stmt.Columns with
            | [ JsonFormatted { Name = { Kind = Identifier "C" }
                                DataType = Varchar { Value = 100; Unit = None }
                                Format = JsonEncoding None
                                Path = None
                                Wrapper = Some { WithWrapper = true
                                                 Conditional = None
                                                 Array = false }
                                Quotes = None
                                OnEmpty = None
                                OnError = None } ] -> ()
            | res -> Assert.Fail(sprintf "Expected JsonFormatted, got %A" res)
        | res -> Assert.Fail(sprintf "Expected JsonTable, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``JSON_TABLE regular columns reject formatted-only clauses`` () =
    // 7.11 — WRAPPER and EMPTY ARRAY/OBJECT belong only to a formatted column.
    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT WITH WRAPPER))"
    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT EMPTY ARRAY ON EMPTY))"
    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT EMPTY OBJECT ON ERROR))"

[<Fact>]
let ``JSON_TABLE formatted columns reject a DEFAULT behavior`` () =
    // 7.11 — DEFAULT belongs only to a regular column's empty/error behavior.
    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT FORMAT JSON DEFAULT 1 ON EMPTY))"
    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT FORMAT JSON DEFAULT 1 ON ERROR))"

[<Fact>]
let ``JSON_TABLE column kinds are not interchangeable with JSON_TABLE_PRIMITIVE`` () =
    // 7.11 — NESTED is only a <JSON table column definition>; FOR CHAINING only a
    // <JSON table primitive column definition>.
    parseFails "SELECT * FROM JSON_TABLE_PRIMITIVE(doc, '$' COLUMNS (NESTED PATH '$.a' COLUMNS (x INT)) ERROR ON ERROR)"

    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (v FOR CHAINING))"

[<Fact>]
let ``JSON_TABLE nested columns verification`` () =
    match
        parse
            "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT, NESTED PATH '$.items' AS it COLUMNS (b VARCHAR(10)))) AS jt"
    with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JsonTable(stmt, Some _) } ] ->
            match stmt.Columns with
            | [ JsonRegular { Name = { Kind = Identifier "A" }
                              DataType = Integer
                              Path = None
                              OnEmpty = None
                              OnError = None }
                JsonNested { Path = "$.items"
                             Name = Some { Kind = Identifier "IT" }
                             Columns = nested } ] ->
                match nested with
                | [ JsonRegular { Name = { Kind = Identifier "B" }
                                  DataType = Varchar { Value = 10; Unit = None }
                                  Path = None
                                  OnEmpty = None
                                  OnError = None } ] -> ()
                | res -> Assert.Fail(sprintf "Expected nested columns, got %A" res)
            | res -> Assert.Fail(sprintf "Expected JsonNested, got %A" res)
        | res -> Assert.Fail(sprintf "Expected JsonTable, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``JSON_TABLE plan clause verification`` () =
    match parse "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN (p OUTER q)) AS jt" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JsonTable(stmt, Some _) } ] ->
            match stmt.Plan with
            | Some(JsonPlanOuter(name, JsonPlanPrimaryName qname)) ->
                match name with
                | { Kind = Identifier "P" } -> ()
                | res -> Assert.Fail(sprintf "Expected plan name P, got %A" res)

                match qname with
                | { Kind = Identifier "Q" } -> ()
                | res -> Assert.Fail(sprintf "Expected plan name Q, got %A" res)
            | res -> Assert.Fail(sprintf "Expected JsonPlanOuter, got %A" res)
        | res -> Assert.Fail(sprintf "Expected JsonTable, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN DEFAULT (INNER, UNION)) AS jt" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JsonTable(stmt, Some _) } ] ->
            match stmt.Plan with
            | Some(JsonPlanDefault { InnerOuter = Some "INNER"
                                     UnionCross = Some "UNION" }) -> ()
            | res -> Assert.Fail(sprintf "Expected JsonPlanDefault, got %A" res)
        | res -> Assert.Fail(sprintf "Expected JsonTable, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``JSON_TABLE plan primary forms verification`` () =
    let planOf (sql: string) =
        match parse sql with
        | Select(SelectQuery s) ->
            match s.From with
            | [ { Kind = JsonTable(stmt, Some _) } ] -> stmt.Plan
            | res -> failwithf "Expected JsonTable, got %A" res
        | res -> failwithf "Expected Select, got %A" res

    match planOf "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN (p INNER q)) AS jt" with
    | Some(JsonPlanInner(_, JsonPlanPrimaryName _)) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonPlanInner, got %A" res)

    match planOf "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN (p UNION q)) AS jt" with
    | Some(JsonPlanUnion [ JsonPlanPrimaryName _; JsonPlanPrimaryName _ ]) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonPlanUnion, got %A" res)

    match planOf "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN (p CROSS q)) AS jt" with
    | Some(JsonPlanCross [ JsonPlanPrimaryName _; JsonPlanPrimaryName _ ]) -> ()
    | res -> Assert.Fail(sprintf "Expected JsonPlanCross, got %A" res)

    match planOf "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN (p CROSS q CROSS r)) AS jt" with
    | Some(JsonPlanCross [ JsonPlanPrimaryName _; JsonPlanPrimaryName _; JsonPlanPrimaryName _ ]) -> ()
    | res -> Assert.Fail(sprintf "Expected a three-operand JsonPlanCross, got %A" res)

    // <JSON table plan> ::= <JSON table path name> | ... — a single path name is a
    // valid plan (a one-element UNION is NOT).
    match planOf "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN (p)) AS jt" with
    | Some(JsonPlanName _) -> ()
    | res -> Assert.Fail(sprintf "Expected a single-name plan, got %A" res)

    // A parenthesized plan is a <plan primary>, not a <plan> — PLAN takes a plan,
    // so PLAN ((p OUTER q)) is rejected.
    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN ((p OUTER q)))"

    match planOf "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN DEFAULT (UNION, INNER)) AS jt" with
    | Some(JsonPlanDefault { InnerOuter = Some "INNER"
                             UnionCross = Some "UNION" }) -> ()
    | res -> Assert.Fail(sprintf "Expected reversed JsonPlanDefault, got %A" res)

[<Fact>]
let ``JSON_TABLE error behavior verification`` () =
    match parse "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) EMPTY ON ERROR) AS jt" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JsonTable({ OnError = Some JsonTableEmpty }, Some _) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected JsonTableEmpty, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``JSON_TABLE verification`` () =
    match
        parse
            "SELECT * FROM JSON_TABLE(doc, '$.a' COLUMNS (id FOR ORDINALITY, name VARCHAR(50) PATH '$.name' NULL ON EMPTY ERROR ON ERROR) ERROR ON ERROR) AS jt"
    with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JsonTable(stmt, corr) } ] ->
            match stmt.Common with
            | { Context = { Kind = Identifier "DOC" }
                Path = "$.a"
                PathName = None
                Passing = [] } -> ()
            | res -> Assert.Fail(sprintf "Expected JsonApiCommon, got %A" res)

            match stmt.Columns with
            | [ JsonOrdinality { Kind = Identifier "ID" }
                JsonRegular { Name = { Kind = Identifier "NAME" }
                              DataType = Varchar { Value = 50; Unit = None }
                              Path = Some "$.name"
                              OnEmpty = Some JsonColumnNull
                              OnError = Some JsonColumnError } ] -> ()
            | res -> Assert.Fail(sprintf "Expected columns, got %A" res)

            Assert.Equal(None, stmt.Plan)
            Assert.Equal(Some JsonTableError, stmt.OnError)

            match corr with
            | Some(name, cols) ->
                match name with
                | { Kind = Identifier "JT" } -> ()
                | res -> Assert.Fail(sprintf "Expected alias JT, got %A" res)

                Assert.Equal(None, cols)
            | res -> Assert.Fail(sprintf "Expected correlation, got %A" res)
        | res -> Assert.Fail(sprintf "Expected JsonTable, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``JSON_TABLE missing columns is rejected`` () =
    parseFails "SELECT * FROM JSON_TABLE(doc, '$.a')"

[<Fact>]
let ``JSON_TABLE_PRIMITIVE verification`` () =
    match
        parse
            "SELECT * FROM JSON_TABLE_PRIMITIVE(doc, '$' COLUMNS (id FOR ORDINALITY, v FOR CHAINING) ERROR ON ERROR) AS jtp"
    with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JsonTablePrimitive(stmt, Some name) } ] ->
            match name with
            | { Kind = Identifier "JTP" } -> ()
            | res -> Assert.Fail(sprintf "Expected alias JTP, got %A" res)

            match stmt.Columns with
            | [ JsonOrdinality { Kind = Identifier "ID" }; JsonChaining { Kind = Identifier "V" } ] -> ()
            | res -> Assert.Fail(sprintf "Expected primitive columns, got %A" res)

            Assert.Equal(None, stmt.Plan)
            Assert.Equal(Some JsonTableError, stmt.OnError)
        | res -> Assert.Fail(sprintf "Expected JsonTablePrimitive, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``TableSource types verification`` () =
    match parse "SELECT * FROM users" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = TableSourceKind.Table({ Kind = Identifier "USERS" }, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Table, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM users AS u" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = TableSourceKind.Table({ Kind = Identifier "USERS" }, Some { Kind = Identifier "U" }) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Table, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM (SELECT id FROM users) sub" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = Subquery(SelectQuery { Columns = [ Column({ Kind = Identifier "ID" }, None) ] },
                              { Kind = Identifier "SUB" },
                              None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Subquery, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Subquery, got %A" res)

[<Fact>]
let ``Schema-qualified table name verification`` () =
    match parse "SELECT * FROM app.users" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = TableSourceKind.Table({ Kind = ColumnReference [ "APP"; "USERS" ] }, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected schema-qualified table, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``LATERAL derived table verification`` () =
    match parse "SELECT * FROM LATERAL (SELECT id FROM users) AS u" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = Lateral(_, { Kind = Identifier "U" }, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Lateral, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``LATERAL without alias is rejected`` () =
    parseFails "SELECT * FROM LATERAL (SELECT id FROM users)"

[<Fact>]
let ``UNNEST derived table verification`` () =
    match parse "SELECT * FROM UNNEST(arr) WITH ORDINALITY AS u" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = Unnest({ Kind = Identifier "ARR" }, true, { Kind = Identifier "U" }, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Unnest, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``UNNEST without alias is rejected`` () = parseFails "SELECT * FROM UNNEST(arr)"

[<Fact>]
let ``TABLE collection expression requires an alias`` () =
    parseFails "SELECT * FROM TABLE(arr)"

    match parse "SELECT * FROM TABLE(arr) AS a" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = TableFunction(_, Some _, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected table function, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``PTF table may omit an alias`` () =
    match parse "SELECT * FROM TABLE(generate_series(1, 3))" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = PtfTable(_, None, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected PTF table, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``ONLY table reference verification`` () =
    match parse "SELECT * FROM ONLY (users)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = Only({ Kind = Identifier "USERS" }, None, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Only, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    // 7.6 <only spec> [ <correlation or recognition> ] — the correlation name is kept.
    match parse "SELECT * FROM ONLY (app.users) AS u (a, b)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = Only(name, alias, cols) } ] ->
            match name with
            | { Kind = ColumnReference [ "APP"; "USERS" ] } -> ()
            | res -> Assert.Fail(sprintf "Expected ONLY name, got %A" res)

            match alias with
            | Some { Kind = Identifier "U" } -> ()
            | res -> Assert.Fail(sprintf "Expected ONLY alias U, got %A" res)

            match cols with
            | Some [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ] -> ()
            | res -> Assert.Fail(sprintf "Expected ONLY column list, got %A" res)
        | res -> Assert.Fail(sprintf "Expected Only with alias, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``TABLE function and PTF derived table verification`` () =
    match parse "SELECT * FROM TABLE (arr) AS t" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = TableFunction({ Kind = Identifier "ARR" }, Some { Kind = Identifier "T" }, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected TableFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM TABLE (generate_series(1, 10)) AS g" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = PtfTable(_, Some { Kind = Identifier "G" }, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected PtfTable, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Delta table without alias is rejected`` () =
    parseFails "SELECT * FROM NEW TABLE (INSERT INTO t VALUES (1))"
    parseFails "SELECT * FROM OLD TABLE (DELETE FROM t WHERE id = 1)"
    parseFails "SELECT * FROM FINAL TABLE (UPDATE t SET id = 1)"

    match parse "SELECT * FROM NEW TABLE (INSERT INTO t VALUES (1)) AS n" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = DataChangeDelta(ResultOption.New, Insert _, Some _, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected DataChangeDelta New, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM OLD TABLE (DELETE FROM t WHERE id = 1) AS o" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = DataChangeDelta(ResultOption.Old, Delete _, Some _, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected DataChangeDelta Old, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM FINAL TABLE (UPDATE t SET id = 1) AS f" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = DataChangeDelta(ResultOption.Final, Update _, Some _, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected DataChangeDelta Final, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    // 7.6 <data change delta table> [ <correlation or recognition> ] — the correlation
    // name is kept.
    match parse "SELECT * FROM OLD TABLE (DELETE FROM t WHERE id = 1) AS d (x)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = DataChangeDelta(ResultOption.Old, Delete _, alias, cols) } ] ->
            match alias with
            | Some { Kind = Identifier "D" } -> ()
            | res -> Assert.Fail(sprintf "Expected delta alias D, got %A" res)

            match cols with
            | Some [ { Kind = Identifier "X" } ] -> ()
            | res -> Assert.Fail(sprintf "Expected delta column list, got %A" res)
        | res -> Assert.Fail(sprintf "Expected DataChangeDelta with alias, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``VALUES as Table Source verification`` () =
    match parse "SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t(id, name)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = ValuesTable(rows,
                                 { Kind = Identifier "T" },
                                 Some [ { Kind = Identifier "ID" }; { Kind = Identifier "NAME" } ]) } ] ->
            Assert.Equal(2, rows.Length)
        | res -> Assert.Fail(sprintf "Expected ValuesTable, got %A" res)
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Subquery with Column Aliases verification`` () =
    match parse "SELECT a, b FROM (SELECT 1, 2 FROM u) AS t(a, b)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = Subquery(_,
                              { Kind = Identifier "T" },
                              Some [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ]) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Subquery with aliases, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Subquery with WITH clause verification`` () =
    match parse "SELECT * FROM (WITH cte AS (SELECT 1 AS val FROM u) SELECT * FROM cte) AS t" with
    | Select(SelectQuery q) ->
        match q.From with
        | [ { Kind = Subquery(WithQuery(false, [ { Name = { Kind = Identifier "CTE" } } ], _),
                              { Kind = Identifier "T" },
                              _) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Subquery with WithQuery, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Join types verification`` () =
    let tests =
        [ "JOIN", InnerJoin
          "INNER JOIN", InnerJoin
          "LEFT JOIN", LeftJoin
          "RIGHT JOIN", RightJoin
          "FULL JOIN", FullJoin ]

    for join, expected in tests do
        match parse (sprintf "SELECT * FROM t1 %s t2 ON t1.id = t2.id" join) with
        | Select(SelectQuery s) ->
            match s.From with
            | [ { Kind = JoinedTable { JoinType = jt } } ] when jt = expected -> ()
            | res -> Assert.Fail(sprintf "Expected Join %A for %s, got %A" expected join res)
        | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``NATURAL CROSS JOIN is rejected`` () =
    parseFails "SELECT * FROM t1 NATURAL CROSS JOIN t2"

[<Fact>]
let ``PARTITION BY join verification`` () =
    match parse "SELECT * FROM t1 PARTITION BY (a, b) JOIN t2 ON t1.id = t2.id" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JoinedTable { PartitionBy = Some [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ] } } ] ->
            ()
        | res -> Assert.Fail(sprintf "Expected PARTITION BY join, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``PARTITION BY join rejects an expression (7.10)`` () =
    // <partitioned join column reference list> is a list of <column reference>s.
    parseFails "SELECT * FROM t1 PARTITION BY (a + b) JOIN t2 ON t1.id = t2.id"

[<Fact>]
let ``NATURAL JOIN and USING verification`` () =
    match parse "SELECT * FROM t1 NATURAL JOIN t2" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JoinedTable { IsNatural = true
                                   JoinType = InnerJoin } } ] -> ()
        | res -> Assert.Fail(sprintf "Expected NATURAL JOIN, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t1 JOIN t2 USING (id, name)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JoinedTable { Condition = Some(Using [ { Kind = Identifier "ID" }; { Kind = Identifier "NAME" } ]) } } ] ->
            ()
        | res -> Assert.Fail(sprintf "Expected JOIN USING, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t1 NATURAL LEFT JOIN t2" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JoinedTable { IsNatural = true
                                   JoinType = LeftJoin } } ] -> ()
        | res -> Assert.Fail(sprintf "Expected NATURAL LEFT JOIN, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``USING with join correlation name verification`` () =
    match parse "SELECT * FROM t1 JOIN t2 USING (id) AS j" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JoinedTable { Condition = Some(Using [ { Kind = Identifier "ID" } ])
                                   UsingAlias = Some { Kind = Identifier "J" } } } ] -> ()
        | res -> Assert.Fail(sprintf "Expected USING AS, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Comma-separated FROM list verification`` () =
    match parse "SELECT * FROM users, orders" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = TableSourceKind.Table({ Kind = Identifier "USERS" }, None) }
            { Kind = TableSourceKind.Table({ Kind = Identifier "ORDERS" }, None) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected two tables, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``SELECT DISTINCT verification`` () =
    match parse "SELECT DISTINCT name FROM users" with
    | Select(SelectQuery s) -> Assert.True(s.IsDistinct)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``GROUP BY grouping elements verification`` () =
    match parse "SELECT a, b FROM t GROUP BY (a, b)" with
    | Select(SelectQuery s) ->
        match s.GroupBy with
        | [ GroupingSet [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ] ] -> ()
        | res -> Assert.Fail(sprintf "Expected GroupingSet (a, b), got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT a FROM t GROUP BY ()" with
    | Select(SelectQuery s) ->
        match s.GroupBy with
        | [ EmptyGroupingSet ] -> ()
        | res -> Assert.Fail(sprintf "Expected EmptyGroupingSet, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT a FROM t GROUP BY DISTINCT a, b" with
    | Select(SelectQuery s) ->
        Assert.True(s.GroupByDistinct)

        match s.GroupBy with
        | [ GroupingSet [ { Kind = Identifier "A" } ]; GroupingSet [ { Kind = Identifier "B" } ] ] -> ()
        | res -> Assert.Fail(sprintf "Expected GroupBy DISTINCT a, b, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT a FROM t GROUP BY GROUPING SETS ((a), (b))" with
    | Select(SelectQuery s) ->
        match s.GroupBy with
        | [ GroupingSets [ GroupingSet [ { Kind = Identifier "A" } ]; GroupingSet [ { Kind = Identifier "B" } ] ] ] ->
            ()
        | res -> Assert.Fail(sprintf "Expected GroupingSets, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``GROUP BY grouping elements reject non column references`` () =
    parseFails "SELECT a FROM t GROUP BY a + 1"
    parseFails "SELECT a FROM t GROUP BY 1"
    parseFails "SELECT a FROM t GROUP BY *"
    parseFails "SELECT a FROM t ROLLUP(a + 1)"
    parseFails "SELECT a FROM t CUBE(a + 1)"
    parseFails "SELECT a FROM t GROUP BY GROUPING SETS ((a + 1))"

[<Fact>]
let ``Window clause verification`` () =
    match parse "SELECT SUM(salary) OVER w FROM emps WINDOW w AS (PARTITION BY dept ORDER BY salary)" with
    | Select(SelectQuery s) ->
        match s.Window.[0] with
        | { Kind = Identifier "W" }, { PartitionBy = [ { Kind = Identifier "DEPT" } ] } -> ()
        | res -> Assert.Fail(sprintf "Expected Window definition, got %A" res)

        match s.Columns.[0] with
        | Column({ Kind = WindowFunction { Function = { Kind = Identifier "SUM" }
                                           Args = [ { Kind = Identifier "SALARY" } ]
                                           IsDistinct = false
                                           Window = { ExistingWindowName = Some { Kind = Identifier "W" }
                                                      PartitionBy = []
                                                      OrderBy = []
                                                      Frame = None } } },
                 _) -> ()
        | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT SUM(a) OVER (w ROWS UNBOUNDED PRECEDING) FROM t WINDOW w AS (PARTITION BY b)" with
    | Select(SelectQuery s) ->
        match s.Window.[0] with
        | { Kind = Identifier "W" }, { PartitionBy = [ { Kind = Identifier "B" } ] } -> ()
        | res -> Assert.Fail(sprintf "Expected Window definition, got %A" res)

        match s.Columns.[0] with
        | Column({ Kind = WindowFunction { Function = { Kind = Identifier "SUM" }
                                           Args = [ { Kind = Identifier "A" } ]
                                           IsDistinct = false
                                           Window = { ExistingWindowName = Some { Kind = Identifier "W" }
                                                      PartitionBy = []
                                                      OrderBy = []
                                                      Frame = Some { Unit = Rows
                                                                     Start = UnboundedPreceding
                                                                     End = None } } } },
                 _) -> ()
        | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Window definition with existing window name verification`` () =
    match parse "SELECT SUM(a) OVER x FROM t WINDOW x AS (w ORDER BY b)" with
    | Select(SelectQuery s) ->
        match s.Window with
        | [ (windowName, def) ] ->
            match windowName.Kind, def.ExistingWindowName with
            | Identifier "X", Some { Kind = Identifier "W" } ->
                match def.OrderBy with
                | [ ({ Kind = Identifier "B" }, _, _) ] -> ()
                | res -> Assert.Fail(sprintf "Expected ORDER BY b, got %A" res)
            | res -> Assert.Fail(sprintf "Expected window x referencing w, got %A" res)
        | res -> Assert.Fail(sprintf "Expected a window definition, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Qualified asterisk verification`` () =
    match parse "SELECT t.* FROM users t" with
    | Select(SelectQuery s) ->
        match s.Columns with
        | [ Column({ Kind = QualifiedStar [ "T" ] }, None) ] -> ()
        | res -> Assert.Fail(sprintf "Expected QualifiedStar, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``All fields reference verification`` () =
    match parse "SELECT t.* AS (a, b) FROM t" with
    | Select(SelectQuery s) ->
        match s.Columns with
        | [ Column({ Kind = AllFieldsReference({ Kind = Identifier "T" },
                                               Some [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ]) },
                   None) ] -> ()
        | res -> Assert.Fail(sprintf "Expected AllFieldsReference, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT s.t.* AS (x) FROM s.t" with
    | Select(SelectQuery s) ->
        match s.Columns with
        | [ Column({ Kind = AllFieldsReference({ Kind = ColumnReference [ "S"; "T" ] },
                                               Some [ { Kind = Identifier "X" } ]) },
                   None) ] -> ()
        | res -> Assert.Fail(sprintf "Expected AllFieldsReference qualified, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    // 7.16 <all fields reference> ::= <value expression primary> <period> <asterisk>
    //     [ AS ( <all fields column name list> ) ]
    match parse "SELECT (a + b).* FROM t" with
    | Select(SelectQuery s) ->
        match s.Columns with
        | [ Column({ Kind = AllFieldsReference({ Kind = Parenthesized _ }, None) }, None) ] -> ()
        | res -> Assert.Fail(sprintf "Expected AllFieldsReference for (a + b).*, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT f(x).* AS (y) FROM t" with
    | Select(SelectQuery s) ->
        match s.Columns with
        | [ Column({ Kind = AllFieldsReference({ Kind = FunctionCall _ }, Some [ { Kind = Identifier "Y" } ]) }, None) ] ->
            ()
        | res -> Assert.Fail(sprintf "Expected AllFieldsReference for f(x).*, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    // 7.16 <all fields reference> uses pValueExpressionPrimary: the primary is a
    // grammar-shaped <value expression primary>, so predicates and the '*' wildcard are
    // rejected before the '.*'.
    parseFails "SELECT EXISTS (SELECT 1).* FROM t"
    parseFails "SELECT ** FROM t"

[<Fact>]
let ``Full SELECT structure verification`` () =
    match
        parse
            "SELECT id, name AS username FROM users u JOIN orders o ON u.id = o.user_id WHERE age > 18 GROUP BY category HAVING num_orders > 5 ORDER BY name DESC"
    with
    | Select(SelectQuery s) ->
        Assert.Equal(2, s.Columns.Length)

        match s.From with
        | [ { Kind = JoinedTable { JoinType = InnerJoin } } ] -> ()
        | res -> Assert.Fail(sprintf "Expected JoinedTable, got %A" res)

        match s.Where with
        | Some { Kind = BinaryOp(GreaterThan, { Kind = Identifier "AGE" }, { Kind = Literal(Number 18m) }) } -> ()
        | res -> Assert.Fail(sprintf "Expected Where age > 18, got %A" res)

        match s.GroupBy with
        | [ GroupingSet [ { Kind = Identifier "CATEGORY" } ] ] -> ()
        | res -> Assert.Fail(sprintf "Expected GroupBy category, got %A" res)

        match s.Having with
        | Some { Kind = BinaryOp(GreaterThan, { Kind = Identifier "NUM_ORDERS" }, { Kind = Literal(Number 5m) }) } -> ()
        | res -> Assert.Fail(sprintf "Expected Having num_orders > 5, got %A" res)

        match s.OrderBy with
        | [ { Kind = Identifier "NAME" }, false, None ] -> ()
        | res -> Assert.Fail(sprintf "Expected OrderBy name DESC, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Explicit table verification`` () =
    match parse "TABLE users" with
    | Select(ExplicitTable { Kind = Identifier "USERS" }) -> ()
    | res -> Assert.Fail(sprintf "Expected ExplicitTable, got %A" res)

[<Fact>]
let ``UNION CORRESPONDING verification`` () =
    match parse "SELECT a FROM t1 UNION CORRESPONDING BY (a) SELECT a FROM t2" with
    | Select(SetOperation(_, op, _)) ->
        Assert.Equal(Union, op.Kind)

        match op.Corresponding with
        | Some(Some [ { Kind = Identifier "A" } ]) -> ()
        | res -> Assert.Fail(sprintf "Expected Union Corresponding, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``OFFSET without ROW or ROWS fails verification`` () =
    match SqlParser.parse "SELECT * FROM t OFFSET 5;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected failure, got %A" res)

[<Fact>]
let ``FETCH clause verification`` () =
    match parse "SELECT * FROM t FETCH FIRST 10 ROWS ONLY" with
    | Select(SelectQuery q) ->
        match q.Fetch with
        | Some { Count = { Kind = Literal(Number 10m) }
                 IsPercent = false
                 WithTies = false } -> ()
        | res -> Assert.Fail(sprintf "Expected Fetch 10, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``FETCH without quantity defaults to 1 verification`` () =
    match parse "SELECT * FROM t FETCH FIRST ROW ONLY" with
    | Select(SelectQuery q) ->
        match q.Fetch with
        | Some { Count = { Kind = Literal(Number 1m) }
                 IsPercent = false
                 WithTies = false } -> ()
        | res -> Assert.Fail(sprintf "Expected Fetch 1, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Offset and Fetch verification`` () =
    match parse "SELECT * FROM users OFFSET 20 ROWS FETCH FIRST 10 ROWS ONLY" with
    | Select(SelectQuery s) ->
        match s.Offset with
        | Some { Kind = Literal(Number 20m) } -> ()
        | res -> Assert.Fail(sprintf "Expected Offset 20, got %A" res)

        match s.Fetch with
        | Some { Count = { Kind = Literal(Number 10m) } } -> ()
        | res -> Assert.Fail(sprintf "Expected Fetch 10, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``FETCH percent and WITH TIES verification`` () =
    match parse "SELECT * FROM t FETCH FIRST 10 PERCENT ROWS ONLY" with
    | Select(SelectQuery q) ->
        match q.Fetch with
        | Some { Count = { Kind = Literal(Number 10m) }
                 IsPercent = true
                 WithTies = false } -> ()
        | res -> Assert.Fail(sprintf "Expected FETCH FIRST 10 PERCENT, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t FETCH NEXT 5 ROWS WITH TIES" with
    | Select(SelectQuery q) ->
        match q.Fetch with
        | Some { Count = { Kind = Literal(Number 5m) }
                 IsPercent = false
                 WithTies = true } -> ()
        | res -> Assert.Fail(sprintf "Expected FETCH NEXT 5 WITH TIES, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Join specification rules (7.10)`` () =
    // <qualified join> requires a <join specification>
    parseFails "SELECT * FROM a JOIN b"
    parseFails "SELECT * FROM a LEFT JOIN b"
    // <cross join> / <natural join> have NO <join specification> slot
    parseFails "SELECT * FROM a CROSS JOIN b ON a.x = b.x"
    parseFails "SELECT * FROM a NATURAL JOIN b USING (x)"

    // positive
    match parse "SELECT * FROM a JOIN b ON a.x = b.x" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JoinedTable _ } ] -> ()
        | res -> Assert.Fail(sprintf "Expected JoinedTable, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Derived tables require a correlation (7.6)`` () =
    parseFails "SELECT * FROM (SELECT 1 FROM t)"
    parseFails "SELECT * FROM LATERAL (SELECT 1 FROM t)"
    parseFails "SELECT * FROM UNNEST (x)"
    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT))"

    // positive
    match parse "SELECT * FROM (SELECT 1 FROM t) AS d" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = Subquery(_, { Kind = Identifier "D" }, _) } ] -> ()
        | res -> Assert.Fail(sprintf "Expected Subquery with alias, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Only joined tables may be parenthesized (7.6)`` () = parseFails "SELECT * FROM (t)"

[<Fact>]
let ``A parenthesized joined table is accepted (7.6)`` () =
    match parse "SELECT * FROM (t1 JOIN t2 ON t1.id = t2.id)" with
    | Select(SelectQuery s) ->
        match s.From with
        | [ { Kind = JoinedTable _ } ] -> ()
        | res -> Assert.Fail(sprintf "Expected a parenthesized joined table, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``A bare asterisk is not a select sublist (7.16)`` () =
    parseFails "SELECT *, a FROM t"
    parseFails "SELECT a, * FROM t"

[<Fact>]
let ``FETCH FIRST PERCENT requires a quantity (7.17)`` () =
    parseFails "SELECT * FROM t FETCH FIRST PERCENT ROWS ONLY"

[<Fact>]
let ``A schema qualified name has at most three parts (5.4)`` () = parseFails "SELECT * FROM a.b.c.d"

[<Fact>]
let ``JSON table plans need at least two operands (7.11)`` () =
    parseFails "SELECT * FROM JSON_TABLE(doc, '$' COLUMNS (a INT) PLAN (p UNION)) AS jt"

[<Fact>]
let ``Updatability clause verification (14.3 / 22.2)`` () =
    // 22.2 <direct select statement: multiple rows> ::= <cursor specification>, and
    // 14.3 <cursor specification> ::= <query expression> [ <updatability clause> ].
    match parse "SELECT * FROM users FOR UPDATE" with
    | Select(SelectQuery s) -> Assert.Equal(Some(LockingClause.ForUpdate None), s.Locking)
    | res -> Assert.Fail(sprintf "Expected FOR UPDATE, got %A" res)

    match parse "SELECT * FROM users FOR READ ONLY" with
    | Select(SelectQuery s) -> Assert.Equal(Some LockingClause.ForReadOnly, s.Locking)
    | res -> Assert.Fail(sprintf "Expected FOR READ ONLY, got %A" res)

    match parse "SELECT * FROM users FOR UPDATE OF a, b" with
    | Select(SelectQuery s) ->
        match s.Locking with
        | Some(LockingClause.ForUpdate(Some cols)) ->
            Assert.Equal<ExpressionKind list>(
                [ ExpressionKind.Identifier "A"; ExpressionKind.Identifier "B" ],
                cols |> List.map (fun c -> c.Kind)
            )
        | other -> Assert.Fail(sprintf "Expected FOR UPDATE OF a, b, got %A" other)
    | res -> Assert.Fail(sprintf "Expected FOR UPDATE OF a, b, got %A" res)

    // The clause belongs to the <cursor specification>, so a subquery or an INSERT ... SELECT
    // (a bare <query expression>) does not accept it.
    parseFails "SELECT * FROM (SELECT * FROM t FOR UPDATE) AS x"
    parseFails "INSERT INTO t SELECT * FROM s FOR UPDATE"
    // 7.17 <query expression> has no <lock clause>, so FOR SHARE stays invalid.
    parseFails "SELECT * FROM users FOR SHARE"

[<Fact>]
let ``FOR SHARE is rejected (not in SQL-2016)`` () =
    parseFails "SELECT * FROM users FOR SHARE"

[<Fact>]
let ``ORDER BY accepts a boolean sort key (10.10)`` () =
    // 10.10 <sort key> ::= <value expression>, and 6.28 <value expression> includes
    // <boolean value expression>.
    parse "SELECT a FROM t ORDER BY a = b" |> ignore
    parse "SELECT a FROM t ORDER BY a IS DISTINCT FROM b" |> ignore
    parse "SELECT a FROM t ORDER BY EXISTS (SELECT 1 FROM t)" |> ignore
    parse "SELECT a FROM t ORDER BY a = b DESC NULLS LAST" |> ignore
    // OFFSET takes a <simple value specification>, never an arithmetic term.
    parseFails "SELECT * FROM t OFFSET 1 + 1 ROWS"
    parseFails "SELECT * FROM t OFFSET (SELECT 1) ROWS"

    match parse "SELECT * FROM t OFFSET x ROWS" with
    | Select(SelectQuery s) ->
        match s.Offset with
        | Some { Kind = Identifier "X" } -> ()
        | other -> Assert.Fail(sprintf "Expected SQL parameter reference for OFFSET, got %A" other)
    | res -> Assert.Fail(sprintf "Expected SQL parameter reference for OFFSET, got %A" res)

[<Fact>]
let ``FETCH FIRST with expression is rejected`` () =
    parseFails "SELECT * FROM t FETCH FIRST 1 + 1 ROWS ONLY"
    parseFails "SELECT * FROM t FETCH FIRST x PERCENT WITH TIES"
    parseFails "SELECT * FROM t FETCH NEXT (SELECT 1) ROWS ONLY"

[<Fact>]
let ``Window frame bound with signed expression is rejected`` () =
    parseFails "SELECT SUM(x) OVER (ORDER BY y ROWS BETWEEN -1 PRECEDING AND CURRENT ROW)"
    parseFails "SELECT SUM(x) OVER (ORDER BY y ROWS BETWEEN 1+1 PRECEDING AND CURRENT ROW)"
    parseFails "SELECT SUM(x) OVER (ORDER BY y RANGE BETWEEN x PRECEDING AND CURRENT ROW)"

[<Fact>]
let ``TABLESAMPLE with invalid percentage is rejected`` () =
    // Comparisons/boolean operators are not <numeric value expression>
    parseFails "SELECT * FROM t TABLESAMPLE SYSTEM (a = b)"
    parseFails "SELECT * FROM t TABLESAMPLE BERNOULLI (a OR b)"
    parseFails "SELECT * FROM t TABLESAMPLE BERNOULLI (EXISTS (SELECT 1))"

[<Fact>]
let ``Window partition with non-column reference is rejected`` () =
    parseFails "SELECT SUM(a) OVER (w) FROM t WINDOW w AS (PARTITION BY 1 + 1)"
    parseFails "SELECT SUM(a) OVER (w) FROM t WINDOW w AS (PARTITION BY a || b)"

[<Fact>]
let ``Parenthesized query primary with ORDER BY verification`` () =
    match parse "(SELECT 1 FROM t ORDER BY 1) UNION SELECT 2 FROM t" with
    | Select(SetOperation(SelectQuery { OrderBy = [ { Kind = Literal(Number 1m) }, true, None ] },
                          { Kind = Union },
                          SelectQuery _)) -> ()
    | res -> Assert.Fail(sprintf "Expected parenthesized ORDER BY, got %A" res)

[<Fact>]
let ``Set operations verification`` () =
    let tests =
        [ "SELECT 1 FROM t UNION SELECT 2 FROM t",
          { Kind = Union
            IsAll = false
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 FROM t UNION ALL SELECT 2 FROM t",
          { Kind = Union
            IsAll = true
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 FROM t INTERSECT SELECT 2 FROM t",
          { Kind = Intersect
            IsAll = false
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 FROM t INTERSECT ALL SELECT 2 FROM t",
          { Kind = Intersect
            IsAll = true
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 FROM t EXCEPT SELECT 2 FROM t",
          { Kind = Except
            IsAll = false
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 FROM t EXCEPT ALL SELECT 2 FROM t",
          { Kind = Except
            IsAll = true
            IsDistinct = false
            Corresponding = None } ]

    for sql, expectedOp in tests do
        match parse sql with
        | Select(SetOperation(SelectQuery _, op, SelectQuery _)) -> Assert.Equal(expectedOp, op)
        | res -> Assert.Fail(sprintf "Expected set operation %A for sql: %s, got %A" expectedOp sql res)

[<Fact>]
let ``INTERSECT binds tighter than UNION`` () =
    match parse "SELECT 1 FROM t UNION SELECT 2 FROM t INTERSECT SELECT 3 FROM t" with
    | Select(SetOperation(SelectQuery _,
                          { Kind = Union },
                          SetOperation(SelectQuery _, { Kind = Intersect }, SelectQuery _))) -> ()
    | res -> Assert.Fail(sprintf "Expected INTERSECT to bind tighter than UNION, got %A" res)

[<Fact>]
let ``ORDER BY applies to whole set operation`` () =
    match parse "SELECT 1 FROM t UNION SELECT 2 FROM t ORDER BY 1" with
    | Select(QueryExpression(SetOperation(SelectQuery _, _, SelectQuery _),
                             [ { Kind = Literal(Number 1m) }, _, _ ],
                             None,
                             None)) -> ()
    | res -> Assert.Fail(sprintf "Expected QueryExpression wrapping set operation, got %A" res)

[<Fact>]
let ``ORDER BY on an explicit table and a table value constructor (7.17)`` () =
    match parse "TABLE t ORDER BY 1" with
    | Select(QueryExpression(ExplicitTable _, [ { Kind = Literal(Number 1m) }, _, _ ], None, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected an ORDER BY over EXPLICIT TABLE, got %A" res)

    match parse "VALUES (1), (2) ORDER BY 1" with
    | Select(QueryExpression(TableValueConstructor _, [ { Kind = Literal(Number 1m) }, _, _ ], None, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected an ORDER BY over VALUES, got %A" res)

[<Fact>]
let ``ORDER BY OFFSET FETCH and locking on a WITH statement verification`` () =
    match parse "WITH cte AS (SELECT 1 FROM t) SELECT * FROM cte ORDER BY 1 OFFSET 1 ROWS FETCH NEXT 1 ROWS ONLY" with
    | WithStatement(false, [ _ ], Select(SelectQuery s)) ->
        match s.OrderBy, s.Fetch, s.Locking with
        | [ ({ Kind = Literal(Number 1m) }, _, _) ], Some { WithTies = false }, None -> ()
        | res -> Assert.Fail(sprintf "Expected ORDER BY/OFFSET/FETCH/locking, got %A" res)
    | res -> Assert.Fail(sprintf "Expected WithStatement, got %A" res)

[<Fact>]
let ``OFFSET and FETCH on a set operation verification`` () =
    match parse "SELECT 1 FROM t UNION SELECT 2 FROM t OFFSET 1 ROWS FETCH NEXT 1 ROWS ONLY" with
    | Select(QueryExpression(SetOperation(_, _, _), _, Some(Some _, Some _), None)) -> ()
    | res -> Assert.Fail(sprintf "Expected QueryExpression over set operation, got %A" res)
