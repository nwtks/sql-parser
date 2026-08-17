module SqlParser.Tests.ExpressionTests

open Xunit
open SqlParser

let parseExpr sql =
    match SqlParser.parse sql with
    | Ok { Kind = Select(SelectQuery s) } -> s.Columns.[0] |> fun (Column(e, _)) -> e
    | Ok res -> failwithf "Expected Select, got %A" res
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parse sql = (parseExpr sql).Kind

let parseFails sql =
    match SqlParser.parse sql with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

[<Fact>]
let ``Literal expressions verification`` () =
    Assert.Equal(Literal(Number 123m), parse "SELECT 123")
    Assert.Equal(Literal(String "hello"), parse "SELECT 'hello'")
    Assert.Equal(Literal(Bool(Some true)), parse "SELECT TRUE")
    Assert.Equal(Literal Null, parse "SELECT NULL")

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
let ``Case expression verification`` () =
    match parse "SELECT CASE WHEN a = 1 THEN 'one' ELSE 'other' END" with
    | Case(None,
           [ { Kind = BinaryOp(Equal, { Kind = Identifier "A" }, { Kind = Literal(Number 1m) }) },
             { Kind = Literal(String "one") } ],
           Some { Kind = Literal(String "other") }) -> ()
    | res -> Assert.Fail(sprintf "Expected CASE, got %A" res)

[<Fact>]
let ``Function call verification`` () =
    match parse "SELECT COUNT(*)" with
    | FunctionCall({ Kind = Identifier "COUNT" }, false, [ { Kind = Star } ], None, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(*), got %A" res)

[<Fact>]
let ``Expression precedence verification`` () =
    match parse "SELECT 1 + 2 * 3" with
    | BinaryOp(Add,
               { Kind = Literal(Number 1m) },
               { Kind = BinaryOp(Multiply, { Kind = Literal(Number 2m) }, { Kind = Literal(Number 3m) }) }) -> ()
    | res -> Assert.Fail(sprintf "Precedence fail: %A" res)

[<Fact>]
let ``Aggregate functions verification`` () =
    match parse "SELECT COUNT(DISTINCT id)" with
    | FunctionCall({ Kind = Identifier "COUNT" }, true, [ { Kind = Identifier "ID" } ], None, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(DISTINCT id), got %A" res)

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
let ``Concatenated hex literal verification`` () =
    match parse "SELECT X'0102' '0304'" with
    | Literal(Literal.Binary [| 1uy; 2uy; 3uy; 4uy |]) -> ()
    | res -> Assert.Fail(sprintf "Expected Binary literal, got %A" res)

[<Fact>]
let ``POSITION verification`` () =
    match parse "SELECT POSITION('a' IN 'abc')" with
    | Position({ Kind = Literal(String "a") }, { Kind = Literal(String "abc") }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Position, got %A" res)

[<Fact>]
let ``TRIM verification`` () =
    match parse "SELECT TRIM(BOTH ' ' FROM ' abc ')" with
    | Trim(Some Both, Some { Kind = Literal(String " ") }, { Kind = Literal(String " abc ") }) -> ()
    | res -> Assert.Fail(sprintf "Expected Trim, got %A" res)

[<Fact>]
let ``EXTRACT verification`` () =
    match parse "SELECT EXTRACT(YEAR FROM hire_date)" with
    | Extract({ Kind = Identifier "YEAR" }, { Kind = Identifier "HIRE_DATE" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Extract, got %A" res)

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
let ``DEFAULT as a general expression is rejected`` () =
    parseFails "SELECT DEFAULT"
    parseFails "SELECT 1 + DEFAULT"

[<Fact>]
let ``FILTER clause verification`` () =
    match parse "SELECT COUNT(*) FILTER (WHERE x > 0)" with
    | FunctionCall({ Kind = Identifier "COUNT" },
                   false,
                   [ { Kind = Star } ],
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
let ``COLLATE verification`` () =
    match parse "SELECT name COLLATE \"C\"" with
    | Collate({ Kind = Identifier "NAME" }, { Kind = Identifier "C" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Collate, got %A" res)
