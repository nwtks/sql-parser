module SqlParser.Tests.ExpressionTests

open Xunit
open SqlParser
open SqlParser.Ast

let parseExpr sql =
    match SqlParser.parse sql with
    | Choice1Of2 { Kind = Select(SelectQuery s)
                   Pos = _ } -> s.Columns.[0] |> fun (Column(e, _)) -> e
    | Choice1Of2 res -> failwithf "Expected Select, got %A" res
    | Choice2Of2(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parse sql = (parseExpr sql).Kind

[<Fact>]
let ``Literal expressions verification`` () =
    Assert.Equal(Literal(Number 123m), parse "SELECT 123")
    Assert.Equal(Literal(String "hello"), parse "SELECT 'hello'")
    Assert.Equal(Literal(Bool true), parse "SELECT TRUE")
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
    | FunctionCall("COUNT", false, [ { Kind = Star } ], None) -> ()
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
    | FunctionCall("COUNT", true, [ { Kind = Identifier "ID" } ], None) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(DISTINCT id), got %A" res)

[<Fact>]
let ``Window functions verification`` () =
    match parse "SELECT ROW_NUMBER() OVER (ORDER BY id DESC)" with
    | WindowFunction { Function = "ROW_NUMBER"
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
    | WindowFunction { Function = "SUM"
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
    | Trim(Some "BOTH", Some { Kind = Literal(String " ") }, { Kind = Literal(String " abc ") }) -> ()
    | res -> Assert.Fail(sprintf "Expected Trim, got %A" res)
