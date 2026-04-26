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
    let sql = "SELECT CASE WHEN a = 1 THEN 'one' ELSE 'other' END"

    match parse sql with
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
let ``Aggregate and Window functions verification`` () =
    match parse "SELECT COUNT(DISTINCT id)" with
    | FunctionCall("COUNT", true, [ { Kind = Identifier "ID" } ], None) -> ()
    | res -> Assert.Fail(sprintf "Expected COUNT(DISTINCT id), got %A" res)

    match parse "SELECT ROW_NUMBER() OVER (ORDER BY id DESC)" with
    | FunctionCall("ROW_NUMBER", false, [], Some window) ->
        match window.OrderBy with
        | [ { Kind = Identifier "ID" }, false, None ] -> ()
        | _ -> Assert.Fail(sprintf "Expected OrderBy id DESC in window, got %A" window.OrderBy)
    | res -> Assert.Fail(sprintf "Expected ROW_NUMBER() OVER ..., got %A" res)

    match parse "SELECT SUM(salary) OVER (PARTITION BY dept_id ORDER BY hire_date)" with
    | FunctionCall("SUM", false, [ { Kind = Identifier "SALARY" } ], Some window) ->
        match window.PartitionBy with
        | [ { Kind = Identifier "DEPT_ID" } ] -> ()
        | _ -> Assert.Fail "Expected PartitionBy dept_id"

        match window.OrderBy with
        | [ { Kind = Identifier "HIRE_DATE" }, true, None ] -> ()
        | _ -> Assert.Fail "Expected OrderBy hire_date"
    | res -> Assert.Fail(sprintf "Expected SUM(...) OVER ..., got %A" res)

[<Fact>]
let ``ORDER BY NULLS verification`` () =
    let parseOrder sql =
        match SqlParser.parse sql with
        | Choice1Of2 { Kind = Select(SelectQuery s) } -> s.OrderBy.[0]
        | _ -> failwith "fail"

    match parseOrder "SELECT id ORDER BY id DESC NULLS FIRST" with
    | { Kind = Identifier "ID" }, false, Some NullsFirst -> ()
    | res -> Assert.Fail(sprintf "Expected id DESC NULLS FIRST, got %A" res)

    match parseOrder "SELECT id ORDER BY name NULLS LAST" with
    | { Kind = Identifier "NAME" }, true, Some NullsLast -> ()
    | res -> Assert.Fail(sprintf "Expected name ASC NULLS LAST, got %A" res)
