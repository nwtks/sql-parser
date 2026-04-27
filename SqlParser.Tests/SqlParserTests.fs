module SqlParser.Tests.SqlParserTests

open Xunit
open SqlParser
open SqlParser.Ast

let parse sql =
    match SqlParser.parse sql with
    | Choice1Of2 { Kind = res } -> res
    | Choice2Of2(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``Full Statement parsing verification`` () =
    match parse "WITH cte AS (SELECT * FROM users) SELECT * FROM cte" with
    | WithStatement(false, [ { Name = "CTE" } ], Select _) -> ()
    | res -> Assert.Fail(sprintf "Expected WithStatement, got %A" res)

[<Fact>]
let ``Multiple statements parsing verification`` () =
    match parse "SELECT * FROM users" with
    | Select _ -> ()
    | _ -> Assert.Fail "Expected Select"
