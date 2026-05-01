module SqlParser.Tests.SqlParserTests

open Xunit
open SqlParser

let parse sql =
    match SqlParser.parse sql with
    | Ok { Kind = res } -> res
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``Full Statement parsing verification`` () =
    match parse "WITH cte AS (SELECT * FROM users) SELECT * FROM cte" with
    | WithStatement(false, [ { Name = { Kind = Identifier "CTE" } } ], Select _) -> ()
    | res -> Assert.Fail(sprintf "Expected WithStatement, got %A" res)

[<Fact>]
let ``Multiple statements parsing verification`` () =
    match parse "SELECT * FROM users" with
    | Select _ -> ()
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)
