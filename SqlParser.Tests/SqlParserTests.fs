module SqlParser.Tests.SqlParserTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
let parse (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
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

[<Fact>]
let ``Direct SQL statement verification`` () =
    match SqlParser.parse "SELECT 1;" with
    | Ok { Kind = Select _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Direct SQL statement with a schema statement verification`` () =
    match SqlParser.parse "CREATE TABLE t (a INT);" with
    | Ok { Kind = CreateTable _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable, got %A" res)

[<Fact>]
let ``Direct SQL statement with trailing whitespace verification`` () =
    match SqlParser.parse "SELECT 1;  " with
    | Ok { Kind = Select _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Direct SQL statement without semicolon is rejected`` () =
    match SqlParser.parse "SELECT 1" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a missing semicolon to be rejected, got %A" res)

[<Fact>]
let ``Direct SQL statement with two semicolons is rejected`` () =
    match SqlParser.parse "SELECT 1;;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a second semicolon to be rejected, got %A" res)

[<Fact>]
let ``Direct SQL statement with only a semicolon is rejected`` () =
    match SqlParser.parse ";" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a bare semicolon to be rejected, got %A" res)
