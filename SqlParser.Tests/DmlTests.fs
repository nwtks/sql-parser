module SqlParser.Tests.DmlTests

open Xunit
open SqlParser
open SqlParser.Ast

let parse sql =
    match SqlParser.parse sql with
    | Choice1Of2 { Kind = res } -> res
    | Choice2Of2(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``INSERT verification`` () =
    let sql = "INSERT INTO users (id, name) VALUES (1, 'alice')"

    match parse sql with
    | Insert { Table = "USERS"
               Columns = Some [ "ID"; "NAME" ]
               Source = Values [ [ { Kind = Literal(Number 1m) }; { Kind = Literal(String "alice") } ] ] } -> ()
    | res -> Assert.Fail(sprintf "Expected Insert, got %A" res)

[<Fact>]
let ``UPDATE verification`` () =
    let sql = "UPDATE users SET name = 'bob' WHERE id = 1"

    match parse sql with
    | Update { Table = "USERS"
               Set = [ "NAME", { Kind = Literal(String "bob") } ]
               Where = Some { Kind = BinaryOp(Equal, { Kind = Identifier "ID" }, { Kind = Literal(Number 1m) }) } } ->
        ()
    | res -> Assert.Fail(sprintf "Expected Update, got %A" res)

[<Fact>]
let ``DELETE verification`` () =
    let sql = "DELETE FROM users WHERE id = 1"

    match parse sql with
    | Delete { Table = "USERS"
               Where = Some { Kind = BinaryOp(Equal, { Kind = Identifier "ID" }, { Kind = Literal(Number 1m) }) } } ->
        ()
    | res -> Assert.Fail(sprintf "Expected Delete, got %A" res)

[<Fact>]
let ``MERGE verification`` () =
    let sql =
        "MERGE INTO target AS t USING source AS s ON t.id = s.id WHEN MATCHED THEN UPDATE SET name = s.name WHEN NOT MATCHED THEN INSERT (id, name) VALUES (s.id, s.name)"

    match parse sql with
    | Merge { Target = "TARGET"
              TargetAlias = Some "T"
              On = { Kind = BinaryOp(Equal,
                                     { Kind = ColumnReference [ "T"; "ID" ] },
                                     { Kind = ColumnReference [ "S"; "ID" ] }) } } -> ()
    | res -> Assert.Fail(sprintf "Expected Merge, got %A" res)
