module SqlParser.Tests.DmlTests

open Xunit
open SqlParser

let parse sql =
    match SqlParser.parse sql with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``INSERT verification`` () =
    match parse "INSERT INTO users (id, name) VALUES (1, 'alice')" with
    | Insert { Table = { Kind = Identifier "USERS" }
               Columns = Some [ { Kind = Identifier "ID" }; { Kind = Identifier "NAME" } ]
               Source = Values [ [ { Kind = Literal(Number 1m) }; { Kind = Literal(String "alice") } ] ] } -> ()
    | res -> Assert.Fail(sprintf "Expected Insert, got %A" res)

[<Fact>]
let ``UPDATE verification`` () =
    match parse "UPDATE users SET name = 'bob' WHERE id = 1" with
    | Update { Table = { Kind = Identifier "USERS" }
               Set = [ { Kind = Identifier "NAME" }, { Kind = Literal(String "bob") } ]
               Where = Some { Kind = BinaryOp(Equal, { Kind = Identifier "ID" }, { Kind = Literal(Number 1m) }) } } ->
        ()
    | res -> Assert.Fail(sprintf "Expected Update, got %A" res)

[<Fact>]
let ``DELETE verification`` () =
    match parse "DELETE FROM users WHERE id = 1" with
    | Delete { Table = { Kind = Identifier "USERS" }
               Where = Some { Kind = BinaryOp(Equal, { Kind = Identifier "ID" }, { Kind = Literal(Number 1m) }) } } ->
        ()
    | res -> Assert.Fail(sprintf "Expected Delete, got %A" res)

[<Fact>]
let ``MERGE verification`` () =
    match
        parse
            "MERGE INTO target AS t USING source AS s ON t.id = s.id WHEN MATCHED THEN UPDATE SET name = s.name WHEN NOT MATCHED THEN INSERT (id, name) VALUES (s.id, s.name)"
    with
    | Merge { Target = { Kind = Identifier "TARGET" }
              TargetAlias = Some { Kind = Identifier "T" }
              On = { Kind = BinaryOp(Equal,
                                     { Kind = ColumnReference [ "T"; "ID" ] },
                                     { Kind = ColumnReference [ "S"; "ID" ] }) } } -> ()
    | res -> Assert.Fail(sprintf "Expected Merge, got %A" res)
