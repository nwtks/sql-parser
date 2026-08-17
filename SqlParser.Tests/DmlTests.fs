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
let ``INSERT into schema-qualified table verification`` () =
    match parse "INSERT INTO app.users (id) VALUES (1)" with
    | Insert { Table = { Kind = ColumnReference [ "APP"; "USERS" ] } } -> ()
    | res -> Assert.Fail(sprintf "Expected Insert into schema-qualified table, got %A" res)

[<Fact>]
let ``UPDATE verification`` () =
    match parse "UPDATE users SET name = 'bob' WHERE id = 1" with
    | Update { Table = { Kind = Identifier "USERS" }
               Set = [ SingleSet({ Kind = Identifier "NAME" }, { Kind = Literal(String "bob") }) ]
               Where = Some { Kind = BinaryOp(Equal, { Kind = Identifier "ID" }, { Kind = Literal(Number 1m) }) } } ->
        ()
    | res -> Assert.Fail(sprintf "Expected Update, got %A" res)

[<Fact>]
let ``INSERT DEFAULT VALUES verification`` () =
    match parse "INSERT INTO users DEFAULT VALUES" with
    | Insert { Table = { Kind = Identifier "USERS" }
               Columns = None
               Source = DefaultValues
               Override = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Insert DefaultValues, got %A" res)

[<Fact>]
let ``INSERT OVERRIDING SYSTEM VALUE verification`` () =
    match parse "INSERT INTO users (id) OVERRIDING SYSTEM VALUE VALUES (1)" with
    | Insert { Table = { Kind = Identifier "USERS" }
               Source = Values [ [ { Kind = Literal(Number 1m) } ] ]
               Override = Some false } -> ()
    | res -> Assert.Fail(sprintf "Expected Insert OVERRIDING SYSTEM VALUE, got %A" res)

[<Fact>]
let ``INSERT VALUES DEFAULT verification`` () =
    match parse "INSERT INTO users (name, age) VALUES (DEFAULT, 30)" with
    | Insert { Source = Values [ [ { Kind = Default }; { Kind = Literal(Number 30m) } ] ] } -> ()
    | res -> Assert.Fail(sprintf "Expected Insert VALUES DEFAULT, got %A" res)

[<Fact>]
let ``UPDATE multiple assignment verification`` () =
    match parse "UPDATE users SET (first, last) = ('a', 'b')" with
    | Update { Set = [ MultipleSet([ { Kind = Identifier "FIRST" }; { Kind = Identifier "LAST" } ], _) ] } -> ()
    | res -> Assert.Fail(sprintf "Expected Update multiple assignment, got %A" res)

[<Fact>]
let ``UPDATE SET DEFAULT verification`` () =
    match parse "UPDATE users SET name = DEFAULT" with
    | Update { Set = [ SingleSet({ Kind = Identifier "NAME" }, { Kind = Default }) ] } -> ()
    | res -> Assert.Fail(sprintf "Expected Update SET DEFAULT, got %A" res)

[<Fact>]
let ``UPDATE with alias verification`` () =
    match parse "UPDATE users AS u SET name = 'x' WHERE u.id = 1" with
    | Update { Table = { Kind = Identifier "USERS" }
               TableAlias = Some { Kind = Identifier "U" } } -> ()
    | res -> Assert.Fail(sprintf "Expected Update with alias, got %A" res)

[<Fact>]
let ``DELETE with alias verification`` () =
    match parse "DELETE FROM users AS u WHERE u.id = 1" with
    | Delete { Table = { Kind = Identifier "USERS" }
               TableAlias = Some { Kind = Identifier "U" } } -> ()
    | res -> Assert.Fail(sprintf "Expected Delete with alias, got %A" res)

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
