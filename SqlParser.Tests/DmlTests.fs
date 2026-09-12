module SqlParser.Tests.DmlTests

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
    | Update { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               Set = [ SingleSet({ Kind = Identifier "NAME" }, { Kind = Literal(String "bob") }) ]
               Where = Some { Kind = BinaryOp(Equal, { Kind = Identifier "ID" }, { Kind = Literal(Number 1m) }) } } ->
        ()
    | res -> Assert.Fail(sprintf "Expected Update, got %A" res)

[<Fact>]
let ``UPDATE ONLY target table verification`` () =
    // 14.13/14.14 <target table> ::= <table name> | ONLY ( <table name> )
    match parse "UPDATE ONLY (users) SET name = 'bob'" with
    | Update { Target = TableTarget({ Kind = Identifier "USERS" }, true) } -> ()
    | res -> Assert.Fail(sprintf "Expected UPDATE ONLY, got %A" res)

    match parse "UPDATE app.users SET name = 'bob'" with
    | Update { Target = TableTarget({ Kind = ColumnReference [ "APP"; "USERS" ] }, false) } -> ()
    | res -> Assert.Fail(sprintf "Expected UPDATE without ONLY, got %A" res)

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
    | Update { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               TableAlias = Some { Kind = Identifier "U" } } -> ()
    | res -> Assert.Fail(sprintf "Expected Update with alias, got %A" res)

[<Fact>]
let ``UPDATE positioned (WHERE CURRENT OF) verification`` () =
    match parse "UPDATE users SET name = 'x' WHERE CURRENT OF cur" with
    | Update { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               Cursor = Some { Kind = Identifier "CUR" }
               Where = None } -> ()
    | res -> Assert.Fail(sprintf "Expected positioned Update, got %A" res)

[<Fact>]
let ``UPDATE without target table (20.27) verification`` () =
    // 20.27 <preparable dynamic update statement: positioned>
    match parse "UPDATE SET name = 'x' WHERE CURRENT OF cur" with
    | Update { Target = OmittedTarget
               Cursor = Some { Kind = Identifier "CUR" }
               Where = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Update without target table, got %A" res)

[<Fact>]
let ``UPDATE FOR PORTION OF verification`` () =
    match parse "UPDATE users FOR PORTION OF p FROM x TO y SET name = 'x'" with
    | Update { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               PortionOf = Some { PeriodName = { Kind = Identifier "P" }
                                  From = { Kind = Identifier "X" }
                                  To = { Kind = Identifier "Y" } } } -> ()
    | res -> Assert.Fail(sprintf "Expected Update FOR PORTION OF, got %A" res)

[<Fact>]
let ``UPDATE mutated set clause verification`` () =
    match parse "UPDATE users SET a.b = 1" with
    | Update { Set = [ MutatedSet({ Kind = Identifier "A" }, { Kind = Identifier "B" }, { Kind = Literal(Number 1m) }) ] } ->
        ()
    | res -> Assert.Fail(sprintf "Expected Update mutated set clause, got %A" res)

[<Fact>]
let ``UPDATE nested mutated set clause verification`` () =
    match parse "UPDATE users SET a.b.c = 1" with
    | Update { Set = [ MutatedSet({ Kind = FieldReference({ Kind = Identifier "A" }, { Kind = Identifier "B" }) },
                                  { Kind = Identifier "C" },
                                  { Kind = Literal(Number 1m) }) ] } -> ()
    | res -> Assert.Fail(sprintf "Expected Update nested mutated set clause, got %A" res)

[<Fact>]
let ``UPDATE mutated set clause error`` () = parseFails "UPDATE users SET a. = 1"

[<Fact>]
let ``UPDATE without target table and no CURRENT OF is rejected`` () = parseFails "UPDATE SET name = 'x'"

[<Fact>]
let ``UPDATE without target table and search WHERE is rejected`` () =
    parseFails "UPDATE SET name = 'x' WHERE id = 1"

[<Fact>]
let ``UPDATE without target table and alias is rejected`` () =
    parseFails "UPDATE AS u SET name = 'x' WHERE CURRENT OF cur"

[<Fact>]
let ``UPDATE without target table and FOR PORTION OF is rejected`` () =
    parseFails "UPDATE FOR PORTION OF p FROM x TO y SET name = 'x' WHERE CURRENT OF cur"

[<Fact>]
let ``DELETE with alias verification`` () =
    match parse "DELETE FROM users AS u WHERE u.id = 1" with
    | Delete { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               TableAlias = Some { Kind = Identifier "U" } } -> ()
    | res -> Assert.Fail(sprintf "Expected Delete with alias, got %A" res)

[<Fact>]
let ``DELETE verification`` () =
    match parse "DELETE FROM users WHERE id = 1" with
    | Delete { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               Where = Some { Kind = BinaryOp(Equal, { Kind = Identifier "ID" }, { Kind = Literal(Number 1m) }) } } ->
        ()
    | res -> Assert.Fail(sprintf "Expected Delete, got %A" res)

[<Fact>]
let ``DELETE ONLY target table verification`` () =
    match parse "DELETE FROM ONLY (app.users)" with
    | Delete { Target = TableTarget({ Kind = ColumnReference [ "APP"; "USERS" ] }, true) } -> ()
    | res -> Assert.Fail(sprintf "Expected DELETE ONLY, got %A" res)

[<Fact>]
let ``DELETE positioned (WHERE CURRENT OF) verification`` () =
    match parse "DELETE FROM users WHERE CURRENT OF cur" with
    | Delete { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               Cursor = Some { Kind = Identifier "CUR" }
               Where = None } -> ()
    | res -> Assert.Fail(sprintf "Expected positioned Delete, got %A" res)

[<Fact>]
let ``DELETE without target table (20.25) verification`` () =
    // 20.25 <preparable dynamic delete statement: positioned>
    match parse "DELETE WHERE CURRENT OF cur" with
    | Delete { Target = OmittedTarget
               Cursor = Some { Kind = Identifier "CUR" }
               Where = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Delete without target table, got %A" res)

[<Fact>]
let ``DELETE without target table and search WHERE is rejected`` () = parseFails "DELETE WHERE id = 1"

[<Fact>]
let ``DELETE without target table and alias is rejected`` () =
    parseFails "DELETE AS u WHERE CURRENT OF cur"

[<Fact>]
let ``DELETE without target table and FOR PORTION OF is rejected`` () =
    parseFails "DELETE FOR PORTION OF p FROM x TO y WHERE CURRENT OF cur"

[<Fact>]
let ``DELETE without FROM and without CURRENT OF is rejected`` () = parseFails "DELETE"

[<Fact>]
let ``DELETE FROM without a table name is rejected`` () = parseFails "DELETE FROM"

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

[<Fact>]
let ``MERGE INSERT OVERRIDING SYSTEM VALUE verification`` () =
    match
        parse
            "MERGE INTO t USING s ON t.id = s.id WHEN NOT MATCHED THEN INSERT (id) OVERRIDING SYSTEM VALUE VALUES (DEFAULT)"
    with
    | Merge { WhenClauses = [ clause ] } ->
        match clause.Action with
        | MergeInsert(cols, Some false, [ { Kind = Default } ]) ->
            match cols with
            | Some [ col ] ->
                match col.Kind with
                | Identifier "ID" -> ()
                | _ -> Assert.Fail(sprintf "Expected insert column ID, got %A" col)
            | _ -> Assert.Fail(sprintf "Expected insert column list, got %A" cols)
        | res -> Assert.Fail(sprintf "Expected MergeInsert, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Merge, got %A" res)

[<Fact>]
let ``MERGE ONLY target table verification`` () =
    match parse "MERGE INTO ONLY (target) USING source ON target.id = source.id WHEN MATCHED THEN DELETE" with
    | Merge { Target = { Kind = Identifier "TARGET" }
              TargetIsOnly = true } -> ()
    | res -> Assert.Fail(sprintf "Expected MERGE ONLY, got %A" res)

[<Fact>]
let ``INSERT insertion target does not accept ONLY`` () =
    // <insertion target> is a plain <table name> (14.11), unlike <target table>.
    parseFails "INSERT INTO ONLY (users) VALUES (1)"

[<Fact>]
let ``MERGE INSERT OVERRIDING USER VALUE verification`` () =
    match
        parse "MERGE INTO t USING s ON t.id = s.id WHEN NOT MATCHED THEN INSERT (id) OVERRIDING USER VALUE VALUES (1)"
    with
    | Merge { WhenClauses = [ clause ] } ->
        match clause.Action with
        | MergeInsert(cols, Some true, [ { Kind = Literal(Number 1m) } ]) ->
            match cols with
            | Some [ col ] ->
                match col.Kind with
                | Identifier "ID" -> ()
                | _ -> Assert.Fail(sprintf "Expected insert column ID, got %A" col)
            | _ -> Assert.Fail(sprintf "Expected insert column list, got %A" cols)
        | res -> Assert.Fail(sprintf "Expected MergeInsert, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Merge, got %A" res)
