module SqlParser.Tests.DdlTests

open Xunit
open SqlParser

let parse sql =
    match SqlParser.parse sql with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseFails sql =
    match SqlParser.parse sql with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

[<Fact>]
let ``CREATE TABLE verification`` () =
    match parse "CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR(100) NOT NULL)" with
    | CreateTable { Table = { Kind = Identifier "USERS" }
                    Columns = [ { Name = { Kind = Identifier "ID" }
                                  DataType = Integer
                                  IsPrimaryKey = true }
                                { Name = { Kind = Identifier "NAME" }
                                  DataType = Varchar(Some 100)
                                  IsNullable = Some false } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable, got %A" res)

[<Fact>]
let ``CREATE TABLE schema-qualified verification`` () =
    match parse "CREATE TABLE app.users (id INT)" with
    | CreateTable { Table = { Kind = ColumnReference [ "APP"; "USERS" ] } } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable schema-qualified, got %A" res)

[<Fact>]
let ``Column-level constraints verification`` () =
    match
        parse
            "CREATE TABLE t (id INT PRIMARY KEY, email VARCHAR(100) UNIQUE, dept_id INT REFERENCES departments (id) ON DELETE CASCADE, age INT CHECK (age > 0))"
    with
    | CreateTable { Columns = [ pkCol; uniqCol; refCol; checkCol ] } ->
        Assert.True(pkCol.IsPrimaryKey)
        Assert.True(uniqCol.IsUnique)

        match refCol.References with
        | Some r ->
            Assert.Equal(Identifier "DEPARTMENTS", r.Table.Kind)
            Assert.Equal(Some(ReferentialAction.Cascade), r.OnDelete)
        | None -> Assert.Fail "Expected column-level REFERENCES"

        match checkCol.Check with
        | Some _ -> ()
        | None -> Assert.Fail "Expected column-level CHECK"
    | res -> Assert.Fail(sprintf "Expected CreateTable with column constraints, got %A" res)

[<Fact>]
let ``DEFAULT CURRENT_TIMESTAMP verification`` () =
    match parse "CREATE TABLE t (created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)" with
    | CreateTable { Columns = [ { DefaultValue = Some { Kind = CurrentTimestamp None } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected DEFAULT CURRENT_TIMESTAMP, got %A" res)

[<Fact>]
let ``User-defined type does not crash`` () =
    match parse "CREATE TABLE t (c MyType)" with
    | CreateTable { Columns = [ { DataType = UserDefinedType { Kind = Identifier "MYTYPE" } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected UserDefinedType, got %A" res)

[<Fact>]
let ``Array type is parsed`` () =
    match parse "CREATE TABLE t (c INT ARRAY)" with
    | CreateTable { Columns = [ { DataType = ArrayType(Integer, None) } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected ArrayType, got %A" res)

[<Fact>]
let ``CREATE TABLE with table constraints verification`` () =
    match
        parse
            "CREATE TABLE orders (id INT, customer_id INT, amount NUMERIC(10,2), CONSTRAINT pk_orders PRIMARY KEY (id), CONSTRAINT fk_customer FOREIGN KEY (customer_id) REFERENCES customers (id) ON DELETE CASCADE, CHECK (amount > 0))"
    with
    | CreateTable { Table = { Kind = Identifier "ORDERS" }
                    Columns = cols
                    Constraints = [ pk; fk; ck ] } ->
        Assert.Equal(3, cols.Length)

        match pk with
        | TableConstraint.PrimaryKey(Some name, [ idCol ]) ->
            Assert.Equal(Identifier "PK_ORDERS", name.Kind)
            Assert.Equal(Identifier "ID", idCol.Kind)
        | c -> Assert.Fail(sprintf "Expected PrimaryKey, got %A" c)

        match fk with
        | TableConstraint.ForeignKey fkCon ->
            Assert.Equal(Identifier "FK_CUSTOMER", fkCon.Name.Value.Kind)
            Assert.Equal(Identifier "CUSTOMERS", fkCon.Table.Kind)
            Assert.Equal(Some(ReferentialAction.Cascade), fkCon.OnDelete)
            Assert.Equal(None, fkCon.OnUpdate)

            match fkCon.RefColumns with
            | Some [ { Kind = Identifier "ID" } ] -> ()
            | _ -> Assert.Fail("Expected referenced column ID")
        | c -> Assert.Fail(sprintf "Expected ForeignKey, got %A" c)

        match ck with
        | TableConstraint.Check(None, _) -> ()
        | c -> Assert.Fail(sprintf "Expected Check, got %A" c)
    | res -> Assert.Fail(sprintf "Expected CreateTable with constraints, got %A" res)

[<Fact>]
let ``DROP statements verification`` () =
    match parse "DROP TABLE users CASCADE" with
    | Drop(DropTable({ Kind = Identifier "USERS" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTable CASCADE, got %A" res)

    match parse "DROP TABLE users RESTRICT" with
    | Drop(DropTable({ Kind = Identifier "USERS" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTable RESTRICT, got %A" res)

    match parse "DROP VIEW my_view" with
    | Drop(DropView { Kind = Identifier "MY_VIEW" }) -> ()
    | res -> Assert.Fail(sprintf "Expected DropView, got %A" res)

    match parse "DROP ROLE admin" with
    | Drop(DropStatement.DropRole { Kind = Identifier "ADMIN" }) -> ()
    | res -> Assert.Fail(sprintf "Expected DropRole, got %A" res)

[<Fact>]
let ``DROP TABLE without drop behavior is rejected`` () = parseFails "DROP TABLE users"

[<Fact>]
let ``CREATE INDEX is rejected (not in SQL-2016)`` () =
    parseFails "CREATE INDEX idx ON users (name)"
    parseFails "CREATE UNIQUE INDEX idx ON users (name)"

[<Fact>]
let ``DROP INDEX is rejected (not in SQL-2016)`` () = parseFails "DROP INDEX idx"

[<Fact>]
let ``CREATE VIEW verification`` () =
    match parse "CREATE VIEW my_view AS SELECT * FROM t1" with
    | CreateView { Name = { Kind = Identifier "MY_VIEW" }
                   Query = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateView, got %A" res)

[<Fact>]
let ``CREATE VIEW with column list verification`` () =
    match parse "CREATE VIEW my_view (a, b) AS SELECT x, y FROM t1" with
    | CreateView { Name = { Kind = Identifier "MY_VIEW" }
                   Columns = Some [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ]
                   Query = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateView with column list, got %A" res)

[<Fact>]
let ``CREATE TABLE AS SELECT verification`` () =
    match parse "CREATE TABLE backup AS SELECT * FROM users" with
    | CreateTable { Table = { Kind = Identifier "BACKUP" }
                    Columns = []
                    AsQuery = Some(SelectQuery _)
                    WithData = None } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable AS SELECT, got %A" res)

    match parse "CREATE TABLE backup (id, name) AS SELECT id, name FROM users WITH NO DATA" with
    | CreateTable { Table = { Kind = Identifier "BACKUP" }
                    AsColumns = Some [ { Kind = Identifier "ID" }; { Kind = Identifier "NAME" } ]
                    AsQuery = Some(SelectQuery _)
                    WithData = Some false } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable AS SELECT WITH NO DATA, got %A" res)

[<Fact>]
let ``CREATE ROLE verification`` () =
    match parse "CREATE ROLE admin" with
    | CreateRole { Kind = Identifier "ADMIN" } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateRole, got %A" res)

    match parse "CREATE ROLE analyst WITH ADMIN CURRENT_USER" with
    | CreateRole { Kind = Identifier "ANALYST" } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateRole WITH ADMIN, got %A" res)

[<Fact>]
let ``GRANT verification`` () =
    match parse "GRANT SELECT, INSERT, UPDATE (name) ON TABLE users TO alice, bob WITH GRANT OPTION" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select None
                                                                PrivilegeAction.Insert None
                                                                PrivilegeAction.Update(Some [ { Kind = Identifier "NAME" } ]) ],
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "ALICE" }; { Kind = Identifier "BOB" } ],
                                           true)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges, got %A" res)

    match parse "GRANT ALL PRIVILEGES ON users TO PUBLIC" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.AllPrivileges,
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "PUBLIC" } ],
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges ALL, got %A" res)

    match parse "GRANT role_a, role_b TO alice WITH ADMIN OPTION" with
    | Grant(GrantStatement.GrantRoles([ { Kind = Identifier "ROLE_A" }; { Kind = Identifier "ROLE_B" } ],
                                      [ { Kind = Identifier "ALICE" } ],
                                      true)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantRoles, got %A" res)

[<Fact>]
let ``REVOKE verification`` () =
    match parse "REVOKE SELECT, DELETE ON users FROM alice" with
    | Revoke(RevokeStatement.RevokePrivileges(Privileges.Actions [ PrivilegeAction.Select None; PrivilegeAction.Delete ],
                                              { Kind = Identifier "USERS" },
                                              [ { Kind = Identifier "ALICE" } ])) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokePrivileges, got %A" res)

    match parse "REVOKE role_a FROM alice" with
    | Revoke(RevokeStatement.RevokeRoles([ { Kind = Identifier "ROLE_A" } ], [ { Kind = Identifier "ALICE" } ])) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokeRoles, got %A" res)

[<Fact>]
let ``ALTER TABLE verification`` () =
    match parse "ALTER TABLE users ADD COLUMN age INT" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AddColumn { Name = { Kind = Identifier "AGE" }
                                        DataType = Integer } } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterTable, got %A" res)

[<Fact>]
let ``ALTER TABLE extended actions verification`` () =
    match parse "ALTER TABLE users ADD CONSTRAINT fk_dept FOREIGN KEY (dept_id) REFERENCES departments (id)" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AddConstraint(TableConstraint.ForeignKey fk) } ->
        Assert.Equal(Identifier "FK_DEPT", fk.Name.Value.Kind)
        Assert.Equal(Identifier "DEPARTMENTS", fk.Table.Kind)

        match fk.Columns with
        | [ { Kind = Identifier "DEPT_ID" } ] -> ()
        | _ -> Assert.Fail("Expected FK column DEPT_ID")
    | res -> Assert.Fail(sprintf "Expected AddConstraint, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN age SET DEFAULT 0" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "AGE" }, ColumnAlteration.SetDefault _) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn SetDefault, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN age DROP NOT NULL" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "AGE" }, ColumnAlteration.DropNotNull) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn DropNotNull, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN name SET DATA TYPE VARCHAR(200)" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "NAME" }, ColumnAlteration.SetDataType(Varchar(Some 200))) } ->
        ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn SetDataType, got %A" res)

    match parse "ALTER TABLE users DROP CONSTRAINT fk_dept" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = DropConstraint { Kind = Identifier "FK_DEPT" } } -> ()
    | res -> Assert.Fail(sprintf "Expected DropConstraint, got %A" res)

[<Fact>]
let ``ALTER TABLE RENAME is rejected (not in SQL-2016)`` () =
    parseFails "ALTER TABLE users RENAME TO accounts"
    parseFails "ALTER TABLE users RENAME COLUMN name TO full_name"

[<Fact>]
let ``TRUNCATE TABLE verification`` () =
    match parse "TRUNCATE TABLE logs" with
    | Truncate({ Kind = Identifier "LOGS" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Truncate, got %A" res)

    match parse "TRUNCATE TABLE logs RESTART IDENTITY" with
    | Truncate({ Kind = Identifier "LOGS" }, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected Truncate RESTART IDENTITY, got %A" res)
