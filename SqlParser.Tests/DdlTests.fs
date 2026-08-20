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
let ``CREATE SEQUENCE verification`` () =
    match parse "CREATE SEQUENCE order_seq START WITH 1 INCREMENT BY 2 MAXVALUE 100 CYCLE" with
    | CreateSequence({ Kind = Identifier "ORDER_SEQ" },
                     [ StartWith 1m; IncrementBy 2m; MaxValue(Some 100m); Cycle true ]) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSequence, got %A" res)

    match parse "CREATE SEQUENCE order_seq NO MINVALUE NO CYCLE" with
    | CreateSequence({ Kind = Identifier "ORDER_SEQ" }, [ MinValue None; Cycle false ]) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSequence NO MINVALUE NO CYCLE, got %A" res)

[<Fact>]
let ``ALTER SEQUENCE verification`` () =
    match parse "ALTER SEQUENCE order_seq RESTART WITH 100" with
    | AlterSequence({ Kind = Identifier "ORDER_SEQ" }, [ Restart(Some 100m) ]) -> ()
    | res -> Assert.Fail(sprintf "Expected AlterSequence RESTART, got %A" res)

    match parse "ALTER SEQUENCE order_seq INCREMENT BY 5 MAXVALUE 1000" with
    | AlterSequence({ Kind = Identifier "ORDER_SEQ" }, [ IncrementBy 5m; MaxValue(Some 1000m) ]) -> ()
    | res -> Assert.Fail(sprintf "Expected AlterSequence INCREMENT, got %A" res)

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
let ``IDENTITY column verification`` () =
    match parse "CREATE TABLE t (id INT GENERATED ALWAYS AS IDENTITY)" with
    | CreateTable { Columns = [ { Name = { Kind = Identifier "ID" }
                                  Identity = Some { IsAlways = true; Options = [] } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected GENERATED ALWAYS AS IDENTITY, got %A" res)

    match parse "CREATE TABLE t (id INT GENERATED BY DEFAULT AS IDENTITY)" with
    | CreateTable { Columns = [ { Identity = Some { IsAlways = false; Options = [] } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected GENERATED BY DEFAULT AS IDENTITY, got %A" res)

    match parse "CREATE TABLE t (id INT)" with
    | CreateTable { Columns = [ { Identity = None } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected no Identity, got %A" res)

[<Fact>]
let ``IDENTITY with sequence options verification`` () =
    match parse "CREATE TABLE t (id INT GENERATED ALWAYS AS IDENTITY (START WITH 100 INCREMENT BY 5))" with
    | CreateTable { Columns = [ { Identity = Some { IsAlways = true
                                                    Options = [ StartWith 100m; IncrementBy 5m ] } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected IDENTITY with options, got %A" res)

[<Fact>]
let ``CREATE TEMPORARY TABLE verification`` () =
    match parse "CREATE GLOBAL TEMPORARY TABLE temp_t (id INT)" with
    | CreateTable { Table = { Kind = Identifier "TEMP_T" }
                    TableScope = Some TableScope.Global } -> ()
    | res -> Assert.Fail(sprintf "Expected GLOBAL TEMPORARY, got %A" res)

    match parse "CREATE LOCAL TEMPORARY TABLE temp_t (id INT)" with
    | CreateTable { Table = { Kind = Identifier "TEMP_T" }
                    TableScope = Some TableScope.Local } -> ()
    | res -> Assert.Fail(sprintf "Expected LOCAL TEMPORARY, got %A" res)

    match parse "CREATE TABLE persistent_t (id INT)" with
    | CreateTable { Table = { Kind = Identifier "PERSISTENT_T" }
                    TableScope = None } -> ()
    | res -> Assert.Fail(sprintf "Expected persistent table, got %A" res)

[<Fact>]
let ``DROP statements verification`` () =
    match parse "DROP TABLE users CASCADE" with
    | Drop(DropTable({ Kind = Identifier "USERS" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTable CASCADE, got %A" res)

    match parse "DROP TABLE users RESTRICT" with
    | Drop(DropTable({ Kind = Identifier "USERS" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTable RESTRICT, got %A" res)

    match parse "DROP VIEW my_view CASCADE" with
    | Drop(DropView({ Kind = Identifier "MY_VIEW" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropView CASCADE, got %A" res)

    match parse "DROP VIEW my_view RESTRICT" with
    | Drop(DropView({ Kind = Identifier "MY_VIEW" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropView RESTRICT, got %A" res)

    match parse "DROP SEQUENCE order_seq CASCADE" with
    | Drop(DropSequence({ Kind = Identifier "ORDER_SEQ" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropSequence CASCADE, got %A" res)

    match parse "DROP SEQUENCE order_seq RESTRICT" with
    | Drop(DropSequence({ Kind = Identifier "ORDER_SEQ" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropSequence RESTRICT, got %A" res)

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
let ``CREATE VIEW with CHECK OPTION verification`` () =
    match parse "CREATE VIEW v AS SELECT * FROM t1 WITH CHECK OPTION" with
    | CreateView { Name = { Kind = Identifier "V" }
                   CheckOption = Some true } -> ()
    | res -> Assert.Fail(sprintf "Expected WITH CHECK OPTION, got %A" res)

    match parse "CREATE VIEW v AS SELECT * FROM t1 WITH CASCADED CHECK OPTION" with
    | CreateView { Name = { Kind = Identifier "V" }
                   CheckOption = Some true } -> ()
    | res -> Assert.Fail(sprintf "Expected WITH CASCADED CHECK OPTION, got %A" res)

    match parse "CREATE VIEW v AS SELECT * FROM t1 WITH LOCAL CHECK OPTION" with
    | CreateView { Name = { Kind = Identifier "V" }
                   CheckOption = Some false } -> ()
    | res -> Assert.Fail(sprintf "Expected WITH LOCAL CHECK OPTION, got %A" res)

    match parse "CREATE VIEW v AS SELECT * FROM t1" with
    | CreateView { Name = { Kind = Identifier "V" }
                   CheckOption = None } -> ()
    | res -> Assert.Fail(sprintf "Expected no CHECK OPTION, got %A" res)

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
                                           false,
                                           true)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges, got %A" res)

    match parse "GRANT ALL PRIVILEGES ON users TO PUBLIC" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.AllPrivileges,
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "PUBLIC" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges ALL, got %A" res)

    match parse "GRANT SELECT ON users TO alice WITH HIERARCHY OPTION WITH GRANT OPTION GRANTED BY CURRENT_USER" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select None ],
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           true,
                                           true)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges WITH HIERARCHY OPTION, got %A" res)

    match parse "GRANT role_a, role_b TO alice WITH ADMIN OPTION" with
    | Grant(GrantStatement.GrantRoles([ { Kind = Identifier "ROLE_A" }; { Kind = Identifier "ROLE_B" } ],
                                      [ { Kind = Identifier "ALICE" } ],
                                      true)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantRoles, got %A" res)

[<Fact>]
let ``REVOKE verification`` () =
    match parse "REVOKE SELECT, DELETE ON users FROM alice CASCADE" with
    | Revoke(RevokeStatement.RevokePrivileges(Privileges.Actions [ PrivilegeAction.Select None; PrivilegeAction.Delete ],
                                              { Kind = Identifier "USERS" },
                                              [ { Kind = Identifier "ALICE" } ],
                                              NoOption,
                                              true)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokePrivileges, got %A" res)

    match parse "REVOKE GRANT OPTION FOR SELECT ON users FROM alice RESTRICT" with
    | Revoke(RevokeStatement.RevokePrivileges(Privileges.Actions [ PrivilegeAction.Select None ],
                                              { Kind = Identifier "USERS" },
                                              [ { Kind = Identifier "ALICE" } ],
                                              GrantOptionFor,
                                              false)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokePrivileges GRANT OPTION FOR, got %A" res)

    match parse "REVOKE HIERARCHY OPTION FOR SELECT ON users FROM alice CASCADE" with
    | Revoke(RevokeStatement.RevokePrivileges(Privileges.Actions [ PrivilegeAction.Select None ],
                                              { Kind = Identifier "USERS" },
                                              [ { Kind = Identifier "ALICE" } ],
                                              HierarchyOptionFor,
                                              true)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokePrivileges HIERARCHY OPTION FOR, got %A" res)

    match parse "REVOKE role_a FROM alice CASCADE" with
    | Revoke(RevokeStatement.RevokeRoles([ { Kind = Identifier "ROLE_A" } ],
                                         [ { Kind = Identifier "ALICE" } ],
                                         false,
                                         true)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokeRoles, got %A" res)

    match parse "REVOKE ADMIN OPTION FOR role_a FROM alice CASCADE" with
    | Revoke(RevokeStatement.RevokeRoles([ { Kind = Identifier "ROLE_A" } ],
                                         [ { Kind = Identifier "ALICE" } ],
                                         true,
                                         true)) -> ()
    | res -> Assert.Fail(sprintf "Expected RevokeRoles ADMIN OPTION FOR, got %A" res)

[<Fact>]
let ``REVOKE requires drop behavior`` () =
    parseFails "REVOKE SELECT ON users FROM alice"
    parseFails "REVOKE role_a FROM alice"

[<Fact>]
let ``GRANT UNDER privilege verification`` () =
    match parse "GRANT UNDER ON TABLE users TO alice" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Under ],
                                           { Kind = Identifier "USERS" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GrantPrivileges UNDER, got %A" res)

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
                   Action = AlterTableAction.AddConstraint(TableConstraint.ForeignKey fk) } ->
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
                   Action = AlterTableAction.DropConstraint { Kind = Identifier "FK_DEPT" } } -> ()
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

[<Fact>]
let ``CREATE SCHEMA verification`` () =
    match parse "CREATE SCHEMA sales AUTHORIZATION alice" with
    | CreateSchema { Name = Some { Kind = Identifier "SALES" }
                     Authorization = Some { Kind = Identifier "ALICE" }
                     CharacterSet = None
                     Path = None
                     Elements = [] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSchema, got %A" res)

    match parse "CREATE SCHEMA AUTHORIZATION bob" with
    | CreateSchema { Name = None
                     Authorization = Some { Kind = Identifier "BOB" }
                     Elements = [] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSchema with only authorization, got %A" res)

    match parse "CREATE SCHEMA s" with
    | CreateSchema { Name = Some { Kind = Identifier "S" }
                     Elements = [] } -> ()
    | res -> Assert.Fail(sprintf "Expected minimal CreateSchema, got %A" res)

    match parse "CREATE SCHEMA s PATH p1, p2 DEFAULT CHARACTER SET utf8" with
    | CreateSchema { Name = Some { Kind = Identifier "S" }
                     CharacterSet = Some { Kind = Identifier "UTF8" }
                     Path = Some [ { Kind = Identifier "P1" }; { Kind = Identifier "P2" } ]
                     Elements = [] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSchema with charset and path, got %A" res)

    match parse "CREATE SCHEMA s CREATE TABLE t (id INT)" with
    | CreateSchema { Name = Some { Kind = Identifier "S" }
                     Elements = [ CreateTable _ ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSchema with element, got %A" res)

[<Fact>]
let ``DROP SCHEMA verification`` () =
    match parse "DROP SCHEMA sales CASCADE" with
    | Drop(DropSchema({ Kind = Identifier "SALES" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropSchema CASCADE, got %A" res)

    match parse "DROP SCHEMA sales RESTRICT" with
    | Drop(DropSchema({ Kind = Identifier "SALES" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropSchema RESTRICT, got %A" res)

[<Fact>]
let ``CREATE DOMAIN verification`` () =
    match parse "CREATE DOMAIN positive_int AS INT DEFAULT 5 CHECK (x > 0)" with
    | CreateDomain { Name = { Kind = Identifier "POSITIVE_INT" }
                     DataType = Integer
                     Default = Some { Kind = Literal(Number 5m) }
                     Constraints = [ c ]
                     Collation = None } ->
        Assert.Equal(None, c.Name)
        Assert.Equal(None, c.Characteristics.InitiallyDeferred)
        Assert.Equal(None, c.Characteristics.Deferrable)
        Assert.Equal(None, c.Characteristics.Enforced)
    | res -> Assert.Fail(sprintf "Expected CreateDomain, got %A" res)

    match parse "CREATE DOMAIN d AS VARCHAR(20) CONSTRAINT c CHECK (x > 0) NOT DEFERRABLE COLLATE en_us" with
    | CreateDomain { Name = { Kind = Identifier "D" }
                     DataType = Varchar(Some 20)
                     Constraints = [ c ]
                     Collation = Some { Kind = Identifier "EN_US" } } ->
        Assert.Equal(Some(Identifier "C"), c.Name |> Option.map (fun e -> e.Kind))
        Assert.Equal(Some false, c.Characteristics.Deferrable)
    | res -> Assert.Fail(sprintf "Expected CreateDomain with constraint and collation, got %A" res)

[<Fact>]
let ``CREATE DOMAIN requires a data type`` () = parseFails "CREATE DOMAIN d"

[<Fact>]
let ``ALTER DOMAIN verification`` () =
    match parse "ALTER DOMAIN d SET DEFAULT 10" with
    | AlterDomain({ Kind = Identifier "D" }, DomainAlteration.SetDefault _) -> ()
    | res -> Assert.Fail(sprintf "Expected AlterDomain SetDefault, got %A" res)

    match parse "ALTER DOMAIN d DROP DEFAULT" with
    | AlterDomain({ Kind = Identifier "D" }, DomainAlteration.DropDefault) -> ()
    | res -> Assert.Fail(sprintf "Expected AlterDomain DropDefault, got %A" res)

    match parse "ALTER DOMAIN d ADD CONSTRAINT c CHECK (x > 0)" with
    | AlterDomain({ Kind = Identifier "D" }, DomainAlteration.AddConstraint { Name = Some { Kind = Identifier "C" } }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected AlterDomain AddConstraint, got %A" res)

    match parse "ALTER DOMAIN d DROP CONSTRAINT c" with
    | AlterDomain({ Kind = Identifier "D" }, DomainAlteration.DropConstraint { Kind = Identifier "C" }) -> ()
    | res -> Assert.Fail(sprintf "Expected AlterDomain DropConstraint, got %A" res)

[<Fact>]
let ``DROP DOMAIN verification`` () =
    match parse "DROP DOMAIN d CASCADE" with
    | Drop(DropDomain({ Kind = Identifier "D" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropDomain CASCADE, got %A" res)

    match parse "DROP DOMAIN d RESTRICT" with
    | Drop(DropDomain({ Kind = Identifier "D" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropDomain RESTRICT, got %A" res)

    parseFails "DROP DOMAIN d"

[<Fact>]
let ``CREATE CHARACTER SET verification`` () =
    match parse "CREATE CHARACTER SET utf8 AS GET utf8" with
    | CreateCharacterSet({ Kind = Identifier "UTF8" }, { Kind = Identifier "UTF8" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateCharacterSet, got %A" res)

    match parse "CREATE CHARACTER SET cs GET latin1 COLLATE en_us" with
    | CreateCharacterSet({ Kind = Identifier "CS" }, { Kind = Identifier "LATIN1" }, Some { Kind = Identifier "EN_US" }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected CreateCharacterSet with collation, got %A" res)

[<Fact>]
let ``DROP CHARACTER SET verification`` () =
    match parse "DROP CHARACTER SET utf8" with
    | Drop(DropCharacterSet { Kind = Identifier "UTF8" }) -> ()
    | res -> Assert.Fail(sprintf "Expected DropCharacterSet, got %A" res)

[<Fact>]
let ``CREATE COLLATION verification`` () =
    match parse "CREATE COLLATION my_coll FOR utf8 FROM existing_coll NO PAD" with
    | CreateCollation({ Kind = Identifier "MY_COLL" },
                      { Kind = Identifier "UTF8" },
                      { Kind = Identifier "EXISTING_COLL" },
                      Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateCollation NO PAD, got %A" res)

    match parse "CREATE COLLATION c2 FOR utf8 FROM ec PAD SPACE" with
    | CreateCollation({ Kind = Identifier "C2" }, _, _, Some false) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateCollation PAD SPACE, got %A" res)

    match parse "CREATE COLLATION c3 FOR utf8 FROM ec" with
    | CreateCollation({ Kind = Identifier "C3" }, _, _, None) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateCollation without pad, got %A" res)

[<Fact>]
let ``DROP COLLATION verification`` () =
    match parse "DROP COLLATION my_coll CASCADE" with
    | Drop(DropCollation({ Kind = Identifier "MY_COLL" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropCollation CASCADE, got %A" res)

    match parse "DROP COLLATION my_coll RESTRICT" with
    | Drop(DropCollation({ Kind = Identifier "MY_COLL" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropCollation RESTRICT, got %A" res)

[<Fact>]
let ``CREATE TRANSLATION verification`` () =
    match parse "CREATE TRANSLATION tr FOR utf8 TO utf16 FROM translit" with
    | CreateTransliteration({ Kind = Identifier "TR" },
                            { Kind = Identifier "UTF8" },
                            { Kind = Identifier "UTF16" },
                            { Kind = Identifier "TRANSLIT" }) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTransliteration, got %A" res)

[<Fact>]
let ``DROP TRANSLATION verification`` () =
    match parse "DROP TRANSLATION tr" with
    | Drop(DropTransliteration { Kind = Identifier "TR" }) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTransliteration, got %A" res)

[<Fact>]
let ``CREATE ASSERTION verification`` () =
    match parse "CREATE ASSERTION a CHECK (x > 0) INITIALLY DEFERRED NOT DEFERRABLE" with
    | CreateAssertion({ Kind = Identifier "A" },
                      _,
                      { InitiallyDeferred = Some true
                        Deferrable = Some false
                        Enforced = None }) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateAssertion, got %A" res)

    match parse "CREATE ASSERTION a2 CHECK (x < 100) NOT ENFORCED" with
    | CreateAssertion({ Kind = Identifier "A2" }, _, { Enforced = Some false }) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateAssertion NOT ENFORCED, got %A" res)

[<Fact>]
let ``DROP ASSERTION verification`` () =
    match parse "DROP ASSERTION a CASCADE" with
    | Drop(DropAssertion({ Kind = Identifier "A" }, Some true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropAssertion CASCADE, got %A" res)

    match parse "DROP ASSERTION a" with
    | Drop(DropAssertion({ Kind = Identifier "A" }, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropAssertion without behavior, got %A" res)

[<Fact>]
let ``CREATE CAST verification`` () =
    match parse "CREATE CAST (INT AS VARCHAR(10)) WITH SPECIFIC FUNCTION f" with
    | CreateCast(Integer, Varchar(Some 10), { Kind = Identifier "F" }, false) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateCast, got %A" res)

    match parse "CREATE CAST (INT AS BIGINT) WITH f AS ASSIGNMENT" with
    | CreateCast(Integer, BigInt, { Kind = Identifier "F" }, true) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateCast AS ASSIGNMENT, got %A" res)

    match parse "CREATE CAST (VARCHAR(5) AS VARCHAR(10)) WITH ROUTINE cast_it" with
    | CreateCast(Varchar(Some 5), Varchar(Some 10), { Kind = Identifier "CAST_IT" }, false) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateCast ROUTINE, got %A" res)

[<Fact>]
let ``DROP CAST verification`` () =
    match parse "DROP CAST (INT AS BIGINT) CASCADE" with
    | Drop(DropCast(Integer, BigInt, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropCast CASCADE, got %A" res)

    match parse "DROP CAST (VARCHAR(5) AS VARCHAR(10)) RESTRICT" with
    | Drop(DropCast(Varchar(Some 5), Varchar(Some 10), false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropCast RESTRICT, got %A" res)

    parseFails "DROP CAST (INT AS BIGINT)"

[<Fact>]
let ``CREATE ORDERING verification`` () =
    match parse "CREATE ORDERING FOR my_type EQUALS ONLY BY RELATIVE WITH SPECIFIC FUNCTION f" with
    | CreateOrdering({ Kind = Identifier "MY_TYPE" },
                     OrderingForm.EqualsOnlyBy(OrderingCategory.Relative { Kind = Identifier "F" })) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateOrdering RELATIVE, got %A" res)

    match parse "CREATE ORDERING FOR t ORDER FULL BY MAP WITH f" with
    | CreateOrdering({ Kind = Identifier "T" }, OrderingForm.OrderFullBy(OrderingCategory.Map { Kind = Identifier "F" })) ->
        ()
    | res -> Assert.Fail(sprintf "Expected CreateOrdering MAP, got %A" res)

    match parse "CREATE ORDERING FOR t EQUALS ONLY BY STATE" with
    | CreateOrdering({ Kind = Identifier "T" }, OrderingForm.EqualsOnlyBy(OrderingCategory.State None)) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateOrdering STATE, got %A" res)

[<Fact>]
let ``DROP ORDERING verification`` () =
    match parse "DROP ORDERING FOR my_type CASCADE" with
    | Drop(DropOrdering({ Kind = Identifier "MY_TYPE" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropOrdering CASCADE, got %A" res)

    match parse "DROP ORDERING FOR my_type RESTRICT" with
    | Drop(DropOrdering({ Kind = Identifier "MY_TYPE" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropOrdering RESTRICT, got %A" res)

    parseFails "DROP ORDERING FOR my_type"

[<Fact>]
let ``CREATE TRANSFORM verification`` () =
    match parse "CREATE TRANSFORM FOR my_type group1 (TO SQL WITH SPECIFIC FUNCTION f, FROM SQL WITH g)" with
    | CreateTransform({ Kind = Identifier "MY_TYPE" },
                      [ { Name = { Kind = Identifier "GROUP1" }
                          Elements = [ TransformElement.ToSql _; TransformElement.FromSql _ ] } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTransform, got %A" res)

    match parse "CREATE TRANSFORMS FOR t g (FROM SQL WITH f)" with
    | CreateTransform({ Kind = Identifier "T" }, [ { Elements = [ TransformElement.FromSql _ ] } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTransform TRANSFORMS, got %A" res)

[<Fact>]
let ``ALTER TRANSFORM verification`` () =
    match parse "ALTER TRANSFORM FOR my_type g1 (ADD (TO SQL WITH f), DROP (TO SQL, FROM SQL CASCADE))" with
    | AlterTransform({ Kind = Identifier "MY_TYPE" },
                     [ { Name = { Kind = Identifier "G1" }
                         Actions = [ addAction; dropAction ] } ]) ->
        match addAction with
        | TransformAlteration.AddTransformElements [ TransformElement.ToSql _ ] -> ()
        | a -> Assert.Fail(sprintf "Expected AddTransformElements, got %A" a)

        match dropAction with
        | TransformAlteration.DropTransformElements([ TransformKind.ToSqlKind; TransformKind.FromSqlKind ], true) -> ()
        | a -> Assert.Fail(sprintf "Expected DropTransformElements, got %A" a)
    | res -> Assert.Fail(sprintf "Expected AlterTransform, got %A" res)

    match parse "ALTER TRANSFORMS FOR t g (DROP (TO SQL RESTRICT))" with
    | AlterTransform({ Kind = Identifier "T" },
                     [ { Actions = [ TransformAlteration.DropTransformElements([ TransformKind.ToSqlKind ], false) ] } ]) ->
        ()
    | res -> Assert.Fail(sprintf "Expected AlterTransform TRANSFORMS, got %A" res)

[<Fact>]
let ``DROP TRANSFORM verification`` () =
    match parse "DROP TRANSFORM ALL FOR my_type CASCADE" with
    | Drop(DropTransform({ Kind = Identifier "MY_TYPE" }, TransformDropTarget.AllTransforms, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTransform ALL CASCADE, got %A" res)

    match parse "DROP TRANSFORMS g1 FOR my_type RESTRICT" with
    | Drop(DropTransform({ Kind = Identifier "MY_TYPE" },
                         TransformDropTarget.TransformGroup { Kind = Identifier "G1" },
                         false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTransform group RESTRICT, got %A" res)

[<Fact>]
let ``DROP TRANSFORM requires drop behavior`` () =
    parseFails "DROP TRANSFORM ALL FOR my_type"

[<Fact>]
let ``GRANT EXECUTE ON routine verification`` () =
    match parse "GRANT EXECUTE ON FUNCTION add TO alice" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Execute ],
                                           { Kind = Identifier "ADD" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GRANT EXECUTE ON FUNCTION, got %A" res)

    match parse "GRANT SELECT ON PROCEDURE p TO bob" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select None ],
                                           { Kind = Identifier "P" },
                                           [ { Kind = Identifier "BOB" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GRANT SELECT ON PROCEDURE, got %A" res)

    match parse "GRANT SELECT (SPECIFIC FUNCTION f) ON TYPE my_type TO alice" with
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select(Some [ { Kind = Identifier "F" } ]) ],
                                           { Kind = Identifier "MY_TYPE" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           false,
                                           false)) -> ()
    | res -> Assert.Fail(sprintf "Expected GRANT SELECT (method list), got %A" res)

[<Fact>]
let ``DROP ROUTINE verification`` () =
    match parse "DROP FUNCTION add CASCADE" with
    | Drop(DropRoutine({ Kind = Identifier "ADD" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropRoutine CASCADE, got %A" res)

    match parse "DROP PROCEDURE p RESTRICT" with
    | Drop(DropRoutine({ Kind = Identifier "P" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropRoutine RESTRICT, got %A" res)

[<Fact>]
let ``DROP TRIGGER verification`` () =
    match parse "DROP TRIGGER trg" with
    | Drop(DropTrigger { Kind = Identifier "TRG" }) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTrigger, got %A" res)

[<Fact>]
let ``CREATE PROCEDURE verification`` () =
    match
        parse "CREATE PROCEDURE add_employee (IN name VARCHAR(100), OUT id INT) LANGUAGE SQL DETERMINISTIC SELECT 1"
    with
    | CreateProcedure { Name = { Kind = Identifier "ADD_EMPLOYEE" }
                        Parameters = [ { Mode = Some ParameterMode.In
                                         Name = Some { Kind = Identifier "NAME" }
                                         DataType = Varchar(Some 100)
                                         IsResult = false
                                         Default = None }
                                       { Mode = Some ParameterMode.Out
                                         Name = Some { Kind = Identifier "ID" }
                                         DataType = Integer
                                         IsResult = false
                                         Default = None } ]
                        Returns = None
                        Characteristics = [ Language "SQL"; Deterministic true ]
                        Body = SqlRoutine(Select _) } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateProcedure, got %A" res)

[<Fact>]
let ``CREATE PROCEDURE with INOUT and DEFAULT verification`` () =
    match parse "CREATE PROCEDURE p (INOUT x INT DEFAULT 5) SPECIFIC p_spec SELECT 1" with
    | CreateProcedure { Parameters = [ { Mode = Some ParameterMode.InOut
                                         Name = Some { Kind = Identifier "X" }
                                         DataType = Integer
                                         IsResult = false
                                         Default = Some _ } ]
                        Characteristics = [ SpecificName { Kind = Identifier "P_SPEC" } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateProcedure INOUT DEFAULT, got %A" res)

[<Fact>]
let ``CREATE PROCEDURE with BEGIN ATOMIC body verification`` () =
    match parse "CREATE PROCEDURE p () BEGIN ATOMIC SELECT 1; SELECT 2; END" with
    | CreateProcedure { Body = RoutineBody.BeginAtomic [ Select _; Select _ ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateProcedure BEGIN ATOMIC, got %A" res)

[<Fact>]
let ``CREATE PROCEDURE with EXTERNAL body verification`` () =
    match parse "CREATE PROCEDURE p () EXTERNAL NAME ext_proc" with
    | CreateProcedure { Body = ExternalRoutine(Some { Kind = Identifier "EXT_PROC" }) } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateProcedure EXTERNAL NAME, got %A" res)

    match parse "CREATE PROCEDURE p () EXTERNAL" with
    | CreateProcedure { Body = ExternalRoutine None } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateProcedure EXTERNAL, got %A" res)

[<Fact>]
let ``CREATE FUNCTION verification`` () =
    match parse "CREATE FUNCTION add (a INT, b INT) RETURNS INT LANGUAGE SQL DETERMINISTIC READS SQL DATA SELECT 1" with
    | CreateFunction { Name = { Kind = Identifier "ADD" }
                       Parameters = [ { Mode = None
                                        Name = Some { Kind = Identifier "A" }
                                        DataType = Integer
                                        IsResult = false
                                        Default = None }
                                      { Mode = None
                                        Name = Some { Kind = Identifier "B" }
                                        DataType = Integer
                                        IsResult = false
                                        Default = None } ]
                       Returns = Some Integer
                       Characteristics = [ Language "SQL"; Deterministic true; SqlDataAccess ReadsSqlData ]
                       Body = SqlRoutine(Select _) } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateFunction, got %A" res)

[<Fact>]
let ``CREATE FUNCTION with result sets and null-call verification`` () =
    match
        parse
            "CREATE FUNCTION f () RETURNS INT DYNAMIC RESULT SETS 5 RETURNS NULL ON NULL INPUT CALLED ON NULL INPUT SELECT 1"
    with
    | CreateFunction { Returns = Some Integer
                       Characteristics = [ DynamicResultSets 5UL; NullCall true; NullCall false ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateFunction characteristics, got %A" res)

[<Fact>]
let ``ALTER ROUTINE verification`` () =
    match parse "ALTER FUNCTION add LANGUAGE SQL RESTRICT" with
    | AlterRoutine { Routine = { Kind = Identifier "ADD" }
                     Characteristics = [ Language "SQL" ] } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterRoutine, got %A" res)

    match parse "ALTER PROCEDURE p NO SQL" with
    | AlterRoutine { Routine = { Kind = Identifier "P" }
                     Characteristics = [ SqlDataAccess NoSql ] } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterRoutine NO SQL, got %A" res)

[<Fact>]
let ``CREATE TRIGGER verification`` () =
    match
        parse
            "CREATE TRIGGER trg AFTER UPDATE OF salary ON employees REFERENCING OLD ROW AS old_row NEW ROW AS new_row FOR EACH ROW WHEN (new_row.salary > old_row.salary) UPDATE employees SET salary = new_row.salary"
    with
    | CreateTrigger { Name = { Kind = Identifier "TRG" }
                      ActionTime = TriggerActionTime.After
                      Event = TriggerEvent.Update(Some [ { Kind = Identifier "SALARY" } ])
                      Table = { Kind = Identifier "EMPLOYEES" }
                      Transitions = [ TransitionTableOrVariable.OldRow { Kind = Identifier "OLD_ROW" }
                                      TransitionTableOrVariable.NewRow { Kind = Identifier "NEW_ROW" } ]
                      Action = { ForEach = Some true
                                 When = Some _
                                 Statement = SingleStatement(Update _) } } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTrigger, got %A" res)

[<Fact>]
let ``CREATE TRIGGER BEFORE INSERT verification`` () =
    match parse "CREATE TRIGGER trg BEFORE INSERT ON t FOR EACH STATEMENT INSERT INTO logs (msg) VALUES ('x')" with
    | CreateTrigger { ActionTime = TriggerActionTime.Before
                      Event = TriggerEvent.Insert
                      Table = { Kind = Identifier "T" }
                      Transitions = []
                      Action = { ForEach = Some false
                                 When = None
                                 Statement = SingleStatement(Insert _) } } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTrigger BEFORE INSERT, got %A" res)

[<Fact>]
let ``CREATE TRIGGER INSTEAD OF with transition tables verification`` () =
    match
        parse
            "CREATE TRIGGER trg INSTEAD OF DELETE ON t REFERENCING OLD TABLE AS old_tbl NEW TABLE AS new_tbl BEGIN ATOMIC DELETE FROM t; END"
    with
    | CreateTrigger { ActionTime = TriggerActionTime.InsteadOf
                      Event = TriggerEvent.Delete
                      Transitions = [ TransitionTableOrVariable.OldTable { Kind = Identifier "OLD_TBL" }
                                      TransitionTableOrVariable.NewTable { Kind = Identifier "NEW_TBL" } ]
                      Action = { ForEach = None
                                 When = None
                                 Statement = TriggeredStatement.BeginAtomic [ Delete _ ] } } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTrigger INSTEAD OF, got %A" res)

[<Fact>]
let ``CREATE PROCEDURE without body is rejected`` () = parseFails "CREATE PROCEDURE p ()"

[<Fact>]
let ``CREATE FUNCTION without RETURNS is rejected`` () =
    parseFails "CREATE FUNCTION f () SELECT 1"

[<Fact>]
let ``CREATE TRIGGER without action is rejected`` () =
    parseFails "CREATE TRIGGER trg AFTER INSERT ON t"

[<Fact>]
let ``CREATE TYPE with member list verification`` () =
    match parse "CREATE TYPE my_type AS (a INT, b VARCHAR(10))" with
    | CreateType { Name = { Kind = Identifier "MY_TYPE" }
                   Under = None
                   Representation = Some(TypeRepresentation.MemberList attrs)
                   Options = []
                   Methods = [] } ->
        match attrs with
        | [ { Name = { Kind = Identifier "A" }
              DataType = Integer
              Default = None
              Collate = None }
            { Name = { Kind = Identifier "B" }
              DataType = Varchar(Some 10)
              Default = None
              Collate = None } ] -> ()
        | _ -> Assert.Fail(sprintf "Unexpected attributes: %A" attrs)
    | res -> Assert.Fail(sprintf "Expected CreateType member list, got %A" res)

[<Fact>]
let ``CREATE TYPE with UNDER verification`` () =
    match parse "CREATE TYPE sub_type UNDER my_type AS (c INT)" with
    | CreateType { Name = { Kind = Identifier "SUB_TYPE" }
                   Under = Some { Kind = Identifier "MY_TYPE" }
                   Representation = Some(TypeRepresentation.MemberList attrs)
                   Options = []
                   Methods = [] } ->
        match attrs with
        | [ { Name = { Kind = Identifier "C" }
              DataType = Integer
              Default = None
              Collate = None } ] -> ()
        | _ -> Assert.Fail(sprintf "Unexpected attributes: %A" attrs)
    | res -> Assert.Fail(sprintf "Expected CreateType UNDER, got %A" res)

[<Fact>]
let ``CREATE TYPE with options verification`` () =
    match parse "CREATE TYPE my_type AS (a INT) INSTANTIABLE NOT FINAL REF USING INT" with
    | CreateType { Options = [ TypeOption.Instantiable true; TypeOption.Final false; TypeOption.RefUsing Integer ] } ->
        ()
    | res -> Assert.Fail(sprintf "Expected CreateType options, got %A" res)

[<Fact>]
let ``CREATE TYPE with REF options verification`` () =
    match parse "CREATE TYPE my_type AS (a INT) REF FROM (a) REF IS SYSTEM GENERATED" with
    | CreateType { Options = [ TypeOption.RefFrom [ { Kind = Identifier "A" } ]; TypeOption.RefIsSystemGenerated ] } ->
        ()
    | res -> Assert.Fail(sprintf "Expected CreateType REF options, got %A" res)

    match parse "CREATE TYPE my_type AS (a INT) CAST (SOURCE AS REF) WITH a" with
    | CreateType { Options = [ TypeOption.CastToRef { Kind = Identifier "A" } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateType CAST option, got %A" res)

[<Fact>]
let ``CREATE TYPE with method specification verification`` () =
    match parse "CREATE TYPE my_type AS (a INT) METHOD m1 (x INT) RETURNS INT LANGUAGE SQL" with
    | CreateType { Methods = [ { Kind = None
                                 Name = { Kind = Identifier "M1" }
                                 Parameters = [ { Mode = None
                                                  Name = Some { Kind = Identifier "X" }
                                                  DataType = Integer
                                                  IsResult = false
                                                  Default = None } ]
                                 Returns = Some Integer
                                 Specific = None
                                 SelfAsResult = false
                                 SelfAsLocator = false
                                 Characteristics = [ Language "SQL" ] } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateType method, got %A" res)

[<Fact>]
let ``CREATE TYPE with INSTANCE and OVERRIDING methods verification`` () =
    match parse "CREATE TYPE my_type AS (a INT) INSTANCE METHOD m1 (x INT) RETURNS INT" with
    | CreateType { Methods = [ { Kind = Some MethodKind.Instance
                                 Name = { Kind = Identifier "M1" }
                                 Returns = Some Integer
                                 Characteristics = [] } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateType INSTANCE method, got %A" res)

    match parse "CREATE TYPE my_type AS (a INT) OVERRIDING METHOD m1 (x INT) RETURNS INT" with
    | CreateType { Methods = [ { Kind = None
                                 Name = { Kind = Identifier "M1" }
                                 Returns = Some Integer } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateType OVERRIDING method, got %A" res)

[<Fact>]
let ``ALTER TYPE attribute actions verification`` () =
    match parse "ALTER TYPE my_type ADD ATTRIBUTE c INT" with
    | AlterType { Name = { Kind = Identifier "MY_TYPE" }
                  Action = AlterTypeAction.AddAttribute { Name = { Kind = Identifier "C" }
                                                          DataType = Integer
                                                          Default = None
                                                          Collate = None } } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterType ADD ATTRIBUTE, got %A" res)

    match parse "ALTER TYPE my_type DROP ATTRIBUTE a RESTRICT" with
    | AlterType { Action = AlterTypeAction.DropAttribute { Kind = Identifier "A" } } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterType DROP ATTRIBUTE, got %A" res)

[<Fact>]
let ``ALTER TYPE method actions verification`` () =
    match parse "ALTER TYPE my_type ADD METHOD m2 (x INT) RETURNS INT" with
    | AlterType { Action = AlterTypeAction.AddMethod({ Kind = None
                                                       Name = { Kind = Identifier "M2" }
                                                       Returns = Some Integer },
                                                     false) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterType ADD METHOD, got %A" res)

    match parse "ALTER TYPE my_type DROP METHOD m1 (INT) RESTRICT" with
    | AlterType { Action = AlterTypeAction.DropMethod(None, { Kind = Identifier "M1" }, [ Integer ]) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterType DROP METHOD, got %A" res)

[<Fact>]
let ``DROP TYPE verification`` () =
    match parse "DROP TYPE my_type RESTRICT" with
    | Drop(DropType({ Kind = Identifier "MY_TYPE" }, false)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropType RESTRICT, got %A" res)

    match parse "DROP TYPE my_type CASCADE" with
    | Drop(DropType({ Kind = Identifier "MY_TYPE" }, true)) -> ()
    | res -> Assert.Fail(sprintf "Expected DropType CASCADE, got %A" res)

[<Fact>]
let ``CREATE TABLE OF type verification`` () =
    match parse "CREATE TABLE t OF my_type" with
    | CreateTable { Table = { Kind = Identifier "T" }
                    Columns = []
                    Constraints = []
                    OfType = Some { Kind = Identifier "MY_TYPE" } } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable OF, got %A" res)

[<Fact>]
let ``CREATE VIEW OF type verification`` () =
    match parse "CREATE VIEW v OF my_type AS SELECT * FROM t" with
    | CreateView { Name = { Kind = Identifier "V" }
                   Columns = None
                   OfType = Some { Kind = Identifier "MY_TYPE" }
                   Query = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateView OF, got %A" res)

[<Fact>]
let ``CREATE TABLE with REF type verification`` () =
    match parse "CREATE TABLE t (r REF(my_type) SCOPE users)" with
    | CreateTable { Columns = [ { Name = { Kind = Identifier "R" }
                                  DataType = ReferenceType(UserDefinedType { Kind = Identifier "MY_TYPE" },
                                                           Some { Kind = Identifier "USERS" }) } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable REF, got %A" res)

[<Fact>]
let ``ALTER TYPE without action is rejected`` () = parseFails "ALTER TYPE my_type"

[<Fact>]
let ``DROP TYPE without behavior is rejected`` () = parseFails "DROP TYPE my_type"
