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

/// 11.60 <returns data type> with neither <locator indication> nor <result cast>.
let private returnsData (ty: DataType) =
    ReturnsData(
        { DataType = ty
          AsLocator = false
          CastFrom = None }
        : ReturnsDataType
    )

/// 11.60 <parameter type> ::= <data type> without <locator indication>.
let private dataTypeParam ty = DataTypeParameter(ty, false)

[<Fact>]
let ``CREATE SEQUENCE verification`` () =
    match parse "CREATE SEQUENCE order_seq START WITH 1 INCREMENT BY 2 MAXVALUE 100 CYCLE" with
    | CreateSequence({ Kind = Identifier "ORDER_SEQ" },
                     [ StartWith 1m; IncrementBy 2m; MaxValue(Some 100m); Cycle true ]) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSequence, got %A" res)

    match parse "CREATE SEQUENCE order_seq NO MINVALUE NO CYCLE" with
    | CreateSequence({ Kind = Identifier "ORDER_SEQ" }, [ MinValue None; Cycle false ]) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSequence NO MINVALUE NO CYCLE, got %A" res)

    match parse "CREATE SEQUENCE order_seq MINVALUE 1" with
    | CreateSequence({ Kind = Identifier "ORDER_SEQ" }, [ MinValue(Some 1m) ]) -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSequence MINVALUE, got %A" res)

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
let ``referential triggered action verification`` () =
    // 11.8 <referential triggered action> — both <update rule> and <delete rule>, in either order
    match parse "CREATE TABLE t (a INT REFERENCES p (x) ON UPDATE CASCADE ON DELETE RESTRICT)" with
    | CreateTable { Columns = [ col ] } ->
        match col.References with
        | Some r ->
            Assert.Equal(Some ReferentialAction.Cascade, r.OnUpdate)
            Assert.Equal(Some ReferentialAction.Restrict, r.OnDelete)
        | None -> Assert.Fail "Expected a column-level REFERENCES"
    | res -> Assert.Fail(sprintf "Expected ON UPDATE / ON DELETE, got %A" res)

    match parse "CREATE TABLE t (a INT REFERENCES p (x) ON DELETE SET NULL ON UPDATE NO ACTION)" with
    | CreateTable { Columns = [ col ] } ->
        match col.References with
        | Some r ->
            Assert.Equal(Some ReferentialAction.SetNull, r.OnDelete)
            Assert.Equal(Some ReferentialAction.NoAction, r.OnUpdate)
        | None -> Assert.Fail "Expected a column-level REFERENCES"
    | res -> Assert.Fail(sprintf "Expected delete-then-update rules, got %A" res)

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
let ``<default option> verification`` () =
    match parse "CREATE TABLE t (c INT DEFAULT 0)" with
    | CreateTable { Columns = [ { DefaultValue = Some { Kind = Literal(Number 0m) } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected DEFAULT 0, got %A" res)

    match parse "CREATE TABLE t (c INT DEFAULT -1)" with
    | CreateTable { Columns = [ { DefaultValue = Some { Kind = Literal(Number -1m) } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected DEFAULT -1, got %A" res)

    match parse "CREATE TABLE t (c INT DEFAULT NULL)" with
    | CreateTable { Columns = [ { DefaultValue = Some { Kind = Literal Null } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected DEFAULT NULL, got %A" res)

    match parse "CREATE TABLE t (c INT DEFAULT USER)" with
    | CreateTable { Columns = [ { DefaultValue = Some { Kind = User } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected DEFAULT USER, got %A" res)

    match parse "CREATE TABLE t (c INT DEFAULT CURRENT_DATE)" with
    | CreateTable { Columns = [ { DefaultValue = Some { Kind = CurrentDate } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected DEFAULT CURRENT_DATE, got %A" res)

[<Fact>]
let ``<default option> rejects a general value expression`` () =
    parseFails "CREATE TABLE t (c INT DEFAULT (1 + 2))"
    parseFails "CREATE TABLE t (c INT DEFAULT a + b)"
    parseFails "CREATE TABLE t (c INT DEFAULT ?)"

[<Fact>]
let ``column generation clause verification`` () =
    match parse "CREATE TABLE t (c INT GENERATED ALWAYS AS (a + b))" with
    | CreateTable { Columns = [ { Generation = Some { Kind = BinaryOp(Add, _, _) } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected a generation clause, got %A" res)

    match parse "CREATE TABLE t (c INT)" with
    | CreateTable { Columns = [ { Generation = None } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected no generation clause, got %A" res)

[<Fact>]
let ``system time period column specification verification`` () =
    match parse "CREATE TABLE t (valid_from TIMESTAMP GENERATED ALWAYS AS ROW START)" with
    | CreateTable { Columns = [ { SystemTimePeriod = Some SystemTimePeriodKind.RowStart } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected ROW START, got %A" res)

    match parse "CREATE TABLE t (valid_to TIMESTAMP GENERATED ALWAYS AS ROW END)" with
    | CreateTable { Columns = [ { SystemTimePeriod = Some SystemTimePeriodKind.RowEnd } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected ROW END, got %A" res)

[<Fact>]
let ``column collate clause verification`` () =
    match parse "CREATE TABLE t (c VARCHAR(10) COLLATE en_us)" with
    | CreateTable { Columns = [ { Collation = Some { Kind = Identifier "EN_US" } } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected a column COLLATE clause, got %A" res)

[<Fact>]
let ``named column constraint with characteristics verification`` () =
    match parse "CREATE TABLE t (c INT CONSTRAINT nn NOT NULL NOT DEFERRABLE)" with
    | CreateTable { Columns = [ { Constraints = [ columnConstraint ] } ] } ->
        Assert.Equal(Identifier "NN", columnConstraint.Name.Value.Kind)
        Assert.Equal(ColumnConstraintKind.NotNull, columnConstraint.Kind)
        Assert.Equal(Some false, columnConstraint.Characteristics.Deferrable)
    | res -> Assert.Fail(sprintf "Expected a named column constraint, got %A" res)

[<Fact>]
let ``IDENTITY combined with a default clause is rejected`` () =
    parseFails "CREATE TABLE t (c INT GENERATED ALWAYS AS IDENTITY DEFAULT 5)"

[<Fact>]
let ``the single-value clause precedes the column constraints`` () =
    // 11.4 — [ <default clause> | ... ] comes before [ <column constraint definition>... ]
    match parse "CREATE TABLE t (c INT DEFAULT 1 NOT NULL)" with
    | CreateTable { Columns = [ col ] } ->
        Assert.Equal(Some(Literal(Number 1m)), col.DefaultValue |> Option.map (fun e -> e.Kind))
        Assert.Equal(Some false, col.IsNullable)
    | res -> Assert.Fail(sprintf "Expected DEFAULT before NOT NULL, got %A" res)

    parseFails "CREATE TABLE t (c INT NOT NULL DEFAULT 1)"
    parseFails "CREATE TABLE t (c INT NOT NULL GENERATED ALWAYS AS (b + 1))"

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
let ``Nested collection types are parsed`` () =
    match parse "CREATE TABLE t (c INT ARRAY ARRAY)" with
    | CreateTable { Columns = [ { DataType = ArrayType(ArrayType(Integer, None), None) } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected nested ArrayType, got %A" res)

    match parse "CREATE TABLE t (c INT MULTISET ARRAY [3])" with
    | CreateTable { Columns = [ { DataType = ArrayType(MultisetType Integer, Some 3) } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected ArrayType of MultisetType, got %A" res)

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

        match pk.Constraint with
        | TableConstraint.PrimaryKey(Some name, [ idCol ]) ->
            Assert.Equal(Identifier "PK_ORDERS", name.Kind)
            Assert.Equal(Identifier "ID", idCol.Kind)
        | c -> Assert.Fail(sprintf "Expected PrimaryKey, got %A" c)

        match fk.Constraint with
        | TableConstraint.ForeignKey fkCon ->
            Assert.Equal(Identifier "FK_CUSTOMER", fkCon.Name.Value.Kind)
            Assert.Equal(Identifier "CUSTOMERS", fkCon.Table.Kind)
            Assert.Equal(Some(ReferentialAction.Cascade), fkCon.OnDelete)
            Assert.Equal(None, fkCon.OnUpdate)

            match fkCon.RefColumns with
            | Some [ { Kind = Identifier "ID" } ] -> ()
            | _ -> Assert.Fail("Expected referenced column ID")
        | c -> Assert.Fail(sprintf "Expected ForeignKey, got %A" c)

        match ck.Constraint with
        | TableConstraint.Check(None, _) -> ()
        | c -> Assert.Fail(sprintf "Expected Check, got %A" c)
    | res -> Assert.Fail(sprintf "Expected CreateTable with constraints, got %A" res)

[<Fact>]
let ``CREATE TABLE with a named UNIQUE table constraint verification`` () =
    match parse "CREATE TABLE t (a INT, b INT, CONSTRAINT uq UNIQUE (a, b))" with
    | CreateTable { Constraints = [ constraintDef ] } ->
        match constraintDef.Constraint with
        | TableConstraint.Unique(Some name, [ first; second ]) ->
            Assert.Equal(Identifier "UQ", name.Kind)
            Assert.Equal(Identifier "A", first.Kind)
            Assert.Equal(Identifier "B", second.Kind)
        | c -> Assert.Fail(sprintf "Expected Unique, got %A" c)
    | res -> Assert.Fail(sprintf "Expected a UNIQUE table constraint, got %A" res)

[<Fact>]
let ``table constraint with characteristics verification`` () =
    match parse "CREATE TABLE t (a INT, CONSTRAINT pk PRIMARY KEY (a) INITIALLY DEFERRED NOT ENFORCED)" with
    | CreateTable { Constraints = [ constraintDef ] } ->
        Assert.Equal(Some true, constraintDef.Characteristics.InitiallyDeferred)
        Assert.Equal(Some false, constraintDef.Characteristics.Enforced)

        match constraintDef.Constraint with
        | TableConstraint.PrimaryKey(Some name, _) -> Assert.Equal(Identifier "PK", name.Kind)
        | c -> Assert.Fail(sprintf "Expected PrimaryKey, got %A" c)
    | res -> Assert.Fail(sprintf "Expected constraint characteristics, got %A" res)

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
                   IsRecursive = false
                   Query = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateView, got %A" res)

    match parse "CREATE RECURSIVE VIEW my_view (a) AS SELECT x FROM t1" with
    | CreateView { Name = { Kind = Identifier "MY_VIEW" }
                   IsRecursive = true
                   Query = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateRecursiveView, got %A" res)

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
    match parse "CREATE TABLE backup AS SELECT * FROM users WITH DATA" with
    | CreateTable { Table = { Kind = Identifier "BACKUP" }
                    Columns = []
                    AsQuery = Some(SelectQuery _)
                    WithData = Some true } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable AS SELECT, got %A" res)

    match parse "CREATE TABLE backup (id, name) AS SELECT id, name FROM users WITH NO DATA" with
    | CreateTable { Table = { Kind = Identifier "BACKUP" }
                    AsColumns = Some [ { Kind = Identifier "ID" }; { Kind = Identifier "NAME" } ]
                    AsQuery = Some(SelectQuery _)
                    WithData = Some false } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable AS SELECT WITH NO DATA, got %A" res)

    // <with or without data> is mandatory in SQL-2016
    parseFails "CREATE TABLE backup AS SELECT * FROM users"

[<Fact>]
let ``CREATE TABLE with like clause verification`` () =
    match parse "CREATE TABLE t (LIKE s INCLUDING IDENTITY EXCLUDING DEFAULTS)" with
    | CreateTable { Like = Some({ Kind = Identifier "S" },
                                [ LikeOption.IncludingIdentity; LikeOption.ExcludingDefaults ]) } -> ()
    | res -> Assert.Fail(sprintf "Expected CREATE TABLE LIKE, got %A" res)

    match parse "CREATE TABLE t (a INT)" with
    | CreateTable { Like = None } -> ()
    | res -> Assert.Fail(sprintf "Expected no LIKE clause, got %A" res)

[<Fact>]
let ``CREATE TABLE with table period definition verification`` () =
    match parse "CREATE TABLE t (a INT, PERIOD FOR SYSTEM_TIME (valid_from, valid_to))" with
    | CreateTable { Periods = [ period ] } ->
        Assert.Equal(TimePeriodSpecification.SystemTimePeriod, period.Specification)
        Assert.Equal(Identifier "VALID_FROM", period.BeginColumn.Kind)
        Assert.Equal(Identifier "VALID_TO", period.EndColumn.Kind)
    | res -> Assert.Fail(sprintf "Expected a table period definition, got %A" res)

    match parse "CREATE TABLE t (a INT, PERIOD FOR business_time (bf, bt))" with
    | CreateTable { Periods = [ period ] } ->
        match period.Specification with
        | TimePeriodSpecification.ApplicationTimePeriod { Kind = Identifier "BUSINESS_TIME" } -> ()
        | s -> Assert.Fail(sprintf "Expected ApplicationTimePeriod, got %A" s)
    | res -> Assert.Fail(sprintf "Expected an application time period, got %A" res)

[<Fact>]
let ``CREATE TABLE with system versioning and ON COMMIT verification`` () =
    match parse "CREATE TABLE t (a INT) WITH SYSTEM VERSIONING" with
    | CreateTable { WithSystemVersioning = true
                    OnCommit = None } -> ()
    | res -> Assert.Fail(sprintf "Expected WITH SYSTEM VERSIONING, got %A" res)

    match parse "CREATE TABLE t (a INT) ON COMMIT PRESERVE ROWS" with
    | CreateTable { WithSystemVersioning = false
                    OnCommit = Some TableCommitAction.PreserveOnCommit } -> ()
    | res -> Assert.Fail(sprintf "Expected ON COMMIT PRESERVE ROWS, got %A" res)

    match parse "CREATE TABLE t (a INT) WITH SYSTEM VERSIONING ON COMMIT DELETE ROWS" with
    | CreateTable { WithSystemVersioning = true
                    OnCommit = Some TableCommitAction.DeleteOnCommit } -> ()
    | res -> Assert.Fail(sprintf "Expected ON COMMIT DELETE ROWS, got %A" res)

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
let ``ALTER TABLE ALTER COLUMN actions verification`` () =
    match parse "ALTER TABLE users ALTER COLUMN age SET DEFAULT 0" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "AGE" }, ColumnAlteration.SetDefault _) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn SetDefault, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN age DROP DEFAULT" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "AGE" }, ColumnAlteration.DropDefault) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn DropDefault, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN age SET NOT NULL" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "AGE" }, ColumnAlteration.SetNotNull) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn SetNotNull, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN age DROP NOT NULL" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "AGE" }, ColumnAlteration.DropNotNull) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn DropNotNull, got %A" res)

    match parse "ALTER TABLE people ALTER COLUMN addr ADD SCOPE addresses_tbl" with
    | AlterTable { Table = { Kind = Identifier "PEOPLE" }
                   Action = AlterColumn({ Kind = Identifier "ADDR" },
                                        ColumnAlteration.AddColumnScope { Kind = Identifier "ADDRESSES_TBL" }) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn AddColumnScope, got %A" res)

    match parse "ALTER TABLE people ALTER COLUMN addr DROP SCOPE CASCADE" with
    | AlterTable { Table = { Kind = Identifier "PEOPLE" }
                   Action = AlterColumn({ Kind = Identifier "ADDR" }, ColumnAlteration.DropColumnScope true) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn DropColumnScope CASCADE, got %A" res)

    match parse "ALTER TABLE people ALTER COLUMN addr DROP SCOPE RESTRICT" with
    | AlterTable { Table = { Kind = Identifier "PEOPLE" }
                   Action = AlterColumn({ Kind = Identifier "ADDR" }, ColumnAlteration.DropColumnScope false) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn DropColumnScope RESTRICT, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN name SET DATA TYPE VARCHAR(200)" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "NAME" }, ColumnAlteration.SetDataType(Varchar(Some 200))) } ->
        ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn SetDataType, got %A" res)

[<Fact>]
let ``ALTER TABLE ALTER COLUMN identity actions verification`` () =
    match parse "ALTER TABLE users ALTER COLUMN id SET GENERATED ALWAYS" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "ID" }, ColumnAlteration.AlterIdentityColumn spec) } ->
        Assert.True(spec.Generation = Some true)
        Assert.Equal(0, List.length spec.Options)
    | res -> Assert.Fail(sprintf "Expected AlterIdentityColumn SET GENERATED ALWAYS, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN id SET GENERATED BY DEFAULT SET INCREMENT BY 2 RESTART WITH 10" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "ID" }, ColumnAlteration.AlterIdentityColumn spec) } ->
        Assert.True(spec.Generation = Some false)
        Assert.Equal<SequenceOption list>([ IncrementBy 2m; Restart(Some 10m) ], spec.Options)
    | res -> Assert.Fail(sprintf "Expected AlterIdentityColumn options, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN id RESTART WITH 100" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "ID" }, ColumnAlteration.AlterIdentityColumn spec) } ->
        Assert.True(Option.isNone spec.Generation)
        Assert.Equal<SequenceOption list>([ Restart(Some 100m) ], spec.Options)
    | res -> Assert.Fail(sprintf "Expected AlterIdentityColumn RESTART, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN id SET NO MAXVALUE SET CYCLE" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "ID" }, ColumnAlteration.AlterIdentityColumn spec) } ->
        Assert.True(Option.isNone spec.Generation)
        Assert.Equal<SequenceOption list>([ MaxValue None; Cycle true ], spec.Options)
    | res -> Assert.Fail(sprintf "Expected AlterIdentityColumn basic options, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN id DROP IDENTITY" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "ID" }, ColumnAlteration.DropIdentity) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn DropIdentity, got %A" res)

    match parse "ALTER TABLE users ALTER COLUMN full_name DROP EXPRESSION" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterColumn({ Kind = Identifier "FULL_NAME" }, ColumnAlteration.DropExpression) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterColumn DropExpression, got %A" res)

[<Fact>]
let ``ALTER TABLE DROP COLUMN verification`` () =
    match parse "ALTER TABLE users DROP COLUMN age CASCADE" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = DropColumn({ Kind = Identifier "AGE" }, true) } -> ()
    | res -> Assert.Fail(sprintf "Expected DropColumn CASCADE, got %A" res)

    match parse "ALTER TABLE users DROP age RESTRICT" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = DropColumn({ Kind = Identifier "AGE" }, false) } -> ()
    | res -> Assert.Fail(sprintf "Expected DropColumn RESTRICT, got %A" res)

[<Fact>]
let ``ALTER TABLE ADD CONSTRAINT verification`` () =
    match parse "ALTER TABLE users ADD CONSTRAINT fk_dept FOREIGN KEY (dept_id) REFERENCES departments (id)" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterTableAction.AddConstraint constraintDef } ->
        match constraintDef.Constraint with
        | TableConstraint.ForeignKey fk ->
            Assert.Equal(Identifier "FK_DEPT", fk.Name.Value.Kind)
            Assert.Equal(Identifier "DEPARTMENTS", fk.Table.Kind)

            match fk.Columns with
            | [ { Kind = Identifier "DEPT_ID" } ] -> ()
            | _ -> Assert.Fail("Expected FK column DEPT_ID")
        | c -> Assert.Fail(sprintf "Expected ForeignKey, got %A" c)
    | res -> Assert.Fail(sprintf "Expected AddConstraint, got %A" res)

[<Fact>]
let ``ALTER TABLE ALTER CONSTRAINT verification`` () =
    match parse "ALTER TABLE users ALTER CONSTRAINT fk_dept NOT ENFORCED" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterTableAction.AlterConstraint({ Kind = Identifier "FK_DEPT" }, false) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterConstraint NOT ENFORCED, got %A" res)

    match parse "ALTER TABLE users ALTER CONSTRAINT fk_dept ENFORCED" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterTableAction.AlterConstraint({ Kind = Identifier "FK_DEPT" }, true) } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterConstraint ENFORCED, got %A" res)

[<Fact>]
let ``ALTER TABLE DROP CONSTRAINT verification`` () =
    match parse "ALTER TABLE users DROP CONSTRAINT fk_dept CASCADE" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterTableAction.DropConstraint({ Kind = Identifier "FK_DEPT" }, true) } -> ()
    | res -> Assert.Fail(sprintf "Expected DropConstraint CASCADE, got %A" res)

    match parse "ALTER TABLE users DROP CONSTRAINT fk_dept RESTRICT" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AlterTableAction.DropConstraint({ Kind = Identifier "FK_DEPT" }, false) } -> ()
    | res -> Assert.Fail(sprintf "Expected DropConstraint RESTRICT, got %A" res)

[<Fact>]
let ``ALTER TABLE ADD PERIOD verification`` () =
    match parse "ALTER TABLE orders ADD PERIOD FOR SYSTEM_TIME (valid_from, valid_to)" with
    | AlterTable { Table = { Kind = Identifier "ORDERS" }
                   Action = AddTablePeriod(period, columns) } ->
        Assert.Equal(SystemTimePeriod, period.Specification)
        Assert.Equal(Identifier "VALID_FROM", period.BeginColumn.Kind)
        Assert.Equal(Identifier "VALID_TO", period.EndColumn.Kind)
        Assert.Equal(0, List.length columns)
    | res -> Assert.Fail(sprintf "Expected AddTablePeriod SYSTEM_TIME, got %A" res)

    match parse "ALTER TABLE orders ADD PERIOD FOR business_time (begin_col, end_col)" with
    | AlterTable { Table = { Kind = Identifier "ORDERS" }
                   Action = AddTablePeriod({ Specification = ApplicationTimePeriod { Kind = Identifier "BUSINESS_TIME" } },
                                           _) } -> ()
    | res -> Assert.Fail(sprintf "Expected AddTablePeriod application time, got %A" res)

    match
        parse
            "ALTER TABLE orders ADD PERIOD FOR SYSTEM_TIME (valid_from, valid_to) ADD COLUMN valid_from TIMESTAMP ADD COLUMN valid_to TIMESTAMP"
    with
    | AlterTable { Table = { Kind = Identifier "ORDERS" }
                   Action = AddTablePeriod(_, columns) } ->
        Assert.Equal<ExpressionKind list>(
            [ Identifier "VALID_FROM"; Identifier "VALID_TO" ],
            columns |> List.map (fun column -> column.Name.Kind)
        )
    | res -> Assert.Fail(sprintf "Expected AddTablePeriod with column list, got %A" res)

[<Fact>]
let ``ALTER TABLE DROP PERIOD verification`` () =
    match parse "ALTER TABLE orders DROP PERIOD FOR SYSTEM_TIME CASCADE" with
    | AlterTable { Table = { Kind = Identifier "ORDERS" }
                   Action = DropTablePeriod(SystemTimePeriod, true) } -> ()
    | res -> Assert.Fail(sprintf "Expected DropTablePeriod SYSTEM_TIME, got %A" res)

    match parse "ALTER TABLE orders DROP PERIOD FOR business_time RESTRICT" with
    | AlterTable { Table = { Kind = Identifier "ORDERS" }
                   Action = DropTablePeriod(ApplicationTimePeriod { Kind = Identifier "BUSINESS_TIME" }, false) } -> ()
    | res -> Assert.Fail(sprintf "Expected DropTablePeriod application time, got %A" res)

[<Fact>]
let ``ALTER TABLE SYSTEM VERSIONING verification`` () =
    match parse "ALTER TABLE orders ADD SYSTEM VERSIONING" with
    | AlterTable { Table = { Kind = Identifier "ORDERS" }
                   Action = AddSystemVersioning } -> ()
    | res -> Assert.Fail(sprintf "Expected AddSystemVersioning, got %A" res)

    match parse "ALTER TABLE orders DROP SYSTEM VERSIONING CASCADE" with
    | AlterTable { Table = { Kind = Identifier "ORDERS" }
                   Action = DropSystemVersioning true } -> ()
    | res -> Assert.Fail(sprintf "Expected DropSystemVersioning CASCADE, got %A" res)

    match parse "ALTER TABLE orders DROP SYSTEM VERSIONING RESTRICT" with
    | AlterTable { Table = { Kind = Identifier "ORDERS" }
                   Action = DropSystemVersioning false } -> ()
    | res -> Assert.Fail(sprintf "Expected DropSystemVersioning RESTRICT, got %A" res)

[<Theory>]
[<InlineData("ALTER TABLE users DROP COLUMN age")>]
[<InlineData("ALTER TABLE users DROP CONSTRAINT fk_dept")>]
[<InlineData("ALTER TABLE orders DROP PERIOD FOR business_time")>]
[<InlineData("ALTER TABLE orders DROP SYSTEM VERSIONING")>]
[<InlineData("ALTER TABLE users ALTER COLUMN id")>]
[<InlineData("ALTER TABLE users ALTER COLUMN id SET GENERATED")>]
[<InlineData("ALTER TABLE users ALTER COLUMN id SET INCREMENT")>]
[<InlineData("ALTER TABLE orders ADD PERIOD FOR SYSTEM_TIME (valid_from)")>]
let ``ALTER TABLE actions missing a mandatory clause are rejected`` (sql: string) = parseFails sql

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

    // 11.1 <schema character set or path> — the character set may precede the path
    match parse "CREATE SCHEMA s DEFAULT CHARACTER SET utf8 PATH p1" with
    | CreateSchema { Name = Some { Kind = Identifier "S" }
                     CharacterSet = Some { Kind = Identifier "UTF8" }
                     Path = Some [ { Kind = Identifier "P1" } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSchema with charset then path, got %A" res)

    match parse "CREATE SCHEMA s CREATE TABLE t (id INT)" with
    | CreateSchema { Name = Some { Kind = Identifier "S" }
                     Elements = [ CreateTable _ ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateSchema with element, got %A" res)

[<Fact>]
let ``schema elements are restricted to the CREATE family and GRANT`` () =
    match parse "CREATE SCHEMA s CREATE VIEW v AS SELECT * FROM t" with
    | CreateSchema { Elements = [ CreateView _ ] } -> ()
    | res -> Assert.Fail(sprintf "Expected a view element, got %A" res)

    match parse "CREATE SCHEMA s GRANT SELECT ON TABLE t TO alice" with
    | CreateSchema { Elements = [ Grant _ ] } -> ()
    | res -> Assert.Fail(sprintf "Expected a grant element, got %A" res)

    // 11.1 <schema element> — DROP / ALTER / TRUNCATE / REVOKE are not schema elements
    parseFails "CREATE SCHEMA s DROP TABLE t CASCADE"
    parseFails "CREATE SCHEMA s ALTER TABLE t ADD COLUMN c INT"
    parseFails "CREATE SCHEMA s TRUNCATE TABLE t"
    parseFails "CREATE SCHEMA s REVOKE SELECT ON TABLE t FROM alice RESTRICT"
    parseFails "CREATE SCHEMA s ALTER DOMAIN d DROP DEFAULT"
    parseFails "CREATE SCHEMA s ALTER SEQUENCE q INCREMENT BY 1"

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
let ``constraint characteristics verification`` () =
    // 10.8 alternative 1: <constraint check time> [ [ NOT ] DEFERRABLE ] [ <constraint enforcement> ]
    match parse "CREATE DOMAIN d AS INT CHECK (x > 0) INITIALLY DEFERRED" with
    | CreateDomain { Constraints = [ c ] } ->
        Assert.Equal(Some true, c.Characteristics.InitiallyDeferred)
        Assert.True(Option.isNone c.Characteristics.Deferrable)
        Assert.True(Option.isNone c.Characteristics.Enforced)
    | res -> Assert.Fail(sprintf "Expected INITIALLY DEFERRED, got %A" res)

    // 10.8 alternative 2: [ [ NOT ] DEFERRABLE ] <constraint check time> [ <constraint enforcement> ]
    match parse "CREATE DOMAIN d AS INT CHECK (x > 0) DEFERRABLE INITIALLY IMMEDIATE NOT ENFORCED" with
    | CreateDomain { Constraints = [ c ] } ->
        Assert.Equal(Some false, c.Characteristics.InitiallyDeferred)
        Assert.Equal(Some true, c.Characteristics.Deferrable)
        Assert.Equal(Some false, c.Characteristics.Enforced)
    | res -> Assert.Fail(sprintf "Expected deferrability-first characteristics, got %A" res)

    // 10.8 alternative 3: <constraint enforcement> alone
    match parse "CREATE DOMAIN d AS INT CHECK (x > 0) NOT DEFERRABLE" with
    | CreateDomain { Constraints = [ c ] } -> Assert.Equal(Some false, c.Characteristics.Deferrable)
    | res -> Assert.Fail(sprintf "Expected NOT DEFERRABLE, got %A" res)

    match parse "CREATE DOMAIN d AS INT CHECK (x > 0) ENFORCED" with
    | CreateDomain { Constraints = [ c ] } -> Assert.Equal(Some true, c.Characteristics.Enforced)
    | res -> Assert.Fail(sprintf "Expected ENFORCED, got %A" res)

    // only the three grammar alternatives are accepted
    parseFails "CREATE DOMAIN d AS INT CHECK (x > 0) ENFORCED NOT DEFERRABLE"
    parseFails "CREATE DOMAIN d AS INT CHECK (x > 0) INITIALLY DEFERRED INITIALLY IMMEDIATE"
    parseFails "CREATE DOMAIN d AS INT CHECK (x > 0) DEFERRABLE DEFERRABLE"

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
                            designator) ->
        Assert.False(designator.IsSpecific)
        Assert.True(Option.isNone designator.RoutineType)
        Assert.Equal(Identifier "TRANSLIT", designator.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected CreateTransliteration, got %A" res)

    match parse "CREATE TRANSLATION tr FOR utf8 TO utf16 FROM SPECIFIC FUNCTION tr_fn" with
    | CreateTransliteration(_, _, _, designator) ->
        Assert.True(designator.IsSpecific)
        Assert.Equal(Some RoutineType.Function, designator.RoutineType)
        Assert.Equal(Identifier "TR_FN", designator.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected CreateTransliteration with a routine, got %A" res)

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
    | CreateCast(Integer, Varchar(Some 10), designator, false) ->
        Assert.True(designator.IsSpecific)
        Assert.Equal(Some RoutineType.Function, designator.RoutineType)
        Assert.Equal(Identifier "F", designator.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected CreateCast, got %A" res)

    match parse "CREATE CAST (INT AS BIGINT) WITH f AS ASSIGNMENT" with
    | CreateCast(Integer, BigInt, designator, true) ->
        Assert.False(designator.IsSpecific)
        Assert.True(Option.isNone designator.RoutineType)
        Assert.Equal(Identifier "F", designator.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected CreateCast AS ASSIGNMENT, got %A" res)

    match parse "CREATE CAST (VARCHAR(5) AS VARCHAR(10)) WITH ROUTINE cast_it" with
    | CreateCast(Varchar(Some 5), Varchar(Some 10), designator, false) ->
        Assert.Equal(Some RoutineType.Routine, designator.RoutineType)
        Assert.Equal(Identifier "CAST_IT", designator.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected CreateCast ROUTINE, got %A" res)

    // <member name> with a <data type list> and FOR <schema-resolved user-defined type name>
    match parse "CREATE CAST (INT AS BIGINT) WITH METHOD m (INT, VARCHAR(2)) FOR my_type" with
    | CreateCast(Integer, BigInt, designator, false) ->
        Assert.Equal(Some(RoutineType.Method None), designator.RoutineType)
        Assert.Equal(Identifier "M", designator.Name.Kind)
        Assert.Equal<DataType list>([ Integer; Varchar(Some 2) ], Option.defaultValue [] designator.DataTypeList)
        Assert.Equal(Some(Identifier "MY_TYPE"), designator.ForType |> Option.map (fun e -> e.Kind))
    | res -> Assert.Fail(sprintf "Expected CreateCast METHOD, got %A" res)

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
    | CreateOrdering({ Kind = Identifier "MY_TYPE" }, OrderingForm.EqualsOnlyBy(OrderingCategory.Relative designator)) ->
        Assert.True(designator.IsSpecific)
        Assert.Equal(Some RoutineType.Function, designator.RoutineType)
        Assert.Equal(Identifier "F", designator.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected CreateOrdering RELATIVE, got %A" res)

    match parse "CREATE ORDERING FOR t ORDER FULL BY MAP WITH f" with
    | CreateOrdering({ Kind = Identifier "T" }, OrderingForm.OrderFullBy(OrderingCategory.Map designator)) ->
        Assert.False(designator.IsSpecific)
        Assert.Equal(Identifier "F", designator.Name.Kind)
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
    | Grant(GrantStatement.GrantPrivileges(Privileges.Actions [ PrivilegeAction.Select(Some(PrivilegeMethods [ designator ])) ],
                                           { Kind = Identifier "MY_TYPE" },
                                           [ { Kind = Identifier "ALICE" } ],
                                           false,
                                           false)) ->
        Assert.True(designator.IsSpecific)
        Assert.Equal(Some RoutineType.Function, designator.RoutineType)
        Assert.Equal(Identifier "F", designator.Name.Kind)
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
                        Parameters = [ first; second ]
                        Returns = None
                        Characteristics = [ Language "SQL"; Deterministic true ]
                        Body = SqlRoutine(Select _) } ->
        Assert.Equal(Some ParameterMode.In, first.Mode)
        Assert.Equal(Some(Identifier "NAME"), first.Name |> Option.map (fun e -> e.Kind))
        Assert.Equal(DataTypeParameter(Varchar(Some 100), false), first.ParameterType)
        Assert.False(first.IsResult)
        Assert.Equal(Some ParameterMode.Out, second.Mode)
        Assert.Equal(Some(Identifier "ID"), second.Name |> Option.map (fun e -> e.Kind))
        Assert.Equal(DataTypeParameter(Integer, false), second.ParameterType)
    | res -> Assert.Fail(sprintf "Expected CreateProcedure, got %A" res)

[<Fact>]
let ``CREATE PROCEDURE with INOUT and DEFAULT verification`` () =
    match parse "CREATE PROCEDURE p (INOUT x INT DEFAULT 5) SPECIFIC p_spec SELECT 1" with
    | CreateProcedure { Parameters = [ param ]
                        Characteristics = [ SpecificName { Kind = Identifier "P_SPEC" } ] } ->
        Assert.Equal(Some ParameterMode.InOut, param.Mode)
        Assert.Equal(Some(Identifier "X"), param.Name |> Option.map (fun e -> e.Kind))
        Assert.Equal(DataTypeParameter(Integer, false), param.ParameterType)
        Assert.False(param.IsResult)
        Assert.True(Option.isSome param.Default)
    | res -> Assert.Fail(sprintf "Expected CreateProcedure INOUT DEFAULT, got %A" res)

[<Fact>]
let ``CREATE PROCEDURE with DESCRIPTOR parameter default verification`` () =
    match parse "CREATE PROCEDURE p (IN x INT DEFAULT DESCRIPTOR (a INT, b)) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } ->
        match param.Default with
        | Some { Kind = DescriptorValueConstructor [ ({ Kind = Identifier "A" }, Some _)
                                                     ({ Kind = Identifier "B" }, None) ] } -> ()
        | other -> Assert.Fail(sprintf "Expected DESCRIPTOR default, got %A" other)
    | res -> Assert.Fail(sprintf "Expected CreateProcedure, got %A" res)

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
                       Parameters = [ first; second ]
                       Returns = returns
                       Characteristics = [ Language "SQL"; Deterministic true; SqlDataAccess ReadsSqlData ]
                       Body = SqlRoutine(Select _) } ->
        Assert.Equal(Some(Identifier "A"), first.Name |> Option.map (fun e -> e.Kind))
        Assert.Equal(DataTypeParameter(Integer, false), first.ParameterType)
        Assert.Equal(Some(Identifier "B"), second.Name |> Option.map (fun e -> e.Kind))
        Assert.Equal(Some(returnsData Integer), returns)
    | res -> Assert.Fail(sprintf "Expected CreateFunction, got %A" res)

[<Fact>]
let ``SQL parameter type verification`` () =
    // `IN mytype` — the identifier after the mode is the <parameter type>, not a parameter name
    match parse "CREATE PROCEDURE p (IN mytype) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } ->
        Assert.True(Option.isNone param.Name)

        match param.ParameterType with
        | DataTypeParameter(UserDefinedType { Kind = Identifier "MYTYPE" }, false) -> ()
        | other -> Assert.Fail(sprintf "Expected an anonymous UDT parameter type, got %A" other)
    | res -> Assert.Fail(sprintf "Expected an anonymous UDT parameter, got %A" res)

    // a named parameter with a UDT type
    match parse "CREATE PROCEDURE p (IN p1 mytype) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } ->
        Assert.Equal(Some(Identifier "P1"), param.Name |> Option.map (fun e -> e.Kind))

        match param.ParameterType with
        | DataTypeParameter(UserDefinedType { Kind = Identifier "MYTYPE" }, false) -> ()
        | other -> Assert.Fail(sprintf "Expected a UDT parameter type, got %A" other)
    | res -> Assert.Fail(sprintf "Expected a named UDT parameter, got %A" res)

    // 11.60 <locator indication> ::= AS LOCATOR
    match parse "CREATE PROCEDURE p (x INT AS LOCATOR) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } -> Assert.Equal(DataTypeParameter(Integer, true), param.ParameterType)
    | res -> Assert.Fail(sprintf "Expected AS LOCATOR, got %A" res)

    // 11.60 <descriptor parameter type> ::= DESCRIPTOR
    match parse "CREATE PROCEDURE p (d DESCRIPTOR) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } ->
        Assert.Equal(Some(Identifier "D"), param.Name |> Option.map (fun e -> e.Kind))
        Assert.Equal(DescriptorParameter, param.ParameterType)
    | res -> Assert.Fail(sprintf "Expected DESCRIPTOR parameter, got %A" res)

[<Fact>]
let ``generic table parameter type verification`` () =
    match parse "CREATE PROCEDURE p (t TABLE) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } -> Assert.Equal(GenericTableParameter(None, None), param.ParameterType)
    | res -> Assert.Fail(sprintf "Expected TABLE parameter, got %A" res)

    match parse "CREATE PROCEDURE p (t TABLE PASS THROUGH WITH ROW SEMANTICS) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } ->
        Assert.Equal(
            GenericTableParameter(Some PassThroughOption.PassThrough, Some GenericTableSemantics.RowSemantics),
            param.ParameterType
        )
    | res -> Assert.Fail(sprintf "Expected TABLE PASS THROUGH, got %A" res)

    match parse "CREATE PROCEDURE p (t TABLE NO PASS THROUGH WITH SET SEMANTICS PRUNE ON EMPTY) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } ->
        Assert.Equal(
            GenericTableParameter(
                Some PassThroughOption.NoPassThrough,
                Some(GenericTableSemantics.SetSemantics(Some GenericTablePruning.PruneOnEmpty))
            ),
            param.ParameterType
        )
    | res -> Assert.Fail(sprintf "Expected TABLE NO PASS THROUGH, got %A" res)

    match parse "CREATE PROCEDURE p (t TABLE WITH SET SEMANTICS KEEP ON EMPTY) SELECT 1" with
    | CreateProcedure { Parameters = [ param ] } ->
        Assert.Equal(
            GenericTableParameter(None, Some(GenericTableSemantics.SetSemantics(Some GenericTablePruning.KeepOnEmpty))),
            param.ParameterType
        )
    | res -> Assert.Fail(sprintf "Expected TABLE KEEP ON EMPTY, got %A" res)

[<Fact>]
let ``CREATE FUNCTION returns type verification`` () =
    // 11.60 <returns table type> ::= TABLE [ <table function column list> ]
    match parse "CREATE FUNCTION f () RETURNS TABLE (a INT, b VARCHAR(2)) SELECT 1" with
    | CreateFunction { Returns = returns } ->
        match returns with
        | Some(ReturnsTable(Some [ first; second ])) ->
            Assert.Equal(Identifier "A", first.Name.Kind)
            Assert.Equal(Integer, first.DataType)
            Assert.Equal(Identifier "B", second.Name.Kind)
            Assert.Equal(Varchar(Some 2), second.DataType)
        | other -> Assert.Fail(sprintf "Expected ReturnsTable, got %A" other)
    | res -> Assert.Fail(sprintf "Expected RETURNS TABLE, got %A" res)

    match parse "CREATE FUNCTION f () RETURNS TABLE SELECT 1" with
    | CreateFunction { Returns = Some(ReturnsTable None) } -> ()
    | res -> Assert.Fail(sprintf "Expected RETURNS TABLE without a column list, got %A" res)

    match parse "CREATE FUNCTION f () RETURNS ONLY PASS THROUGH SELECT 1" with
    | CreateFunction { Returns = Some ReturnsOnlyPassThrough } -> ()
    | res -> Assert.Fail(sprintf "Expected RETURNS ONLY PASS THROUGH, got %A" res)

    // 11.60 <result cast> ::= CAST FROM <result cast from type>
    match parse "CREATE FUNCTION f () RETURNS INT CAST FROM BIGINT SELECT 1" with
    | CreateFunction { Returns = returns } ->
        match returns with
        | Some(ReturnsData { DataType = Integer
                             CastFrom = Some(BigInt, false) }) -> ()
        | other -> Assert.Fail(sprintf "Expected a result cast, got %A" other)
    | res -> Assert.Fail(sprintf "Expected RETURNS INT CAST FROM BIGINT, got %A" res)

    // <locator indication> on the returns data type
    match parse "CREATE FUNCTION f () RETURNS INT AS LOCATOR SELECT 1" with
    | CreateFunction { Returns = Some(ReturnsData { AsLocator = true }) } -> ()
    | res -> Assert.Fail(sprintf "Expected RETURNS INT AS LOCATOR, got %A" res)

[<Fact>]
let ``CREATE FUNCTION with result sets and null-call verification`` () =
    match parse "CREATE FUNCTION f () RETURNS INT DYNAMIC RESULT SETS 5 RETURNS NULL ON NULL INPUT SELECT 1" with
    | CreateFunction { Returns = returns
                       Characteristics = [ DynamicResultSets 5UL; NullCall true ] } ->
        Assert.Equal(Some(returnsData Integer), returns)
    | res -> Assert.Fail(sprintf "Expected CreateFunction characteristics, got %A" res)

    match parse "CREATE FUNCTION f () RETURNS INT CALLED ON NULL INPUT SELECT 1" with
    | CreateFunction { Returns = returns
                       Characteristics = [ NullCall false ] } -> Assert.Equal(Some(returnsData Integer), returns)
    | res -> Assert.Fail(sprintf "Expected CreateFunction CALLED ON NULL INPUT, got %A" res)

[<Fact>]
let ``CREATE FUNCTION rejects duplicate characteristics`` () =
    parseFails "CREATE FUNCTION f () RETURNS INT CALLED ON NULL INPUT RETURNS NULL ON NULL INPUT SELECT 1"
    parseFails "CREATE FUNCTION f () RETURNS INT LANGUAGE SQL LANGUAGE SQL SELECT 1"

[<Fact>]
let ``routine characteristics are accepted in any order`` () =
    // 11.60 <routine characteristics> ::= [ <routine characteristic>... ] — the order is unconstrained
    match parse "CREATE FUNCTION f () RETURNS INT DETERMINISTIC LANGUAGE SQL READS SQL DATA SELECT 1" with
    | CreateFunction { Characteristics = [ Deterministic true; Language "SQL"; SqlDataAccess ReadsSqlData ] } -> ()
    | res -> Assert.Fail(sprintf "Expected characteristics in the given order, got %A" res)

[<Fact>]
let ``routine characteristic catalogue verification`` () =
    let sql =
        "CREATE FUNCTION f () RETURNS INT PARAMETER STYLE SQL SPECIFIC f_spec OLD SAVEPOINT LEVEL NAME ext NO SQL SELECT 1"

    match parse sql with
    | CreateFunction { Characteristics = [ ParameterStyle "SQL"
                                           SpecificName { Kind = Identifier "F_SPEC" }
                                           SavepointLevel false
                                           ExternalName { Kind = Identifier "EXT" }
                                           SqlDataAccess NoSql ] } -> ()
    | res -> Assert.Fail(sprintf "Expected the full characteristic catalogue, got %A" res)

[<Fact>]
let ``ALTER ROUTINE verification`` () =
    match parse "ALTER FUNCTION add LANGUAGE SQL RESTRICT" with
    | AlterRoutine { Routine = routine
                     Characteristics = [ Language "SQL" ] } ->
        Assert.Equal(Some RoutineType.Function, routine.RoutineType)
        Assert.Equal(Identifier "ADD", routine.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected AlterRoutine, got %A" res)

    match parse "ALTER PROCEDURE p NO SQL" with
    | AlterRoutine { Routine = routine
                     Characteristics = [ SqlDataAccess NoSql ] } ->
        Assert.Equal(Some RoutineType.Procedure, routine.RoutineType)
        Assert.Equal(Identifier "P", routine.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected AlterRoutine NO SQL, got %A" res)

    // <routine type> with a <data type list> and FOR <schema-resolved user-defined type name>
    match parse "ALTER METHOD m (INT) FOR my_type NO SQL" with
    | AlterRoutine { Routine = routine } ->
        Assert.Equal(Some(RoutineType.Method None), routine.RoutineType)
        Assert.Equal(Identifier "M", routine.Name.Kind)
        Assert.Equal<DataType list>([ Integer ], Option.defaultValue [] routine.DataTypeList)
        Assert.Equal(Some(Identifier "MY_TYPE"), routine.ForType |> Option.map (fun e -> e.Kind))
    | res -> Assert.Fail(sprintf "Expected AlterRoutine METHOD, got %A" res)

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
                                 Parameters = [ param ]
                                 Returns = Some Integer
                                 Specific = None
                                 SelfAsResult = false
                                 SelfAsLocator = false
                                 Characteristics = [ Language "SQL" ] } ] } ->
        Assert.Equal(Some(Identifier "X"), param.Name |> Option.map (fun e -> e.Kind))
        Assert.Equal(DataTypeParameter(Integer, false), param.ParameterType)
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
                    OfType = Some { Kind = Identifier "MY_TYPE" }
                    Under = None } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable OF, got %A" res)

    // 11.3 <subtable clause> ::= UNDER <supertable clause>
    match parse "CREATE TABLE sub_t OF my_type UNDER super_t" with
    | CreateTable { OfType = Some { Kind = Identifier "MY_TYPE" }
                    Under = Some { Kind = Identifier "SUPER_T" } } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable OF ... UNDER, got %A" res)

[<Fact>]
let ``CREATE VIEW OF type verification`` () =
    match parse "CREATE VIEW v OF my_type AS SELECT * FROM t" with
    | CreateView { Name = { Kind = Identifier "V" }
                   Columns = None
                   OfType = Some { Kind = Identifier "MY_TYPE" }
                   Under = None
                   Query = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateView OF, got %A" res)

    // 11.32 <subview clause> ::= UNDER <table name>
    match parse "CREATE VIEW sub_v OF my_type UNDER super_v AS SELECT * FROM t" with
    | CreateView { OfType = Some { Kind = Identifier "MY_TYPE" }
                   Under = Some { Kind = Identifier "SUPER_V" } } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateView OF ... UNDER, got %A" res)

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
