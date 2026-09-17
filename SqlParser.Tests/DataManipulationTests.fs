module SqlParser.Tests.DataManipulationTests

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

// Cursor statements (14.1–14.7), the temporary table declaration (14.16), the locator
// statements (14.17/14.18) and positioned DELETE/UPDATE (14.8/14.13, 20.25/20.27) are
// <SQL procedure statement>s (13.4), not directly executable (22.1), so they use the
// general entry point.
let parseStatement (sql: string) =
    match SqlParser.parseStatement (sql.TrimEnd() + ";") with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseStatementFails (sql: string) =
    match SqlParser.parseStatement (sql.TrimEnd() + ";") with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

[<Theory>]
[<InlineData("SENSITIVE", "Sensitive")>]
[<InlineData("INSENSITIVE", "Insensitive")>]
[<InlineData("ASENSITIVE", "Asensitive")>]
let ``DECLARE CURSOR sensitivity verification`` (keyword: string) (expected: string) =
    match parseStatement (sprintf "DECLARE cur %s CURSOR FOR SELECT a FROM t" keyword) with
    | DeclareCursor { Properties = { Sensitivity = Some actual } } -> Assert.Equal(expected, sprintf "%A" actual)
    | res -> Assert.Fail(sprintf "Expected %s sensitivity, got %A" keyword res)

[<Theory>]
[<InlineData("SCROLL", "Scroll")>]
[<InlineData("NO SCROLL", "NoScroll")>]
let ``DECLARE CURSOR scrollability verification`` (keyword: string) (expected: string) =
    match parseStatement (sprintf "DECLARE cur %s CURSOR FOR SELECT a FROM t" keyword) with
    | DeclareCursor { Properties = { Scrollability = Some actual } } -> Assert.Equal(expected, sprintf "%A" actual)
    | res -> Assert.Fail(sprintf "Expected %s scrollability, got %A" keyword res)

[<Theory>]
[<InlineData("WITH HOLD", "WithHold")>]
[<InlineData("WITHOUT HOLD", "WithoutHold")>]
let ``DECLARE CURSOR holdability verification`` (keyword: string) (expected: string) =
    match parseStatement (sprintf "DECLARE cur CURSOR %s FOR SELECT a FROM t" keyword) with
    | DeclareCursor { Properties = { Holdability = Some actual } } -> Assert.Equal(expected, sprintf "%A" actual)
    | res -> Assert.Fail(sprintf "Expected %s holdability, got %A" keyword res)

[<Theory>]
[<InlineData("WITH RETURN", "WithReturn")>]
[<InlineData("WITHOUT RETURN", "WithoutReturn")>]
let ``DECLARE CURSOR returnability verification`` (keyword: string) (expected: string) =
    match parseStatement (sprintf "DECLARE cur CURSOR %s FOR SELECT a FROM t" keyword) with
    | DeclareCursor { Properties = { Returnability = Some actual } } -> Assert.Equal(expected, sprintf "%A" actual)
    | res -> Assert.Fail(sprintf "Expected %s returnability, got %A" keyword res)

[<Fact>]
let ``DECLARE CURSOR verification`` () =
    match parseStatement "DECLARE cur CURSOR FOR SELECT a FROM t" with
    | DeclareCursor { Name = { Kind = Identifier "CUR" }
                      Properties = { Sensitivity = None
                                     Scrollability = None
                                     Holdability = None
                                     Returnability = None }
                      Specification = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareCursor, got %A" res)

[<Fact>]
let ``DECLARE CURSOR with all cursor properties verification`` () =
    match parseStatement "DECLARE cur INSENSITIVE NO SCROLL CURSOR WITH HOLD WITH RETURN FOR SELECT a FROM t" with
    | DeclareCursor { Properties = { Sensitivity = Some Insensitive
                                     Scrollability = Some NoScroll
                                     Holdability = Some WithHold
                                     Returnability = Some WithReturn } } -> ()
    | res -> Assert.Fail(sprintf "Expected all cursor properties, got %A" res)

[<Fact>]
let ``DECLARE CURSOR with updatability clause verification`` () =
    match parseStatement "DECLARE cur CURSOR FOR SELECT a FROM t FOR UPDATE OF a" with
    | DeclareCursor { Specification = SelectQuery _
                      Updatability = Some(ForUpdate(Some [ { Kind = Identifier "A" } ])) } -> ()
    | res -> Assert.Fail(sprintf "Expected FOR UPDATE OF a, got %A" res)

[<Fact>]
let ``DECLARE CURSOR without CURSOR keyword is rejected`` () =
    parseStatementFails "DECLARE cur FOR SELECT a FROM t"

[<Fact>]
let ``DECLARE CURSOR without FOR is rejected`` () =
    parseStatementFails "DECLARE cur CURSOR"

[<Fact>]
let ``OPEN verification`` () =
    match parseStatement "OPEN cur" with
    | Open({ Kind = Identifier "CUR" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Open, got %A" res)

[<Fact>]
let ``OPEN USING arguments verification`` () =
    // 20.19 <dynamic open statement> ::= OPEN <conventional dynamic cursor name> [ <input using clause> ]
    match parseStatement "OPEN cur USING 1, 2" with
    | Open({ Kind = Identifier "CUR" },
           Some(UsingArguments [ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) } ])) -> ()
    | res -> Assert.Fail(sprintf "Expected OPEN USING arguments, got %A" res)

[<Fact>]
let ``OPEN USING descriptor verification`` () =
    match parseStatement "OPEN cur USING SQL DESCRIPTOR d" with
    | Open({ Kind = Identifier "CUR" }, Some(UsingDescriptor { Kind = Identifier "D" })) -> ()
    | res -> Assert.Fail(sprintf "Expected OPEN USING descriptor, got %A" res)

[<Fact>]
let ``OPEN without cursor name is rejected`` () = parseStatementFails "OPEN"

[<Fact>]
let ``FETCH verification`` () =
    match parseStatement "FETCH cur INTO a, b" with
    | Fetch(None, { Kind = Identifier "CUR" }, UsingArguments [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ]) ->
        ()
    | res -> Assert.Fail(sprintf "Expected Fetch, got %A" res)

[<Fact>]
let ``FETCH INTO SQL DESCRIPTOR verification`` () =
    // 20.20 <dynamic fetch statement> ::= FETCH [ [ <fetch orientation> ] FROM ] <dynamic cursor name> <output using clause>
    match parseStatement "FETCH cur INTO SQL DESCRIPTOR d" with
    | Fetch(None, { Kind = Identifier "CUR" }, UsingDescriptor { Kind = Identifier "D" }) -> ()
    | res -> Assert.Fail(sprintf "Expected FETCH INTO SQL DESCRIPTOR, got %A" res)

[<Fact>]
let ``FETCH INTO DESCRIPTOR without SQL keyword verification`` () =
    match parseStatement "FETCH cur INTO DESCRIPTOR d" with
    | Fetch(None, { Kind = Identifier "CUR" }, UsingDescriptor { Kind = Identifier "D" }) -> ()
    | res -> Assert.Fail(sprintf "Expected FETCH INTO DESCRIPTOR, got %A" res)

[<Fact>]
let ``FETCH NEXT FROM verification`` () =
    match parseStatement "FETCH NEXT FROM cur INTO a" with
    | Fetch(Some Next, { Kind = Identifier "CUR" }, UsingArguments [ { Kind = Identifier "A" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Fetch NEXT FROM, got %A" res)

[<Fact>]
let ``FETCH FROM verification`` () =
    match parseStatement "FETCH FROM cur INTO a" with
    | Fetch(None, { Kind = Identifier "CUR" }, UsingArguments [ { Kind = Identifier "A" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Fetch FROM, got %A" res)

[<Fact>]
let ``FETCH ABSOLUTE verification`` () =
    match parseStatement "FETCH ABSOLUTE 5 FROM cur INTO a" with
    | Fetch(Some(Absolute { Kind = Literal(Number 5m) }),
            { Kind = Identifier "CUR" },
            UsingArguments [ { Kind = Identifier "A" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Fetch ABSOLUTE, got %A" res)

[<Fact>]
let ``FETCH RELATIVE verification`` () =
    // <simple value specification> admits a <signed numeric literal> (5.3), so `-1` is one literal.
    match parseStatement "FETCH RELATIVE -1 FROM cur INTO a" with
    | Fetch(Some(Relative { Kind = Literal(Number -1m) }),
            { Kind = Identifier "CUR" },
            UsingArguments [ { Kind = Identifier "A" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Fetch RELATIVE, got %A" res)

[<Fact>]
let ``DECLARE cursor name is a local qualified name`` () =
    // 5.4 <local qualified name> admits only the MODULE qualifier.
    parseStatement "DECLARE MODULE.c CURSOR FOR SELECT 1 FROM t" |> ignore
    parseStatementFails "DECLARE a.b CURSOR FOR SELECT 1"

[<Fact>]
let ``FETCH without INTO is rejected`` () = parseStatementFails "FETCH cur"

[<Fact>]
let ``CLOSE verification`` () =
    match parseStatement "CLOSE cur" with
    | Close { Kind = Identifier "CUR" } -> ()
    | res -> Assert.Fail(sprintf "Expected Close, got %A" res)

[<Fact>]
let ``CLOSE without cursor name is rejected`` () = parseStatementFails "CLOSE"

[<Fact>]
let ``SELECT INTO verification`` () =
    match parseStatement "SELECT a, b INTO x, y FROM t WHERE id = 1" with
    | SelectInto { IsDistinct = false
                   Columns = [ Column({ Kind = Identifier "A" }, None); Column({ Kind = Identifier "B" }, None) ]
                   Into = [ { Kind = Identifier "X" }; { Kind = Identifier "Y" } ]
                   From = [ { Kind = TableSourceKind.Table({ Kind = Identifier "T" }, None) } ]
                   Where = Some _ } -> ()
    | res -> Assert.Fail(sprintf "Expected SelectInto, got %A" res)

[<Fact>]
let ``SELECT DISTINCT INTO verification`` () =
    match parseStatement "SELECT DISTINCT a INTO x FROM t" with
    | SelectInto { IsDistinct = true
                   Columns = [ Column({ Kind = Identifier "A" }, None) ]
                   Into = [ { Kind = Identifier "X" } ]
                   From = [ { Kind = TableSourceKind.Table({ Kind = Identifier "T" }, None) } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected SelectInto DISTINCT, got %A" res)

[<Fact>]
let ``SELECT INTO GROUP BY verification`` () =
    match parseStatement "SELECT a INTO x FROM t GROUP BY a" with
    | SelectInto { Columns = [ _ ]
                   GroupBy = [ GroupingSet [ { Kind = Identifier "A" } ] ]
                   GroupByDistinct = false } -> ()
    | res -> Assert.Fail(sprintf "Expected SelectInto GROUP BY, got %A" res)

[<Fact>]
let ``SELECT INTO GROUP BY DISTINCT verification`` () =
    match parseStatement "SELECT a INTO x FROM t GROUP BY DISTINCT a" with
    | SelectInto { GroupBy = [ GroupingSet [ _ ] ]
                   GroupByDistinct = true } -> ()
    | res -> Assert.Fail(sprintf "Expected SelectInto GROUP BY DISTINCT, got %A" res)

[<Fact>]
let ``SELECT INTO WINDOW verification`` () =
    match parseStatement "SELECT a INTO x FROM t WINDOW w AS (PARTITION BY a)" with
    | SelectInto s ->
        match s.Window with
        | [ (windowName, def) ] ->
            match windowName.Kind, def.PartitionBy with
            | Identifier "W", [ { Kind = Identifier "A" } ] -> ()
            | res -> Assert.Fail(sprintf "Expected window w partitioning by a, got %A" res)
        | res -> Assert.Fail(sprintf "Expected a window definition, got %A" res)
    | res -> Assert.Fail(sprintf "Expected SelectInto, got %A" res)

[<Fact>]
let ``SELECT INTO without select list is rejected`` () = parseStatementFails "SELECT INTO x"

[<Fact>]
let ``SELECT INTO without a FROM clause is rejected`` () = parseStatementFails "SELECT a INTO x"

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
    match parseStatement "DELETE FROM users WHERE CURRENT OF cur" with
    | Delete { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               Cursor = Some { Kind = Identifier "CUR" }
               Where = None } -> ()
    | res -> Assert.Fail(sprintf "Expected positioned Delete, got %A" res)

[<Fact>]
let ``DELETE without target table (20.25) verification`` () =
    // 20.25 <preparable dynamic delete statement: positioned>
    match parseStatement "DELETE WHERE CURRENT OF cur" with
    | Delete { Target = OmittedTarget
               Cursor = Some { Kind = Identifier "CUR" }
               Where = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Delete without target table, got %A" res)

[<Fact>]
let ``DELETE without target table and search WHERE is rejected`` () =
    parseStatementFails "DELETE WHERE id = 1"

[<Fact>]
let ``DELETE without target table and alias is rejected`` () =
    parseStatementFails "DELETE AS u WHERE CURRENT OF cur"

[<Fact>]
let ``DELETE without target table and FOR PORTION OF is rejected`` () =
    parseStatementFails "DELETE FOR PORTION OF p FROM x TO y WHERE CURRENT OF cur"

[<Fact>]
let ``DELETE with FOR PORTION OF and WHERE CURRENT OF is rejected`` () =
    parseStatementFails "DELETE FROM t FOR PORTION OF p FROM x TO y WHERE CURRENT OF cur"

[<Fact>]
let ``DELETE without FROM and without CURRENT OF is rejected`` () = parseFails "DELETE"

[<Fact>]
let ``DELETE FROM without a table name is rejected`` () = parseFails "DELETE FROM"

[<Fact>]
let ``TRUNCATE TABLE verification`` () =
    match parse "TRUNCATE TABLE logs" with
    | Truncate({ Kind = Identifier "LOGS" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Truncate, got %A" res)

    match parse "TRUNCATE TABLE logs RESTART IDENTITY" with
    | Truncate({ Kind = Identifier "LOGS" }, Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected Truncate RESTART IDENTITY, got %A" res)

[<Fact>]
let ``TRUNCATE without the TABLE keyword is rejected`` () = parseFails "TRUNCATE logs"

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
let ``INSERT DEFAULT VALUES verification`` () =
    match parse "INSERT INTO users DEFAULT VALUES" with
    | Insert { Table = { Kind = Identifier "USERS" }
               Columns = None
               Source = DefaultValues
               Override = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Insert DefaultValues, got %A" res)

[<Fact>]
let ``INSERT DEFAULT VALUES rejects a column list and an override clause`` () =
    parseFails "INSERT INTO users (id) DEFAULT VALUES"
    parseFails "INSERT INTO users OVERRIDING USER VALUE DEFAULT VALUES"

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
let ``INSERT insertion target does not accept ONLY`` () =
    // <insertion target> is a plain <table name> (14.11), unlike <target table>.
    parseFails "INSERT INTO ONLY (users) VALUES (1)"

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

[<Fact>]
let ``MERGE with mismatched action branch is rejected`` () =
    // WHEN MATCHED requires UPDATE/DELETE; WHEN NOT MATCHED requires INSERT
    parseFails "MERGE INTO t USING s ON (1=1) WHEN NOT MATCHED THEN UPDATE SET a = 1"
    parseFails "MERGE INTO t USING s ON (1=1) WHEN MATCHED THEN INSERT VALUES (1)"

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
    match parseStatement "UPDATE users SET name = 'x' WHERE CURRENT OF cur" with
    | Update { Target = TableTarget({ Kind = Identifier "USERS" }, false)
               Cursor = Some { Kind = Identifier "CUR" }
               Where = None } -> ()
    | res -> Assert.Fail(sprintf "Expected positioned Update, got %A" res)

[<Fact>]
let ``UPDATE without target table (20.27) verification`` () =
    // 20.27 <preparable dynamic update statement: positioned>
    match parseStatement "UPDATE SET name = 'x' WHERE CURRENT OF cur" with
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
let ``UPDATE without target table and no CURRENT OF is rejected`` () =
    parseStatementFails "UPDATE SET name = 'x'"

[<Fact>]
let ``UPDATE without target table and search WHERE is rejected`` () =
    parseStatementFails "UPDATE SET name = 'x' WHERE id = 1"

[<Fact>]
let ``UPDATE without target table and alias is rejected`` () =
    parseStatementFails "UPDATE AS u SET name = 'x' WHERE CURRENT OF cur"

[<Fact>]
let ``UPDATE without target table and FOR PORTION OF is rejected`` () =
    parseStatementFails "UPDATE FOR PORTION OF p FROM x TO y SET name = 'x' WHERE CURRENT OF cur"

[<Fact>]
let ``UPDATE with FOR PORTION OF and WHERE CURRENT OF is rejected`` () =
    parseStatementFails "UPDATE t FOR PORTION OF p FROM x TO y SET a = 1 WHERE CURRENT OF cur"

[<Fact>]
let ``Temporary table declaration verification`` () =
    match parseStatement "DECLARE LOCAL TEMPORARY TABLE t (a INT, b VARCHAR(10)) ON COMMIT PRESERVE ROWS" with
    | DeclareTemporaryTable { Name = { Kind = Identifier "T" }
                              Columns = [ { Name = { Kind = Identifier "A" } }; { Name = { Kind = Identifier "B" } } ]
                              Constraints = []
                              OnCommit = Some PreserveOnCommit } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareTemporaryTable, got %A" res)

[<Fact>]
let ``Temporary table declaration without ON COMMIT verification`` () =
    match parseStatement "DECLARE LOCAL TEMPORARY TABLE t (a INT)" with
    | DeclareTemporaryTable { Columns = [ _ ]
                              Constraints = []
                              OnCommit = None } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareTemporaryTable, got %A" res)

[<Fact>]
let ``Temporary table declaration with table constraint verification`` () =
    match parseStatement "DECLARE LOCAL TEMPORARY TABLE t (a INT, PRIMARY KEY (a)) ON COMMIT DELETE ROWS" with
    | DeclareTemporaryTable { Columns = [ _ ]
                              Constraints = [ { Constraint = TableConstraint.PrimaryKey(None,
                                                                                        [ { Kind = Identifier "A" } ]) } ]
                              OnCommit = Some DeleteOnCommit } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareTemporaryTable, got %A" res)

[<Fact>]
let ``Temporary table declaration without table element list is rejected`` () =
    parseStatementFails "DECLARE LOCAL TEMPORARY TABLE t"

[<Fact>]
let ``FREE LOCATOR verification`` () =
    match parseStatement "FREE LOCATOR :loc" with
    | FreeLocator [ { Kind = Parameter ":LOC" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected FreeLocator, got %A" res)

[<Fact>]
let ``FREE LOCATOR multiple references verification`` () =
    match parseStatement "FREE LOCATOR :a, ?" with
    | FreeLocator [ { Kind = Parameter ":A" }; { Kind = Parameter "?" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected FreeLocator with two references, got %A" res)

[<Fact>]
let ``FREE LOCATOR without locator reference is rejected`` () = parseStatementFails "FREE LOCATOR"

[<Fact>]
let ``HOLD LOCATOR verification`` () =
    match parseStatement "HOLD LOCATOR :loc" with
    | HoldLocator [ { Kind = Parameter ":LOC" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected HoldLocator, got %A" res)

[<Fact>]
let ``HOLD LOCATOR multiple references verification`` () =
    match parseStatement "HOLD LOCATOR ?, :b" with
    | HoldLocator [ { Kind = Parameter "?" }; { Kind = Parameter ":B" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected HoldLocator with two references, got %A" res)

[<Fact>]
let ``HOLD LOCATOR without locator reference is rejected`` () = parseStatementFails "HOLD LOCATOR"
