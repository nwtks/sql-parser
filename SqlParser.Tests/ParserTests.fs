module SqlParser.Tests.ParserTests

open Xunit
open SqlParser
open SqlParser.Ast

let parse sql =
    match SqlParser.parse sql with
    | Choice1Of2 result -> result
    | Choice2Of2(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``Position information is captured`` () =
    match SqlParser.parse "SELECT\n  id\nFROM\n  users" with
    | Choice1Of2(Select (SelectQuery s), pos) ->
        Assert.Equal(1L, pos.Line)

        match s.Columns with
        | [ Column((Identifier "ID", idPos), _) ] ->
            Assert.Equal(2L, idPos.Line)
            Assert.Equal(3L, idPos.Column)
        | _ -> Assert.Fail "Expected ID column"
    | _ -> Assert.Fail "Expected successful parse"

[<Fact>]
let ``Error position is captured`` () =
    match SqlParser.parse "SELECT *\nFROM\n  users\nWHERE" with
    | Choice2Of2(ParseError(_, pos)) ->
        Assert.Equal(4L, pos.Line)
        Assert.Equal(6L, pos.Column)
    | _ -> Assert.Fail "Expected failure"

[<Fact>]
let ``Literal types verification`` () =
    let tests =
        [ "'hello'", Literal(String "hello")
          "123.45", Literal(Number 123.45m)
          "TRUE", Literal(Bool true)
          "FALSE", Literal(Bool false)
          "NULL", Literal Null
          "DATE '2023-01-01'", Literal(Date "2023-01-01")
          "TIME '12:00:00'", Literal(Time "12:00:00")
          "TIMESTAMP '2023-01-01 12:00:00'", Literal(Timestamp "2023-01-01 12:00:00")
          "INTERVAL '1' DAY", Literal(Interval "1 DAY")
          "X'01AF'", Literal(Binary [| 1uy; 175uy |]) ]

    for sql, expected in tests do
        match parse (sprintf "SELECT %s" sql) with
        | Select (SelectQuery s), _ ->
            match s.Columns with
            | [ Column((e, _), None) ] -> Assert.Equal(expected, e)
            | _ -> Assert.Fail "Expected Literal"
        | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Binary and Unary operators verification`` () =
    let tests =
        [ "1 + 2", Add
          "1 - 2", Subtract
          "1 * 2", Multiply
          "1 / 2", Divide
          "1 = 2", Equal
          "1 <> 2", NotEqual
          "1 != 2", NotEqual
          "1 < 2", LessThan
          "1 <= 2", LessThanOrEqual
          "1 > 2", GreaterThan
          "1 >= 2", GreaterThanOrEqual
          "1 AND 2", And
          "1 OR 2", Or ]

    for sql, op in tests do
        match parse (sprintf "SELECT %s" sql) with
        | Select (SelectQuery s), _ ->
            match s.Columns with
            | [ Column((BinaryOp(e, (Literal(Number 1m), _), (Literal(Number 2m), _)), _), None) ] ->
                Assert.Equal(op, e)
            | _ -> Assert.Fail "Expected BinaryOp"
        | _ -> Assert.Fail "Expected Select"

    match parse "SELECT NOT TRUE" with
    | Select (SelectQuery s), _ ->
        match s.Columns with
        | [ Column((UnaryOp("NOT", (Literal(Bool true), _)), _), None) ] -> ()
        | _ -> Assert.Fail "Expected UnaryOp"
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``TableSource types (Table, Subquery, Aliases) verification`` () =
    match parse "SELECT * FROM users" with
    | Select (SelectQuery s), _ ->
        match s.From with
        | Some(Table("USERS", None), _) -> ()
        | _ -> Assert.Fail "Expected Table"
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT * FROM users u" with
    | Select (SelectQuery s), _ ->
        match s.From with
        | Some(Table("USERS", Some "U"), _) -> ()
        | _ -> Assert.Fail "Expected Table"
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT * FROM (SELECT id FROM users) sub" with
    | Select (SelectQuery s), _ ->
        match s.From with
        | Some(Subquery(SelectQuery ({ Columns = [ Column((Identifier "ID", _), None) ]; From = Some(Table("USERS", None), _) }),
                        "SUB"),
               _) -> ()
        | _ -> Assert.Fail "Expected Subquery"
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Join types verification`` () =
    let tests =
        [ "JOIN", InnerJoin
          "INNER JOIN", InnerJoin
          "LEFT JOIN", LeftJoin
          "RIGHT JOIN", RightJoin
          "FULL JOIN", FullJoin ]

    for join, expected in tests do
        match parse (sprintf "SELECT * FROM t1 %s t2 ON t1.id = t2.id" join) with
        | Select (SelectQuery s), _ ->
            match s.Joins with
            | [ { JoinType = jt
                  Table = Table("T2", None), _
                  On = Some(BinaryOp(Equal, (Identifier "T1.ID", _), (Identifier "T2.ID", _)), _) } ] ->
                Assert.Equal(expected, jt)
            | _ -> Assert.Fail "Expected Join"
        | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Full SELECT structure verification`` () =
    let sql =
        "
            SELECT id, name AS username 
            FROM users u
            JOIN orders o ON u.id = o.user_id
            WHERE age > 18
            GROUP BY category
            HAVING num_orders > 5
            ORDER BY name DESC
            LIMIT 10"

    match parse sql with
    | Select (SelectQuery s), _ ->
        match s.Columns with
        | [ Column((Identifier "ID", _), None); Column((Identifier "NAME", _), Some "USERNAME") ] -> ()
        | _ -> Assert.Fail "Expected Columns"

        match s.From with
        | Some(Table("USERS", Some "U"), _) -> ()
        | _ -> Assert.Fail "Expected Table"

        match s.Joins with
        | [ { JoinType = InnerJoin
              Table = Table("ORDERS", Some "O"), _
              On = Some(BinaryOp(Equal, (Identifier "U.ID", _), (Identifier "O.USER_ID", _)), _) } ] -> ()
        | _ -> Assert.Fail "Expected Columns"

        match s.Where with
        | Some(BinaryOp(GreaterThan, (Identifier "AGE", _), (Literal(Number 18m), _)), _) -> ()
        | _ -> Assert.Fail "Expected Where"

        match s.GroupBy with
        | [ Identifier "CATEGORY", _ ] -> ()
        | _ -> Assert.Fail "Expected GroupBy"


        match s.Having with
        | Some(BinaryOp(GreaterThan, (Identifier "NUM_ORDERS", _), (Literal(Number 5m), _)), _) -> ()
        | _ -> Assert.Fail "Expected Having"

        match s.OrderBy with
        | [ (Identifier "NAME", _), false ] -> ()
        | _ -> Assert.Fail "Expected OrderBy"

        Assert.Equal(Some 10, s.Limit)
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``INSERT verification`` () =
    match parse "INSERT INTO t (c1) VALUES (1)" with
    | Insert i, _ ->
        Assert.Equal("T", i.Table)
        Assert.Equal(Some [ "C1" ], i.Columns)

        match i.Source with
        | InsertSource.Values [ [ Literal(Number 1m), _ ] ] -> ()
        | _ -> Assert.Fail "Expected Values"
    | _ -> Assert.Fail "Expected Insert"

[<Fact>]
let ``INSERT SELECT verification`` () =
    match parse "INSERT INTO t (c1, c2) SELECT a, b FROM src" with
    | Insert i, _ ->
        Assert.Equal("T", i.Table)
        Assert.Equal(Some [ "C1"; "C2" ], i.Columns)

        match i.Source with
        | InsertSource.Query(SelectQuery s) ->
            match s.From with
            | Some(Table("SRC", None), _) -> ()
            | _ -> Assert.Fail "Expected FROM src"
        | _ -> Assert.Fail "Expected Query source"
    | _ -> Assert.Fail "Expected Insert"

    // INSERT INTO t SELECT * FROM src (without column list)
    match parse "INSERT INTO t SELECT * FROM src" with
    | Insert i, _ ->
        Assert.Equal(None, i.Columns)
        match i.Source with
        | InsertSource.Query(SelectQuery _) -> ()
        | _ -> Assert.Fail "Expected Query source"
    | _ -> Assert.Fail "Expected Insert"


[<Fact>]
let ``UPDATE verification`` () =
    match parse "UPDATE t SET c1 = 1 WHERE id = 1" with
    | Update u, _ ->
        Assert.Equal("T", u.Table)

        match u.Set with
        | [ "C1", (Literal(Number 1m), _) ] -> ()
        | _ -> Assert.Fail "Expected Set"

        match u.Where with
        | Some(BinaryOp(Equal, (Identifier "ID", _), (Literal(Number 1m), _)), _) -> ()
        | _ -> Assert.Fail "Expected Where"
    | _ -> Assert.Fail "Expected Update"


[<Fact>]
let ``DELETE verification`` () =
    match parse "DELETE FROM t WHERE id = 1" with
    | Delete d, _ ->
        Assert.Equal("T", d.Table)

        match d.Where with
        | Some(BinaryOp(Equal, (Identifier "ID", _), (Literal(Number 1m), _)), _) -> ()
        | _ -> Assert.Fail "Expected Where"

    | _ -> Assert.Fail "Expected Delete"

[<Fact>]
let ``Invalid literal formats are rejected`` () =
    let invalidSqls =
        [ "SELECT DATE '2023-13-40'"
          "SELECT TIME 'invalid-time'"
          "SELECT TIMESTAMP '2023-01-01 25:00:00'"
          "SELECT X'01Z'"
          "SELECT X'01A'" ]

    for sql in invalidSqls do
        match SqlParser.parse sql with
        | Choice2Of2(ParseError _) -> ()
        | _ -> Assert.Fail(sprintf "Expected parse error for invalid SQL: %s" sql)

[<Fact>]
let ``Set operations verification`` () =
    let tests =
        [ "SELECT 1 UNION SELECT 2", Union
          "SELECT 1 UNION ALL SELECT 2", UnionAll
          "SELECT 1 INTERSECT SELECT 2", Intersect
          "SELECT 1 INTERSECT ALL SELECT 2", IntersectAll
          "SELECT 1 EXCEPT SELECT 2", Except
          "SELECT 1 EXCEPT ALL SELECT 2", ExceptAll ]

    for sql, expectedOp in tests do
        match SqlParser.parse sql with
        | Choice1Of2(Select(SetOperation(SelectQuery _, op, SelectQuery _)), _) ->
            Assert.Equal(expectedOp, op)
        | _ -> Assert.Fail(sprintf "Expected set operation %A for sql: %s" expectedOp sql)

[<Fact>]
let ``CTE verification`` () =
    let sql = "WITH cte AS (SELECT 1) SELECT * FROM cte"
    match SqlParser.parse sql with
    | Choice1Of2(WithStatement(false, [ { Name = "CTE"; Columns = None; Query = SelectQuery _ } ], Select(SelectQuery _)), _) ->
        ()
    | _ -> Assert.Fail("Expected CTE")

    let sqlRecursive = "WITH RECURSIVE cte (n) AS (SELECT 1 UNION SELECT 2) SELECT * FROM cte"
    match SqlParser.parse sqlRecursive with
    | Choice1Of2(WithStatement(true, [ { Name = "CTE"; Columns = Some ["N"]; Query = SetOperation _ } ], Select(SelectQuery _)), _) ->
        ()
    | _ -> Assert.Fail("Expected Recursive CTE")

[<Fact>]
let ``WITH DML verification`` () =
    // WITH ... INSERT
    let sqlInsert = "WITH src AS (SELECT id, name FROM staging) INSERT INTO users (id, name) SELECT id, name FROM src"
    match SqlParser.parse sqlInsert with
    | Choice1Of2(WithStatement(false, [ { Name = "SRC" } ], Insert _), _) -> ()
    | _ -> Assert.Fail("Expected WITH INSERT")

    // WITH ... UPDATE
    let sqlUpdate = "WITH to_upd AS (SELECT id FROM orders WHERE status = 'x') UPDATE orders SET status = 'done' WHERE id = 1"
    match SqlParser.parse sqlUpdate with
    | Choice1Of2(WithStatement(false, [ { Name = "TO_UPD" } ], Update _), _) -> ()
    | _ -> Assert.Fail("Expected WITH UPDATE")

    // WITH ... DELETE
    let sqlDelete = "WITH old AS (SELECT id FROM logs) DELETE FROM logs WHERE id = 1"
    match SqlParser.parse sqlDelete with
    | Choice1Of2(WithStatement(false, [ { Name = "OLD" } ], Delete _), _) -> ()
    | _ -> Assert.Fail("Expected WITH DELETE")

[<Fact>]
let ``MERGE verification`` () =
    // WHEN MATCHED -> UPDATE, WHEN NOT MATCHED -> INSERT
    let sql = """
        MERGE INTO target t
        USING source s ON t.id = s.id
        WHEN MATCHED THEN UPDATE SET name = s.name
        WHEN NOT MATCHED THEN INSERT (id, name) VALUES (s.id, s.name)"""

    match SqlParser.parse sql with
    | Choice1Of2(Merge m, _) ->
        Assert.Equal("TARGET", m.Target)
        Assert.Equal(Some "T", m.TargetAlias)

        match m.WhenClauses with
        | [ { MatchCondition = Matched; Condition = None; Action = MergeUpdate _ }
            { MatchCondition = NotMatched; Condition = None; Action = MergeInsert _ } ] -> ()
        | _ -> Assert.Fail("Expected MATCHED UPDATE and NOT MATCHED INSERT")
    | _ -> Assert.Fail("Expected MERGE")

    // WHEN MATCHED AND ... -> DELETE
    let sqlDelete = """
        MERGE INTO target t
        USING source s ON t.id = s.id
        WHEN MATCHED AND t.flag = 1 THEN DELETE"""

    match SqlParser.parse sqlDelete with
    | Choice1Of2(Merge m, _) ->
        match m.WhenClauses with
        | [ { MatchCondition = Matched; Condition = Some _; Action = MergeDelete } ] -> ()
        | _ -> Assert.Fail("Expected MATCHED AND DELETE")
    | _ -> Assert.Fail("Expected MERGE")
