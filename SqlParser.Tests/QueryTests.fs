module SqlParser.Tests.QueryTests

open Xunit
open SqlParser
open SqlParser.Ast

let parse sql =
    match SqlParser.parse sql with
    | Choice1Of2 { Kind = res } -> res
    | Choice2Of2(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``TableSource types verification`` () =
    match parse "SELECT * FROM users" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = Table("USERS", None) } -> ()
        | _ -> Assert.Fail "Expected Table"
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT * FROM users AS u" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = Table("USERS", Some "U") } -> ()
        | _ -> Assert.Fail "Expected Table"
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT * FROM (SELECT id FROM users) sub" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = Subquery(SelectQuery { Columns = [ Column({ Kind = Identifier "ID" }, None) ] }, "SUB", None) } ->
            ()
        | res -> Assert.Fail(sprintf "Expected Subquery, got %A" res)
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
        | Select(SelectQuery s) ->
            match s.From with
            | Some { Kind = JoinedTable { JoinType = jt } } when jt = expected -> ()
            | res -> Assert.Fail(sprintf "Expected Join %A for %s, got %A" expected join res)
        | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Full SELECT structure verification`` () =
    let sql =
        "SELECT id, name AS username FROM users u JOIN orders o ON u.id = o.user_id WHERE age > 18 GROUP BY category HAVING num_orders > 5 ORDER BY name DESC"

    match parse sql with
    | Select(SelectQuery s) ->
        Assert.Equal(2, s.Columns.Length)

        match s.From with
        | Some { Kind = JoinedTable { JoinType = InnerJoin } } -> ()
        | res -> Assert.Fail(sprintf "Expected JoinedTable, got %A" res)

        match s.Where with
        | Some { Kind = BinaryOp(GreaterThan, { Kind = Identifier "AGE" }, { Kind = Literal(Number 18m) }) } -> ()
        | res -> Assert.Fail(sprintf "Expected Where age > 18, got %A" res)

        match s.GroupBy with
        | [ { Kind = Identifier "CATEGORY" } ] -> ()
        | res -> Assert.Fail(sprintf "Expected GroupBy category, got %A" res)

        match s.Having with
        | Some { Kind = BinaryOp(GreaterThan, { Kind = Identifier "NUM_ORDERS" }, { Kind = Literal(Number 5m) }) } -> ()
        | res -> Assert.Fail(sprintf "Expected Having num_orders > 5, got %A" res)

        match s.OrderBy with
        | [ { Kind = Identifier "NAME" }, false, None ] -> ()
        | res -> Assert.Fail(sprintf "Expected OrderBy name DESC, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Set operations verification`` () =
    let tests =
        [ "SELECT 1 UNION SELECT 2", Union false
          "SELECT 1 UNION ALL SELECT 2", Union true
          "SELECT 1 INTERSECT SELECT 2", Intersect false
          "SELECT 1 INTERSECT ALL SELECT 2", Intersect true
          "SELECT 1 EXCEPT SELECT 2", Except false
          "SELECT 1 EXCEPT ALL SELECT 2", Except true ]

    for sql, expectedOp in tests do
        match parse sql with
        | Select(SetOperation(SelectQuery _, op, SelectQuery _)) -> Assert.Equal(expectedOp, op)
        | res -> Assert.Fail(sprintf "Expected set operation %A for sql: %s, got %A" expectedOp sql res)

[<Fact>]
let ``SELECT DISTINCT verification`` () =
    match parse "SELECT DISTINCT name FROM users" with
    | Select(SelectQuery s) -> Assert.True(s.IsDistinct)
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Limit and Offset verification`` () =
    match parse "SELECT * FROM users LIMIT 10 OFFSET 5" with
    | Select(SelectQuery s) ->
        match s.Offset with
        | Some { Kind = Literal(Number 5m) } -> ()
        | _ -> Assert.Fail "Expected Offset 5"

        match s.Fetch with
        | Some { Kind = Literal(Number 10m) } -> ()
        | _ -> Assert.Fail "Expected Fetch 10"
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT * FROM users OFFSET 20 ROWS FETCH FIRST 10 ROWS ONLY" with
    | Select(SelectQuery s) ->
        match s.Offset with
        | Some { Kind = Literal(Number 20m) } -> ()
        | _ -> Assert.Fail "Expected Offset 20"

        match s.Fetch with
        | Some { Kind = Literal(Number 10m) } -> ()
        | _ -> Assert.Fail "Expected Fetch 10"
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``VALUES as Table Source verification`` () =
    let sql = "SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t(id, name)"

    match parse sql with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = ValuesTable(rows, "T", Some [ "ID"; "NAME" ]) } -> Assert.Equal(2, rows.Length)
        | res -> Assert.Fail(sprintf "Expected ValuesTable, got %A" res)
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Subquery with Column Aliases verification`` () =
    let sql = "SELECT a, b FROM (SELECT 1, 2) AS t(a, b)"

    match parse sql with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = Subquery(_, "T", Some [ "A"; "B" ]) } -> ()
        | res -> Assert.Fail(sprintf "Expected Subquery with aliases, got %A" res)
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``NATURAL JOIN and USING verification`` () =
    match parse "SELECT * FROM t1 NATURAL JOIN t2" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = JoinedTable { IsNatural = true
                                      JoinType = InnerJoin } } -> ()
        | res -> Assert.Fail(sprintf "Expected NATURAL JOIN, got %A" res)
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT * FROM t1 JOIN t2 USING (id, name)" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = JoinedTable { Condition = Some(Using [ "ID"; "NAME" ]) } } -> ()
        | res -> Assert.Fail(sprintf "Expected JOIN USING, got %A" res)
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT * FROM t1 NATURAL LEFT JOIN t2" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = JoinedTable { IsNatural = true
                                      JoinType = LeftJoin } } -> ()
        | res -> Assert.Fail(sprintf "Expected NATURAL LEFT JOIN, got %A" res)
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Locking clause verification`` () =
    match parse "SELECT * FROM users FOR UPDATE" with
    | Select(SelectQuery s) -> Assert.Equal(Some ForUpdate, s.Locking)
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT * FROM users FOR SHARE" with
    | Select(SelectQuery s) -> Assert.Equal(Some ForShare, s.Locking)
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Window functions and clauses verification`` () =
    match parse "SELECT SUM(salary) OVER (ORDER BY id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)" with
    | Select(SelectQuery s) ->
        match s.Columns.[0] with
        | Column({ Kind = FunctionCall("SUM", _, _, Some window) }, _) ->
            match window.Frame with
            | Some { Unit = Rows
                     Start = UnboundedPreceding
                     End = Some CurrentRow } -> ()
            | res -> Assert.Fail(sprintf "Expected ROWS BETWEEN ..., got %A" res)
        | _ -> Assert.Fail "Expected FunctionCall"
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT AVG(price) OVER (ORDER BY dt RANGE BETWEEN 1 PRECEDING AND 1 FOLLOWING)" with
    | Select(SelectQuery s) ->
        match s.Columns.[0] with
        | Column({ Kind = FunctionCall("AVG", _, _, Some window) }, _) ->
            match window.Frame with
            | Some { Unit = Range
                     Start = Preceding { Kind = Literal(Number 1m) }
                     End = Some(Following { Kind = Literal(Number 1m) }) } -> ()
            | res -> Assert.Fail(sprintf "Expected RANGE BETWEEN 1 PRECEDING ..., got %A" res)
        | _ -> Assert.Fail "Expected FunctionCall"
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT SUM(salary) OVER w FROM emps WINDOW w AS (PARTITION BY dept ORDER BY salary)" with
    | Select(SelectQuery s) ->
        Assert.Equal(1, s.Window.Length)
        let name, def = s.Window.[0]
        Assert.Equal("W", name)

        match def.PartitionBy with
        | [ { Kind = Identifier "DEPT" } ] -> ()
        | _ -> Assert.Fail "Expected PARTITION BY dept"

        match s.Columns.[0] with
        | Column({ Kind = FunctionCall("SUM", _, _, Some window) }, _) ->
            Assert.Equal(Some "W", window.ExistingWindowName)
        | _ -> Assert.Fail "Expected FunctionCall with window name"
    | _ -> Assert.Fail "Expected Select"

    match parse "SELECT SUM(a) OVER (w ROWS UNBOUNDED PRECEDING) FROM t WINDOW w AS (PARTITION BY b)" with
    | Select(SelectQuery s) ->
        match s.Columns.[0] with
        | Column({ Kind = FunctionCall("SUM", _, _, Some window) }, _) ->
            Assert.Equal(Some "W", window.ExistingWindowName)

            match window.Frame with
            | Some { Unit = Rows
                     Start = UnboundedPreceding
                     End = None } -> ()
            | res -> Assert.Fail(sprintf "Expected ROWS UNBOUNDED PRECEDING, got %A" res)
        | _ -> Assert.Fail "Expected FunctionCall"
    | _ -> Assert.Fail "Expected Select"
