module SqlParser.Tests.QueryTests

open Xunit
open SqlParser

let parse sql =
    match SqlParser.parse sql with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``TableSource types verification`` () =
    match parse "SELECT * FROM users" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = Table({ Kind = Identifier "USERS" }, None) } -> ()
        | res -> Assert.Fail(sprintf "Expected Table, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM users AS u" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = Table({ Kind = Identifier "USERS" }, Some { Kind = Identifier "U" }) } -> ()
        | res -> Assert.Fail(sprintf "Expected Table, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM (SELECT id FROM users) sub" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = Subquery(SelectQuery { Columns = [ Column({ Kind = Identifier "ID" }, None) ] },
                                 { Kind = Identifier "SUB" },
                                 None) } -> ()
        | res -> Assert.Fail(sprintf "Expected Subquery, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

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
        | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Full SELECT structure verification`` () =
    match
        parse
            "SELECT id, name AS username FROM users u JOIN orders o ON u.id = o.user_id WHERE age > 18 GROUP BY category HAVING num_orders > 5 ORDER BY name DESC"
    with
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
        [ "SELECT 1 UNION SELECT 2",
          { Kind = Union
            IsAll = false
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 UNION ALL SELECT 2",
          { Kind = Union
            IsAll = true
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 INTERSECT SELECT 2",
          { Kind = Intersect
            IsAll = false
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 INTERSECT ALL SELECT 2",
          { Kind = Intersect
            IsAll = true
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 EXCEPT SELECT 2",
          { Kind = Except
            IsAll = false
            IsDistinct = false
            Corresponding = None }
          "SELECT 1 EXCEPT ALL SELECT 2",
          { Kind = Except
            IsAll = true
            IsDistinct = false
            Corresponding = None } ]

    for sql, expectedOp in tests do
        match parse sql with
        | Select(SetOperation(SelectQuery _, op, SelectQuery _)) -> Assert.Equal(expectedOp, op)
        | res -> Assert.Fail(sprintf "Expected set operation %A for sql: %s, got %A" expectedOp sql res)

[<Fact>]
let ``SELECT DISTINCT verification`` () =
    match parse "SELECT DISTINCT name FROM users" with
    | Select(SelectQuery s) -> Assert.True(s.IsDistinct)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Offset and Fetch verification`` () =
    match parse "SELECT * FROM users OFFSET 20 ROWS FETCH FIRST 10 ROWS ONLY" with
    | Select(SelectQuery s) ->
        match s.Offset with
        | Some { Kind = Literal(Number 20m) } -> ()
        | res -> Assert.Fail(sprintf "Expected Offset 20, got %A" res)

        match s.Fetch with
        | Some { Count = { Kind = Literal(Number 10m) } } -> ()
        | res -> Assert.Fail(sprintf "Expected Fetch 10, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``VALUES as Table Source verification`` () =
    match parse "SELECT * FROM (VALUES (1, 'a'), (2, 'b')) AS t(id, name)" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = ValuesTable(rows,
                                    { Kind = Identifier "T" },
                                    Some [ { Kind = Identifier "ID" }; { Kind = Identifier "NAME" } ]) } ->
            Assert.Equal(2, rows.Length)
        | res -> Assert.Fail(sprintf "Expected ValuesTable, got %A" res)
    | _ -> Assert.Fail "Expected Select"

[<Fact>]
let ``Subquery with Column Aliases verification`` () =
    match parse "SELECT a, b FROM (SELECT 1, 2) AS t(a, b)" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = Subquery(_,
                                 { Kind = Identifier "T" },
                                 Some [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ]) } -> ()
        | res -> Assert.Fail(sprintf "Expected Subquery with aliases, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``NATURAL JOIN and USING verification`` () =
    match parse "SELECT * FROM t1 NATURAL JOIN t2" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = JoinedTable { IsNatural = true
                                      JoinType = InnerJoin } } -> ()
        | res -> Assert.Fail(sprintf "Expected NATURAL JOIN, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t1 JOIN t2 USING (id, name)" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = JoinedTable { Condition = Some(Using [ { Kind = Identifier "ID" }; { Kind = Identifier "NAME" } ]) } } ->
            ()
        | res -> Assert.Fail(sprintf "Expected JOIN USING, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM t1 NATURAL LEFT JOIN t2" with
    | Select(SelectQuery s) ->
        match s.From with
        | Some { Kind = JoinedTable { IsNatural = true
                                      JoinType = LeftJoin } } -> ()
        | res -> Assert.Fail(sprintf "Expected NATURAL LEFT JOIN, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Locking clause verification`` () =
    match parse "SELECT * FROM users FOR UPDATE" with
    | Select(SelectQuery s) -> Assert.Equal(Some ForUpdate, s.Locking)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT * FROM users FOR SHARE" with
    | Select(SelectQuery s) -> Assert.Equal(Some ForShare, s.Locking)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Window functions verification`` () =
    match parse "SELECT SUM(salary) OVER (ORDER BY id ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)" with
    | Select(SelectQuery s) ->
        match s.Columns.[0] with
        | Column({ Kind = WindowFunction { Function = { Kind = Identifier "SUM" }
                                           Args = [ { Kind = Identifier "SALARY" } ]
                                           IsDistinct = false
                                           Window = { ExistingWindowName = None
                                                      PartitionBy = []
                                                      OrderBy = [ ({ Kind = Identifier "ID" }, true, _) ]
                                                      Frame = Some { Unit = Rows
                                                                     Start = UnboundedPreceding
                                                                     End = Some CurrentRow } } } },
                 _) -> ()
        | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT AVG(price) OVER (ORDER BY dt RANGE BETWEEN 1 PRECEDING AND 1 FOLLOWING)" with
    | Select(SelectQuery s) ->
        match s.Columns.[0] with
        | Column({ Kind = WindowFunction { Function = { Kind = Identifier "AVG" }
                                           Args = [ { Kind = Identifier "PRICE" } ]
                                           IsDistinct = false
                                           Window = { ExistingWindowName = None
                                                      PartitionBy = []
                                                      OrderBy = [ { Kind = Identifier "DT" }, true, None ]
                                                      Frame = Some { Unit = Range
                                                                     Start = Preceding { Kind = Literal(Number 1m) }
                                                                     End = Some(Following { Kind = Literal(Number 1m) }) } } } },
                 _) -> ()
        | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Window clause verification`` () =
    match parse "SELECT SUM(salary) OVER w FROM emps WINDOW w AS (PARTITION BY dept ORDER BY salary)" with
    | Select(SelectQuery s) ->
        match s.Window.[0] with
        | { Kind = Identifier "W" }, { PartitionBy = [ { Kind = Identifier "DEPT" } ] } -> ()
        | res -> Assert.Fail(sprintf "Expected Window definition, got %A" res)

        match s.Columns.[0] with
        | Column({ Kind = WindowFunction { Function = { Kind = Identifier "SUM" }
                                           Args = [ { Kind = Identifier "SALARY" } ]
                                           IsDistinct = false
                                           Window = { ExistingWindowName = Some { Kind = Identifier "W" }
                                                      PartitionBy = []
                                                      OrderBy = []
                                                      Frame = None } } },
                 _) -> ()
        | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

    match parse "SELECT SUM(a) OVER (w ROWS UNBOUNDED PRECEDING) FROM t WINDOW w AS (PARTITION BY b)" with
    | Select(SelectQuery s) ->
        match s.Window.[0] with
        | { Kind = Identifier "W" }, { PartitionBy = [ { Kind = Identifier "B" } ] } -> ()
        | res -> Assert.Fail(sprintf "Expected Window definition, got %A" res)

        match s.Columns.[0] with
        | Column({ Kind = WindowFunction { Function = { Kind = Identifier "SUM" }
                                           Args = [ { Kind = Identifier "A" } ]
                                           IsDistinct = false
                                           Window = { ExistingWindowName = Some { Kind = Identifier "W" }
                                                      PartitionBy = []
                                                      OrderBy = []
                                                      Frame = Some { Unit = Rows
                                                                     Start = UnboundedPreceding
                                                                     End = None } } } },
                 _) -> ()
        | res -> Assert.Fail(sprintf "Expected WindowFunction, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``ORDER BY NULLS verification`` () =
    let parseOrder sql =
        match parse sql with
        | Select(SelectQuery s) -> s.OrderBy.[0]
        | res -> failwithf "Expected Select, got %A" res

    match parseOrder "SELECT id ORDER BY id DESC NULLS FIRST" with
    | { Kind = Identifier "ID" }, false, Some NullsFirst -> ()
    | res -> Assert.Fail(sprintf "Expected id DESC NULLS FIRST, got %A" res)

    match parseOrder "SELECT id ORDER BY name NULLS LAST" with
    | { Kind = Identifier "NAME" }, true, Some NullsLast -> ()
    | res -> Assert.Fail(sprintf "Expected name ASC NULLS LAST, got %A" res)


[<Fact>]
let ``BETWEEN verification`` () =
    match parse "SELECT * FROM t WHERE x BETWEEN 1 AND 10" with
    | Select(SelectQuery q) ->
        match q.Where with
        | Some { Kind = Between({ Kind = Identifier "X" },
                                false,
                                false,
                                { Kind = Literal(Number 1m) },
                                { Kind = Literal(Number 10m) }) } -> ()
        | res -> Assert.Fail(sprintf "Expected Between, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``IN list verification`` () =
    match parse "SELECT * FROM t WHERE x IN (1, 2, 3)" with
    | Select(SelectQuery q) ->
        match q.Where with
        | Some { Kind = InList({ Kind = Identifier "X" },
                               false,
                               [ { Kind = Literal(Number 1m) }
                                 { Kind = Literal(Number 2m) }
                                 { Kind = Literal(Number 3m) } ]) } -> ()
        | res -> Assert.Fail(sprintf "Expected InList, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``IS NULL verification`` () =
    match parse "SELECT * FROM t WHERE x IS NOT NULL" with
    | Select(SelectQuery q) ->
        match q.Where with
        | Some { Kind = IsNull({ Kind = Identifier "X" }, true) } -> ()
        | res -> Assert.Fail(sprintf "Expected IsNull(true), got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``FETCH clause verification`` () =
    match parse "SELECT * FROM t FETCH FIRST 10 ROWS ONLY" with
    | Select(SelectQuery q) ->
        match q.Fetch with
        | Some { Count = { Kind = Literal(Number 10m) }
                 IsPercent = false
                 WithTies = false } -> ()
        | res -> Assert.Fail(sprintf "Expected Fetch 10, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``UNION CORRESPONDING verification`` () =
    match parse "SELECT a FROM t1 UNION CORRESPONDING BY (a) SELECT a FROM t2" with
    | Select(SetOperation(_, op, _)) ->
        Assert.Equal(Union, op.Kind)

        match op.Corresponding with
        | Some(Some [ { Kind = Identifier "A" } ]) -> ()
        | res -> Assert.Fail(sprintf "Expected Union Corresponding, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Unicode identifier verification`` () =
    match parse "SELECT U&\"id\" FROM t" with
    | Select(SelectQuery q) ->
        match q.Columns with
        | [ Column({ Kind = Identifier "id" }, _) ] -> ()
        | res -> Assert.Fail(sprintf "Expected Identifier ID, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Subquery with WITH clause verification`` () =
    match parse "SELECT * FROM (WITH cte AS (SELECT 1 AS val) SELECT * FROM cte) AS t" with
    | Select(SelectQuery q) ->
        match q.From with
        | Some { Kind = Subquery(WithQuery(false, [ { Name = { Kind = Identifier "CTE" } } ], _),
                                 { Kind = Identifier "T" },
                                 _) } -> ()
        | res -> Assert.Fail(sprintf "Expected Subquery with WithQuery, got %A" res)
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)
