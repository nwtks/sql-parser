namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module QueryParser =
    // This module implements the query (SELECT) parts of the SQL-2016 grammar:
    //
    //   7.6  <table reference> / <table factor> / <table primary>
    //   7.10 <joined table>
    //   7.13 <group by clause>
    //   7.15 <window clause>
    //   7.16 <query specification>
    //   7.17 <query expression> / <query expression body> / <query term> / <query primary>
    //        / <simple table> / <with clause> / <order by clause> / <result offset clause>
    //        / <fetch first clause>
    //
    // The trailing clauses (ORDER BY / OFFSET / FETCH / LOCKING) are parsed at the
    // <query expression> level so that they apply to the whole query (including set
    // operations and WITH queries), not just the last SELECT — see
    // applyOrderByOffsetFetch below.

    // Re-export of the top-level query parser. The actual definition lives in
    // pQueryExpression at the bottom of this module (7.17 <query expression>).
    let pQuery = ExpressionParser.pQuery

    // <sort specification> element: <sort key> [ <ordering specification> ] [ <null ordering> ]
    let pOrderByItem = ExpressionParser.pOrderByItem

    let withTablePosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { TableSource.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // ---------------------------------------------------------------------------
    // 7.6 <table reference> / 7.10 <joined table>
    //
    //   <table reference>  ::= <table factor> | <joined table>
    //   <table factor>     ::= <table primary> [ <sample clause> ]
    //   <table primary>    ::= <table or query name> [ <correlation or recognition> ]
    //                        | <derived table> <correlation or recognition>
    //                        | <lateral derived table> <correlation or recognition>
    //                        | <collection derived table> <correlation or recognition>
    //                        | <parenthesized joined table>
    //   <correlation or recognition> ::= [ AS ] <correlation name> [ ( <derived column list> ) ]
    // ---------------------------------------------------------------------------

    // <correlation name> ::= [ AS ] <identifier>   (a.k.a. table alias)
    let pCorrelationName =
        attempt (pKeyword "AS") >>. pIdentifierExpr <|> pIdentifierExpr

    // <correlation or recognition> ::= [ AS ] <correlation name> [ ( <derived column list> ) ]
    let pCorrelationOrRecognition =
        pCorrelationName
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))

    // <table value constructor> ::= VALUES <row value expression>
    //     [ { <comma> <row value expression> }... ]
    //   Used both as a <simple table> (7.17) and as a <derived table> inside
    //   <table primary> (7.6).
    let pTableValueConstructor =
        let pRow =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))

        pKeyword "VALUES" >>. sepBy1 pRow (token (pstring ","))

    // <sample method> ::= BERNOULLI | SYSTEM
    let pSampleMethod =
        pKeyword "BERNOULLI" >>% "BERNOULLI" <|> (pKeyword "SYSTEM" >>% "SYSTEM")

    // <repeatable clause> ::= REPEATABLE ( <repeat argument> )
    let pRepeatableClause =
        pKeyword "REPEATABLE"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression

    // <sample clause> ::= TABLESAMPLE <sample method>
    //     ( <sample percentage> ) [ <repeatable clause> ]
    let pSampleClause =
        pKeyword "TABLESAMPLE" >>. pSampleMethod
        .>>. between (token (pstring "(")) (token (pstring ")")) pExpression
        .>>. opt pRepeatableClause
        |>> fun ((method, percent), repeat) -> (method, percent, repeat)

    // Forward reference so <table primary> can nest a parenthesized <joined table>.
    let pTableReference, pTableReferenceRef =
        createParserForwardedToRef<TableSource, unit> ()

    // <table primary> — the base table source, without the optional <sample clause>
    // (which is applied in the <table factor> rule below).
    let pTablePrimary =
        let pBase =
            choice
                [ // <derived table> = <table subquery>, i.e. ( <query expression> ).
                  // A parenthesized <table value constructor> (VALUES ...) is matched
                  // *before* the subquery form so that FROM ( VALUES ... ) AS t(c1,c2)
                  // yields the dedicated ValuesTable node rather than a wrapped
                  // TableValueConstructor subquery.
                  attempt (
                      between (token (pstring "(")) (token (pstring ")")) pTableValueConstructor
                      .>>. pCorrelationOrRecognition
                      |>> fun (rows, (name, cols)) -> ValuesTable(rows, name, cols)
                  )
                  |> withTablePosition
                  attempt (
                      between (token (pstring "(")) (token (pstring ")")) pQuery
                      .>>. pCorrelationOrRecognition
                      |>> fun (q, (name, cols)) -> Subquery(q, name, cols)
                  )
                  |> withTablePosition
                  attempt (
                      pKeyword "LATERAL"
                      >>. between (token (pstring "(")) (token (pstring ")")) pQuery
                      .>>. pCorrelationOrRecognition
                      |>> fun (q, (name, cols)) -> Lateral(q, name, cols)
                  )
                  |> withTablePosition
                  attempt (
                      pKeyword "UNNEST"
                      >>. between (token (pstring "(")) (token (pstring ")")) pExpression
                      .>>. opt (pKeyword "WITH" >>. pKeyword "ORDINALITY" >>% true)
                      .>>. pCorrelationOrRecognition
                      |>> fun ((expr, ord), (name, cols)) -> Unnest(expr, Option.defaultValue false ord, name, cols)
                  )
                  |> withTablePosition
                  attempt (between (token (pstring "(")) (token (pstring ")")) pTableReference)
                  attempt (pQualifiedName .>>. opt (attempt pCorrelationName) |>> Table)
                  |> withTablePosition ]

        // <table factor> ::= <table primary> [ <sample clause> ]
        pBase .>>. opt pSampleClause
        |>> fun (tbl, sample) ->
            match sample with
            | Some(method, percent, repeat) ->
                { TableSource.Kind = TableSample(tbl, method, percent, repeat)
                  Pos = tbl.Pos }
            | None -> tbl

    // <join type> ::= INNER | <outer join type> [ OUTER ]   (no CROSS — see pJoinType)
    let pJoinTypeWithoutCross =
        choice
            [ attempt (pKeyword "LEFT" .>> opt (pKeyword "OUTER") .>> pKeyword "JOIN")
              >>% LeftJoin
              attempt (pKeyword "RIGHT" .>> opt (pKeyword "OUTER") .>> pKeyword "JOIN")
              >>% RightJoin
              attempt (pKeyword "FULL" .>> opt (pKeyword "OUTER") .>> pKeyword "JOIN")
              >>% FullJoin
              attempt (pKeyword "INNER" .>> pKeyword "JOIN") >>% InnerJoin
              pKeyword "JOIN" >>% InnerJoin ]

    // <cross join> ::= <table reference> CROSS JOIN <table factor>
    let pJoinType =
        choice
            [ attempt (pKeyword "CROSS" .>> pKeyword "JOIN") >>% CrossJoin
              pJoinTypeWithoutCross ]

    // <join specification> ::= <join condition> | <named columns join>
    let pJoinSpecification =
        choice
            [ pKeyword "ON" >>. pExpression |>> On
              pKeyword "USING"
              >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
              |>> Using ]

    // One <joined table> suffix, folded into a left-associative chain:
    //   <joined table> ::= <cross join> | <qualified join> | <natural join>
    // NATURAL is mutually exclusive with CROSS JOIN per the grammar (<natural join>
    // uses <join type>, which has no CROSS), so "NATURAL CROSS JOIN" is rejected.
    let pJoinedTableSuffix =
        let pNatural = opt (pKeyword "NATURAL" >>% true)

        pNatural
        >>= fun nat ->
            let joinType =
                if Option.isSome nat then
                    pJoinTypeWithoutCross
                else
                    pJoinType

            joinType .>>. pTablePrimary .>>. opt pJoinSpecification
            |>> fun ((jt, right), cond) -> Option.defaultValue false nat, jt, right, cond

    // <table reference> ::= <table factor> | <joined table>
    pTableReferenceRef.Value <-
        pTablePrimary .>>. many pJoinedTableSuffix
        |>> fun (first, rests) ->
            rests
            |> List.fold
                (fun acc (nat, jt, right, cond) ->
                    { Kind =
                        JoinedTable
                            { JoinType = jt
                              IsNatural = nat
                              Left = acc
                              Right = right
                              Condition = cond }
                      Pos = acc.Pos })
                first

    // ---------------------------------------------------------------------------
    // 7.15 <window clause>
    //
    //   <window clause>     ::= WINDOW <window definition list>
    //   <window definition> ::= <new window name> AS <window specification>
    //   <window specification> ::= ( [ <existing window name> ] [ <window partition clause> ]
    //                               [ <window order clause> ] [ <window frame clause> ] )
    // ---------------------------------------------------------------------------

    let pWindowDefinition =
        pIdentifierExpr .>> pKeyword "AS"
        .>>. between
            (token (pstring "("))
            (token (pstring ")"))
            (opt pIdentifierExpr
             .>>. opt (
                 pKeyword "PARTITION"
                 >>. pKeyword "BY"
                 >>. sepBy1 pExpression (token (pstring ","))
             )
             .>>. opt (pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ",")))
             .>>. opt pWindowFrame
             |>> fun (((name, pb), ob), frame) ->
                 { ExistingWindowName = name
                   PartitionBy = Option.defaultValue [] pb
                   OrderBy = Option.defaultValue [] ob
                   Frame = frame })

    let pWindowClause =
        pKeyword "WINDOW" >>. sepBy1 pWindowDefinition (token (pstring ","))

    // ---------------------------------------------------------------------------
    // 7.13 <group by clause>
    //
    //   <grouping element> ::= <ordinary grouping set> | <rollup list> | <cube list>
    //                        | <grouping sets specification> | <empty grouping set>
    // ---------------------------------------------------------------------------

    let pGroupingElement, pGroupingElementRef =
        createParserForwardedToRef<GroupingElement, unit> ()

    // <ordinary grouping set> ::= <grouping column reference>
    //                          | ( <grouping column reference list> )
    let pOrdinaryGroupingSet =
        choice
            [ attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
                  |>> GroupingSet
              )
              pExpression |>> fun e -> GroupingSet [ e ] ]

    // <grouping sets specification> ::= GROUPING SETS ( <grouping set list> )
    let pGroupingSetsSpecification =
        pKeyword "GROUPING" .>> pKeyword "SETS"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pGroupingElement (token (pstring ",")))
        |>> GroupingSets

    // <rollup list> ::= ROLLUP ( <ordinary grouping set list> )
    let pRollupList =
        pKeyword "ROLLUP"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pOrdinaryGroupingSet (token (pstring ",")))
        |>> Rollup

    // <cube list> ::= CUBE ( <ordinary grouping set list> )
    let pCubeList =
        pKeyword "CUBE"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pOrdinaryGroupingSet (token (pstring ",")))
        |>> Cube

    // <empty grouping set> ::= ( )
    let pEmptyGroupingSet =
        between (token (pstring "(")) (token (pstring ")")) (preturn EmptyGroupingSet)

    pGroupingElementRef.Value <-
        choice
            [ attempt pGroupingSetsSpecification
              attempt pRollupList
              attempt pCubeList
              attempt pEmptyGroupingSet
              attempt pOrdinaryGroupingSet ]

    // ---------------------------------------------------------------------------
    // 7.16 <query specification>
    //
    //   <query specification> ::= SELECT [ <set quantifier> ] <select list> <table expression>
    //   <table expression>    ::= [ <from clause> ] [ <where clause> ] [ <group by clause> ]
    //                             [ <having clause> ] [ <window clause> ]
    //   <select list>         ::= <asterisk> | <select sublist> [ { <comma> <select sublist> }... ]
    //   <select sublist>      ::= <derived column> | <qualified asterisk>
    //   <derived column>      ::= <value expression> [ <as clause> ]
    // ---------------------------------------------------------------------------

    // <set quantifier> ::= DISTINCT | ALL
    let pSetQuantifier =
        opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))

    // <derived column> ::= <value expression> [ <as clause> ]
    let pDerivedColumn =
        attempt (pExpression .>>. opt (attempt pCorrelationName)) |>> Column

    // <qualified asterisk> ::= <asterisked identifier chain> <period> <asterisk>
    let pQualifiedAsterisk =
        attempt (
            getPosition
            .>>. (pIdentifier .>>. many (attempt (token (pstring ".") >>. pIdentifier))
                  .>> token (pstring ".")
                  .>> pchar '*')
            .>> ws
            |>> fun (pos, (first, rest)) ->
                Column(
                    { Kind = QualifiedStar(first :: rest)
                      Pos = { Line = pos.Line; Column = pos.Column } },
                    None
                )
        )

    // <select list> element: <asterisk> | <select sublist>
    let pSelectSublist =
        pQualifiedAsterisk
        <|> pDerivedColumn
        <|> (pstring "*" .>> ws >>% Star |> withExprPosition |>> fun e -> Column(e, None))

    // <from clause> ::= FROM <table reference list>
    let pFromClause = pKeyword "FROM" >>. sepBy1 pTableReference (token (pstring ","))

    // <where clause> ::= WHERE <search condition>
    let pWhereClause = pKeyword "WHERE" >>. pExpression

    // <group by clause> ::= GROUP BY [ <set quantifier> ] <grouping element list>
    let pGroupByClause =
        pKeyword "GROUP" >>. pKeyword "BY" >>. pSetQuantifier
        .>>. sepBy1 pGroupingElement (token (pstring ","))

    // <having clause> ::= HAVING <search condition>
    let pHavingClause = pKeyword "HAVING" >>. pExpression

    // <query specification> — the SELECT core without ORDER BY/OFFSET/FETCH/LOCKING.
    // Those trailing clauses are parsed at the <query expression> level (7.17) so
    // they apply to the whole query, not just the last SELECT.
    let pSelectBase =
        pipe5
            (pKeyword "SELECT" >>. pSetQuantifier
             .>>. sepBy1 pSelectSublist (token (pstring ",")))
            (opt (attempt pFromClause))
            (opt (attempt pWhereClause))
            (opt (attempt pGroupByClause))
            (opt (attempt pHavingClause))
            (fun (dist, cols) from whr grp hav ->
                let grpDistinct, grpList =
                    match grp with
                    | Some(d, l) -> Option.defaultValue false d, l
                    | None -> false, []

                (Option.defaultValue false dist, cols), from, whr, grpDistinct, grpList, hav)

    let pQuerySpecification =
        pipe2 pSelectBase (opt (attempt pWindowClause)) (fun baseResult window ->
            let distInfo, from, whr, grpDistinct, grpList, hav = baseResult
            let dist, colsList = distInfo

            { IsDistinct = dist
              Columns = colsList
              From = Option.defaultValue [] from
              Where = whr
              GroupBy = grpList
              GroupByDistinct = grpDistinct
              Having = hav
              Window = Option.defaultValue [] window
              OrderBy = []
              Offset = None
              Fetch = None
              Locking = None })

    // ---------------------------------------------------------------------------
    // 7.17 <query expression>
    //
    //   <query expression> ::= [ <with clause> ] <query expression body>
    //                          [ <order by clause> ] [ <result offset clause> ]
    //                          [ <fetch first clause> ]
    //   <query expression body> ::= <query term>
    //       | <query expression body> UNION|EXCEPT [ <corresponding spec> ] <query term>
    //   <query term> ::= <query primary>
    //       | <query term> INTERSECT [ <corresponding spec> ] <query primary>
    //   <query primary> ::= <simple table>
    //       | ( <query expression body> [ <order by clause> ] [ <result offset clause> ]
    //           [ <fetch first clause> ] )
    //   <simple table> ::= <query specification> | <table value constructor> | <explicit table>
    // ---------------------------------------------------------------------------

    // <simple table> — set-operation operands do not consume ORDER BY/OFFSET/FETCH/
    // LOCKING so those apply to the whole expression.
    let pSimpleTable =
        choice
            [ attempt (pQuerySpecification |>> SelectQuery)
              attempt (pTableValueConstructor |>> TableValueConstructor)
              attempt (pKeyword "TABLE" >>. pQualifiedName |>> ExplicitTable) ]

    // <order by clause> ::= ORDER BY <sort specification list>
    let pOrderByClause =
        pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ","))

    // <result offset clause> ::= OFFSET <offset row count> { ROW | ROWS }
    let pResultOffsetClause =
        pKeyword "OFFSET" >>. pExpression
        .>> (attempt (pKeyword "ROWS") <|> pKeyword "ROW")

    // <fetch first clause> ::= FETCH { FIRST | NEXT } [ <fetch first quantity> ]
    //     { ROW | ROWS } { ONLY | WITH TIES }
    // (a missing quantity defaults to 1 per the grammar)
    let pFetchFirstClause =
        getPosition
        >>= fun pos ->
            let defaultCount: Expression =
                { Kind = Literal(Number 1m)
                  Pos = { Line = pos.Line; Column = pos.Column } }

            pKeyword "FETCH" >>. (pKeyword "FIRST" <|> pKeyword "NEXT") >>. opt pExpression
            .>>. opt (pKeyword "PERCENT" >>% true)
            .>> (attempt (pKeyword "ROWS") <|> pKeyword "ROW")
            .>>. (pKeyword "ONLY" >>% false <|> (pKeyword "WITH" >>. pKeyword "TIES" >>% true))
            |>> fun ((countOpt, isPercent), withTies) ->
                { Count = Option.defaultValue defaultCount countOpt
                  IsPercent = Option.defaultValue false isPercent
                  WithTies = withTies }

    // [ <result offset clause> ] [ <fetch first clause> ]
    let pOffsetFetch =
        choice
            [ attempt (pResultOffsetClause .>>. opt pFetchFirstClause)
              |>> fun (o, f) -> Some o, f
              attempt pFetchFirstClause |>> fun f -> None, Some f ]

    // <updatability clause> ::= FOR READ ONLY | FOR UPDATE
    //   (only FOR UPDATE is currently supported)
    let pLockingClause = pKeyword "FOR" >>. pKeyword "UPDATE" >>% ForUpdate

    // <corresponding spec> ::= CORRESPONDING [ BY ( <corresponding column list> ) ]
    let pCorrespondingSpec =
        pKeyword "CORRESPONDING"
        >>. opt (
            pKeyword "BY"
            >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
        )

    // Apply ORDER BY, OFFSET, FETCH, LOCKING to the whole query expression.
    // For a plain SELECT they are folded into the SelectStatement; for set
    // operations and WITH queries they are attached via QueryExpression so the
    // scope is the entire result, not just the last operand.
    let applyOrderByOffsetFetch
        (orderBy: (Expression * bool * NullsOrder option) list)
        (limitOffset: (Expression option * FetchClause option) option)
        (locking: LockingClause option)
        (q: Query)
        =
        let hasTopLevelClauses =
            (not orderBy.IsEmpty) || Option.isSome limitOffset || Option.isSome locking

        if not hasTopLevelClauses then
            q
        else
            match q with
            | SelectQuery s ->
                SelectQuery
                    { s with
                        OrderBy = orderBy @ s.OrderBy
                        Offset = limitOffset |> Option.bind fst
                        Fetch = limitOffset |> Option.bind snd
                        Locking = locking }
            | SetOperation _
            | WithQuery _
            | ExplicitTable _
            | TableValueConstructor _ -> QueryExpression(q, orderBy, limitOffset, locking)
            | QueryExpression _ ->
                // Already wrapped (defensive; not produced by the current grammar)
                q

    let pQueryExpressionBody, pQueryExpressionBodyRef =
        createParserForwardedToRef<Query, unit> ()

    // <query primary> ::= <simple table>
    //   | ( <query expression body> [ <order by clause> ] [ <result offset clause> ]
    //       [ <fetch first clause> ] )
    let pQueryPrimary =
        choice
            [ attempt (
                  between
                      (token (pstring "("))
                      (token (pstring ")"))
                      (pQueryExpressionBody
                       .>>. opt pOrderByClause
                       .>>. opt pOffsetFetch
                       .>>. opt pLockingClause
                       |>> fun (((body, orderBy), limitOffset), locking) ->
                           applyOrderByOffsetFetch (Option.defaultValue [] orderBy) limitOffset locking body)
              )
              pSimpleTable ]

    // INTERSECT operator (higher precedence than UNION/EXCEPT)
    let pIntersectOp =
        pKeyword "INTERSECT"
        >>. opt (pKeyword "ALL" >>% (true, false) <|> (pKeyword "DISTINCT" >>% (false, true)))
        .>>. opt pCorrespondingSpec
        |>> fun (quant, corr) ->
            let isAll, isDist = Option.defaultValue (false, false) quant

            { Kind = Intersect
              IsAll = isAll
              IsDistinct = isDist
              Corresponding = corr }

    // <query term> ::= <query primary> | <query term> INTERSECT ... <query primary>
    let pQueryTerm =
        chainl1 pQueryPrimary (pIntersectOp |>> fun op -> fun l r -> SetOperation(l, op, r))

    // UNION/EXCEPT operator (lower precedence)
    let pUnionExceptOp =
        (choice [ pKeyword "UNION" >>% Union; pKeyword "EXCEPT" >>% Except ])
        .>>. opt (pKeyword "ALL" >>% (true, false) <|> (pKeyword "DISTINCT" >>% (false, true)))
        .>>. opt pCorrespondingSpec
        |>> fun ((kind, quant), corr) ->
            let isAll, isDist = Option.defaultValue (false, false) quant

            { Kind = kind
              IsAll = isAll
              IsDistinct = isDist
              Corresponding = corr }

    // <query expression body> ::= <query term>
    //     | <query expression body> UNION|EXCEPT [ <corresponding spec> ] <query term>
    pQueryExpressionBodyRef.Value <- chainl1 pQueryTerm (pUnionExceptOp |>> fun op -> fun l r -> SetOperation(l, op, r))

    // <query expression> ::= [ <with clause> ] <query expression body>
    //     [ <order by clause> ] [ <result offset clause> ] [ <fetch first clause> ]
    let pQueryExpression =
        let pWithOrQuery =
            choice
                [ attempt (
                      pWithClause
                      .>>. pQueryExpressionBody
                      .>>. opt pOrderByClause
                      .>>. opt pOffsetFetch
                      .>>. opt pLockingClause
                      |>> fun (((((recu, ctes), body), orderBy), limitOffset), locking) ->
                          (WithQuery(recu, ctes, body), Option.defaultValue [] orderBy, limitOffset, locking)
                  )
                  attempt (
                      pQueryExpressionBody
                      .>>. opt pOrderByClause
                      .>>. opt pOffsetFetch
                      .>>. opt pLockingClause
                      |>> fun (((body, orderBy), limitOffset), locking) ->
                          (body, Option.defaultValue [] orderBy, limitOffset, locking)
                  ) ]

        pWithOrQuery
        |>> fun (body, orderBy, limitOffset, locking) -> applyOrderByOffsetFetch orderBy limitOffset locking body

    pQueryRef.Value <- pQueryExpression
