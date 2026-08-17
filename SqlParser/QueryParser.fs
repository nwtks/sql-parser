namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module QueryParser =
    let pQuery = ExpressionParser.pQuery

    let withTablePosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { TableSource.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    let pAlias = attempt (pKeyword "AS") >>. pIdentifierExpr <|> pIdentifierExpr

    let pFullAlias =
        pAlias
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))

    let pValuesSource =
        let pRow =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))

        pKeyword "VALUES" >>. sepBy1 pRow (token (pstring ","))

    let pJoinType =
        choice
            [ attempt (pKeyword "LEFT" .>> opt (pKeyword "OUTER") .>> pKeyword "JOIN")
              >>% LeftJoin
              attempt (pKeyword "RIGHT" .>> opt (pKeyword "OUTER") .>> pKeyword "JOIN")
              >>% RightJoin
              attempt (pKeyword "FULL" .>> opt (pKeyword "OUTER") .>> pKeyword "JOIN")
              >>% FullJoin
              attempt (pKeyword "INNER" .>> pKeyword "JOIN") >>% InnerJoin
              attempt (pKeyword "CROSS" .>> pKeyword "JOIN") >>% CrossJoin
              pKeyword "JOIN" >>% InnerJoin ]

    let pJoinCondition =
        choice
            [ pKeyword "ON" >>. pExpression |>> On
              pKeyword "USING"
              >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
              |>> Using ]

    let pColumnExpr = attempt (pExpression .>>. opt (attempt pAlias)) |>> Column

    let pColumnSource =
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
        <|> pColumnExpr
        <|> (pstring "*" .>> ws >>% Star |> withExprPosition |>> fun e -> Column(e, None))

    let pOrderByItem = ExpressionParser.pOrderByItem

    let pOffset =
        pKeyword "OFFSET" >>. pExpression
        .>> (attempt (pKeyword "ROWS") <|> pKeyword "ROW")

    let pFetch =
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

    let pOffsetFetch =
        choice
            [ attempt (pOffset .>>. opt pFetch) |>> fun (o, f) -> Some o, f
              attempt pFetch |>> fun f -> None, Some f ]

    let pLockingClause = pKeyword "FOR" >>. pKeyword "UPDATE" >>% ForUpdate

    let pWindowDefItem =
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
        pKeyword "WINDOW" >>. sepBy1 pWindowDefItem (token (pstring ","))

    let pTableSource, pTableSourceRef = createParserForwardedToRef<TableSource, unit> ()

    let pTableSampleClause =
        pKeyword "TABLESAMPLE" >>. pIdentifier
        .>>. between (token (pstring "(")) (token (pstring ")")) pExpression
        .>>. opt (
            pKeyword "REPEATABLE"
            >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        )
        |>> fun ((method, percent), repeat) -> (method, percent, repeat)

    let pTablePrimary =
        let pBase =
            choice
                [ attempt (
                      between (token (pstring "(")) (token (pstring ")")) pQuery .>>. pFullAlias
                      |>> fun (q, (name, cols)) -> Subquery(q, name, cols)
                  )
                  |> withTablePosition
                  attempt (
                      between (token (pstring "(")) (token (pstring ")")) pValuesSource
                      .>>. pFullAlias
                      |>> fun (rows, (name, cols)) -> ValuesTable(rows, name, cols)
                  )
                  |> withTablePosition
                  attempt (
                      pKeyword "LATERAL"
                      >>. between (token (pstring "(")) (token (pstring ")")) pQuery
                      .>>. pFullAlias
                      |>> fun (q, (name, cols)) -> Lateral(q, name, cols)
                  )
                  |> withTablePosition
                  attempt (
                      pKeyword "UNNEST"
                      >>. between (token (pstring "(")) (token (pstring ")")) pExpression
                      .>>. opt (pKeyword "WITH" >>. pKeyword "ORDINALITY" >>% true)
                      .>>. pFullAlias
                      |>> fun ((expr, ord), (name, cols)) -> Unnest(expr, Option.defaultValue false ord, name, cols)
                  )
                  |> withTablePosition
                  attempt (between (token (pstring "(")) (token (pstring ")")) pTableSource)
                  attempt (pQualifiedName .>>. opt (attempt pAlias) |>> Table)
                  |> withTablePosition ]

        pBase .>>. opt pTableSampleClause
        |>> fun (tbl, sample) ->
            match sample with
            | Some(method, percent, repeat) ->
                { TableSource.Kind = TableSample(tbl, method, percent, repeat)
                  Pos = tbl.Pos }
            | None -> tbl

    let pJoinedTableSuffix =
        opt (pKeyword "NATURAL" >>% true)
        .>>. pJoinType
        .>>. pTablePrimary
        .>>. opt pJoinCondition
        |>> fun (((nat, jt), right), cond) -> Option.defaultValue false nat, jt, right, cond

    pTableSourceRef.Value <-
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

    let pGroupingElement, pGroupingElementRef =
        createParserForwardedToRef<GroupingElement, unit> ()

    let pOrdinaryGroupingSet =
        choice
            [ attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
                  |>> GroupingSet
              )
              pExpression |>> fun e -> GroupingSet [ e ] ]

    let pGroupingSetsSpec =
        pKeyword "GROUPING" .>> pKeyword "SETS"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pGroupingElement (token (pstring ",")))
        |>> GroupingSets

    let pRollupCube =
        choice
            [ attempt (
                  pKeyword "ROLLUP"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (sepBy1 pOrdinaryGroupingSet (token (pstring ",")))
                  |>> Rollup
              )
              attempt (
                  pKeyword "CUBE"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (sepBy1 pOrdinaryGroupingSet (token (pstring ",")))
                  |>> Cube
              ) ]

    let pEmptyGroupingSet =
        between (token (pstring "(")) (token (pstring ")")) (preturn EmptyGroupingSet)

    pGroupingElementRef.Value <-
        choice
            [ attempt pGroupingSetsSpec
              attempt pRollupCube
              attempt pEmptyGroupingSet
              attempt pOrdinaryGroupingSet ]

    let pSelectBase =
        pipe5
            (pKeyword "SELECT"
             >>. opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
             .>>. sepBy1 pColumnSource (token (pstring ",")))
            (opt (attempt (pKeyword "FROM" >>. sepBy1 pTableSource (token (pstring ",")))))
            (opt (attempt (pKeyword "WHERE" >>. pExpression)))
            (opt (
                attempt (
                    pKeyword "GROUP"
                    >>. pKeyword "BY"
                    >>. opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
                    .>>. sepBy1 pGroupingElement (token (pstring ","))
                )
            ))
            (opt (attempt (pKeyword "HAVING" >>. pExpression)))
            (fun (dist, cols) from whr grp hav -> (Option.defaultValue false dist, cols), from, whr, grp, hav)

    // SELECT core: query specification without ORDER BY/OFFSET/FETCH/LOCKING.
    // These trailing clauses are parsed at the query-expression level so that
    // they apply to the whole query (including set operations), not just the
    // last SELECT.
    let pSelectCore =
        pipe2 pSelectBase (opt (attempt pWindowClause)) (fun baseResult window ->
            let distInfo, from, whr, grp, hav = baseResult
            let dist, colsList = distInfo

            let grpDistinct, grpList =
                match grp with
                | Some(d, l) -> Option.defaultValue false d, l
                | None -> false, []

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

    // Simple table: query specification (set-operation operands do not consume
    // ORDER BY/OFFSET/FETCH/LOCKING so those apply to the whole expression).
    let pSimpleTable = pSelectCore |>> SelectQuery

    let pCorresponding =
        pKeyword "CORRESPONDING"
        >>. opt (
            pKeyword "BY"
            >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
        )

    let pQueryExpressionBody, pQueryExpressionBodyRef =
        createParserForwardedToRef<Query, unit> ()

    // Query primary: parenthesized query expression body or simple table
    let pQueryPrimary =
        choice
            [ attempt (between (token (pstring "(")) (token (pstring ")")) pQueryExpressionBody)
              pSimpleTable ]

    // INTERSECT operator (higher precedence than UNION/EXCEPT)
    let pIntersectOp =
        pKeyword "INTERSECT"
        >>. opt (pKeyword "ALL" >>% (true, false) <|> (pKeyword "DISTINCT" >>% (false, true)))
        .>>. opt pCorresponding
        |>> fun (quant, corr) ->
            let isAll, isDist = Option.defaultValue (false, false) quant

            { Kind = Intersect
              IsAll = isAll
              IsDistinct = isDist
              Corresponding = corr }

    // Query term: query primary { INTERSECT query primary }
    let pQueryTerm =
        chainl1 pQueryPrimary (pIntersectOp |>> fun op -> fun l r -> SetOperation(l, op, r))

    // UNION/EXCEPT operator (lower precedence)
    let pUnionExceptOp =
        (choice [ pKeyword "UNION" >>% Union; pKeyword "EXCEPT" >>% Except ])
        .>>. opt (pKeyword "ALL" >>% (true, false) <|> (pKeyword "DISTINCT" >>% (false, true)))
        .>>. opt pCorresponding
        |>> fun ((kind, quant), corr) ->
            let isAll, isDist = Option.defaultValue (false, false) quant

            { Kind = kind
              IsAll = isAll
              IsDistinct = isDist
              Corresponding = corr }

    // Query expression body: query term { (UNION | EXCEPT) query term }
    pQueryExpressionBodyRef.Value <- chainl1 pQueryTerm (pUnionExceptOp |>> fun op -> fun l r -> SetOperation(l, op, r))

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
            | WithQuery _ -> QueryExpression(q, orderBy, limitOffset, locking)
            | QueryExpression _ ->
                // Already wrapped (defensive; not produced by the current grammar)
                q

    // Query expression: [WITH] query expression body [ORDER BY] [OFFSET/FETCH] [LOCKING]
    let pQueryExpression =
        let pOrderByClause =
            pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ","))

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
