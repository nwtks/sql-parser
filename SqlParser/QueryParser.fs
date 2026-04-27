namespace SqlParser

open FParsec
open SqlParser.Ast
open SqlParser.Lexer
open SqlParser.ExpressionParser

module QueryParser =
    let withTablePosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { TableSource.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    let pAlias = attempt (pKeyword "AS") >>. pIdentifier <|> pIdentifier

    let pFullAlias =
        pAlias
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifier (token (pstring ","))))

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
              >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifier (token (pstring ",")))
              |>> Using ]

    let pColumnSource =
        attempt (pExpression .>>. opt (attempt pAlias)) |>> Column
        <|> (pstring "*" .>> ws >>% Star |> withExprPosition |>> fun e -> Column(e, None))

    let pOrderByItem = ExpressionParser.pOrderByItem

    let pOffset =
        pKeyword "OFFSET" >>. pExpression
        .>> opt (attempt (pKeyword "ROWS") <|> pKeyword "ROW")

    let pFetch =
        pKeyword "FETCH" >>. (pKeyword "FIRST" <|> pKeyword "NEXT") >>. pExpression
        .>>. opt (pKeyword "PERCENT" >>% true)
        .>> (attempt (pKeyword "ROWS") <|> pKeyword "ROW")
        .>>. (pKeyword "ONLY" >>% false <|> (pKeyword "WITH" >>. pKeyword "TIES" >>% true))
        |>> fun ((count, isPercent), withTies) ->
            { Count = count
              IsPercent = Option.defaultValue false isPercent
              WithTies = withTies }

    let pOffsetFetch =
        choice
            [ attempt (pOffset .>>. opt pFetch) |>> fun (o, f) -> Some o, f
              attempt pFetch |>> fun f -> None, Some f ]

    let pLockingClause =
        pKeyword "FOR"
        >>. (pKeyword "UPDATE" >>% ForUpdate <|> (pKeyword "SHARE" >>% ForShare))

    let pWindowDefItem =
        pIdentifier .>> pKeyword "AS"
        .>>. between
            (token (pstring "("))
            (token (pstring ")"))
            (opt pIdentifier
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

    let pTablePrimary =
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
              attempt (between (token (pstring "(")) (token (pstring ")")) pTableSource)
              attempt (pIdentifier .>>. opt (attempt pAlias) |>> Table) |> withTablePosition ]

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

    let pSelectBase =
        pipe5
            (pKeyword "SELECT"
             >>. opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
             .>>. sepBy1 pColumnSource (token (pstring ",")))
            (opt (attempt (pKeyword "FROM" >>. pTableSource)))
            (opt (attempt (pKeyword "WHERE" >>. pExpression)))
            (opt (attempt (pKeyword "GROUP" >>. pKeyword "BY" >>. sepBy1 pExpression (token (pstring ",")))))
            (opt (attempt (pKeyword "HAVING" >>. pExpression)))
            (fun (dist, cols) from whr grp hav -> (Option.defaultValue false dist, cols), from, whr, grp, hav)

    let pSelectStatement =
        pipe5
            pSelectBase
            (opt (attempt pWindowClause))
            (opt (attempt (pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ",")))))
            (opt pOffsetFetch)
            (opt pLockingClause)
            (fun baseResult window orderBy limitOffset locking ->
                let distInfo, from, whr, grp, hav = baseResult
                let dist, colsList = distInfo

                { IsDistinct = dist
                  Columns = colsList
                  From = from
                  Where = whr
                  GroupBy = Option.defaultValue [] grp
                  Having = hav
                  Window = Option.defaultValue [] window
                  OrderBy = Option.defaultValue [] orderBy
                  Offset = limitOffset |> Option.bind fst
                  Fetch = limitOffset |> Option.bind snd
                  Locking = locking })

    let pSetOperator =
        let pCorresponding =
            pKeyword "CORRESPONDING"
            >>. opt (
                pKeyword "BY"
                >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifier (token (pstring ",")))
            )

        let pOp opKind opStr =
            pKeyword opStr
            >>. opt (pKeyword "ALL" >>% (true, false) <|> (pKeyword "DISTINCT" >>% (false, true)))
            .>>. opt pCorresponding
            |>> fun (quant, corr) ->
                let isAll, isDist = Option.defaultValue (false, false) quant

                { Kind = opKind
                  IsAll = isAll
                  IsDistinct = isDist
                  Corresponding = corr }

        choice [ pOp Union "UNION"; pOp Intersect "INTERSECT"; pOp Except "EXCEPT" ]

    let pQuery = ExpressionParser.pQuery

    pQueryRef.Value <-
        let pQueryBody =
            chainl1 (pSelectStatement |>> SelectQuery) (pSetOperator |>> fun op -> fun l r -> SetOperation(l, op, r))

        attempt (
            pWithClause .>>. pQueryBody
            |>> fun ((recu, ctes), q) -> WithQuery(recu, ctes, q)
        )
        <|> pQueryBody
