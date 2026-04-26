namespace SqlParser

open FParsec
open SqlParser.Ast
open SqlParser.ExpressionParser

module SqlParser =
    let ws = spaces
    let token p = p .>> ws
    let keyword s = pstringCI s .>> ws
    let pIdentifierToken = token pIdentifier
    let pExpressionToken = token pExpression
    let pAlias = keyword "AS" >>? pIdentifierToken <|> pIdentifierToken

    let withExprPosition (p: Parser<ExpressionKind, unit>) : Parser<Expression, unit> =
        getPosition .>>. p
        |>> fun (pos, kind) -> kind, { Line = pos.Line; Column = pos.Column }

    let withTablePosition (p: Parser<TableSourceKind, unit>) : Parser<TableSource, unit> =
        getPosition .>>. p
        |>> fun (pos, kind) -> kind, { Line = pos.Line; Column = pos.Column }

    let withStmtPosition (p: Parser<StatementKind, unit>) : Parser<Statement, unit> =
        getPosition .>>. p
        |>> fun (pos, kind) -> kind, { Line = pos.Line; Column = pos.Column }

    let pColumnSource =
        withExprPosition (token (pstring "*") >>% Star)
        |>> fun expr -> Column(expr, None)
        <|> (pExpressionToken .>>. opt pAlias |>> fun (expr, alias) -> Column(expr, alias))

    let pQuery, pQueryRef = createParserForwardedToRef<Query, unit> ()

    let pSelectQuery, pSelectQueryRef =
        createParserForwardedToRef<SelectStatement, unit> ()

    let pTableSource =
        withTablePosition (
            between (token (pstring "(")) (token (pstring ")")) pQuery .>>. pIdentifierToken
            |>> Subquery
            <|> (pIdentifierToken .>>. opt pAlias |>> fun (name, alias) -> Table(name, alias))
        )

    let pJoinType =
        choice
            [ attempt (keyword "LEFT" .>> keyword "JOIN") >>% LeftJoin
              attempt (keyword "RIGHT" .>> keyword "JOIN") >>% RightJoin
              attempt (keyword "FULL" .>> keyword "JOIN") >>% FullJoin
              attempt (keyword "INNER" .>> keyword "JOIN") >>% InnerJoin
              keyword "JOIN" >>% InnerJoin ]

    let pJoin =
        pJoinType .>>. pTableSource .>> keyword "ON" .>>. pExpressionToken
        |>> fun ((jt, table), on) ->
            { JoinType = jt
              Table = table
              On = Some on }

    let pOrderByItem =
        pExpressionToken
        .>>. opt (keyword "ASC" >>% true <|> (keyword "DESC" >>% false))
        |>> fun (expr, asc) -> expr, Option.defaultValue true asc

    let pSelectBase =
        pipe5
            (keyword "SELECT" >>. sepBy1 pColumnSource (token (pstring ",")))
            (opt (keyword "FROM" >>. pTableSource))
            (many pJoin)
            (opt (keyword "WHERE" >>. pExpressionToken))
            (opt (
                attempt (keyword "GROUP" .>> keyword "BY")
                >>. sepBy1 pExpressionToken (token (pstring ","))
            ))
            (fun cols from joins whr grp -> cols, from, joins, whr, grp)

    pSelectQueryRef.Value <-
        pipe3
            pSelectBase
            (opt (keyword "HAVING" >>. pExpressionToken))
            (opt (
                attempt (keyword "ORDER" .>> keyword "BY")
                >>. sepBy1 pOrderByItem (token (pstring ","))
            ))
            (fun (cols, from, joins, whr, grp) having ord -> cols, from, joins, whr, grp, having, ord)
        .>>. opt (keyword "LIMIT" >>. token pint32)
        |>> fun ((cols, from, joins, whr, grp, having, ord), limit) ->
            { Columns = cols
              From = from
              Joins = joins
              Where = whr
              GroupBy = Option.defaultValue [] grp
              Having = having
              OrderBy = Option.defaultValue [] ord
              Limit = limit }

    let pSetOperator =
        choice
            [ attempt (keyword "UNION" .>> keyword "ALL") >>% UnionAll
              keyword "UNION" >>% Union
              attempt (keyword "INTERSECT" .>> keyword "ALL") >>% IntersectAll
              keyword "INTERSECT" >>% Intersect
              attempt (keyword "EXCEPT" .>> keyword "ALL") >>% ExceptAll
              keyword "EXCEPT" >>% Except ]

    let pCte =
        pIdentifierToken
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierToken (token (pstring ","))))
        .>> keyword "AS"
        .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> fun ((name, cols), query) ->
            { Name = name
              Columns = cols
              Query = query }

    let pWith =
        keyword "WITH" >>. opt (keyword "RECURSIVE" >>% true)
        .>>. sepBy1 pCte (token (pstring ","))
        |>> fun (isRecursive, ctes) -> Option.defaultValue false isRecursive, ctes

    let pSetOperationQuery =
        chainl1
            (pSelectQuery |>> SelectQuery)
            (pSetOperator |>> fun op -> fun left right -> SetOperation(left, op, right))

    pQueryRef.Value <- pSetOperationQuery

    let pInsertSource =
        let pRow =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpressionToken (token (pstring ",")))

        keyword "VALUES" >>. sepBy1 pRow (token (pstring ",")) |>> Values
        <|> (pQuery |>> Query)

    let pInsert =
        keyword "INSERT" >>. keyword "INTO" >>. pIdentifierToken
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierToken (token (pstring ","))))
        .>>. pInsertSource
        |>> fun ((table, cols), source) ->
            Insert
                { Table = table
                  Columns = cols
                  Source = source }

    let pUpdate =
        keyword "UPDATE" >>. pIdentifierToken .>> keyword "SET"
        .>>. sepBy1 (pIdentifierToken .>> token (pstring "=") .>>. pExpressionToken) (token (pstring ","))
        .>>. opt (keyword "WHERE" >>. pExpressionToken)
        |>> fun ((table, sets), whr) ->
            Update
                { Table = table
                  Set = sets
                  Where = whr }

    let pDelete =
        keyword "DELETE" >>. keyword "FROM" >>. pIdentifierToken
        .>>. opt (keyword "WHERE" >>. pExpressionToken)
        |>> fun (table, whr) -> Delete { Table = table; Where = whr }

    let pMergeAction =
        choice
            [ attempt (keyword "UPDATE" >>. keyword "SET")
              >>. sepBy1 (pIdentifierToken .>> token (pstring "=") .>>. pExpressionToken) (token (pstring ","))
              |>> MergeUpdate
              keyword "DELETE" >>% MergeDelete
              keyword "INSERT"
              >>. opt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierToken (token (pstring ",")))
              )
              .>> keyword "VALUES"
              .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpressionToken (token (pstring ",")))
              |>> MergeInsert ]

    let pMergeWhen =
        keyword "WHEN"
        >>. choice
                [ attempt (keyword "NOT" .>> keyword "MATCHED") >>% NotMatched
                  keyword "MATCHED" >>% Matched ]
        .>>. opt (keyword "AND" >>. pExpressionToken)
        .>> keyword "THEN"
        .>>. pMergeAction
        |>> fun ((cond, extra), action) ->
            { MatchCondition = cond
              Condition = extra
              Action = action }

    let pMerge =
        keyword "MERGE" >>. keyword "INTO" >>. pIdentifierToken .>>. opt pAlias
        .>> keyword "USING"
        .>>. pTableSource
        .>> keyword "ON"
        .>>. pExpressionToken
        .>>. many1 pMergeWhen
        |>> fun ((((target, alias), source), on), whens) ->
            Merge
                { Target = target
                  TargetAlias = alias
                  Source = source
                  On = on
                  WhenClauses = whens }

    let pStatement =
        withStmtPosition (
            opt pWith .>>. choice [ pQuery |>> Select; pInsert; pUpdate; pDelete; pMerge ]
            |>> function
                | Some(isRec, ctes), stmtKind -> WithStatement(isRec, ctes, stmtKind)
                | None, stmtKind -> stmtKind
        )

    let parse sql =
        match run (ws >>. pStatement .>> eof) sql with
        | Success(result, _, _) -> Choice1Of2 result
        | Failure(errorMsg, parserError, _) ->
            Choice2Of2(
                ParseError(
                    errorMsg,
                    { Line = parserError.Position.Line
                      Column = parserError.Position.Column }
                )
            )
