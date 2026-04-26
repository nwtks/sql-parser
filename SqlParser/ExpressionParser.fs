namespace SqlParser

open FParsec
open SqlParser.Ast
open SqlParser.Lexer
open SqlParser.Types

module ExpressionParser =
    let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit> ()

    let withExprPosition (p: Parser<ExpressionKind, unit>) : Parser<Expression, unit> =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    let pLiteralExpr =
        choice
            [ attempt (pCharacterStringLiteral |>> String |>> Literal)
              attempt (pNumericLiteral |>> Number |>> Literal)
              attempt (pBooleanLiteral |>> Bool |>> Literal)
              attempt (pKeyword "NULL" >>% Null |>> Literal)
              attempt (pDateLiteral |>> Date |>> Literal)
              attempt (pTimeLiteral |>> Time |>> Literal)
              attempt (pTimestampLiteral |>> Timestamp |>> Literal)
              attempt (pHexStringLiteral |>> Binary |>> Literal)
              attempt (pNationalCharacterStringLiteral |>> NationalString |>> Literal) ]
        |> withExprPosition

    let pIdentifierExpr = pIdentifier |>> Identifier |> withExprPosition
    let pStarExpr = pstring "*" .>> ws >>% Star |> withExprPosition

    let pColumnReferenceExpr =
        sepBy1 pIdentifier (token (pstring "."))
        |>> function
            | [ id ] -> Identifier id
            | ids -> ColumnReference ids
        |> withExprPosition

    let pQuery, pQueryRef = createParserForwardedToRef<Query, unit> ()

    let pSubqueryExpr =
        between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> SubqueryExpression
        |> withExprPosition

    let pCastExpr =
        pKeyword "CAST"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        |>> Cast
        |> withExprPosition

    let pWindowFrame =
        let pUnit = pKeyword "ROWS" >>% Rows <|> (pKeyword "RANGE" >>% Range)

        let pBound =
            choice
                [ attempt (pKeyword "UNBOUNDED" >>. pKeyword "PRECEDING" >>% UnboundedPreceding)
                  attempt (pKeyword "UNBOUNDED" >>. pKeyword "FOLLOWING" >>% UnboundedFollowing)
                  attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% CurrentRow)
                  attempt (pExpression .>> pKeyword "PRECEDING" |>> Preceding)
                  attempt (pExpression .>> pKeyword "FOLLOWING" |>> Following) ]

        let pExclusion =
            pKeyword "EXCLUDE"
            >>. choice
                    [ attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% ExcludeCurrentRow)
                      attempt (pKeyword "GROUP" >>% ExcludeGroup)
                      attempt (pKeyword "TIES" >>% ExcludeTies)
                      attempt (pKeyword "NO" >>. pKeyword "OTHERS" >>% ExcludeNoOthers) ]

        pUnit
        .>>. choice
            [ attempt (
                  pKeyword "BETWEEN" >>. pBound .>> pKeyword "AND" .>>. pBound
                  |>> fun (s, e) -> s, Some e
              )
              pBound |>> fun s -> s, None ]
        .>>. opt pExclusion
        |>> fun ((unit, (start, endBound)), exclusion) ->
            { Unit = unit
              Start = start
              End = endBound
              Exclusion = exclusion }

    let pCaseExpr =
        let pWhenThen = pKeyword "WHEN" >>. pExpression .>> pKeyword "THEN" .>>. pExpression

        pKeyword "CASE" >>. opt pExpression
        .>>. many1 pWhenThen
        .>>. opt (pKeyword "ELSE" >>. pExpression)
        .>> pKeyword "END"
        |>> fun ((cond, whens), els) -> Case(cond, whens, els)
        |> withExprPosition

    let pOrderByItem =
        let pNullsOrder =
            pKeyword "NULLS"
            >>. (pKeyword "FIRST" >>% NullsFirst <|> (pKeyword "LAST" >>% NullsLast))

        pExpression
        .>>. opt (attempt (pKeyword "ASC" >>% true) <|> attempt (pKeyword "DESC" >>% false))
        .>>. opt (attempt pNullsOrder)
        |>> fun ((expr, asc), nulls) -> expr, Option.defaultValue true asc, nulls

    let pWindowDefinition =
        let pPartitionBy =
            pKeyword "PARTITION"
            >>. pKeyword "BY"
            >>. sepBy1 pExpression (token (pstring ","))

        let pOrderBy =
            pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ","))

        pKeyword "OVER"
        >>. (between
                 (token (pstring "("))
                 (token (pstring ")"))
                 (opt pIdentifier .>>. opt pPartitionBy .>>. opt pOrderBy .>>. opt pWindowFrame
                  |>> fun (((name, pb), ob), frame) ->
                      { ExistingWindowName = name
                        PartitionBy = Option.defaultValue [] pb
                        OrderBy = Option.defaultValue [] ob
                        Frame = frame })
             <|> (pIdentifier
                  |>> fun name ->
                      { ExistingWindowName = Some name
                        PartitionBy = []
                        OrderBy = []
                        Frame = None }))

    let pFunctionCallExpr =
        let pArgs =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
                 .>>. sepBy pExpression (token (pstring ",")))

        pIdentifier .>>. pArgs .>>. opt pWindowDefinition
        |>> fun ((name, (dist, args)), window) -> FunctionCall(name, Option.defaultValue false dist, args, window)
        |> withExprPosition

    let pTerm =
        choice
            [ attempt pCastExpr
              attempt pCaseExpr
              attempt pFunctionCallExpr
              attempt pSubqueryExpr
              attempt pLiteralExpr
              attempt pStarExpr
              pColumnReferenceExpr
              between (token (pstring "(")) (token (pstring ")")) pExpression ]

    let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
    pExpressionRef.Value <- opp.ExpressionParser
    opp.TermParser <- pTerm

    let addInfix op precedence assoc mapping =
        opp.AddOperator(InfixOperator(op, ws, precedence, assoc, fun x y -> { Kind = mapping x y; Pos = x.Pos }))

    let addPrefix op precedence mapping =
        opp.AddOperator(PrefixOperator(op, ws, precedence, true, fun x -> { Kind = mapping x; Pos = x.Pos }))

    addPrefix "+" 8 (fun e -> UnaryOp(Plus, e))
    addPrefix "-" 8 (fun e -> UnaryOp(Minus, e))

    addInfix "*" 7 Associativity.Left (fun x y -> BinaryOp(Multiply, x, y))
    addInfix "/" 7 Associativity.Left (fun x y -> BinaryOp(Divide, x, y))

    addInfix "+" 6 Associativity.Left (fun x y -> BinaryOp(Add, x, y))
    addInfix "-" 6 Associativity.Left (fun x y -> BinaryOp(Subtract, x, y))
    addInfix "||" 6 Associativity.Left (fun x y -> BinaryOp(Concatenate, x, y))

    addInfix "=" 5 Associativity.Left (fun x y -> BinaryOp(Equal, x, y))
    addInfix "<>" 5 Associativity.Left (fun x y -> BinaryOp(NotEqual, x, y))
    addInfix "!=" 5 Associativity.Left (fun x y -> BinaryOp(NotEqual, x, y))
    addInfix "<" 5 Associativity.Left (fun x y -> BinaryOp(LessThan, x, y))
    addInfix "<=" 5 Associativity.Left (fun x y -> BinaryOp(LessThanOrEqual, x, y))
    addInfix ">" 5 Associativity.Left (fun x y -> BinaryOp(GreaterThan, x, y))
    addInfix ">=" 5 Associativity.Left (fun x y -> BinaryOp(GreaterThanOrEqual, x, y))

    let wsWord = notFollowedBy (asciiLetter <|> digit <|> pchar '_') >>. ws

    opp.AddOperator(
        InfixOperator(
            "LIKE",
            wsWord,
            5,
            Associativity.Left,
            fun x y ->
                { Kind = BinaryOp(StringLike, x, y)
                  Pos = x.Pos }
        )
    )

    opp.AddOperator(
        InfixOperator(
            "SIMILAR TO",
            wsWord,
            5,
            Associativity.Left,
            fun x y ->
                { Kind = BinaryOp(SimilarTo, x, y)
                  Pos = x.Pos }
        )
    )

    opp.AddOperator(PrefixOperator("NOT", wsWord, 4, true, fun x -> { Kind = UnaryOp(Not, x); Pos = x.Pos }))

    opp.AddOperator(
        InfixOperator(
            "AND",
            wsWord,
            3,
            Associativity.Left,
            fun x y ->
                { Kind = BinaryOp(And, x, y)
                  Pos = x.Pos }
        )
    )

    opp.AddOperator(
        InfixOperator(
            "OR",
            wsWord,
            2,
            Associativity.Left,
            fun x y ->
                { Kind = BinaryOp(Or, x, y)
                  Pos = x.Pos }
        )
    )
