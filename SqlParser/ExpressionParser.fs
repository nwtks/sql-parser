namespace SqlParser

open FParsec
open SqlParser.Ast
open SqlParser.Lexer
open SqlParser.Types

module ExpressionParser =
    let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit> ()

    let withExprPosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Expression.Kind = kind
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
              attempt (pHexStringLiteral |>> Ast.Binary |>> Literal)
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

    let pNullifExpr =
        pKeyword "NULLIF"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> token (pstring ",") .>>. pExpression)
        |>> fun (e1, e2) ->
            Case(
                None,
                [ ({ Kind = BinaryOp(Equal, e1, e2)
                     Pos = e1.Pos },
                   { Kind = Literal Null; Pos = e2.Pos }) ],
                Some e1
            )
        |> withExprPosition

    [<TailCall>]
    let rec buildCoalesce acc =
        function
        | [] -> acc
        | e :: tail ->
            let newAcc =
                Case(
                    None,
                    [ ({ Kind =
                          UnaryOp(
                              Not,
                              { Kind = BinaryOp(Equal, e, { Kind = Literal Null; Pos = e.Pos })
                                Pos = e.Pos }
                          )
                         Pos = e.Pos },
                       e) ],
                    Some { Kind = acc; Pos = e.Pos }
                )

            tail |> buildCoalesce newAcc

    let pCoalesceExpr =
        pKeyword "COALESCE"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
        |>> fun exprs ->
            match List.rev exprs with
            | [] -> Literal Null
            | last :: rest -> buildCoalesce last.Kind rest
        |> withExprPosition

    let pCaseExpr =
        getPosition
        >>= fun pos ->
            let pResultExpr =
                pExpression
                <|> (pKeyword "NULL"
                     >>% { Kind = Literal Null
                           Pos = { Line = pos.Line; Column = pos.Column } })

            let pSimpleWhenClause =
                pKeyword "WHEN" >>. sepBy1 pExpression (token (pstring ",")) .>> pKeyword "THEN"
                .>>. pResultExpr

            let pSearchedWhenClause =
                pKeyword "WHEN" >>. pExpression .>> pKeyword "THEN" .>>. pResultExpr

            pKeyword "CASE"
            >>. choice
                    [ attempt (pExpression .>>. many1 pSimpleWhenClause)
                      |>> fun (op, whens) ->
                          let flattened =
                              whens |> List.collect (fun (vals, res) -> vals |> List.map (fun v -> v, res))

                          Case(Some op, flattened, None)
                      many1 pSearchedWhenClause |>> fun whens -> Case(None, whens, None) ]
            .>>. opt (pKeyword "ELSE" >>. pResultExpr)
            .>> pKeyword "END"
            |>> fun (caseBase, els) ->
                match caseBase with
                | Case(op, whens, _) -> Case(op, whens, els)
                | kind -> kind
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

    let pParameterExpr =
        pQuestionMark >>% "?" <|> pHostParameter |>> Parameter |> withExprPosition

    let pExtractExpr =
        pKeyword "EXTRACT"
        >>. between (token (pstring "(")) (token (pstring ")")) (pIdentifier .>> pKeyword "FROM" .>>. pExpression)
        |>> Extract
        |> withExprPosition

    let pPositionExpr =
        pKeyword "POSITION"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "IN"
                 .>>. pExpression
                 .>>. opt (pKeyword "USING" >>. pIdentifier))
        |>> (fun ((target, source), unit) -> Position(target, source, unit))
        |> withExprPosition

    let pTrimExpr =
        let pSpec = opt (pKeyword "LEADING" <|> pKeyword "TRAILING" <|> pKeyword "BOTH")

        pKeyword "TRIM"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pSpec .>>. opt pExpression .>> pKeyword "FROM" .>>. pExpression)
        |>> (fun ((spec, char), source) -> Trim(spec, char, source))
        |> withExprPosition

    let pPredicateSuffix pExpr =
        choice
            [ attempt (
                  opt (pKeyword "NOT") .>> pKeyword "BETWEEN"
                  .>>. opt (pKeyword "ASYMMETRIC" <|> pKeyword "SYMMETRIC")
                  .>>. pExpr
                  .>> pKeyword "AND"
                  .>>. pExpr
                  |>> fun (((isNot, sym), start), endBound) ->
                      fun e ->
                          { Expression.Kind = Between(e, Option.isSome isNot, Option.isSome sym, start, endBound)
                            Pos = e.Pos }
              )
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "IN"
                  .>>. between
                      (token (pstring "("))
                      (token (pstring ")"))
                      (attempt pQuery |>> Choice1Of2
                       <|> (sepBy1 pExpr (token (pstring ",")) |>> Choice2Of2))
                  |>> fun (isNot, res) ->
                      fun e ->
                          let kind =
                              match res with
                              | Choice1Of2 q -> InSubquery(e, Option.isSome isNot, q)
                              | Choice2Of2 l -> InList(e, Option.isSome isNot, l)

                          { Expression.Kind = kind; Pos = e.Pos }
              )
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT")
                  .>>. (pKeyword "NULL" >>% Choice1Of2()
                        <|> (pKeyword "TRUE" >>% Choice2Of2(Some true))
                        <|> (pKeyword "FALSE" >>% Choice2Of2(Some false))
                        <|> (pKeyword "UNKNOWN" >>% Choice2Of2 None))
                  |>> fun (isNot, res) ->
                      fun e ->
                          let kind =
                              match res with
                              | Choice1Of2() -> IsNull(e, Option.isSome isNot)
                              | Choice2Of2 b -> IsBoolean(e, Option.isSome isNot, b)

                          { Expression.Kind = kind; Pos = e.Pos }
              ) ]

    let pFunctionCallExpr =
        let pArgs =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
                 .>>. sepBy pExpression (token (pstring ",")))

        pIdentifierRaw .>>. pArgs .>>. opt pWindowDefinition
        |>> fun ((name, (dist, args)), window) ->
            match window with
            | Some w ->
                WindowFunction
                    { Function = name
                      Args = args
                      IsDistinct = Option.defaultValue false dist
                      Window = w }
            | None -> FunctionCall(name, Option.defaultValue false dist, args, None)
        |> withExprPosition

    let pTerm =
        choice
            [ attempt pCastExpr
              attempt pCaseExpr
              attempt pNullifExpr
              attempt pCoalesceExpr
              attempt pExtractExpr
              attempt pPositionExpr
              attempt pTrimExpr
              attempt pFunctionCallExpr
              attempt pSubqueryExpr
              attempt pLiteralExpr
              attempt pParameterExpr
              attempt pStarExpr
              pColumnReferenceExpr
              between (token (pstring "(")) (token (pstring ")")) pExpression ]

    let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
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

    let pPredicateExpr =
        opp.ExpressionParser .>>. many (pPredicateSuffix opp.ExpressionParser)
        |>> fun (e, suffixes) -> List.fold (fun acc f -> f acc) e suffixes

    let pNotExpr, pNotExprRef = createParserForwardedToRef<Expression, unit> ()

    pNotExprRef.Value <-
        (attempt (pKeyword "NOT" >>. pNotExpr)
         |>> fun e ->
             { Expression.Kind = UnaryOp(Not, e)
               Pos = e.Pos })
        <|> pPredicateExpr

    let pAndExpr =
        chainl1
            pNotExpr
            (pKeyword "AND"
             >>% fun l r ->
                 { Expression.Kind = BinaryOp(And, l, r)
                   Pos = l.Pos })

    let pOrExpr =
        chainl1
            pAndExpr
            (pKeyword "OR"
             >>% fun l r ->
                 { Expression.Kind = BinaryOp(Or, l, r)
                   Pos = l.Pos })

    pExpressionRef.Value <- pOrExpr
