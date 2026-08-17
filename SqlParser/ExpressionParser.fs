namespace SqlParser

open FParsec
open SqlParser.Lexer

module ExpressionParser =
    // This module implements the <value expression> family of the SQL-2016 grammar.
    // Non-terminal references (section numbers from sql-2016-grammar.txt):
    //
    //   6.3  <value expression primary> / <general value specification> / <literal>
    //   6.12 <case expression>
    //   6.13 <cast specification> / <case abbreviation> (NULLIF, COALESCE)
    //   6.17 <character substring function> / <character overlay function>
    //   6.18 <trim function>
    //   6.21 <datetime value function>
    //   6.25 <position expression>
    //   6.27 <extract expression>
    //   7.15 <window name or specification> (the OVER clause)
    //   8    <predicate> / <boolean value expression> / <boolean term> / <boolean factor>
    //   8.7  <quantified comparison predicate>
    //   8.10 <exists predicate> / 8.11 <unique predicate>
    //   10.9 <routine invocation>
    //   ...plus <scalar subquery> and <quantified subquery> as primary alternatives.
    let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit> ()
    let pDataType, pDataTypeRef = createParserForwardedToRef<DataType, unit> ()
    let pQuery, pQueryRef = createParserForwardedToRef<Query, unit> ()

    let withExprPosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Expression.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 6.3 <literal> ::= NULL | <character string literal> | <numeric literal>
    //     | <boolean literal> | <datetime literal> | <interval literal> | <hex string literal>
    let pLiteralExpr =
        choice
            [ attempt (pKeyword "NULL" >>% Null |>> Literal)
              attempt (pCharacterStringLiteral |>> String |>> Literal)
              attempt (pNationalCharacterStringLiteral |>> NationalString |>> Literal)
              attempt (pUnicodeCharacterStringLiteral |>> UnicodeString |>> Literal)
              attempt (pNumericLiteral |>> Number |>> Literal)
              attempt (pBooleanLiteral |>> Bool |>> Literal)
              attempt (pDateLiteral |>> Date |>> Literal)
              attempt (pTimeLiteral |>> Time |>> Literal)
              attempt (pTimestampLiteral |>> Timestamp |>> Literal)
              attempt (pIntervalLiteral |>> Interval |>> Literal)
              attempt (pHexStringLiteral |>> Literal.Binary |>> Literal) ]
        |> withExprPosition

    let pIdentifierExpr = pIdentifier |>> Identifier |> withExprPosition
    let pStarExpr = pstring "*" .>> ws >>% Star |> withExprPosition

    let pColumnReferenceExpr =
        sepBy1 pIdentifier (token (pstring "."))
        |>> function
            | [ id ] -> Identifier id
            | ids -> ColumnReference ids
        |> withExprPosition

    let pQualifiedName =
        pIdentifier .>>. many (token (pstring ".") >>. pIdentifier)
        |>> fun (first, rest) ->
            let parts = first :: rest

            match parts with
            | [ s ] -> Identifier s
            | _ -> ColumnReference parts
        |> withExprPosition

    // 6.13 <cast specification> ::= CAST ( <value expression> AS <data type> )
    let pCastSpecification =
        pKeyword "CAST"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        |>> Cast
        |> withExprPosition

    let pWindowFrame =
        // <window frame units> ::= ROWS | RANGE | GROUPS
        let pUnit =
            pKeyword "ROWS" >>% Rows
            <|> (pKeyword "RANGE" >>% Range)
            <|> (pKeyword "GROUPS" >>% Groups)

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

    // 6.13 <case abbreviation> ::= NULLIF ( <value expression> , <value expression> )
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

    // 6.13 <case abbreviation> ::= COALESCE ( <value expression> [ { , <value expression> }... ] )
    let pCoalesceExpr =
        pKeyword "COALESCE"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
        |>> fun exprs -> Case(None, exprs |> List.map (fun e -> { Kind = IsNull(e, true); Pos = e.Pos }, e), None)
        |> withExprPosition

    // 6.12 <case expression> ::= CASE <case operand> <simple when clause>...
    //     | CASE <searched when clause>... [ ELSE <result> ] END
    let pCaseExpression =
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

    // 7.15 <window name or specification> — the OVER (...) clause attached to a
    // window function (also parsed at the query level for the WINDOW clause).
    let pWindowNameOrSpecification =
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
                 (opt pIdentifierExpr
                  .>>. opt pPartitionBy
                  .>>. opt pOrderBy
                  .>>. opt pWindowFrame
                  |>> fun (((name, pb), ob), frame) ->
                      { ExistingWindowName = name
                        PartitionBy = Option.defaultValue [] pb
                        OrderBy = Option.defaultValue [] ob
                        Frame = frame })
             <|> (pIdentifierExpr
                  |>> fun name ->
                      { ExistingWindowName = Some name
                        PartitionBy = []
                        OrderBy = []
                        Frame = None }))

    // 6.3 <general value specification> (parameter forms): `?` is a
    // <dynamic parameter specification>, `:name` a <host parameter>.
    // Literals are handled separately by pLiteralExpr above.
    let pGeneralValueSpecification =
        pQuestionMark >>% "?" <|> pHostParameter |>> Parameter |> withExprPosition

    // 6.27 <extract expression> ::= EXTRACT ( <extract field> FROM <extract source> )
    let pExtractExpression =
        pKeyword "EXTRACT"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (getPosition
                 .>>. (choice
                     [ pKeyword "YEAR" >>% "YEAR"
                       pKeyword "MONTH" >>% "MONTH"
                       pKeyword "DAY" >>% "DAY"
                       pKeyword "HOUR" >>% "HOUR"
                       pKeyword "MINUTE" >>% "MINUTE"
                       pKeyword "SECOND" >>% "SECOND"
                       pIdentifierRaw ])
                 .>> pKeyword "FROM"
                 .>>. pExpression)
        |>> fun ((pos, field), src) ->
            Extract(
                { Kind = Identifier field
                  Pos = { Line = pos.Line; Column = pos.Column } },
                src
            )
        |> withExprPosition

    // 6.25 <position expression> ::= POSITION ( <character value expression> IN
    //     <character value expression> [ USING <char length units> ] )
    let pPositionExpression =
        pKeyword "POSITION"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "IN"
                 .>>. pExpression
                 .>>. opt (pKeyword "USING" >>. pIdentifierExpr))
        |>> (fun ((target, source), unit) -> ExpressionKind.Position(target, source, unit))
        |> withExprPosition

    // 6.18 <trim function> ::= TRIM ( [ <trim specification> ] [ <trim character> ]
    //     FROM <trim source> )
    let pTrimFunction =
        let pSpec =
            opt (
                pKeyword "LEADING" >>% Leading
                <|> (pKeyword "TRAILING" >>% Trailing)
                <|> (pKeyword "BOTH" >>% Both)
            )

        pKeyword "TRIM"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pSpec .>>. opt pExpression .>> pKeyword "FROM" .>>. pExpression)
        |>> (fun ((spec, char), source) -> Trim(spec, char, source))
        |> withExprPosition

    // 6.17 <character substring function> ::= SUBSTRING ( <character value expression>
    //     FROM <start position> [ FOR <string length> ] [ USING <char length units> ] )
    let pCharacterSubstringFunction =
        pKeyword "SUBSTRING"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "FROM"
                 .>>. pExpression
                 .>>. opt (pKeyword "FOR" >>. pExpression)
                 .>>. opt (pKeyword "USING" >>. pIdentifierRaw))
        |>> fun (((src, start), len), units) -> Substring(src, start, len, units)
        |> withExprPosition

    // 6.17 <character overlay function> ::= OVERLAY ( <character value expression>
    //     PLACING <character value expression> FROM <start position> [ FOR <string length> ] )
    let pOverlayFunction =
        pKeyword "OVERLAY"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "PLACING" .>>. pExpression .>> pKeyword "FROM"
                 .>>. pExpression
                 .>>. opt (pKeyword "FOR" >>. pExpression))
        |>> fun (((src, placing), start), len) -> Overlay(src, placing, start, len)
        |> withExprPosition

    // 6.21 <datetime value function> ::= CURRENT_DATE | CURRENT_TIMESTAMP [ <left paren>
    //     <time precision> <right paren> ] | CURRENT_TIME ... | LOCALTIMESTAMP ... | LOCALTIME ...
    let pDateTimeValueFunction =
        let pPrecision =
            opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)

        choice
            [ pKeyword "CURRENT_DATE" >>% CurrentDate
              pKeyword "CURRENT_TIMESTAMP" >>. pPrecision |>> CurrentTimestamp
              pKeyword "CURRENT_TIME" >>. pPrecision |>> CurrentTime
              pKeyword "LOCALTIMESTAMP" >>. pPrecision |>> LocalTimestamp
              pKeyword "LOCALTIME" >>. pPrecision |>> LocalTime ]
        |> withExprPosition

    // 10.9 <routine invocation> ::= <routine name> <SQL argument list>
    //   plus optional OVER (window), FILTER (WHERE), WITHIN GROUP (ORDER BY) clauses.
    let pRoutineInvocation =
        let pArgs =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
                 .>>. sepBy pExpression (token (pstring ",")))

        let pFilter =
            pKeyword "FILTER"
            >>. between (token (pstring "(")) (token (pstring ")")) (pKeyword "WHERE" >>. pExpression)

        let pWithinGroup =
            pKeyword "WITHIN"
            >>. pKeyword "GROUP"
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ",")))

        pIdentifierRaw |>> Identifier |> withExprPosition
        .>>. pArgs
        .>>. opt pWindowNameOrSpecification
        .>>. opt pFilter
        .>>. opt pWithinGroup
        |>> fun ((((name, (dist, args)), window), filter), withinGroup) ->
            match window with
            | Some w ->
                WindowFunction
                    { Function = name
                      Args = args
                      IsDistinct = Option.defaultValue false dist
                      Window = w }
            | None -> FunctionCall(name, Option.defaultValue false dist, args, None, filter, withinGroup)
        |> withExprPosition

    // 6.3 <scalar subquery> ::= ( <subquery> )
    let pScalarSubquery =
        between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> SubqueryExpression
        |> withExprPosition

    let pCte =
        pIdentifierExpr
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))
        .>> pKeyword "AS"
        .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> fun ((name, cols), q) ->
            { Cte.Name = name
              Columns = cols
              Query = q }

    let pWithClause =
        pKeyword "WITH" >>. opt (pKeyword "RECURSIVE" >>% true)
        .>>. sepBy1 pCte (token (pstring ","))
        |>> fun (recu, ctes) -> Option.defaultValue false recu, ctes

    let pQuantifier =
        choice
            [ pKeyword "ANY" >>% Quantifier.Any
              pKeyword "SOME" >>% Quantifier.SomeQuantifier
              pKeyword "ALL" >>% Quantifier.All ]

    // 8.7 <quantified comparison predicate> — the ANY | SOME | ALL subquery term.
    // Only valid as the right operand of a comparison operator (see comparisonOp).
    let pQuantifiedSubqueryTerm =
        pQuantifier .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> fun (quant, q) -> QuantifiedSubquery(quant, q)
        |> withExprPosition

    // 8 <predicate> — a postfix predicate applied to a <value expression primary>:
    //   <between predicate>, <in predicate>, <null predicate>, <distinct predicate>,
    //   <overlaps predicate>, <like predicate>, <similar predicate>, plus <collate clause>.
    let pPredicate pExpr =
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
              )
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT")
                  .>> pKeyword "DISTINCT"
                  .>> pKeyword "FROM"
                  .>>. pExpr
                  |>> fun (isNot, r) ->
                      fun l ->
                          { Kind = IsDistinctFrom(l, Option.isSome isNot, r)
                            Pos = l.Pos }
              )
              attempt (
                  pKeyword "OVERLAPS" >>. pExpr
                  |>> fun r -> fun l -> { Kind = Overlaps(l, r); Pos = l.Pos }
              )
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "LIKE"
                  .>>. pExpr
                  .>>. opt (pKeyword "ESCAPE" >>. pExpr)
                  |>> fun ((isNot, pattern), escape) ->
                      fun l ->
                          { Kind = Like(l, Option.isSome isNot, pattern, escape)
                            Pos = l.Pos }
              )
              attempt (
                  opt (pKeyword "NOT") .>> attempt (pKeyword "SIMILAR" .>> pKeyword "TO")
                  .>>. pExpr
                  .>>. opt (pKeyword "ESCAPE" >>. pExpr)
                  |>> fun ((isNot, pattern), escape) ->
                      fun l ->
                          { Kind = SimilarTo(l, Option.isSome isNot, pattern, escape)
                            Pos = l.Pos }
              )
              attempt (
                  pKeyword "COLLATE" >>. pIdentifierExpr
                  |>> fun collation ->
                      fun e ->
                          { Kind = Collate(e, collation)
                            Pos = e.Pos }
              ) ]

    // 8.10 <exists predicate> ::= EXISTS ( <subquery> )
    let pExistsPredicate =
        pKeyword "EXISTS" >>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> Exists
        |> withExprPosition

    // 8.11 <unique predicate> ::= UNIQUE ( <subquery> )
    let pUniquePredicate =
        pKeyword "UNIQUE" >>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> ExpressionKind.Unique
        |> withExprPosition

    // 6.3 <value expression primary> — the atomic building block of every <value
    // expression>, used as the term parser of the operator-precedence parser below.
    let pValueExpressionPrimary =
        choice
            [ attempt pCastSpecification
              attempt pCaseExpression
              attempt pNullifExpr
              attempt pCoalesceExpr
              attempt pExtractExpression
              attempt pPositionExpression
              attempt pTrimFunction
              attempt pCharacterSubstringFunction
              attempt pOverlayFunction
              attempt pDateTimeValueFunction
              attempt pRoutineInvocation
              attempt pScalarSubquery
              attempt pExistsPredicate
              attempt pUniquePredicate
              attempt pLiteralExpr
              attempt pGeneralValueSpecification
              attempt pStarExpr
              attempt pQuantifiedSubqueryTerm
              pColumnReferenceExpr
              between (token (pstring "(")) (token (pstring ")")) pExpression ]

    // DEFAULT is only valid in specific contexts (INSERT VALUES, UPDATE SET), not as a
    // general expression. This parser is used by the DML parser for those contexts.
    let pDefaultValue: Parser<Expression, unit> =
        pKeyword "DEFAULT" >>% Default |> withExprPosition

    let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
    opp.TermParser <- pValueExpressionPrimary

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

    let comparisonOp (op: BinaryOperator) (x: Expression) (y: Expression) : ExpressionKind =
        match y.Kind with
        | QuantifiedSubquery(quant, q) -> QuantifiedComparison(op, quant, x, q)
        | _ -> BinaryOp(op, x, y)

    addInfix "=" 5 Associativity.Left (comparisonOp Equal)
    addInfix "<>" 5 Associativity.Left (comparisonOp NotEqual)
    addInfix "<" 5 Associativity.Left (comparisonOp LessThan)
    addInfix "<=" 5 Associativity.Left (comparisonOp LessThanOrEqual)
    addInfix ">" 5 Associativity.Left (comparisonOp GreaterThan)
    addInfix ">=" 5 Associativity.Left (comparisonOp GreaterThanOrEqual)

    // <boolean test>: a <value expression> plus an optional postfix <predicate>.
    let pBooleanTest =
        opp.ExpressionParser .>>. many (pPredicate opp.ExpressionParser)
        |>> fun (e, suffixes) -> List.fold (fun acc f -> f acc) e suffixes

    // <boolean factor> ::= [ NOT ] <boolean test>
    let pNotExpr, pNotExprRef = createParserForwardedToRef<Expression, unit> ()

    pNotExprRef.Value <-
        (attempt (pKeyword "NOT" >>. pNotExpr)
         |>> fun e ->
             { Expression.Kind = UnaryOp(Not, e)
               Pos = e.Pos })
        <|> pBooleanTest

    // <boolean term> ::= <boolean factor> | <boolean term> AND <boolean factor>
    let pAndExpr =
        chainl1
            pNotExpr
            (pKeyword "AND"
             >>% fun l r ->
                 { Expression.Kind = BinaryOp(And, l, r)
                   Pos = l.Pos })

    // <boolean value expression> ::= <boolean term> | <boolean value expression> OR <boolean term>
    let pOrExpr =
        chainl1
            pAndExpr
            (pKeyword "OR"
             >>% fun l r ->
                 { Expression.Kind = BinaryOp(Or, l, r)
                   Pos = l.Pos })

    // ANY/SOME/ALL (subquery) is only valid as the right operand of a comparison operator,
    // where `comparisonOp` rewrites it into QuantifiedComparison. Any QuantifiedSubquery that
    // survives (i.e. was not rewritten) is standalone and must be rejected.
    let rec containsStandaloneQuantifiedSubquery (e: Expression) =
        match e.Kind with
        | QuantifiedSubquery _ -> true
        | BinaryOp(_, l, r) -> containsStandaloneQuantifiedSubquery l || containsStandaloneQuantifiedSubquery r
        | UnaryOp(_, x) -> containsStandaloneQuantifiedSubquery x
        | FunctionCall(name, _, args, _, filter, withinGroup) ->
            containsStandaloneQuantifiedSubquery name
            || List.exists containsStandaloneQuantifiedSubquery args
            || Option.exists containsStandaloneQuantifiedSubquery filter
            || Option.exists (List.exists (fun (e, _, _) -> containsStandaloneQuantifiedSubquery e)) withinGroup
        | Cast(x, _) -> containsStandaloneQuantifiedSubquery x
        | Case(cond, whens, elseExpr) ->
            Option.exists containsStandaloneQuantifiedSubquery cond
            || List.exists
                (fun (w, t) -> containsStandaloneQuantifiedSubquery w || containsStandaloneQuantifiedSubquery t)
                whens
            || Option.exists containsStandaloneQuantifiedSubquery elseExpr
        | WindowFunction wf ->
            containsStandaloneQuantifiedSubquery wf.Function
            || List.exists containsStandaloneQuantifiedSubquery wf.Args
        | Between(x, _, _, lo, hi) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery lo
            || containsStandaloneQuantifiedSubquery hi
        | InList(x, _, items) ->
            containsStandaloneQuantifiedSubquery x
            || List.exists containsStandaloneQuantifiedSubquery items
        | IsNull(x, _) -> containsStandaloneQuantifiedSubquery x
        | IsBoolean(x, _, _) -> containsStandaloneQuantifiedSubquery x
        | IsDistinctFrom(x, _, y) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery y
        | Overlaps(x, y) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery y
        | Collate(x, c) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery c
        | Like(x, _, p, esc) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery p
            || Option.exists containsStandaloneQuantifiedSubquery esc
        | SimilarTo(x, _, p, esc) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery p
            || Option.exists containsStandaloneQuantifiedSubquery esc
        | Extract(x, f) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery f
        | Position(x, s, len) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery s
            || Option.exists containsStandaloneQuantifiedSubquery len
        | Trim(_, src, x) ->
            Option.exists containsStandaloneQuantifiedSubquery src
            || containsStandaloneQuantifiedSubquery x
        | Substring(x, s, len, _) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery s
            || Option.exists containsStandaloneQuantifiedSubquery len
        | Overlay(x, p, f, len) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery p
            || containsStandaloneQuantifiedSubquery f
            || Option.exists containsStandaloneQuantifiedSubquery len
        | QuantifiedComparison(_, _, x, _) -> containsStandaloneQuantifiedSubquery x
        | _ -> false

    pExpressionRef.Value <-
        pOrExpr
        >>= fun e ->
            if containsStandaloneQuantifiedSubquery e then
                fail "quantified subquery requires a comparison operator"
            else
                preturn e
