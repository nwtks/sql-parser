namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module PredicateParser =
    // 8.2 <comparison predicate> ::= <row value predicand> <comp op> <row value predicand>
    // 8.9 <quantified comparison predicate> — rewrites <comp op> <quantifier> <table subquery> on the right
    // The addInfix calls below register the six <comp op>s on ExpressionParser's `opp` — the
    // operators belong to `opp`, but the rules are 8.2's, so the registrations live here. The
    // right-hand <quantifier> <table subquery> term is built by pPredicatePrimary's local
    // pQuantifiedSubqueryTerm below.
    let private comparisonOp op x (y: Expression) =
        match y.Kind with
        | QuantifiedSubquery(quant, q) -> QuantifiedComparison(op, quant, x, q)
        | _ -> BinaryOp(op, x, y)

    // 8.2 <comp op> ::= <equals operator> | <not equals operator> | <less than operator> | <less than or equals operator> | <greater than operator> | <greater than or equals operator>
    addInfix "=" 5 Associativity.Left (comparisonOp Equal)
    addInfix "<>" 5 Associativity.Left (comparisonOp NotEqual)
    addInfix "<" 5 Associativity.Left (comparisonOp LessThan)
    addInfix "<=" 5 Associativity.Left (comparisonOp LessThanOrEqual)
    addInfix ">" 5 Associativity.Left (comparisonOp GreaterThan)
    addInfix ">=" 5 Associativity.Left (comparisonOp GreaterThanOrEqual)

    // 8.2 <comp op> — used by the 6.12 <when operand> comparison part 2 only: under 8.1 a
    // comparison is an `opp` infix operator (ExpressionParser.fs), not a predicate suffix.
    let private pComparisonOperator =
        choice
            [ attempt (token (pstring "<=")) >>% BinaryOperator.LessThanOrEqual
              attempt (token (pstring "<>")) >>% BinaryOperator.NotEqual
              attempt (token (pstring ">=")) >>% BinaryOperator.GreaterThanOrEqual
              attempt (token (pstring "=")) >>% BinaryOperator.Equal
              attempt (token (pstring "<")) >>% BinaryOperator.LessThan
              attempt (token (pstring ">")) >>% BinaryOperator.GreaterThan ]

    // 8.20 <period predicate> operators (OVERLAPS is covered by the existing Overlaps case).
    // Module-level because pPredicatePrimary needs a lookahead of it.
    let private pPeriodPredicateOperator =
        choice
            [ pKeyword "EQUALS" >>% PeriodEquals
              pKeyword "CONTAINS" >>% PeriodContains
              pKeyword "PRECEDES" >>% PeriodPrecedes
              pKeyword "SUCCEEDS" >>% PeriodSucceeds
              attempt (pKeyword "IMMEDIATELY" >>. pKeyword "PRECEDES" >>% PeriodImmediatelyPrecedes)
              attempt (pKeyword "IMMEDIATELY" >>. pKeyword "SUCCEEDS" >>% PeriodImmediatelySucceeds) ]

    // 8.20 <period predicand> ::= <period reference> | PERIOD ( <period start value>, <period end value> )
    // Both slots are <datetime value expression> per the grammar, not general expressions.
    let private pPeriodPredicand =
        pKeyword "PERIOD"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pDatetimeValueExpression .>> token (pstring ",") .>>. pDatetimeValueExpression)
        |>> (fun (start, finish) -> PeriodValue(start, finish))
        |> withExprPosition

    // 8.1 <predicate> — a postfix predicate applied to a <value expression primary>:
    //   <between predicate>, <in predicate>, <null predicate>, <distinct predicate>,
    //   <overlaps predicate>, <like predicate>, <similar predicate>, plus <collate clause>.
    // Returns the suffix as a function applied to the part-1 operand.
    // `forWhenOperand = true` selects the subset 6.12 <when operand> lists: the
    // <comparison predicate part 2> / <quantified comparison predicate part 2> forms (infix
    // operators under 8.1) replace the alternatives 6.12 does not list (6.39 <boolean test>,
    // <distinct predicate>, <collate clause>, <type predicate>, <JSON predicate>,
    // <member>/<submultiset>/<set>/<period> predicates). `includeBooleanTest = false` drops
    // the 6.39 <boolean test>, which needs a <boolean primary> on its left (ExpressionParser's
    // pBooleanTestSuffixes decides between the three forms). The 8.19/8.20 sub-parsers are
    // local because this is their only consumer.
    let private pPredicateImpl forWhenOperand includeBooleanTest pExpr =
        // A predicate part-2 operand is a <row value predicand>: a TOP-LEVEL
        // boolean-producing expression (comparison, AND/OR/NOT, or another predicate)
        // is rejected. A PARENTHESIZED expression is a 6.39 <boolean predicand> and stays
        // legal — the AST keeps a Parenthesized node, so `x BETWEEN (1 = 1) AND 2` parses.
        let pOperand =
            pExpr
            >>= fun e ->
                if isBooleanTopLevel e then
                    fail "a predicate part-2 operand must be a <row value predicand>"
                else
                    preturn e

        // 7.2 <row value expression> ::= <row value special case> | <explicit row value constructor>
        //   <row value special case> ::= <nonparenthesized value expression primary>
        // A parenthesized value expression (`(1)`) is a <value expression primary> but NOT a
        // <row value special case>; a term (`1 + 1`) or a signed primary is not a primary at all.
        // An explicit row value constructor (including a <row subquery>) is the other alternative.
        let isRowValueExpression e =
            not (isBooleanTopLevel e)
            && match e.Kind with
               | RowValueConstructor _ -> true // <explicit row value constructor>
               | SubqueryExpression _ -> true // <row subquery>
               | Parenthesized _ -> false // <parenthesized value expression>
               | BinaryOp _ -> false // a term, not a primary
               | UnaryOp _ -> false // [ <sign> ] <numeric primary>, not a primary
               | _ -> true // a <nonparenthesized value expression primary>

        // A grammar slot that requires a *value* expression (8.5/8.6/8.7 pattern and escape,
        // 8.16/8.17 multiset operand): a boolean and an explicit row value constructor are both
        // excluded, so `'a' LIKE (1, 2)` is rejected. Character-vs-numeric distinctions stay
        // semantic — a parse-only library cannot see them.
        let isValueShaped e =
            not (isBooleanTopLevel e)
            && match e.Kind with
               | RowValueConstructor _ -> false
               | _ -> true

        // A slot whose grammar is a *value* expression — the <character pattern> and
        // <escape character> of 8.5, the <similar pattern> of 8.6, the <XQuery pattern> /
        // <XQuery option flag> of 8.7, and the <multiset value expression> of 8.16/8.17.
        // A boolean is rejected (pOperand's check) and so is an explicit row value
        // constructor: `'a' LIKE (1, 2)` is not a <character pattern>.
        let pValueOperand =
            pExpr
            >>= fun e ->
                if isValueShaped e then
                    preturn e
                else
                    fail "this operand must be a <value expression> (8.5/8.6/8.7)"

        // 8.2 <comparison predicate part 2> ::= <comp op> <row value predicand>
        // 8.9 <quantified comparison predicate part 2> ::= <comp op> <quantifier> <table subquery>
        // 6.12 <when operand> only (see the `forWhenOperand` note above); the <case operand>
        // becomes the left-hand <row value predicand>. The quantifier is local because this
        // is its only consumer.
        let pComparisonPart2 =
            // 8.9 <quantifier> ::= ALL | SOME | ANY
            let pQuantifier =
                choice
                    [ pKeyword "ANY" >>% Quantifier.Any
                      pKeyword "SOME" >>% Quantifier.SomeQuantifier
                      pKeyword "ALL" >>% Quantifier.All ]

            attempt (
                pComparisonOperator
                .>>. (attempt (
                          pQuantifier
                          .>>. between (token (pstring "(")) (token (pstring ")")) QueryParser.pQueryExpression
                      )
                      |>> Choice1Of2
                      <|> (pOperand |>> Choice2Of2))
                |>> fun (op, operand) ->
                    fun e ->
                        match operand with
                        | Choice1Of2(quant, q) ->
                            { Expression.Kind = QuantifiedComparison(op, quant, e, q)
                              Pos = e.Pos }
                        | Choice2Of2 value ->
                            { Expression.Kind = BinaryOp(op, e, value)
                              Pos = e.Pos }
            )

        // 8.3 <between predicate part 2> ::= [ NOT ] BETWEEN [ ASYMMETRIC | SYMMETRIC ]
        //     <row value predicand> AND <row value predicand>
        let pBetweenPart2 =
            attempt (
                opt (pKeyword "NOT") .>> pKeyword "BETWEEN"
                .>>. opt (pKeyword "ASYMMETRIC" <|> pKeyword "SYMMETRIC")
                .>>. pOperand
                .>> pKeyword "AND"
                .>>. pOperand
                |>> fun (((isNot, sym), start), endBound) ->
                    fun e ->
                        { Expression.Kind =
                            ExpressionKind.Between(e, Option.isSome isNot, Option.isSome sym, start, endBound)
                          Pos = e.Pos }
            )

        // 8.4 <in value list> ::= <row value expression> [ { <comma> <row value expression> }... ]
        // `1 + 1` (a term) and `(1)` (a parenthesized value expression) are neither a
        // <row value special case> nor an <explicit row value constructor>.
        let pInValueItem =
            pExpr
            >>= fun e ->
                if isRowValueExpression e then
                    preturn e
                else
                    fail "an <in value list> element must be a <row value expression> (8.4)"

        // 8.4 <in predicate part 2> ::= [ NOT ] IN { <table subquery> | <in predicate value> }
        let pInPart2 =
            attempt (
                opt (pKeyword "NOT") .>> pKeyword "IN"
                .>>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (attempt QueryParser.pQueryExpression |>> Choice1Of2
                     <|> (sepBy1 pInValueItem (token (pstring ",")) |>> Choice2Of2))
                |>> fun (isNot, res) ->
                    fun e ->
                        let kind =
                            match res with
                            | Choice1Of2 q -> InSubquery(e, Option.isSome isNot, q)
                            | Choice2Of2 l -> InList(e, Option.isSome isNot, l)

                        { Expression.Kind = kind; Pos = e.Pos }
            )

        // 8.5 <like predicate> ::= <character string value expression> [ NOT ] LIKE <character string pattern> [ ESCAPE <escape character> ]
        let pLikePart2 =
            attempt (
                opt (pKeyword "NOT") .>> pKeyword "LIKE"
                .>>. pValueOperand
                .>>. opt (pKeyword "ESCAPE" >>. pValueOperand)
                |>> fun ((isNot, pattern), escape) ->
                    fun l ->
                        { Expression.Kind = Like(l, Option.isSome isNot, pattern, escape)
                          Pos = l.Pos }
            )

        // 8.6 <similar predicate> ::= <character string value expression> [ NOT ] SIMILAR TO <character string pattern> [ ESCAPE <escape character> ]
        // Both operands are <character value expression>s, so they take the same value-shape
        // check as the LIKE operands (pValueOperand).
        let pSimilarPart2 =
            attempt (
                opt (pKeyword "NOT") .>> attempt (pKeyword "SIMILAR" .>> pKeyword "TO")
                .>>. pValueOperand
                .>>. opt (pKeyword "ESCAPE" >>. pValueOperand)
                |>> fun ((isNot, pattern), escape) ->
                    fun l ->
                        { Expression.Kind = SimilarTo(l, Option.isSome isNot, pattern, escape)
                          Pos = l.Pos }
            )

        // 8.7 <regex like predicate> ::= [ NOT ] LIKE_REGEX <XQuery pattern>
        //     [ FLAG <XQuery option flag> ]
        let pRegexLikePart2 =
            attempt (
                opt (pKeyword "NOT") .>> pKeyword "LIKE_REGEX"
                .>>. pValueOperand
                .>>. opt (pKeyword "FLAG" >>. pValueOperand)
                |>> fun ((isNot, pattern), flag) ->
                    fun e ->
                        { Expression.Kind = RegexLike(e, Option.isSome isNot, pattern, flag)
                          Pos = e.Pos }
            )

        // 8.8 <null predicate part 2> ::= IS [ NOT ] NULL
        let pNullPart2 =
            attempt (
                pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "NULL"
                |>> fun isNot ->
                    fun e ->
                        { Expression.Kind = IsNull(e, Option.isSome isNot)
                          Pos = e.Pos }
            )

        // 8.12 <normalized predicate> ::= IS [ NOT ] [ <normal form> ] NORMALIZED
        let pNormalizedPart2 =
            attempt (
                pKeyword "IS" >>. opt (pKeyword "NOT") .>>. opt pNormalForm
                .>> pKeyword "NORMALIZED"
                |>> fun (isNot, form) ->
                    fun e ->
                        { Expression.Kind = IsNormalized(e, Option.isSome isNot, form)
                          Pos = e.Pos }
            )

        // 8.13 <match predicate> ::= MATCH [ UNIQUE ] [ SIMPLE | PARTIAL | FULL ]
        //     <table subquery>
        let pMatchPart2 =
            attempt (
                pKeyword "MATCH" >>. opt (pKeyword "UNIQUE" >>% true)
                .>>. opt (
                    pKeyword "SIMPLE" >>% Simple
                    <|> (pKeyword "PARTIAL" >>% Partial)
                    <|> (pKeyword "FULL" >>% Full)
                )
                .>>. between (token (pstring "(")) (token (pstring ")")) QueryParser.pQueryExpression
                |>> fun ((isUnique, matchOption), q) ->
                    fun e ->
                        { Expression.Kind = Match(e, Option.isSome isUnique, matchOption, q)
                          Pos = e.Pos }
            )

        // 8.14 <overlaps predicate> ::= <row value predicand 1> OVERLAPS <row value predicand 2>
        let pOverlapsPart2 =
            attempt (
                pKeyword "OVERLAPS" >>. pOperand
                |>> fun r ->
                    fun l ->
                        { Expression.Kind = Overlaps(l, r)
                          Pos = l.Pos }
            )

        // 8.15 <distinct predicate> ::= <row value predicand> IS [ NOT ] DISTINCT FROM <row value predicand>
        let pDistinctPart2 =
            attempt (
                pKeyword "IS" >>. opt (pKeyword "NOT")
                .>> pKeyword "DISTINCT"
                .>> pKeyword "FROM"
                .>>. pOperand
                |>> fun (isNot, r) ->
                    fun l ->
                        { Expression.Kind = IsDistinctFrom(l, Option.isSome isNot, r)
                          Pos = l.Pos }
            )

        // 8.16 <member predicate> ::= [ NOT ] MEMBER [ OF ] <multiset value expression>
        let pMemberPart2 =
            attempt (
                opt (pKeyword "NOT") .>> pKeyword "MEMBER" .>> opt (pKeyword "OF")
                .>>. pValueOperand
                |>> fun (isNot, multiset) ->
                    fun e ->
                        { Expression.Kind = MemberOf(e, Option.isSome isNot, multiset)
                          Pos = e.Pos }
            )

        // 8.17 <submultiset predicate> ::= [ NOT ] SUBMULTISET [ OF ] <multiset value expression>
        let pSubmultisetPart2 =
            attempt (
                opt (pKeyword "NOT") .>> pKeyword "SUBMULTISET" .>> opt (pKeyword "OF")
                .>>. pValueOperand
                |>> fun (isNot, multiset) ->
                    fun e ->
                        { Expression.Kind = SubmultisetOf(e, Option.isSome isNot, multiset)
                          Pos = e.Pos }
            )

        // 8.18 <set predicate> ::= IS [ NOT ] A SET
        let pSetPart2 =
            attempt (
                pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "A" .>> pKeyword "SET"
                |>> fun isNot ->
                    fun e ->
                        { Expression.Kind = IsSet(e, Option.isSome isNot)
                          Pos = e.Pos }
            )

        // 8.19 <user-defined type specification> ::= <user-defined type name> | ONLY <user-defined type name>
        let pUserDefinedTypeSpecification =
            choice
                [ pKeyword "ONLY" >>. pSchemaQualifiedNameExpression |>> Exclusive
                  pSchemaQualifiedNameExpression |>> Inclusive ]

        // 8.19 <type predicate> ::= IS [ NOT ] OF ( <type list> )
        let pTypePart2 =
            attempt (
                pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "OF"
                .>>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (sepBy1 pUserDefinedTypeSpecification (token (pstring ",")))
                |>> fun (isNot, types) ->
                    fun e ->
                        { Expression.Kind = IsOfType(e, Option.isSome isNot, types)
                          Pos = e.Pos }
            )

        // 8.20 <period predicate> ::= <period predicate operator> <period predicand>
        // EQUALS / PRECEDES / SUCCEEDS / IMMEDIATELY ... require a <period predicand>
        // on the right; only CONTAINS admits a <point in time> (<datetime value
        // expression>). The LEFT operand is re-checked once the whole expression is parsed
        // (ExpressionParser.findExpressionViolationIn).
        let pPeriodPart2 =
            attempt (
                pPeriodPredicateOperator
                >>= fun kind ->
                    (if kind = PeriodContains then
                         // CONTAINS admits a <point in time> (<datetime value expression>).
                         attempt pPeriodPredicand <|> pOperand |>> fun right -> kind, right
                     else
                         // EQUALS / PRECEDES / SUCCEEDS / IMMEDIATELY ... require a
                         // <period predicand>: PERIOD ( ... ) or a <period reference>
                         // (a plain name).
                          attempt pPeriodPredicand <|> pSchemaQualifiedNameExpression
                          |>> fun right -> kind, right)
                    |>> fun (kind, right) ->
                        fun left ->
                            { Expression.Kind = PeriodPredicate(kind, left, right)
                              Pos = left.Pos }
            )

        // 8.22 <JSON predicate> ::= <string value expression> [ <JSON input clause> ]
        //     IS [ NOT ] JSON [ <JSON predicate type constraint> ] [ <JSON key uniqueness constraint> ]
        // The optional FORMAT slot is part of this suffix parser: pPredicate applies the
        // whole suffix (FORMAT included) to the already-parsed part-1 operand.
        let pJsonPart2 =
            attempt (
                opt pJsonInputClause
                .>>. (pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "JSON")
                .>>. opt (
                    pKeyword "VALUE" >>% JsonTypeValue
                    <|> (pKeyword "ARRAY" >>% JsonTypeArray)
                    <|> (pKeyword "OBJECT" >>% JsonTypeObject)
                    <|> (pKeyword "SCALAR" >>% JsonTypeScalar)
                )
                .>>. opt (
                    attempt (
                        pKeyword "WITH" >>% Some true <|> (pKeyword "WITHOUT" >>% Some false)
                        .>> pKeyword "UNIQUE"
                        .>> opt (pKeyword "KEYS")
                    )
                )
                |>> fun (((format, isNot), typeConstraint), unique) ->
                    fun e ->
                        { Expression.Kind =
                            IsJson(e, format, Option.isSome isNot, typeConstraint, Option.flatten unique)
                          Pos = e.Pos }
            )

        // 10.7 <collate clause> ::= COLLATE <collation name>
        let pCollatePart2 =
            attempt (
                pKeyword "COLLATE" >>. pIdentifierExpression
                |>> fun collation ->
                    fun e ->
                        { Expression.Kind = Collate(e, collation)
                          Pos = e.Pos }
            )

        // The alternatives in the two lists below are in the historical dispatch order
        // (every one of them backtracks); the definitions above follow the clause numbers.
        let pPredicateBranches =
            [ pBetweenPart2; pInPart2; pNullPart2 ]
            @ (if includeBooleanTest then [ pBooleanTestPart2 ] else [])
            @ [ pDistinctPart2
                pOverlapsPart2
                pLikePart2
                pSimilarPart2
                pCollatePart2
                pNormalizedPart2
                pTypePart2
                pJsonPart2
                pRegexLikePart2
                pMatchPart2
                pMemberPart2
                pSubmultisetPart2
                pSetPart2
                pPeriodPart2 ]

        let pWhenOperandBranches =
            [ pComparisonPart2
              pBetweenPart2
              pInPart2
              pLikePart2
              pSimilarPart2
              pRegexLikePart2
              pNullPart2
              pNormalizedPart2
              pMatchPart2
              pOverlapsPart2 ]

        choice (
            if forWhenOperand then
                pWhenOperandBranches
            else
                pPredicateBranches
        )

    // 8.1 <predicate> suffix — all alternatives (ExpressionParser.pBooleanTestSuffixes).
    let pPredicate pExpr = pPredicateImpl false true pExpr

    // 8.1 <predicate> without the 6.39 <boolean test> — used where the accumulated expression
    // is a <row value predicand> but not a <boolean primary> (`1 + 1 BETWEEN 1 AND 2` is legal,
    // `1 + 1 IS TRUE` is not).
    let pPredicateNoBooleanTest pExpr = pPredicateImpl false false pExpr

    // 6.12 <when operand> — the predicate part-2 alternatives as a function applied to the
    // <case operand> (ExpressionParser.pCaseExpression, via the pWhenOperandPart2 forward ref).
    let pWhenOperandPart2 pExpr = pPredicateImpl true false pExpr

    // 8.10 <exists predicate> ::= EXISTS ( <subquery> )
    let private pExistsPredicate =
        pKeyword "EXISTS"
        >>. between (token (pstring "(")) (token (pstring ")")) QueryParser.pQueryExpression
        |>> Exists
        |> withExprPosition

    // 8.11 <unique predicate> ::= UNIQUE ( <subquery> )
    let private pUniquePredicate =
        pKeyword "UNIQUE"
        >>. between (token (pstring "(")) (token (pstring ")")) QueryParser.pQueryExpression
        |>> ExpressionKind.Unique
        |> withExprPosition

    // 8.23 <JSON exists predicate> ::= JSON_EXISTS ( <JSON API common syntax>
    //     [ <JSON exists error behavior> ON ERROR ] )
    let private pJsonExistsPredicate =
        // 8.23 <JSON exists error behavior> ::= TRUE | FALSE | UNKNOWN | ERROR
        let pJsonExistsErrorBehavior =
            choice
                [ pKeyword "TRUE" >>% JsonExistsTrue
                  pKeyword "FALSE" >>% JsonExistsFalse
                  pKeyword "UNKNOWN" >>% JsonExistsUnknown
                  pKeyword "ERROR" >>% JsonExistsError ]

        pKeyword "JSON_EXISTS"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pJsonApiCommonSyntax
                 .>>. opt (pJsonExistsErrorBehavior .>> pKeyword "ON" .>> pKeyword "ERROR"))
        |>> fun (common, onError) -> JsonExists(common, onError)
        |> withExprPosition

    // Bundles the §8 parsers that are also <value expression primary> alternatives — 8.9
    // ANY/SOME/ALL (subquery), 8.10 EXISTS, 8.11 UNIQUE, 8.20 PERIOD () and 8.23 JSON_EXISTS —
    // so ExpressionParser needs one forward ref instead of five. This is not a grammar rule:
    // it is a wiring helper. Each alternative backtracks. The 8.9 sub-parsers are local
    // because this is their only consumer.
    let private pPredicatePrimary =
        // 8.9 <quantifier> ::= ALL | SOME | ANY
        let pQuantifier =
            choice
                [ pKeyword "ANY" >>% Quantifier.Any
                  pKeyword "SOME" >>% Quantifier.SomeQuantifier
                  pKeyword "ALL" >>% Quantifier.All ]

        // Only valid as the right operand of a comparison operator (see comparisonOp in ExpressionParser).
        // 8.9 <quantified comparison predicate> — the ANY | SOME | ALL subquery term
        let pQuantifiedSubqueryTerm =
            pQuantifier
            .>>. between (token (pstring "(")) (token (pstring ")")) QueryParser.pQueryExpression
            |>> fun (quant, q) -> QuantifiedSubquery(quant, q)
            |> withExprPosition

        choice
            [ attempt pQuantifiedSubqueryTerm
              attempt pExistsPredicate
              attempt pUniquePredicate
              attempt pJsonExistsPredicate
              // 8.20 — PERIOD ( <start>, <end> ) is a <period predicand>, which exists
              // only inside a <period predicate>: require a period-predicate operator
              // to follow, so it cannot leak as a standalone atom.
              attempt (pPeriodPredicand .>> lookAhead pPeriodPredicateOperator) ]

    pPredicatePrimaryRef.Value <- pPredicatePrimary
