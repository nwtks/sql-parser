namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

// 8 Predicates
//
// This module is compiled after QueryParser.fs (clause order §6 → §7 → §8), but §6.3
// <value expression primary> and §6.39 <boolean test> consume these parsers. The reverse
// dependency is resolved with two forward refs declared in ExpressionParser.fs and wired in
// SqlParser.fs: pPredicate (the 8.1 postfix) and pPredicatePrimary (the 8.9/8.10/8.11/8.20/
// 8.23 atoms bundled into one). The local definitions shadow the opened forward refs of the
// same name — the `Ref` cells are the same objects.
module PredicateParser =
    // 8.20 <period predicand> ::= <period reference> | PERIOD ( <start> , <end> )
    let pPeriodPredicand =
        pKeyword "PERIOD"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> token (pstring ",") .>>. pExpression)
        |>> (fun (start, finish) -> PeriodValue(start, finish))
        |> withExprPosition

    // 8.1 <predicate> — a postfix predicate applied to a <value expression primary>:
    //   <between predicate>, <in predicate>, <null predicate>, <distinct predicate>,
    //   <overlaps predicate>, <like predicate>, <similar predicate>, plus <collate clause>.
    // The 8.19/8.20 sub-parsers are local because this is their only consumer.
    let pPredicate pExpr =
        // 8.19 <user-defined type specification> ::= <user-defined type name> | ONLY <user-defined type name>
        let pUserDefinedTypeSpecification =
            choice
                [ pKeyword "ONLY" >>. pSchemaQualifiedNameExpression |>> Exclusive
                  pSchemaQualifiedNameExpression |>> Inclusive ]

        // 8.20 <period predicate> operators (OVERLAPS is covered by the existing Overlaps case)
        let pPeriodPredicateOperator =
            choice
                [ pKeyword "EQUALS" >>% PeriodEquals
                  pKeyword "CONTAINS" >>% PeriodContains
                  pKeyword "PRECEDES" >>% PeriodPrecedes
                  pKeyword "SUCCEEDS" >>% PeriodSucceeds
                  attempt (pKeyword "IMMEDIATELY" >>. pKeyword "PRECEDES" >>% PeriodImmediatelyPrecedes)
                  attempt (pKeyword "IMMEDIATELY" >>. pKeyword "SUCCEEDS" >>% PeriodImmediatelySucceeds) ]

        choice
            [ // 8.3 <between predicate> ::= <row value predicand> [ NOT ] BETWEEN [ ASYMMETRIC | SYMMETRIC ] <row value predicand> AND <row value predicand>
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "BETWEEN"
                  .>>. opt (pKeyword "ASYMMETRIC" <|> pKeyword "SYMMETRIC")
                  .>>. pExpr
                  .>> pKeyword "AND"
                  .>>. pExpr
                  |>> fun (((isNot, sym), start), endBound) ->
                      fun e ->
                          { Expression.Kind =
                              ExpressionKind.Between(e, Option.isSome isNot, Option.isSome sym, start, endBound)
                            Pos = e.Pos }
              )
              // 8.4 <in predicate> ::= <row value predicand> [ NOT ] IN { <table subquery> | <in predicate value list> }
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
              // 8.8 <null predicate> ::= <row value predicand> IS [ NOT ] NULL — 6.39 <boolean test> ::= <boolean primary> IS [ NOT ] { TRUE | FALSE | UNKNOWN }
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
              // 8.15 <distinct predicate> ::= <row value predicand> IS [ NOT ] DISTINCT FROM <row value predicand>
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT")
                  .>> pKeyword "DISTINCT"
                  .>> pKeyword "FROM"
                  .>>. pExpr
                  |>> fun (isNot, r) ->
                      fun l ->
                          { Expression.Kind = IsDistinctFrom(l, Option.isSome isNot, r)
                            Pos = l.Pos }
              )
              // 8.14 <overlaps predicate> ::= <row value predicand 1> OVERLAPS <row value predicand 2>
              attempt (
                  pKeyword "OVERLAPS" >>. pExpr
                  |>> fun r ->
                      fun l ->
                          { Expression.Kind = Overlaps(l, r)
                            Pos = l.Pos }
              )
              // 8.5 <like predicate> ::= <character string value expression> [ NOT ] LIKE <character string pattern> [ ESCAPE <escape character> ]
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "LIKE"
                  .>>. pExpr
                  .>>. opt (pKeyword "ESCAPE" >>. pExpr)
                  |>> fun ((isNot, pattern), escape) ->
                      fun l ->
                          { Expression.Kind = Like(l, Option.isSome isNot, pattern, escape)
                            Pos = l.Pos }
              )
              // 8.6 <similar predicate> ::= <character string value expression> [ NOT ] SIMILAR TO <character string pattern> [ ESCAPE <escape character> ]
              attempt (
                  opt (pKeyword "NOT") .>> attempt (pKeyword "SIMILAR" .>> pKeyword "TO")
                  .>>. pExpr
                  .>>. opt (pKeyword "ESCAPE" >>. pExpr)
                  |>> fun ((isNot, pattern), escape) ->
                      fun l ->
                          { Expression.Kind = SimilarTo(l, Option.isSome isNot, pattern, escape)
                            Pos = l.Pos }
              )
              // 10.7 <collate clause> ::= COLLATE <collation name>
              attempt (
                  pKeyword "COLLATE" >>. pIdentifierExpression
                  |>> fun collation ->
                      fun e ->
                          { Expression.Kind = Collate(e, collation)
                            Pos = e.Pos }
              )
              // 8.12 <normalized predicate> ::= IS [ NOT ] [ <normal form> ] NORMALIZED
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT") .>>. opt pNormalForm
                  .>> pKeyword "NORMALIZED"
                  |>> fun (isNot, form) ->
                      fun e ->
                          { Expression.Kind = IsNormalized(e, Option.isSome isNot, form)
                            Pos = e.Pos }
              )
              // 8.19 <type predicate> ::= IS [ NOT ] OF ( <type list> )
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
              // 8.22 <JSON predicate> ::= IS [ NOT ] JSON [ VALUE | ARRAY | OBJECT | SCALAR ]
              //     [ WITH | WITHOUT UNIQUE [ KEYS ] ]
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "JSON"
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
                  |>> fun ((isNot, typeConstraint), unique) ->
                      fun e ->
                          { Expression.Kind = IsJson(e, Option.isSome isNot, typeConstraint, Option.flatten unique)
                            Pos = e.Pos }
              )
              // 8.7 <regex like predicate> ::= [ NOT ] LIKE_REGEX <XQuery pattern>
              //     [ FLAG <XQuery option flag> ]
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "LIKE_REGEX"
                  .>>. pExpr
                  .>>. opt (pKeyword "FLAG" >>. pExpr)
                  |>> fun ((isNot, pattern), flag) ->
                      fun e ->
                          { Expression.Kind = RegexLike(e, Option.isSome isNot, pattern, flag)
                            Pos = e.Pos }
              )
              // 8.13 <match predicate> ::= MATCH [ UNIQUE ] [ SIMPLE | PARTIAL | FULL ]
              //     <table subquery>
              attempt (
                  pKeyword "MATCH" >>. opt (pKeyword "UNIQUE" >>% true)
                  .>>. opt (
                      pKeyword "SIMPLE" >>% Simple
                      <|> (pKeyword "PARTIAL" >>% Partial)
                      <|> (pKeyword "FULL" >>% Full)
                  )
                  .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
                  |>> fun ((isUnique, matchOption), q) ->
                      fun e ->
                          { Expression.Kind = Match(e, Option.isSome isUnique, matchOption, q)
                            Pos = e.Pos }
              )
              // 8.16 <member predicate> ::= [ NOT ] MEMBER [ OF ] <multiset value expression>
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "MEMBER" .>> opt (pKeyword "OF") .>>. pExpr
                  |>> fun (isNot, multiset) ->
                      fun e ->
                          { Expression.Kind = MemberOf(e, Option.isSome isNot, multiset)
                            Pos = e.Pos }
              )
              // 8.17 <submultiset predicate> ::= [ NOT ] SUBMULTISET [ OF ] <multiset value expression>
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "SUBMULTISET" .>> opt (pKeyword "OF")
                  .>>. pExpr
                  |>> fun (isNot, multiset) ->
                      fun e ->
                          { Expression.Kind = SubmultisetOf(e, Option.isSome isNot, multiset)
                            Pos = e.Pos }
              )
              // 8.18 <set predicate> ::= IS [ NOT ] A SET
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "A" .>> pKeyword "SET"
                  |>> fun isNot ->
                      fun e ->
                          { Expression.Kind = IsSet(e, Option.isSome isNot)
                            Pos = e.Pos }
              )
              // 8.20 <period predicate> ::= <period predicate operator> <period predicand>
              attempt (
                  pPeriodPredicateOperator .>>. (attempt pPeriodPredicand <|> pExpr)
                  |>> fun (kind, right) ->
                      fun left ->
                          { Expression.Kind = PeriodPredicate(kind, left, right)
                            Pos = left.Pos }
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

    // 8.23 <JSON exists error behavior> ::= TRUE | FALSE | UNKNOWN | ERROR
    let pJsonExistsErrorBehavior =
        choice
            [ pKeyword "TRUE" >>% JsonExistsTrue
              pKeyword "FALSE" >>% JsonExistsFalse
              pKeyword "UNKNOWN" >>% JsonExistsUnknown
              pKeyword "ERROR" >>% JsonExistsError ]

    // 8.23 <JSON exists predicate> ::= JSON_EXISTS ( <JSON API common syntax>
    //     [ <JSON exists error behavior> ON ERROR ] )
    let pJsonExistsPredicate =
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
    let pPredicatePrimary =
        // 8.9 <quantifier> ::= ALL | SOME | ANY
        let pQuantifier =
            choice
                [ pKeyword "ANY" >>% Quantifier.Any
                  pKeyword "SOME" >>% Quantifier.SomeQuantifier
                  pKeyword "ALL" >>% Quantifier.All ]

        // Only valid as the right operand of a comparison operator (see comparisonOp in ExpressionParser).
        // 8.9 <quantified comparison predicate> — the ANY | SOME | ALL subquery term
        let pQuantifiedSubqueryTerm =
            pQuantifier .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
            |>> fun (quant, q) -> QuantifiedSubquery(quant, q)
            |> withExprPosition

        choice
            [ attempt pQuantifiedSubqueryTerm
              attempt pExistsPredicate
              attempt pUniquePredicate
              attempt pJsonExistsPredicate
              attempt pPeriodPredicand ]
