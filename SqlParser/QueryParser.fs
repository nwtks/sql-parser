namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module QueryParser =
    // 7.8 <row pattern measures> ::= MEASURES <row pattern measure list>
    let private pRowPatternMeasures =
        // 7.8 <row pattern measure definition> ::= <row pattern measure expression> AS <measure name>
        let pRowPatternMeasure =
            pExpression .>> pKeyword "AS" .>>. pIdentifierExpression
            |>> fun (expr, name) ->
                { RowPatternMeasure.Expression = expr
                  Name = name }

        pKeyword "MEASURES" >>. sepBy1 pRowPatternMeasure (token (pstring ","))

    // 7.9 <row pattern> — forward ref (recursive)
    let private pRowPattern, private pRowPatternRef =
        createParserForwardedToRef<RowPattern, unit> ()

    // 7.9 <row pattern term> ::= <row pattern factor> | <row pattern term> <row pattern factor>
    let private pRowPatternTerm =
        // 7.9 <row pattern quantifier>
        let pRowPatternQuantifier =
            choice
                [ attempt (
                      token (pstring "*") >>. opt (token (pstring "?"))
                      |>> fun q -> RowPatternQuantifier.Star(Option.isSome q)
                  )
                  attempt (
                      token (pstring "+") >>. opt (token (pstring "?"))
                      |>> fun q -> RowPatternQuantifier.Plus(Option.isSome q)
                  )
                  attempt (
                      token (pstring "?") >>. opt (token (pstring "?"))
                      |>> fun q -> RowPatternQuantifier.Question(Option.isSome q)
                  )
                  attempt (
                      between
                          (token pLeftBrace)
                          (token pRightBrace)
                          (opt pUnsignedIntegerExpr .>> token (pstring ",") .>>. opt pUnsignedIntegerExpr)
                      .>>. opt (token (pstring "?"))
                      |>> fun ((lo, hi), q) -> RowPatternQuantifier.Brace(lo, hi, Option.isSome q)
                  )
                  attempt (
                      between (token pLeftBrace) (token pRightBrace) pUnsignedIntegerExpr
                      |>> RowPatternQuantifier.BraceExact
                  ) ]

        // 7.9 <row pattern primary>
        let pRowPatternPrimary =
            choice
                [ attempt (
                      token pLeftBraceMinus >>. pRowPattern .>> token pRightMinusBrace
                      |>> RowPatternExclude
                  )
                  attempt (token pCircumflex >>% RowPatternAnchorStart)
                  attempt (token pDollarSign >>% RowPatternAnchorEnd)
                  attempt (
                      pKeyword "PERMUTE"
                      >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pRowPattern (token (pstring ",")))
                      |>> RowPatternPermute
                  )
                  attempt (
                      between (token (pstring "(")) (token (pstring ")")) (opt pRowPattern)
                      |>> RowPatternGroup
                  )
                  pIdentifierExpression |>> RowPatternVariable ]

        // 7.9 <row pattern factor> ::= <row pattern primary> [ <row pattern quantifier> ]
        let pRowPatternFactor =
            pRowPatternPrimary .>>. opt (attempt pRowPatternQuantifier)
            |>> fun (primary, quant) ->
                { Primary = primary
                  Quantifier = quant }

        many1 pRowPatternFactor |>> fun factors -> { Factors = factors }

    // 7.9 <row pattern> ::= <row pattern term> | <row pattern alternation>
    pRowPatternRef.Value <- sepBy1 pRowPatternTerm (token pVerticalBar) |>> fun terms -> { Terms = terms }

    // 7.9 <row pattern common syntax> ::= [ AFTER MATCH <skip to> ] [ INITIAL | SEEK ]
    //     PATTERN ( <row pattern> ) [ <subset clause> ] DEFINE <definition list>
    let private pRowPatternCommon =
        // 7.9 <row pattern skip to>
        let pRowPatternSkipTo =
            pKeyword "SKIP"
            >>. pKeyword "TO"
            >>. choice
                    [ attempt (pKeyword "NEXT" >>. pKeyword "ROW" >>% SkipToNextRow)
                      attempt (pKeyword "PAST" >>. pKeyword "LAST" >>. pKeyword "ROW" >>% SkipPastLastRow)
                      attempt (pKeyword "FIRST" >>. pIdentifierExpression |>> SkipToFirst)
                      attempt (pKeyword "LAST" >>. pIdentifierExpression |>> SkipToLast)
                      pIdentifierExpression |>> SkipTo ]

        // 7.9 <row pattern subset item> ::= <var> = ( <var> [ , <var> ]... )
        let pRowPatternSubset =
            pIdentifierExpression .>> token (pstring "=")
            .>>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 pIdentifierExpression (token (pstring ",")))
            |>> fun (name, vars) -> { Name = name; Variables = vars }

        // 7.9 <row pattern definition> ::= <var> AS <search condition>
        let pRowPatternDefinition =
            pIdentifierExpression .>> pKeyword "AS" .>>. pExpression
            |>> fun (name, cond) -> { Name = name; Condition = cond }

        opt (attempt (pKeyword "AFTER" >>. pKeyword "MATCH" >>. pRowPatternSkipTo))
        .>>. opt (attempt (pKeyword "INITIAL" >>% true <|> (pKeyword "SEEK" >>% false)))
        .>> pKeyword "PATTERN"
        .>>. between (token (pstring "(")) (token (pstring ")")) pRowPattern
        .>>. opt (attempt (pKeyword "SUBSET" >>. sepBy1 pRowPatternSubset (token (pstring ","))))
        .>> pKeyword "DEFINE"
        .>>. sepBy1 pRowPatternDefinition (token (pstring ","))
        |>> fun ((((after, ios), pattern), subset), define) ->
            { AfterMatch = after
              InitialOrSeek = ios
              Pattern = pattern
              Subset = Option.defaultValue [] subset
              Define = define }

    // 7.15 <window frame clause> — consumed by the 6.10 <window name or specification>
    // below (define-before-use): F#'s `let` bindings are not recursive, so the 6.10 body
    // references this binding directly. Its optional leading <row pattern measures> and
    // trailing <row pattern common syntax> (7.8/7.9) are defined immediately above.
    // 7.15 <window frame clause> ::= [ <row pattern measures> ] <window frame units> <window frame extent> [ <window frame exclusion> ] [ <row pattern common syntax> ]
    let private pWindowFrameClause =
        // 7.15 <window frame units> ::= ROWS | RANGE | GROUPS
        let pUnit =
            pKeyword "ROWS" >>% Rows
            <|> (pKeyword "RANGE" >>% Range)
            <|> (pKeyword "GROUPS" >>% Groups)

        // 7.15 <window frame bound> ::= UNBOUNDED PRECEDING | UNBOUNDED FOLLOWING | CURRENT ROW | <unsigned value specification> PRECEDING | <unsigned value specification> FOLLOWING
        // <unsigned value specification> = <unsigned literal> | <general value specification>; pValueSpecification models this exactly.
        let pBound =
            choice
                [ attempt (pKeyword "UNBOUNDED" >>. pKeyword "PRECEDING" >>% UnboundedPreceding)
                  attempt (pKeyword "UNBOUNDED" >>. pKeyword "FOLLOWING" >>% UnboundedFollowing)
                  attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% CurrentRow)
                  attempt (pValueSpecification .>> pKeyword "PRECEDING" |>> Preceding)
                  attempt (pValueSpecification .>> pKeyword "FOLLOWING" |>> Following) ]

        // 7.15 <window frame start> ::= UNBOUNDED PRECEDING | <window frame preceding> | CURRENT ROW
        // — a frame that is not a BETWEEN (and the first bound of one) cannot start at FOLLOWING.
        let pBoundStart =
            choice
                [ attempt (pKeyword "UNBOUNDED" >>. pKeyword "PRECEDING" >>% UnboundedPreceding)
                  attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% CurrentRow)
                  attempt (pValueSpecification .>> pKeyword "PRECEDING" |>> Preceding) ]

        // 7.15 <window frame bound 1> ::= <window frame start> | UNBOUNDED FOLLOWING
        let pBound1 =
            pBoundStart
            <|> (attempt (pKeyword "UNBOUNDED" >>. pKeyword "FOLLOWING") >>% UnboundedFollowing)

        // 7.15 <window frame exclusion> ::= EXCLUDE CURRENT ROW | EXCLUDE GROUP | EXCLUDE TIES | EXCLUDE NO OTHERS
        let pExclusion =
            pKeyword "EXCLUDE"
            >>. choice
                    [ attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% ExcludeCurrentRow)
                      attempt (pKeyword "GROUP" >>% ExcludeGroup)
                      attempt (pKeyword "TIES" >>% ExcludeTies)
                      attempt (pKeyword "NO" >>. pKeyword "OTHERS" >>% ExcludeNoOthers) ]

        // 7.15 <window frame clause> ::= [ <row pattern measures> ] <units> <extent>
        //     [ <exclusion> ] [ <row pattern common syntax> ]
        opt (attempt pRowPatternMeasures)
        .>>. pUnit
        .>>. choice
            [ attempt (
                  pKeyword "BETWEEN" >>. pBound1 .>> pKeyword "AND" .>>. pBound
                  |>> fun (s, e) -> s, Some e
              )
              pBoundStart |>> fun s -> s, None ]
        .>>. opt pExclusion
        .>>. opt (attempt pRowPatternCommon)
        |>> fun ((((measures, unit), (start, endBound)), exclusion), rowPattern) ->
            { Unit = unit
              Start = start
              End = endBound
              Exclusion = exclusion
              Measures = measures
              RowPattern = rowPattern }

    // 10.10 <sort specification> — the 6.10 body below uses it directly; the 10.4 / 10.9 /
    // 10.11 bodies in ExpressionParser.fs (compiled before this module) reach it through the
    // forward refs wired below.
    // 10.10 <sort specification> ::= <sort key> [ <ordering specification> ] [ <null ordering> ]
    // 10.4 <ordering specification> ::= ASC | DESC — 10.10 <null ordering> ::= NULLS FIRST | NULLS LAST
    let pSortSpecification =
        let pNullsOrder =
            pKeyword "NULLS"
            >>. (pKeyword "FIRST" >>% NullsFirst <|> (pKeyword "LAST" >>% NullsLast))

        pNonBooleanValueExpression
        .>>. opt (attempt (pKeyword "ASC" >>% true) <|> attempt (pKeyword "DESC" >>% false))
        .>>. opt (attempt pNullsOrder)
        |>> fun ((expr, asc), nulls) -> expr, Option.defaultValue true asc, nulls

    pSortSpecificationRef.Value <- pSortSpecification

    // — the OVER (...) clause attached to a window function (also parsed at the query level for the WINDOW clause).
    // 6.10 <window name or specification> ::= <window name> | <window specification>
    let private pWindowNameOrSpecification =
        let pPartitionBy =
            // 7.15 <window partition column reference> ::= <column reference> [ <collate clause> ]
            pKeyword "PARTITION"
            >>. pKeyword "BY"
            >>. sepBy1 pColumnReferenceWithCollate (token (pstring ","))

        let pOrderBy =
            pKeyword "ORDER"
            >>. pKeyword "BY"
            >>. sepBy1 pSortSpecification (token (pstring ","))

        // 7.15 — an <existing window name> cannot be MEASURES when MEASURES starts a
        // <row pattern measures> clause (the optional leading clause of the
        // <window frame clause>). Reject MEASURES followed by <expr> AS <name> so the
        // frame's MEASURES clause is not swallowed as a window name. The whole parser
        // is wrapped in attempt because notFollowedBy marks its failure as fatal, which
        // would otherwise escape the surrounding opt.
        let pExistingWindowName =
            attempt (
                pIdentifierExpression
                >>= fun name ->
                    match name.Kind with
                    | Identifier "MEASURES" -> notFollowedBy (pExpression .>> pKeyword "AS") >>% name
                    | _ -> preturn name
            )

        pKeyword "OVER"
        >>. (between
                 (token (pstring "("))
                 (token (pstring ")"))
                 (opt pExistingWindowName
                  .>>. opt pPartitionBy
                  .>>. opt pOrderBy
                  .>>. opt pWindowFrameClause
                  |>> fun (((name, pb), ob), frame) ->
                      { ExistingWindowName = name
                        PartitionBy = Option.defaultValue [] pb
                        OrderBy = Option.defaultValue [] ob
                        Frame = frame })
             <|> (pIdentifierExpression
                  |>> fun name ->
                      { ExistingWindowName = Some name
                        PartitionBy = []
                        OrderBy = []
                        Frame = None }))

    pWindowNameOrSpecificationRef.Value <- pWindowNameOrSpecification

    // 7.3 <table value constructor> ::= VALUES <row value expression>
    //     [ { <comma> <row value expression> }... ]
    //   Used both as a <simple table> (7.17) and as a <derived table> inside
    //   <table primary> (7.6).
    let private pTableValueConstructor =
        let pRow =
            choice
                [ attempt (
                      pKeyword "ROW"
                      >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
                  )
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ","))) ]

        pKeyword "VALUES" >>. sepBy1 pRow (token (pstring ","))

    // 7.6 <correlation name> ::= [ AS ] <identifier>   (a.k.a. table alias)
    let private pCorrelationName =
        attempt (pKeyword "AS") >>. pIdentifierExpression <|> pIdentifierExpression

    // 7.6 <correlation or recognition> ::= [ AS ] <correlation name> [ ( <derived column list> ) ]
    let private pCorrelationOrRecognition =
        pCorrelationName
        .>>. opt (
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpression (token (pstring ",")))
        )

    // 7.6 <table reference> ::= <table factor> | <joined table>
    // Forward reference so <table primary> can nest a parenthesized <joined table>.
    let pTableReference, private pTableReferenceRef =
        createParserForwardedToRef<TableSource, unit> ()

    // 7.6 <data change delta table> / 7.6 <data change statement> — forward ref
    // Forward reference to the DML statement parsers (defined in DataManipulationParser.fs,
    // which is compiled after this module). Used by <data change delta table> (7.6):
    //   FINAL|NEW|OLD TABLE ( <data change statement> )
    let private pDataChangeStatement, pDataChangeStatementRef =
        createParserForwardedToRef<StatementKind, unit> ()

    // 7.11 <JSON table columns clause> (recursive — nested columns contain one)
    let private pJsonTableColumnsClause, private pJsonTableColumnsClauseRef =
        createParserForwardedToRef<JsonTableColumn list, unit> ()

    let private pJsonTableColumnsClauseBody allowNested =
        // 7.11 <JSON table column empty behavior> ::= ERROR | NULL | DEFAULT <value expression>
        // 7.11 <JSON table column error behavior> ::= ERROR | NULL | DEFAULT <value expression>
        //     (formatted columns additionally allow EMPTY ARRAY | EMPTY OBJECT)
        let pJsonTableColumnEmptyErrorBehavior =
            choice
                [ pKeyword "ERROR" >>% JsonColumnError
                  pKeyword "NULL" >>% JsonColumnNull
                  pKeyword "DEFAULT" >>. pExpression |>> JsonColumnDefault
                  pKeyword "EMPTY" >>. pKeyword "ARRAY" >>% JsonColumnEmptyArray
                  pKeyword "EMPTY" >>. pKeyword "OBJECT" >>% JsonColumnEmptyObject ]

        // 7.11 <JSON table column definition>
        let pJsonTableColumnDefinition =
            choice
                [ // <JSON table nested columns definition> must precede the regular/formatted
                  // column branch: <data type> accepts any identifier as a user-defined type,
                  // so "NESTED PATH '$.items' ..." would otherwise be misread as a regular
                  // column named NESTED of user-defined type PATH.
                  attempt (
                      pKeyword "NESTED" >>. opt (pKeyword "PATH" >>% ()) >>. pCharacterStringLiteral
                      .>>. opt (attempt (pKeyword "AS" >>. pIdentifierExpression))
                      .>>. pJsonTableColumnsClause
                      |>> fun ((path, name), cols) ->
                          JsonNested
                              { Path = path
                                Name = name
                                Columns = cols }
                  )
                  attempt (
                      pIdentifierExpression .>> pKeyword "FOR" .>> pKeyword "ORDINALITY"
                      |>> JsonOrdinality
                  )
                  attempt (
                      pIdentifierExpression .>> pKeyword "FOR" .>> pKeyword "CHAINING"
                      |>> JsonChaining
                  )
                  attempt (
                      pIdentifierExpression .>>. pDataType
                      >>= fun (name, dt) ->
                          opt (attempt (pKeyword "FORMAT" >>. pJsonRepresentation))
                          >>= fun fmt ->
                              opt (attempt (pKeyword "PATH" >>. pCharacterStringLiteral))
                              >>= fun path ->
                                  // <JSON query wrapper behavior> ::= WITHOUT [ ARRAY ] WRAPPER
                                  //   | WITH [ UNCONDITIONAL | CONDITIONAL ] [ ARRAY ] WRAPPER
                                  // The behavior comes first; WRAPPER closes it.
                                  opt (attempt (pJsonQueryWrapper .>> pKeyword "WRAPPER"))
                                  >>= fun wrapper ->
                                      opt (
                                          attempt (
                                              pKeyword "QUOTES" >>. pJsonQueryQuotes
                                              .>> opt (pKeyword "ON" .>> pKeyword "SCALAR" .>> pKeyword "STRING")
                                          )
                                      )
                                      >>= fun quotes ->
                                          opt (
                                              attempt (
                                                  pJsonTableColumnEmptyErrorBehavior
                                                  .>> pKeyword "ON"
                                                  .>> pKeyword "EMPTY"
                                              )
                                          )
                                          >>= fun onEmpty ->
                                              opt (
                                                  attempt (
                                                      pJsonTableColumnEmptyErrorBehavior
                                                      .>> pKeyword "ON"
                                                      .>> pKeyword "ERROR"
                                                  )
                                              )
                                              >>= fun onError ->
                                                  // 7.11: WRAPPER / QUOTES belong only to a formatted column, and
                                                  // EMPTY ARRAY / EMPTY OBJECT only to a formatted column's empty/error
                                                  // behavior; DEFAULT only to a regular column's.
                                                  let hasFormattedOnlyBehavior =
                                                      onEmpty = Some JsonColumnEmptyArray
                                                      || onEmpty = Some JsonColumnEmptyObject
                                                      || onError = Some JsonColumnEmptyArray
                                                      || onError = Some JsonColumnEmptyObject

                                                  let isDefault =
                                                      function
                                                      | Some(JsonColumnDefault _) -> true
                                                      | _ -> false

                                                  if
                                                      fmt.IsNone
                                                      && (Option.isSome wrapper
                                                          || Option.isSome quotes
                                                          || hasFormattedOnlyBehavior)
                                                  then
                                                      fail
                                                          "WRAPPER, QUOTES and EMPTY ARRAY/OBJECT require FORMAT JSON (7.11)."
                                                  elif fmt.IsSome && (isDefault onEmpty || isDefault onError) then
                                                      fail "DEFAULT is not a formatted column behavior (7.11)."
                                                  else
                                                      match fmt with
                                                      | Some f ->
                                                          preturn (
                                                              JsonFormatted
                                                                  { Name = name
                                                                    DataType = dt
                                                                    Format = f
                                                                    Path = path
                                                                    Wrapper = wrapper
                                                                    Quotes = quotes
                                                                    OnEmpty = onEmpty
                                                                    OnError = onError }
                                                          )
                                                      | None ->
                                                          preturn (
                                                              JsonRegular
                                                                  { Name = name
                                                                    DataType = dt
                                                                    Path = path
                                                                    OnEmpty = onEmpty
                                                                    OnError = onError }
                                                          )
                  ) ]

        // 7.11 — the two column-definition dialects are not interchangeable: NESTED belongs only to
        // <JSON table column definition> and FOR CHAINING only to <JSON table primitive column definition>.
        let validateColumnKind allowNested cols =
            let hasNested =
                cols
                |> List.exists (function
                    | JsonNested _ -> true
                    | _ -> false)

            let hasChaining =
                cols
                |> List.exists (function
                    | JsonChaining _ -> true
                    | _ -> false)

            if allowNested && hasChaining then
                fail "FOR CHAINING is only valid in a JSON_TABLE_PRIMITIVE column list (7.11)."
            elif not allowNested && hasNested then
                fail "NESTED is only valid in a JSON_TABLE column list (7.11)."
            else
                preturn cols

        pKeyword "COLUMNS"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 pJsonTableColumnDefinition (token (pstring ",")))
        >>= validateColumnKind allowNested

    pJsonTableColumnsClauseRef.Value <- pJsonTableColumnsClauseBody true

    // 7.11 <JSON table primitive columns clause> for a JSON_TABLE_PRIMITIVE, whose column
    // definitions have no <JSON table nested columns> alternative (7.11). No forward ref:
    // its only consumer is defined after this point.
    let private pJsonTablePrimitiveColumnsClause = pJsonTableColumnsClauseBody false

    // 7.11 <JSON table plan>
    let private pJsonTablePlan, private pJsonTablePlanRef =
        createParserForwardedToRef<JsonTablePlan, unit> ()

    // 7.11 <JSON table plan primary> ::= <path name> | ( <plan> )
    // No forward ref: nothing refers to the plan primary before this definition, while the
    // plan ref above is still needed (the plan body below refers back to the plan primary).
    let private pJsonTablePlanPrimary =
        choice
            [ attempt (
                  between (token (pstring "(")) (token (pstring ")")) pJsonTablePlan
                  |>> JsonPlanPrimaryGroup
              )
              pIdentifierExpression |>> JsonPlanPrimaryName ]

    pJsonTablePlanRef.Value <-
        choice
            [ attempt (
                  pIdentifierExpression .>> pKeyword "OUTER" .>>. pJsonTablePlanPrimary
                  |>> fun (n, p) -> JsonPlanOuter(n, p)
              )
              attempt (
                  pIdentifierExpression .>> pKeyword "INNER" .>>. pJsonTablePlanPrimary
                  |>> fun (n, p) -> JsonPlanInner(n, p)
              )
              // 7.11 <JSON table plan union> ::= <plan primary> UNION <plan primary>
              //     [ { UNION <plan primary> }... ] — at least TWO operands.
              attempt (
                  pJsonTablePlanPrimary .>>. many1 (pKeyword "UNION" >>. pJsonTablePlanPrimary)
                  |>> fun (h, t) -> JsonPlanUnion(h :: t)
              )
              attempt (
                  pJsonTablePlanPrimary .>>. many1 (pKeyword "CROSS" >>. pJsonTablePlanPrimary)
                  |>> fun (h, t) -> JsonPlanCross(h :: t)
              )
              pIdentifierExpression |>> JsonPlanName ]

    // Helper for the <table primary> alternatives below: attaches the source position
    // to the TableSource being built.
    let private withTablePosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { TableSource.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 7.6 <table primary> ::= [ ONLY ] [ <table or query name> | <derived table> | <lateral derived table>
    //     | <collection derived table> | <table function derived table> | <data change delta table>
    //     | <JSON table> | <JSON table primitive> ] — without the optional <sample clause>
    //     (applied in the <table factor> rule below).
    let private pTablePrimary =
        // 7.6 <sample method> ::= BERNOULLI | SYSTEM
        let pSampleMethod =
            pKeyword "BERNOULLI" >>% "BERNOULLI" <|> (pKeyword "SYSTEM" >>% "SYSTEM")

        // 7.6 <repeatable clause> ::= REPEATABLE ( <repeat argument> )
        // <repeat argument> ::= <numeric value expression>
        let pRepeatableClause =
            pKeyword "REPEATABLE"
            >>. between (token (pstring "(")) (token (pstring ")")) pNumericValueExpression

        // 7.6 <sample clause> ::= TABLESAMPLE <sample method>
        //     ( <sample percentage> ) [ <repeatable clause> ]
        // <sample percentage> ::= <numeric value expression>
        let pSampleClause =
            pKeyword "TABLESAMPLE" >>. pSampleMethod
            .>>. between (token (pstring "(")) (token (pstring ")")) pNumericValueExpression
            .>>. opt pRepeatableClause
            |>> fun ((method, percent), repeat) -> method, percent, repeat

        // 7.6 <query system time period specification> ::=
        //     FOR SYSTEM_TIME AS OF <point in time>
        //   | FOR SYSTEM_TIME BETWEEN [ ASYMMETRIC | SYMMETRIC ] <p1> AND <p2>
        //   | FOR SYSTEM_TIME FROM <p1> TO <p2>
        // <point in time> is a <datetime value expression> (6.35), whose '+'/'-' chain has no
        // boolean operators, so BETWEEN's AND is not consumed as a boolean operator.
        let pQuerySystemTimePeriodSpecification =
            let pPointInTime = pDatetimeValueExpression

            pKeyword "FOR"
            >>. pKeyword "SYSTEM_TIME"
            >>. (attempt (pKeyword "AS" >>. pKeyword "OF" >>. pPointInTime |>> SystemTimeSpec.AsOf)
                 <|> attempt (
                     pKeyword "BETWEEN"
                     >>. opt (
                         pKeyword "ASYMMETRIC" >>% SystemTimeSymmetry.Asymmetric
                         <|> (pKeyword "SYMMETRIC" >>% SystemTimeSymmetry.Symmetric)
                     )
                     .>>. pPointInTime
                     .>> pKeyword "AND"
                     .>>. pPointInTime
                     |>> fun ((symmetry, lo), hi) -> SystemTimeSpec.Between(lo, hi, symmetry)
                 )
                 <|> (pKeyword "FROM" >>. pPointInTime .>> pKeyword "TO" .>>. pPointInTime
                      |>> fun (lo, hi) -> SystemTimeSpec.FromTo(lo, hi)))

        // 7.7 <row pattern empty match handling>
        let pRowPatternEmptyMatchHandling =
            choice
                [ attempt (pKeyword "SHOW" >>. pKeyword "EMPTY" >>. pKeyword "MATCHES" >>% ShowEmptyMatches)
                  attempt (pKeyword "OMIT" >>. pKeyword "EMPTY" >>. pKeyword "MATCHES" >>% OmitEmptyMatches)
                  pKeyword "WITH" >>. pKeyword "UNMATCHED" >>. pKeyword "ROWS"
                  >>% WithUnmatchedRows ]

        // 7.7 <row pattern rows per match>
        let pRowPatternRowsPerMatch =
            choice
                [ attempt (
                      pKeyword "ONE" >>. pKeyword "ROW" >>. pKeyword "PER" >>. pKeyword "MATCH"
                      >>% OneRowPerMatch
                  )
                  attempt (
                      pKeyword "ALL" >>. pKeyword "ROWS" >>. pKeyword "PER" >>. pKeyword "MATCH"
                      .>>. opt (attempt pRowPatternEmptyMatchHandling)
                      |>> fun (_, h) -> AllRowsPerMatch h
                  ) ]

        // 7.7 <row pattern recognition clause> ::= MATCH_RECOGNIZE ( [ <partition by> ] [ <order by> ] [ <measures> ] [ <rows per match> ] <common syntax> )
        let pRowPatternRecognitionClause =
            pKeyword "MATCH_RECOGNIZE"
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (opt (
                        attempt (
                            pKeyword "PARTITION"
                            >>. pKeyword "BY"
                            >>. sepBy1 pColumnReferenceWithCollate (token (pstring ","))
                        )
                     )
                     .>>. opt (
                         attempt (
                             pKeyword "ORDER"
                             >>. pKeyword "BY"
                             >>. sepBy1 pSortSpecification (token (pstring ","))
                         )
                     )
                     .>>. opt (attempt pRowPatternMeasures)
                     .>>. opt (attempt pRowPatternRowsPerMatch)
                     .>>. pRowPatternCommon
                     |>> fun ((((pb, ob), measures), rpm), common) ->
                         { PartitionBy = Option.defaultValue [] pb
                           OrderBy = Option.defaultValue [] ob
                           Measures = Option.defaultValue [] measures
                           RowsPerMatch = rpm
                           Common = common })

        // 7.11 <JSON table default plan choices>
        let pJsonTableDefaultPlanChoices =
            choice
                [ attempt (
                      pKeyword "INNER" >>% "INNER" <|> (pKeyword "OUTER" >>% "OUTER")
                      .>>. opt (
                          attempt (
                              token (pstring ",")
                              >>. (pKeyword "UNION" >>% "UNION" <|> (pKeyword "CROSS" >>% "CROSS"))
                          )
                      )
                      |>> fun (io, uc) ->
                          { InnerOuter = Some io
                            UnionCross = uc }
                  )
                  attempt (
                      pKeyword "UNION" >>% "UNION" <|> (pKeyword "CROSS" >>% "CROSS")
                      .>>. opt (
                          attempt (
                              token (pstring ",")
                              >>. (pKeyword "INNER" >>% "INNER" <|> (pKeyword "OUTER" >>% "OUTER"))
                          )
                      )
                      |>> fun (uc, io) ->
                          { InnerOuter = io
                            UnionCross = Some uc }
                  ) ]

        // 7.11 <JSON table plan clause> ::= PLAN ( <plan> ) | PLAN DEFAULT ( <choices> )
        let pJsonTablePlanClause =
            pKeyword "PLAN"
            >>. choice
                    [ attempt (
                          pKeyword "DEFAULT"
                          >>. between (token (pstring "(")) (token (pstring ")")) pJsonTableDefaultPlanChoices
                          |>> JsonPlanDefault
                      )
                      between (token (pstring "(")) (token (pstring ")")) pJsonTablePlan ]

        // 7.11 <JSON table error behavior> ::= ERROR | EMPTY
        let pJsonTableErrorBehavior =
            choice [ pKeyword "ERROR" >>% JsonTableError; pKeyword "EMPTY" >>% JsonTableEmpty ]

        // 7.11 <JSON table> ::= JSON_TABLE ( <JSON API common syntax> <columns clause>
        //     [ <plan clause> ] [ <error behavior> ON ERROR ] )
        let pJsonTable =
            pKeyword "JSON_TABLE"
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (pJsonApiCommonSyntax
                     .>>. pJsonTableColumnsClause
                     .>>. opt (attempt pJsonTablePlanClause)
                     .>>. opt (attempt (pJsonTableErrorBehavior .>> pKeyword "ON" .>> pKeyword "ERROR"))
                     |>> fun (((common, cols), plan), onError) ->
                         { Common = common
                           Columns = cols
                           Plan = plan
                           OnError = onError })

        // 7.11 <JSON table primitive> ::= JSON_TABLE_PRIMITIVE ( <JSON API common syntax>
        //     <columns clause> <error behavior> ON ERROR )
        let pJsonTablePrimitive =
            pKeyword "JSON_TABLE_PRIMITIVE"
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (pJsonApiCommonSyntax
                     .>>. pJsonTablePrimitiveColumnsClause
                     .>>. (pJsonTableErrorBehavior .>> pKeyword "ON" .>> pKeyword "ERROR")
                     |>> fun ((common, cols), onError) ->
                         { Common = common
                           Columns = cols
                           Plan = None
                           OnError = Some onError })

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
                  // 7.6 <parenthesized joined table> ::= ( <parenthesized joined table> )
                  //     | ( <joined table> ) — only JOINED tables may be parenthesized.
                  attempt (
                      between (token (pstring "(")) (token (pstring ")")) pTableReference
                      >>= fun joined ->
                          match joined.Kind with
                          | TableSourceKind.JoinedTable _ -> preturn joined
                          | _ -> fail "<parenthesized joined table> requires a joined table"
                  )
                  // <only spec> ::= ONLY ( <table or query name> ) [ <correlation or recognition> ]
                  attempt (
                      pKeyword "ONLY"
                      >>. between (token (pstring "(")) (token (pstring ")")) pSchemaQualifiedNameExpression
                      .>>. opt (attempt pCorrelationOrRecognition)
                      |>> fun (name, corr) ->
                          let alias, cols =
                              match corr with
                              | Some(name, cols) -> Some name, cols
                              | None -> None, None

                          Only(name, alias, cols)
                  )
                  |> withTablePosition
                  // 7.6 <table function derived table>
                  // 7.6 <PTF derived table> ::= TABLE ( <expr> )
                  // A collection-derived table requires a correlation name; the PTF
                  // form follows its own production and may omit one.
                  attempt (
                      pKeyword "TABLE"
                      >>. between (token (pstring "(")) (token (pstring ")")) pExpression
                      .>>. opt (attempt pCorrelationOrRecognition)
                      >>= fun (expr, corr) ->
                          let alias, cols =
                              match corr with
                              | Some(name, cols) -> Some name, cols
                              | None -> None, None

                          match expr.Kind with
                          | FunctionCall _ -> preturn (PtfTable(expr, alias, cols))
                          | _ when Option.isNone corr -> fail "a table function requires a correlation name"
                          | _ -> preturn (TableFunction(expr, alias, cols))
                  )
                  |> withTablePosition
                  // <data change delta table> ::= <result option> TABLE ( <data change statement> )
                  attempt (
                      pKeyword "FINAL" >>% ResultOption.Final
                      <|> (pKeyword "NEW" >>% ResultOption.New)
                      <|> (pKeyword "OLD" >>% ResultOption.Old)
                      .>> pKeyword "TABLE"
                      .>>. between (token (pstring "(")) (token (pstring ")")) pDataChangeStatement
                      .>>. pCorrelationOrRecognition
                      |>> fun ((result, stmt), (name, cols)) -> DataChangeDelta(result, stmt, Some name, cols)
                  )
                  |> withTablePosition
                  // <JSON table> <correlation or recognition> — the correlation is MANDATORY.
                  attempt (
                      pJsonTable .>>. pCorrelationOrRecognition
                      |>> fun (stmt, corr) -> JsonTable(stmt, Some corr)
                  )
                  |> withTablePosition
                  // <JSON table primitive> <correlation name> — the correlation name is MANDATORY.
                  attempt (
                      pJsonTablePrimitive .>>. pCorrelationName
                      |>> fun (stmt, name) -> JsonTablePrimitive(stmt, Some name)
                  )
                  |> withTablePosition
                  // <table or query name> <row pattern recognition clause and name>
                  //   ::= [ [ AS ] <input name> [ ( <input cols> ) ] ] MATCH_RECOGNIZE ( ... )
                  //       [ [ AS ] <output name> [ ( <output cols> ) ] ]
                  // Must precede the plain <table or query name> branch below so that
                  // "t MATCH_RECOGNIZE(...)" is not consumed as just "t".
                  // <row pattern recognition clause and name> ::=
                  //   [ [ AS ] <row pattern input name> [ <input derived column list> ] ]
                  //       <row pattern recognition clause>
                  //       [ [ AS ] <row pattern output name> [ <output derived column list> ] ]
                  // <row pattern input name> ::= <correlation name> — a plain identifier,
                  // NOT a schema-qualified name; there is exactly ONE optional name group
                  // before the recognition clause.
                  attempt (
                      opt (
                          attempt (
                              opt (pKeyword "AS") >>. pCorrelationName
                              .>>. opt (
                                  between
                                      (token (pstring "("))
                                      (token (pstring ")"))
                                      (sepBy1 pIdentifierExpression (token (pstring ",")))
                              )
                          )
                      )
                      .>>. pRowPatternRecognitionClause
                      .>>. opt (
                          attempt (
                              opt (pKeyword "AS") >>. pCorrelationName
                              .>>. opt (
                                  between
                                      (token (pstring "("))
                                      (token (pstring ")"))
                                      (sepBy1 pIdentifierExpression (token (pstring ",")))
                              )
                          )
                      )
                      |>> fun ((input, recog), output) -> MatchRecognize(input, recog, output)
                  )
                  |> withTablePosition
                  // <table or query name> [ <query system time period specification> ]
                  //     [ <correlation or recognition> ]
                  attempt (
                      getPosition
                      .>>. (pSchemaQualifiedNameExpression
                            .>>. opt (attempt pQuerySystemTimePeriodSpecification)
                            .>>. opt (attempt pCorrelationName))
                      |>> fun (pos, ((name, sysTime), alias)) ->
                          let pos' = { Line = pos.Line; Column = pos.Column }

                          let baseTable =
                              { TableSource.Kind = TableSourceKind.Table(name, alias)
                                Pos = pos' }

                          match sysTime with
                          | Some spec ->
                              { TableSource.Kind = SystemTime(baseTable, spec)
                                Pos = pos' }
                          | None -> baseTable
                  ) ]

        // 7.6 <table factor> ::= <table primary> [ <sample clause> ]
        pBase .>>. opt pSampleClause
        |>> fun (tbl, sample) ->
            match sample with
            | Some(method, percent, repeat) ->
                { TableSource.Kind = TableSample(tbl, method, percent, repeat)
                  Pos = tbl.Pos }
            | None -> tbl

    // 7.10 <joined table> — one suffix folded into a left-associative chain:
    // 7.10 <joined table> ::= <cross join> | <qualified join> | <natural join>
    // NATURAL is mutually exclusive with CROSS JOIN per the grammar (<natural join>
    // uses <join type>, which has no CROSS), so "NATURAL CROSS JOIN" is rejected.
    let private pJoinedTableSuffix =
        // 7.10 <join type> ::= INNER | <outer join type> [ OUTER ]   (no CROSS — see pJoinType)
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

        // 7.10 <cross join> ::= <table reference> CROSS JOIN <table factor>
        let pJoinType =
            choice
                [ attempt (pKeyword "CROSS" .>> pKeyword "JOIN") >>% CrossJoin
                  pJoinTypeWithoutCross ]

        // 7.10 <join specification> ::= <join condition> | <named columns join>
        // 7.10 <named columns join> ::= USING ( <join column list> ) [ AS <join correlation name> ]
        // Returns the condition plus the optional USING join correlation name.
        let pJoinSpecification =
            choice
                [ pKeyword "ON" >>. pExpression |>> fun e -> On e, None
                  pKeyword "USING"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (sepBy1 pIdentifierExpression (token (pstring ",")))
                  .>>. opt (attempt (pKeyword "AS" >>. pIdentifierExpression))
                  |>> fun (cols, alias) -> Using cols, alias ]

        // 7.10 <partitioned join column reference list> ::= ( <column reference> [ { , <column reference> }... ] )
        // <partitioned join column reference> ::= <column reference> — column references only,
        // not arbitrary value expressions.
        let pPartitionedJoinColumnReferenceList =
            pKeyword "PARTITION"
            >>. pKeyword "BY"
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (sepBy1 pColumnReferenceExpression (token (pstring ",")))

        let pNatural = opt (pKeyword "NATURAL" >>% true)

        pNatural
        >>= fun nat ->
            let joinType =
                if Option.isSome nat then
                    pJoinTypeWithoutCross
                else
                    pJoinType

            opt (attempt pPartitionedJoinColumnReferenceList)
            >>= fun partitionBy ->
                joinType .>>. pTablePrimary
                >>= fun (jt, right) ->
                    // 7.10 <qualified join> ::= { <table reference> | <partitioned join table> }
                    //     [ <join type> ] JOIN <table reference> <join specification>
                    // <cross join> and <natural join> have NO <join specification> slot.
                    if jt = CrossJoin || Option.isSome nat then
                        preturn (Option.defaultValue false nat, jt, right, None, None, partitionBy)
                    else
                        pJoinSpecification
                        |>> fun (cond, usingAlias) ->
                            (Option.defaultValue false nat, jt, right, Some cond, usingAlias, partitionBy)

    // 7.6 <table reference> ::= <table factor> | <joined table>
    pTableReferenceRef.Value <-
        pTablePrimary .>>. many pJoinedTableSuffix
        |>> fun (first, rests) ->
            rests
            |> List.fold
                (fun acc (nat, jt, right, cond, usingAlias, partitionBy) ->
                    { Kind =
                        JoinedTable
                            { JoinType = jt
                              IsNatural = nat
                              Left = acc
                              Right = right
                              Condition = cond
                              UsingAlias = usingAlias
                              PartitionBy = partitionBy }
                      Pos = acc.Pos })
                first

    // 7.5 <from clause> ::= FROM <table reference list>
    let pFromClause = pKeyword "FROM" >>. sepBy1 pTableReference (token (pstring ","))

    // 7.12 <where clause> ::= WHERE <search condition>
    let pWhereClause = pKeyword "WHERE" >>. pExpression

    // 7.16 <set quantifier> ::= DISTINCT | ALL
    // Forced inversion: consumed by the 7.13 <group by clause> below.
    let pSetQuantifier =
        opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))

    // 7.13 <grouping element> — forward ref
    let private pGroupingElement, private pGroupingElementRef =
        createParserForwardedToRef<GroupingElement, unit> ()

    // 7.13 <ordinary grouping set> ::= <grouping column reference>
    //                          | ( <grouping column reference list> )
    let private pOrdinaryGroupingSet =
        choice
            [ attempt (
                  between
                      (token (pstring "("))
                      (token (pstring ")"))
                      (sepBy1 pColumnReferenceWithCollate (token (pstring ",")))
                  |>> GroupingSet
              )
              pColumnReferenceWithCollate |>> fun e -> GroupingSet [ e ] ]

    // 7.13 <rollup list> ::= ROLLUP ( <ordinary grouping set list> )
    let private pRollupList =
        pKeyword "ROLLUP"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pOrdinaryGroupingSet (token (pstring ",")))
        |>> Rollup

    // 7.13 <cube list> ::= CUBE ( <ordinary grouping set list> )
    let private pCubeList =
        pKeyword "CUBE"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pOrdinaryGroupingSet (token (pstring ",")))
        |>> Cube

    // 7.13 <grouping sets specification> ::= GROUPING SETS ( <grouping set list> )
    let private pGroupingSetsSpecification =
        pKeyword "GROUPING" .>> pKeyword "SETS"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pGroupingElement (token (pstring ",")))
        |>> GroupingSets

    // 7.13 <empty grouping set> ::= ( )
    let private pEmptyGroupingSet =
        between (token (pstring "(")) (token (pstring ")")) (preturn EmptyGroupingSet)

    // Dispatch order is behaviour, not clause order — GROUPING SETS stays first here even
    // though the definitions above follow the 7.13 production order.
    pGroupingElementRef.Value <-
        choice
            [ attempt pGroupingSetsSpecification
              attempt pRollupList
              attempt pCubeList
              attempt pEmptyGroupingSet
              attempt pOrdinaryGroupingSet ]

    // 7.13 <group by clause> ::= GROUP BY [ <set quantifier> ] <grouping element list>
    let pGroupByClause =
        pKeyword "GROUP" >>. pKeyword "BY" >>. pSetQuantifier
        .>>. sepBy1 pGroupingElement (token (pstring ","))

    // 7.14 <having clause> ::= HAVING <search condition>
    let pHavingClause = pKeyword "HAVING" >>. pExpression

    // 7.15 <window clause> ::= WINDOW <window definition list>
    let pWindowClause =
        // 7.15 <window definition> ::= <new window name> AS <window specification>
        let pWindowDefinition =
            pIdentifierExpression .>> pKeyword "AS"
            .>>. between
                (token (pstring "("))
                (token (pstring ")"))
                (opt pIdentifierExpression
                 .>>. opt (
                     // <window partition clause> ::= PARTITION BY <window partition column reference list>
                     // <window partition column reference> ::= <column reference> [ <collate clause> ]
                     pKeyword "PARTITION"
                     >>. pKeyword "BY"
                     >>. sepBy1 pColumnReferenceWithCollate (token (pstring ","))
                 )
                 .>>. opt (
                     pKeyword "ORDER"
                     >>. pKeyword "BY"
                     >>. sepBy1 pSortSpecification (token (pstring ","))
                 )
                 .>>. opt pWindowFrameClause
                 |>> fun (((name, pb), ob), frame) ->
                     { ExistingWindowName = name
                       PartitionBy = Option.defaultValue [] pb
                       OrderBy = Option.defaultValue [] ob
                       Frame = frame })

        pKeyword "WINDOW" >>. sepBy1 pWindowDefinition (token (pstring ","))

    // 7.16 <select sublist> ::= <derived column> | <qualified asterisk> | <all fields reference>
    // (the bare <asterisk> alternative of <select list> is handled by pSelectList below;
    // the expression parser no longer accepts a bare `*`, so no guard is needed here)
    let pSelectSublist =
        // 7.16 <derived column> ::= <value expression> [ <as clause> ]
        let pDerivedColumn =
            attempt (pExpression .>>. opt (attempt pCorrelationName)) |>> Column

        // 7.16 <all fields reference> ::= <value expression primary> <period> <asterisk>
        //                            [ AS ( <all fields column name list> ) ]
        let pAllFieldsAsClause =
            opt (
                attempt (
                    pKeyword "AS"
                    >>. between
                            (token (pstring "("))
                            (token (pstring ")"))
                            (sepBy1 pIdentifierExpression (token (pstring ",")))
                )
            )

        // 7.16 <qualified asterisk> ::= <asterisked identifier chain> <period> <asterisk>
        //                            | <all fields reference>
        // The <asterisked identifier chain> form is tried first so that a plain `t.*` keeps
        // yielding QualifiedStar; the general <all fields reference> form then handles any other
        // <value expression primary> (e.g. `(a + b).*`) and yields AllFieldsReference.
        let pQualifiedAsterisk =
            attempt (
                getPosition
                .>>. (pIdentifier .>>. many (attempt (token (pstring ".") >>. pIdentifier))
                      .>> token (pstring ".")
                      .>> pchar '*'
                      .>> ws)
                .>>. pAllFieldsAsClause
                .>> ws
                |>> fun ((pos, (first, rest)), fieldList) ->
                    let pos' = { Line = pos.Line; Column = pos.Column }

                    match fieldList with
                    | None ->
                        Column(
                            { Expression.Kind = QualifiedStar(first :: rest)
                              Pos = pos' },
                            None
                        )
                    | Some cols ->
                        let expr =
                            match first :: rest with
                            | [ s ] ->
                                { Expression.Kind = Identifier s
                                  Pos = pos' }
                            | ids ->
                                { Expression.Kind = ColumnReference ids
                                  Pos = pos' }

                        Column(
                            { Expression.Kind = AllFieldsReference(expr, Some cols)
                              Pos = pos' },
                            None
                        )
            )
            <|> attempt (
                getPosition
                .>>. (pValueExpressionPrimary .>> token (pstring ".") .>> pchar '*' .>> ws)
                .>>. pAllFieldsAsClause
                .>> ws
                |>> fun ((pos, expr), cols) ->
                    Column(
                        { Expression.Kind = AllFieldsReference(expr, cols)
                          Pos = { Line = pos.Line; Column = pos.Column } },
                        None
                    )
            )

        pQualifiedAsterisk <|> pDerivedColumn

    // 7.16 <query specification> ::= SELECT [ <set quantifier> ] <select list> <table expression>
    let private pQuerySpecification =
        // 7.16 <select list> ::= <asterisk> | <select sublist> [ { <comma> <select sublist> }... ]
        // A bare <asterisk> is an alternative to the whole sublist list, NOT a sublist
        // itself — "SELECT *, a" is rejected.
        let pSelectList =
            (pstring "*" .>> ws .>>. getPosition
             |>> fun (_, pos) ->
                 [ Column(
                       { Expression.Kind = ExpressionKind.Star
                         Pos = { Line = pos.Line; Column = pos.Column } },
                       None
                   ) ])
            <|> sepBy1 pSelectSublist (token (pstring ","))

        // <query specification> — the SELECT core without ORDER BY/OFFSET/FETCH/LOCKING.
        // Those trailing clauses are parsed at the <query expression> level (7.17) so
        // they apply to the whole query, not just the last SELECT.
        let pSelectBase =
            pipe5
                (pKeyword "SELECT" >>. pSetQuantifier .>>. pSelectList)
                pFromClause
                (opt (attempt pWhereClause))
                (opt (attempt pGroupByClause))
                (opt (attempt pHavingClause))
                (fun (dist, cols) from whr grp hav ->
                    let grpDistinct, grpList =
                        match grp with
                        | Some(d, l) -> Option.defaultValue false d, l
                        | None -> false, []

                    (Option.defaultValue false dist, cols), from, whr, grpDistinct, grpList, hav)

        pipe2 pSelectBase (opt (attempt pWindowClause)) (fun baseResult window ->
            let distInfo, from, whr, grpDistinct, grpList, hav = baseResult
            let dist, colsList = distInfo

            { IsDistinct = dist
              Columns = colsList
              From = from
              Where = whr
              GroupBy = grpList
              GroupByDistinct = grpDistinct
              Having = hav
              Window = Option.defaultValue [] window
              OrderBy = []
              Offset = None
              Fetch = None
              Locking = None })

    // 7.17 <corresponding spec> ::= CORRESPONDING [ BY ( <corresponding column list> ) ]
    let private pCorrespondingSpec =
        pKeyword "CORRESPONDING"
        >>. opt (
            pKeyword "BY"
            >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpression (token (pstring ",")))
        )

    // 7.17 <order by clause> ::= ORDER BY <sort specification list>
    let private pOrderByClause =
        pKeyword "ORDER"
        >>. pKeyword "BY"
        >>. sepBy1 pSortSpecification (token (pstring ","))

    // 7.17 [ <result offset clause> ] [ <fetch first clause> ]
    let private pOffsetFetch =
        // 7.17 <result offset clause> ::= OFFSET <offset row count> { ROW | ROWS }
        // <offset row count> ::= <simple value specification> (strict: literals + host params only)
        let pResultOffsetClause =
            pKeyword "OFFSET" >>. pSimpleValueSpecification
            .>> (attempt (pKeyword "ROWS") <|> pKeyword "ROW")

        // 7.17 <fetch first clause> ::= FETCH { FIRST | NEXT } [ <fetch first quantity> ]
        //     { ROW | ROWS } { ONLY | WITH TIES }
        // (a missing quantity defaults to 1 per the grammar)
        let pFetchFirstClause =
            getPosition
            >>= fun pos ->
                let defaultCount: Expression =
                    { Kind = Literal(Number 1m)
                      Pos = { Line = pos.Line; Column = pos.Column } }

                // <fetch first row count> ::= <simple value specification> (strict)
                // <fetch first percentage> ::= <simple value specification> PERCENT — the
                // quantity is MANDATORY in the percentage form.
                pKeyword "FETCH"
                >>. (pKeyword "FIRST" <|> pKeyword "NEXT")
                >>. ((pSimpleValueSpecification .>>. opt (pKeyword "PERCENT" >>% true)
                      |>> fun (count, isPercent) -> Some count, Option.defaultValue false isPercent)
                     <|> preturn (None, false))
                .>> (attempt (pKeyword "ROWS") <|> pKeyword "ROW")
                .>>. (pKeyword "ONLY" >>% false <|> (pKeyword "WITH" >>. pKeyword "TIES" >>% true))
                |>> fun ((countOpt, isPercent), withTies) ->
                    { Count = Option.defaultValue defaultCount countOpt
                      IsPercent = isPercent
                      WithTies = withTies }

        choice
            [ attempt (pResultOffsetClause .>>. opt pFetchFirstClause)
              |>> fun (o, f) -> Some o, f
              attempt pFetchFirstClause |>> fun f -> None, Some f ]

    // Apply ORDER BY, OFFSET, FETCH, LOCKING to the whole query expression.
    // For a plain SELECT they are folded into the SelectStatement; for set
    // operations and WITH queries they are attached via QueryExpression so the
    // scope is the entire result, not just the last operand.
    let private applyOrderByOffsetFetch (orderBy: (Expression * bool * NullsOrder option) list) limitOffset locking q =
        let hasTopLevelClauses =
            not orderBy.IsEmpty || Option.isSome limitOffset || Option.isSome locking

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

    // 7.17 <query expression body> ::= <query term> | <query expression body> UNION|EXCEPT ... | <query expression body> EXCEPT ...
    let private pQueryExpressionBody, private pQueryExpressionBodyRef =
        createParserForwardedToRef<Query, unit> ()

    // 7.17 <query term> ::= <query primary> | <query term> INTERSECT ... <query primary>
    let private pQueryTerm =
        // 7.17 <simple table> ::= <query specification> | <table value constructor> | <explicit table>
        // Set-operation operands do not consume ORDER BY/OFFSET/FETCH/LOCKING so those apply to the whole expression.
        let pSimpleTable =
            choice
                [ attempt (pQuerySpecification |>> SelectQuery)
                  attempt (pTableValueConstructor |>> TableValueConstructor)
                  attempt (pKeyword "TABLE" >>. pSchemaQualifiedNameExpression |>> ExplicitTable) ]

        // 7.17 <query primary> ::= <simple table>
        //   | ( <query expression body> [ <order by clause> ] [ <result offset clause> ]
        //       [ <fetch first clause> ] )
        let pQueryPrimary =
            choice
                [ attempt (
                      between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pQueryExpressionBody .>>. opt pOrderByClause .>>. opt pOffsetFetch
                           |>> fun ((body, orderBy), limitOffset) ->
                               applyOrderByOffsetFetch (Option.defaultValue [] orderBy) limitOffset None body)
                  )
                  pSimpleTable ]

        // 7.17 <query term> — INTERSECT operator (higher precedence than UNION/EXCEPT)
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

        chainl1 pQueryPrimary (pIntersectOp |>> fun op -> fun l r -> SetOperation(l, op, r))

    // 7.17 <query expression body> — UNION/EXCEPT operator (lower precedence than INTERSECT)
    let private pUnionExceptOp =
        choice [ pKeyword "UNION" >>% Union; pKeyword "EXCEPT" >>% Except ]
        .>>. opt (pKeyword "ALL" >>% (true, false) <|> (pKeyword "DISTINCT" >>% (false, true)))
        .>>. opt pCorrespondingSpec
        |>> fun ((kind, quant), corr) ->
            let isAll, isDist = Option.defaultValue (false, false) quant

            { Kind = kind
              IsAll = isAll
              IsDistinct = isDist
              Corresponding = corr }

    // 7.17 <query expression body> ::= <query term>
    //     | <query expression body> UNION|EXCEPT [ <corresponding spec> ] <query term>
    pQueryExpressionBodyRef.Value <- chainl1 pQueryTerm (pUnionExceptOp |>> fun op -> fun l r -> SetOperation(l, op, r))

    // 7.17 <with clause> ::= WITH [ RECURSIVE ] <with list>
    let pWithClause =
        // 7.18 <search clause> ::= SEARCH { DEPTH FIRST | BREADTH FIRST } BY <cols> SET <col>
        // Local because pWithListElement is the only consumer of the 7.18 clauses.
        let pSearchClause =
            pKeyword "SEARCH"
            >>. (attempt (pKeyword "DEPTH" >>. pKeyword "FIRST" >>% true)
                 <|> (pKeyword "BREADTH" >>. pKeyword "FIRST" >>% false))
            .>> pKeyword "BY"
            .>>. sepBy1 pIdentifierExpression (token (pstring ","))
            .>> pKeyword "SET"
            .>>. pIdentifierExpression
            |>> fun ((isDepthFirst, orderBy), setCol) ->
                { IsDepthFirst = isDepthFirst
                  OrderBy = orderBy
                  SetColumn = setCol }

        // 7.18 <cycle clause> ::= CYCLE <cols> SET <col> TO <mark> DEFAULT <default> USING <path>
        let pCycleClause =
            pKeyword "CYCLE" >>. sepBy1 pIdentifierExpression (token (pstring ","))
            .>> pKeyword "SET"
            .>>. pIdentifierExpression
            .>> pKeyword "TO"
            .>>. pExpression
            .>> pKeyword "DEFAULT"
            .>>. pExpression
            .>> pKeyword "USING"
            .>>. pIdentifierExpression
            |>> fun ((((cols, setCol), mark), defaultVal), path) ->
                { CycleColumns = cols
                  SetColumn = setCol
                  MarkValue = mark
                  DefaultValue = defaultVal
                  PathColumn = path }

        // 7.17 <with list element> ::= <query name> [ ( <with column list> ) ] AS <table subquery> [ <search or cycle clause> ]
        let pWithListElement =
            pIdentifierExpression
            .>>. opt (
                between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpression (token (pstring ",")))
            )
            .>> pKeyword "AS"
            .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
            .>>. (opt (attempt pSearchClause) .>>. opt (attempt pCycleClause))
            |>> fun (((name, cols), q), (search, cycle)) ->
                { Cte.Name = name
                  Columns = cols
                  Query = q
                  SearchClause = search
                  CycleClause = cycle }

        pKeyword "WITH" >>. opt (pKeyword "RECURSIVE" >>% true)
        .>>. sepBy1 pWithListElement (token (pstring ","))
        |>> fun (recu, ctes) -> Option.defaultValue false recu, ctes

    // 7.17 <query expression> ::= [ <with clause> ] <query expression body>
    //     [ <order by clause> ] [ <result offset clause> ] [ <fetch first clause> ]
    let pQueryExpression =
        let pWithOrQuery =
            choice
                [ attempt (
                      pWithClause
                      .>>. pQueryExpressionBody
                      .>>. opt pOrderByClause
                      .>>. opt pOffsetFetch
                      |>> fun ((((recu, ctes), body), orderBy), limitOffset) ->
                          (WithQuery(recu, ctes, body), Option.defaultValue [] orderBy, limitOffset, None)
                  )
                  attempt (
                      pQueryExpressionBody .>>. opt pOrderByClause .>>. opt pOffsetFetch
                      |>> fun ((body, orderBy), limitOffset) ->
                          (body, Option.defaultValue [] orderBy, limitOffset, None)
                  ) ]

        pWithOrQuery
        |>> fun (body, orderBy, limitOffset, locking) -> applyOrderByOffsetFetch orderBy limitOffset locking body

    pQueryRef.Value <- pQueryExpression
