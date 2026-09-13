namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module QueryParser =
    // 7.17 <query expression> — re-export (actual definition is pQueryExpression at bottom)
    let pQuery = ExpressionParser.pQuery

    // 10.10 <sort specification> ::= <sort key> [ <ordering specification> ] [ <null ordering> ]
    let pOrderByItem = ExpressionParser.pOrderByItem

    let withTablePosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { TableSource.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 7.3 <table value constructor> ::= VALUES <row value expression>
    //     [ { <comma> <row value expression> }... ]
    //   Used both as a <simple table> (7.17) and as a <derived table> inside
    //   <table primary> (7.6).
    let pTableValueConstructor =
        let pRow =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))

        pKeyword "VALUES" >>. sepBy1 pRow (token (pstring ","))

    // 7.6 <correlation name> ::= [ AS ] <identifier>   (a.k.a. table alias)
    let pCorrelationName =
        attempt (pKeyword "AS") >>. pIdentifierExpr <|> pIdentifierExpr

    // 7.6 <correlation or recognition> ::= [ AS ] <correlation name> [ ( <derived column list> ) ]
    let pCorrelationOrRecognition =
        pCorrelationName
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))

    // 7.6 <sample method> ::= BERNOULLI | SYSTEM
    let pSampleMethod =
        pKeyword "BERNOULLI" >>% "BERNOULLI" <|> (pKeyword "SYSTEM" >>% "SYSTEM")

    // 7.6 <repeatable clause> ::= REPEATABLE ( <repeat argument> )
    let pRepeatableClause =
        pKeyword "REPEATABLE"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression

    // 7.6 <sample clause> ::= TABLESAMPLE <sample method>
    //     ( <sample percentage> ) [ <repeatable clause> ]
    let pSampleClause =
        pKeyword "TABLESAMPLE" >>. pSampleMethod
        .>>. between (token (pstring "(")) (token (pstring ")")) pExpression
        .>>. opt pRepeatableClause
        |>> fun ((method, percent), repeat) -> method, percent, repeat

    // 7.6 <table reference> ::= <table factor> | <joined table>
    // Forward reference so <table primary> can nest a parenthesized <joined table>.
    let pTableReference, pTableReferenceRef =
        createParserForwardedToRef<TableSource, unit> ()

    // 7.6 <data change delta table> / 7.6 <data change statement> — forward ref
    // Forward reference to the DML statement parsers (defined in DataManipulationParser.fs,
    // which is compiled after this module). Used by <data change delta table> (7.6):
    //   FINAL|NEW|OLD TABLE ( <data change statement> )
    let pDataChangeStatement, pDataChangeStatementRef =
        createParserForwardedToRef<StatementKind, unit> ()

    // 7.6 <query system time period specification> ::=
    //     FOR SYSTEM_TIME AS OF <point in time>
    //   | FOR SYSTEM_TIME BETWEEN [ ASYMMETRIC | SYMMETRIC ] <p1> AND <p2>
    //   | FOR SYSTEM_TIME FROM <p1> TO <p2>
    // <point in time> is a <datetime value expression> (6.35), whose '+'/'-' chain has no
    // boolean operators, so BETWEEN's AND is not consumed as a boolean operator.
    let pSystemTimeSpec =
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
    let pMatchRecognizeClause =
        pKeyword "MATCH_RECOGNIZE"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (
                    attempt (
                        pKeyword "PARTITION"
                        >>. pKeyword "BY"
                        >>. sepBy1 pExpression (token (pstring ","))
                    )
                 )
                 .>>. opt (attempt (pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ","))))
                 .>>. opt (attempt pRowPatternMeasures)
                 .>>. opt (attempt pRowPatternRowsPerMatch)
                 .>>. pRowPatternCommon
                 |>> fun ((((pb, ob), measures), rpm), common) ->
                     { PartitionBy = Option.defaultValue [] pb
                       OrderBy = Option.defaultValue [] ob
                       Measures = Option.defaultValue [] measures
                       RowsPerMatch = rpm
                       Common = common })

    // 7.11 <JSON table column empty/error behavior> ::= ERROR | NULL | DEFAULT <value expression>
    //     (formatted columns additionally allow EMPTY ARRAY | EMPTY OBJECT)
    let pJsonColumnBehavior =
        choice
            [ pKeyword "ERROR" >>% JsonColumnError
              pKeyword "NULL" >>% JsonColumnNull
              pKeyword "DEFAULT" >>. pExpression |>> JsonColumnDefault
              pKeyword "EMPTY" >>. pKeyword "ARRAY" >>% JsonColumnEmptyArray
              pKeyword "EMPTY" >>. pKeyword "OBJECT" >>% JsonColumnEmptyObject ]

    // 7.11 <JSON table columns clause> (recursive — nested columns contain one)
    let pJsonTableColumnsClause, pJsonTableColumnsClauseRef =
        createParserForwardedToRef<JsonTableColumn list, unit> ()

    // 7.11 <JSON table column definition>
    let pJsonTableColumn =
        choice
            [ // <JSON table nested columns definition> must precede the regular/formatted
              // column branch: <data type> accepts any identifier as a user-defined type,
              // so "NESTED PATH '$.items' ..." would otherwise be misread as a regular
              // column named NESTED of user-defined type PATH.
              attempt (
                  pKeyword "NESTED" >>. opt (pKeyword "PATH" >>% ()) >>. pCharacterStringLiteral
                  .>>. opt (attempt (pKeyword "AS" >>. pIdentifierExpr))
                  .>>. pJsonTableColumnsClause
                  |>> fun ((path, name), cols) ->
                      JsonNested
                          { Path = path
                            Name = name
                            Columns = cols }
              )
              attempt (pIdentifierExpr .>> pKeyword "FOR" .>> pKeyword "ORDINALITY" |>> JsonOrdinality)
              attempt (pIdentifierExpr .>> pKeyword "FOR" .>> pKeyword "CHAINING" |>> JsonChaining)
              attempt (
                  pIdentifierExpr .>>. pDataType
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
                                      opt (attempt (pJsonColumnBehavior .>> pKeyword "ON" .>> pKeyword "EMPTY"))
                                      >>= fun onEmpty ->
                                          opt (attempt (pJsonColumnBehavior .>> pKeyword "ON" .>> pKeyword "ERROR"))
                                          |>> fun onError ->
                                              match fmt with
                                              | Some f ->
                                                  JsonFormatted
                                                      { Name = name
                                                        DataType = dt
                                                        Format = f
                                                        Path = path
                                                        Wrapper = wrapper
                                                        Quotes = quotes
                                                        OnEmpty = onEmpty
                                                        OnError = onError }
                                              | None ->
                                                  JsonRegular
                                                      { Name = name
                                                        DataType = dt
                                                        Path = path
                                                        OnEmpty = onEmpty
                                                        OnError = onError }
              ) ]

    pJsonTableColumnsClauseRef.Value <-
        pKeyword "COLUMNS"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pJsonTableColumn (token (pstring ",")))

    // 7.11 <JSON table plan primary> ::= <path name> | ( <plan> )
    let pJsonTablePlanPrimary, pJsonTablePlanPrimaryRef =
        createParserForwardedToRef<JsonTablePlanPrimary, unit> ()

    // 7.11 <JSON table plan>
    let pJsonTablePlan, pJsonTablePlanRef =
        createParserForwardedToRef<JsonTablePlan, unit> ()

    pJsonTablePlanPrimaryRef.Value <-
        choice
            [ attempt (
                  between (token (pstring "(")) (token (pstring ")")) pJsonTablePlan
                  |>> JsonPlanPrimaryGroup
              )
              pIdentifierExpr |>> JsonPlanPrimaryName ]

    pJsonTablePlanRef.Value <-
        choice
            [ attempt (
                  pIdentifierExpr .>> pKeyword "OUTER" .>>. pJsonTablePlanPrimary
                  |>> fun (n, p) -> JsonPlanOuter(n, p)
              )
              attempt (
                  pIdentifierExpr .>> pKeyword "INNER" .>>. pJsonTablePlanPrimary
                  |>> fun (n, p) -> JsonPlanInner(n, p)
              )
              attempt (sepBy1 pJsonTablePlanPrimary (pKeyword "UNION") |>> JsonPlanUnion)
              attempt (sepBy1 pJsonTablePlanPrimary (pKeyword "CROSS") |>> JsonPlanCross)
              pIdentifierExpr |>> JsonPlanName ]

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
    let pJsonTableStatement =
        pKeyword "JSON_TABLE"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pJsonApiCommon
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
    let pJsonTablePrimitiveStatement =
        pKeyword "JSON_TABLE_PRIMITIVE"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pJsonApiCommon
                 .>>. pJsonTableColumnsClause
                 .>>. (pJsonTableErrorBehavior .>> pKeyword "ON" .>> pKeyword "ERROR")
                 |>> fun ((common, cols), onError) ->
                     { Common = common
                       Columns = cols
                       Plan = None
                       OnError = Some onError })

    // 7.6 <table primary> ::= [ ONLY ] [ <table or query name> | <derived table> | <lateral derived table>
    //     | <collection derived table> | <table function derived table> | <data change delta table>
    //     | <JSON table> | <JSON table primitive> ] — without the optional <sample clause>
    //     (applied in the <table factor> rule below).
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
                  // <only spec> ::= ONLY ( <table or query name> ) [ <correlation or recognition> ]
                  attempt (
                      pKeyword "ONLY"
                      >>. between (token (pstring "(")) (token (pstring ")")) pQualifiedNameExpr
                      .>>. opt (attempt pCorrelationOrRecognition)
                      |>> fun (name, corr) ->
                          let alias, cols =
                              match corr with
                              | Some(name, cols) -> Some name, cols
                              | None -> None, None

                          Only(name, alias, cols)
                  )
                  |> withTablePosition
                  // <table function derived table> / <PTF derived table> ::= TABLE ( <expr> )
                  // A routine invocation (function call) is classified as a PTF table;
                  // any other collection value expression as a table function.
                  attempt (
                      pKeyword "TABLE"
                      >>. between (token (pstring "(")) (token (pstring ")")) pExpression
                      .>>. opt (attempt pCorrelationOrRecognition)
                      |>> fun (expr, corr) ->
                          let alias, cols =
                              match corr with
                              | Some(name, cols) -> Some name, cols
                              | None -> None, None

                          match expr.Kind with
                          | FunctionCall _ -> PtfTable(expr, alias, cols)
                          | _ -> TableFunction(expr, alias, cols)
                  )
                  |> withTablePosition
                  // <data change delta table> ::= <result option> TABLE ( <data change statement> )
                  attempt (
                      pKeyword "FINAL" >>% ResultOption.Final
                      <|> (pKeyword "NEW" >>% ResultOption.New)
                      <|> (pKeyword "OLD" >>% ResultOption.Old)
                      .>> pKeyword "TABLE"
                      .>>. between (token (pstring "(")) (token (pstring ")")) pDataChangeStatement
                      .>>. opt (attempt pCorrelationOrRecognition)
                      |>> fun ((result, stmt), corr) ->
                          let alias, cols =
                              match corr with
                              | Some(name, cols) -> Some name, cols
                              | None -> None, None

                          DataChangeDelta(result, stmt, alias, cols)
                  )
                  |> withTablePosition
                  // <JSON table> <correlation or recognition>
                  attempt (
                      pJsonTableStatement .>>. opt (attempt pCorrelationOrRecognition)
                      |>> fun (stmt, corr) -> JsonTable(stmt, corr)
                  )
                  |> withTablePosition
                  // <JSON table primitive> <correlation name>
                  attempt (
                      pJsonTablePrimitiveStatement .>>. opt (attempt pCorrelationName)
                      |>> fun (stmt, name) -> JsonTablePrimitive(stmt, name)
                  )
                  |> withTablePosition
                  // <table or query name> <row pattern recognition clause and name>
                  //   ::= [ [ AS ] <input name> [ ( <input cols> ) ] ] MATCH_RECOGNIZE ( ... )
                  //       [ [ AS ] <output name> [ ( <output cols> ) ] ]
                  // Must precede the plain <table or query name> branch below so that
                  // "t MATCH_RECOGNIZE(...)" is not consumed as just "t".
                  attempt (
                      pQualifiedNameExpr
                      .>>. opt (
                          attempt (
                              pCorrelationName
                              .>>. opt (
                                  between
                                      (token (pstring "("))
                                      (token (pstring ")"))
                                      (sepBy1 pIdentifierExpr (token (pstring ",")))
                              )
                          )
                      )
                      .>>. pMatchRecognizeClause
                      .>>. opt (
                          attempt (
                              pCorrelationName
                              .>>. opt (
                                  between
                                      (token (pstring "("))
                                      (token (pstring ")"))
                                      (sepBy1 pIdentifierExpr (token (pstring ",")))
                              )
                          )
                      )
                      |>> fun (((name, input), recog), output) -> MatchRecognize(name, input, recog, output)
                  )
                  |> withTablePosition
                  // <table or query name> [ <query system time period specification> ]
                  //     [ <correlation or recognition> ]
                  attempt (
                      getPosition
                      .>>. (pQualifiedNameExpr
                            .>>. opt (attempt pSystemTimeSpec)
                            .>>. opt (attempt pCorrelationName))
                      |>> fun (pos, ((name, sysTime), alias)) ->
                          let pos' = { Line = pos.Line; Column = pos.Column }

                          let baseTable =
                              { TableSource.Kind = Table(name, alias)
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
              >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
              .>>. opt (attempt (pKeyword "AS" >>. pIdentifierExpr))
              |>> fun (cols, alias) -> Using cols, alias ]

    // 7.10 <partitioned join column reference list> ::= ( <column reference> [ { , <column reference> }... ] )
    // <partitioned join column reference> ::= <column reference> — column references only,
    // not arbitrary value expressions.
    let pPartitionBy =
        pKeyword "PARTITION"
        >>. pKeyword "BY"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pColumnReferenceExpr (token (pstring ",")))

    // 7.10 <joined table> — one suffix folded into a left-associative chain:
    // 7.10 <joined table> ::= <cross join> | <qualified join> | <natural join>
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

            joinType
            .>>. pTablePrimary
            .>>. opt (attempt pPartitionBy)
            .>>. opt pJoinSpecification
            |>> fun (((jt, right), partitionBy), cond) ->
                let condition, usingAlias =
                    match cond with
                    | Some(c, a) -> Some c, a
                    | None -> None, None

                Option.defaultValue false nat, jt, right, condition, usingAlias, partitionBy

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

    // 7.13 <grouping element> — forward ref
    let pGroupingElement, pGroupingElementRef =
        createParserForwardedToRef<GroupingElement, unit> ()

    // 7.13 <ordinary grouping set> ::= <grouping column reference>
    //                          | ( <grouping column reference list> )
    let pOrdinaryGroupingSet =
        choice
            [ attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
                  |>> GroupingSet
              )
              pExpression |>> fun e -> GroupingSet [ e ] ]

    // 7.13 <grouping sets specification> ::= GROUPING SETS ( <grouping set list> )
    let pGroupingSetsSpecification =
        pKeyword "GROUPING" .>> pKeyword "SETS"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pGroupingElement (token (pstring ",")))
        |>> GroupingSets

    // 7.13 <rollup list> ::= ROLLUP ( <ordinary grouping set list> )
    let pRollupList =
        pKeyword "ROLLUP"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pOrdinaryGroupingSet (token (pstring ",")))
        |>> Rollup

    // 7.13 <cube list> ::= CUBE ( <ordinary grouping set list> )
    let pCubeList =
        pKeyword "CUBE"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pOrdinaryGroupingSet (token (pstring ",")))
        |>> Cube

    // 7.13 <empty grouping set> ::= ( )
    let pEmptyGroupingSet =
        between (token (pstring "(")) (token (pstring ")")) (preturn EmptyGroupingSet)

    pGroupingElementRef.Value <-
        choice
            [ attempt pGroupingSetsSpecification
              attempt pRollupList
              attempt pCubeList
              attempt pEmptyGroupingSet
              attempt pOrdinaryGroupingSet ]

    // 7.15 <window definition> ::= <new window name> AS <window specification>
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

    // 7.15 <window clause> ::= WINDOW <window definition list>
    let pWindowClause =
        pKeyword "WINDOW" >>. sepBy1 pWindowDefinition (token (pstring ","))

    // 7.16 <set quantifier> ::= DISTINCT | ALL
    let pSetQuantifier =
        opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))

    // 7.16 <derived column> ::= <value expression> [ <as clause> ]
    let pDerivedColumn =
        attempt (pExpression .>>. opt (attempt pCorrelationName)) |>> Column

    // 7.16 <qualified asterisk> ::= <asterisked identifier chain> <period> <asterisk>
    //                            | <all fields reference>
    // 7.16 <all fields reference> ::= <value expression primary> <period> <asterisk>
    //                            [ AS ( <all fields column name list> ) ]
    // The <asterisked identifier chain> form is tried first so that a plain `t.*` keeps
    // yielding QualifiedStar; the general <all fields reference> form then handles any other
    // <value expression primary> (e.g. `(a + b).*`) and yields AllFieldsReference.
    let pAllFieldsAsClause =
        opt (
            attempt (
                pKeyword "AS"
                >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
            )
        )

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

    // 7.16 <select list> ::= <asterisk> | <select sublist> [ { <comma> <select sublist> }... ]
    let pSelectSublist =
        pQualifiedAsterisk
        <|> pDerivedColumn
        <|> (pstring "*" .>> ws >>% ExpressionKind.Star |> withExprPosition
             |>> fun e -> Column(e, None))

    // 7.5 <from clause> ::= FROM <table reference list>
    let pFromClause = pKeyword "FROM" >>. sepBy1 pTableReference (token (pstring ","))

    // 7.12 <where clause> ::= WHERE <search condition>
    let pWhereClause = pKeyword "WHERE" >>. pExpression

    // 7.13 <group by clause> ::= GROUP BY [ <set quantifier> ] <grouping element list>
    let pGroupByClause =
        pKeyword "GROUP" >>. pKeyword "BY" >>. pSetQuantifier
        .>>. sepBy1 pGroupingElement (token (pstring ","))

    // 7.14 <having clause> ::= HAVING <search condition>
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

    // 7.16 <query specification> ::= SELECT [ <set quantifier> ] <select list> <table expression>
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

    // 7.17 <simple table> ::= <query specification> | <table value constructor> | <explicit table>
    // Set-operation operands do not consume ORDER BY/OFFSET/FETCH/LOCKING so those apply to the whole expression.
    let pSimpleTable =
        choice
            [ attempt (pQuerySpecification |>> SelectQuery)
              attempt (pTableValueConstructor |>> TableValueConstructor)
              attempt (pKeyword "TABLE" >>. pQualifiedNameExpr |>> ExplicitTable) ]

    // 7.17 <order by clause> ::= ORDER BY <sort specification list>
    let pOrderByClause =
        pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ","))

    // 7.17 <result offset clause> ::= OFFSET <offset row count> { ROW | ROWS }
    let pResultOffsetClause =
        pKeyword "OFFSET" >>. pExpression
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

            pKeyword "FETCH" >>. (pKeyword "FIRST" <|> pKeyword "NEXT") >>. opt pExpression
            .>>. opt (pKeyword "PERCENT" >>% true)
            .>> (attempt (pKeyword "ROWS") <|> pKeyword "ROW")
            .>>. (pKeyword "ONLY" >>% false <|> (pKeyword "WITH" >>. pKeyword "TIES" >>% true))
            |>> fun ((countOpt, isPercent), withTies) ->
                { Count = Option.defaultValue defaultCount countOpt
                  IsPercent = Option.defaultValue false isPercent
                  WithTies = withTies }

    // 7.17 [ <result offset clause> ] [ <fetch first clause> ]
    let pOffsetFetch =
        choice
            [ attempt (pResultOffsetClause .>>. opt pFetchFirstClause)
              |>> fun (o, f) -> Some o, f
              attempt pFetchFirstClause |>> fun f -> None, Some f ]

    // 7.17 <corresponding spec> ::= CORRESPONDING [ BY ( <corresponding column list> ) ]
    let pCorrespondingSpec =
        pKeyword "CORRESPONDING"
        >>. opt (
            pKeyword "BY"
            >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
        )

    // 14.3 <updatability clause> ::= FOR { READ ONLY | UPDATE [ OF <column name list> ] }
    let pLockingClause =
        pKeyword "FOR"
        >>. (attempt (
                 pKeyword "UPDATE"
                 >>. opt (attempt (pKeyword "OF" >>. sepBy1 pIdentifierExpr (token (pstring ","))))
                 |>> ForUpdate
             )
             <|> (pKeyword "READ" >>. pKeyword "ONLY" >>% ForReadOnly))

    // Apply ORDER BY, OFFSET, FETCH, LOCKING to the whole query expression.
    // For a plain SELECT they are folded into the SelectStatement; for set
    // operations and WITH queries they are attached via QueryExpression so the
    // scope is the entire result, not just the last operand.
    let applyOrderByOffsetFetch (orderBy: (Expression * bool * NullsOrder option) list) limitOffset locking q =
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
    let pQueryExpressionBody, pQueryExpressionBodyRef =
        createParserForwardedToRef<Query, unit> ()

    // 7.17 <query primary> ::= <simple table>
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
                       .>>. opt (attempt pLockingClause)
                       |>> fun (((body, orderBy), limitOffset), locking) ->
                           applyOrderByOffsetFetch (Option.defaultValue [] orderBy) limitOffset locking body)
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

    // 7.17 <query term> ::= <query primary> | <query term> INTERSECT ... <query primary>
    let pQueryTerm =
        chainl1 pQueryPrimary (pIntersectOp |>> fun op -> fun l r -> SetOperation(l, op, r))

    // 7.17 <query expression body> — UNION/EXCEPT operator (lower precedence than INTERSECT)
    let pUnionExceptOp =
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
                      .>>. opt (attempt pLockingClause)
                      |>> fun (((((recu, ctes), body), orderBy), limitOffset), locking) ->
                          (WithQuery(recu, ctes, body), Option.defaultValue [] orderBy, limitOffset, locking)
                  )
                  attempt (
                      pQueryExpressionBody
                      .>>. opt pOrderByClause
                      .>>. opt pOffsetFetch
                      .>>. opt (attempt pLockingClause)
                      |>> fun (((body, orderBy), limitOffset), locking) ->
                          (body, Option.defaultValue [] orderBy, limitOffset, locking)
                  ) ]

        pWithOrQuery
        |>> fun (body, orderBy, limitOffset, locking) -> applyOrderByOffsetFetch orderBy limitOffset locking body

    pQueryRef.Value <- pQueryExpression
