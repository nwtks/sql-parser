namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module DataManipulationParser =
    // 5.4 <local qualified name> ::= [ <local qualifier> <period> ] <qualified identifier>
    // 14.1 <cursor name> ::= <local qualified name>; <local qualifier> ::= MODULE is the only
    // qualifier a cursor name admits, so `DECLARE a.b CURSOR ...` is rejected.
    let private pLocalQualifiedNameExpression =
        getPosition
        .>>. opt (attempt (pKeyword "MODULE" >>. token (pstring ".")))
        .>>. pIdentifier
        |>> fun ((pos, qualifier), name) ->
            { Expression.Kind =
                (match qualifier with
                 | Some _ -> ColumnReference [ "MODULE"; name ]
                 | None -> Identifier name)
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 14.2 <cursor properties> ::= [ <cursor sensitivity> ] [ <cursor scrollability> ] CURSOR
    //     [ <cursor holdability> ] [ <cursor returnability> ]
    let pCursorProperties =
        // 14.2 <cursor sensitivity> ::= SENSITIVE | INSENSITIVE | ASENSITIVE
        let pCursorSensitivity =
            attempt (pKeyword "ASENSITIVE" >>% CursorSensitivity.Asensitive)
            <|> attempt (pKeyword "INSENSITIVE" >>% CursorSensitivity.Insensitive)
            <|> (pKeyword "SENSITIVE" >>% CursorSensitivity.Sensitive)

        // 14.2 <cursor scrollability> ::= SCROLL | NO SCROLL
        let pCursorScrollability =
            attempt (pKeyword "NO" >>. pKeyword "SCROLL" >>% CursorScrollability.NoScroll)
            <|> (pKeyword "SCROLL" >>% CursorScrollability.Scroll)

        // 14.2 <cursor holdability> ::= WITH HOLD | WITHOUT HOLD
        let pCursorHoldability =
            attempt (pKeyword "WITHOUT" >>. pKeyword "HOLD" >>% CursorHoldability.WithoutHold)
            <|> (pKeyword "WITH" >>. pKeyword "HOLD" >>% CursorHoldability.WithHold)

        // 14.2 <cursor returnability> ::= WITH RETURN | WITHOUT RETURN
        let pCursorReturnability =
            attempt (pKeyword "WITHOUT" >>. pKeyword "RETURN" >>% CursorReturnability.WithoutReturn)
            <|> (pKeyword "WITH" >>. pKeyword "RETURN" >>% CursorReturnability.WithReturn)

        opt (attempt pCursorSensitivity) .>>. opt (attempt pCursorScrollability)
        .>> pKeyword "CURSOR"
        .>>. opt (attempt pCursorHoldability)
        .>>. opt (attempt pCursorReturnability)
        |>> fun (((sensitivity, scrollability), holdability), returnability) ->
            { Sensitivity = sensitivity
              Scrollability = scrollability
              Holdability = holdability
              Returnability = returnability }

    // 14.1 <declare cursor> ::= DECLARE <cursor name> <cursor properties> FOR <cursor specification>
    // 14.3 <cursor specification> ::= <query expression> [ <updatability clause> ]
    // The <updatability clause> belongs to the <cursor specification>, NOT to the
    // <query expression> (7.17 has no such slot).
    // No entry point exposes 14.1 (an SQL-client module statement): neither 13.4 nor
    // 22.1 lists <declare cursor> — see docs/trade-off.md. Kept for a future §21 surface.
    let pDeclareCursor =
        // 14.3 <updatability clause> ::= FOR { READ ONLY | UPDATE [ OF <column name list> ] }
        let pUpdatabilityClause =
            pKeyword "FOR"
            >>. (attempt (
                     pKeyword "UPDATE"
                     >>. opt (attempt (pKeyword "OF" >>. sepBy1 pIdentifierExpression (token (pstring ","))))
                     |>> ForUpdate
                 )
                 <|> (pKeyword "READ" >>. pKeyword "ONLY" >>% ForReadOnly))

        pKeyword "DECLARE" >>. pLocalQualifiedNameExpression .>>. pCursorProperties
        .>> pKeyword "FOR"
        .>>. QueryParser.pQueryExpression
        .>>. opt (attempt pUpdatabilityClause)
        |>> fun (((name, properties), specification), updatability) ->
            { Name = name
              Properties = properties
              Specification = specification
              Updatability = updatability }
            |> DeclareCursor

    // 20.4/23.1 <simple target specification> ::= <host parameter name> | <SQL parameter reference>
    //     | <column reference> | <embedded variable name>
    // (<embedded variable name> is a host-language construct and degrades to <host parameter name>,
    //  as elsewhere — see docs/trade-off.md.)
    let pSimpleTargetSpecification =
        choice
            [ pColumnReferenceExpression
              getPosition .>>. (pQuestionMark >>% "?" <|> pHostParameter)
              |>> fun (pos, name) ->
                  { Expression.Kind = Parameter name
                    Pos = { Line = pos.Line; Column = pos.Column } } ]

    // 6.4 <target specification> ::=
    //     <host parameter specification> | <SQL parameter reference> | <column reference>
    //   | <target array element specification> | <dynamic parameter specification>
    //   | <embedded variable name>
    // Broader than <simple target specification> (23.1): admits `?`, an
    // <indicator parameter>, and <target array element specification>. Shared by
    // 14.5 <fetch target list>, 14.7 <select target list> and 20.12 <into argument>.
    let pTargetSpecification =
        // 6.4 <target array element specification> ::=
        //   <target array reference> <left bracket> <simple value specification> <right bracket>
        let pTargetArrayElement =
            attempt (
                (pColumnReferenceExpression
                 <|> (getPosition .>>. pHostParameter
                      |>> fun (pos, name) ->
                          { Expression.Kind = Parameter name
                            Pos = { Line = pos.Line; Column = pos.Column } }))
                .>>. between (token (pstring "[")) (token (pstring "]")) pSimpleValueSpecification
                |>> fun (arr, idx) ->
                    { Expression.Kind = ArrayElement(arr, idx)
                      Pos = arr.Pos }
            )

        // 6.4 <host parameter specification> ::= <host parameter name> [ <indicator parameter> ]
        // <indicator parameter> ::= [ INDICATOR ] <host parameter name>
        let pHostParameterSpecification =
            getPosition .>>. pHostParameter
            .>>. opt (attempt (opt (pKeyword "INDICATOR") >>. pHostParameter))
            |>> fun ((pos, name), indicator) ->
                let p = { Line = pos.Line; Column = pos.Column }

                match indicator with
                | Some ind ->
                    { Expression.Kind = IndicatorParameter(name, { Kind = Parameter ind; Pos = p })
                      Pos = p }
                | None -> { Expression.Kind = Parameter name; Pos = p }

        choice
            [ pTargetArrayElement
              pHostParameterSpecification
              (getPosition .>>. pQuestionMark
               |>> fun (pos, _) ->
                   { Expression.Kind = Parameter "?"
                     Pos = { Line = pos.Line; Column = pos.Column } })
              pColumnReferenceExpression ]

    // 20.10 <using descriptor> / 20.12 <into descriptor>
    // The `[ SQL ] DESCRIPTOR <descriptor name>` tail shared by both.
    let private pDescriptorName =
        opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR" >>. pIdentifierExpression

    // 20.11 <input using clause> / 20.12 <output using clause> — shared by
    // 20.19 <dynamic open statement>, 20.20 <dynamic fetch statement> and
    // 20.13 <execute statement>. Defined here (and not in DynamicParser.fs)
    // because this module is compiled before DynamicParser.fs. Clause order is
    // inverted for 14.4 pOpenStatement / 14.5 pFetchStatement, which consume them.

    // 20.11 <input using clause> ::= <using arguments> | <using input descriptor>
    // <using arguments> ::= USING <using argument> [ { <comma> <using argument> }... ]
    let pInputUsingClause =
        pKeyword "USING"
        >>. (attempt (pDescriptorName |>> UsingClause.UsingDescriptor)
             <|> (sepBy1 pExpression (token (pstring ",")) |>> UsingClause.UsingArguments))

    // 20.12 <output using clause> ::= <into arguments> | <into descriptor>
    // <into arguments> ::= INTO <into argument> [ { <comma> <into argument> }... ]
    // 20.12 <into argument> ::= <target specification>
    let pOutputUsingClause =
        pKeyword "INTO"
        >>. (attempt (pDescriptorName |>> UsingClause.UsingDescriptor)
             <|> (sepBy1 pTargetSpecification (token (pstring ","))
                  |>> UsingClause.UsingArguments))

    // 14.4 <open statement> ::= OPEN <cursor name>
    // 20.19 <dynamic open statement> ::= OPEN <conventional dynamic cursor name> [ <input using clause> ]
    let pOpenStatement =
        pKeyword "OPEN" >>. pLocalQualifiedNameExpression
        .>>. opt (attempt pInputUsingClause)
        |>> Open

    // 14.5 <fetch statement> ::= FETCH [ [ <fetch orientation> ] FROM ]
    //                                <cursor name> INTO <fetch target list>
    // 20.20 <dynamic fetch statement> ::= FETCH [ [ <fetch orientation> ] FROM ]
    //                                <dynamic cursor name> <output using clause>
    let pFetchStatement =
        // 14.5 <fetch orientation> ::= NEXT | PRIOR | FIRST | LAST | { ABSOLUTE | RELATIVE } <simple value specification>
        let pFetchOrientation =
            pKeyword "NEXT" >>% Next
            <|> (pKeyword "PRIOR" >>% Prior)
            <|> (pKeyword "FIRST" >>% First)
            <|> (pKeyword "LAST" >>% Last)
            <|> (pKeyword "ABSOLUTE" >>. pSimpleValueSpecification |>> Absolute)
            <|> (pKeyword "RELATIVE" >>. pSimpleValueSpecification |>> Relative)

        // The optional group is `[ [ <fetch orientation> ] FROM ]` as a unit:
        // an orientation without FROM is not valid (backtracks, then the bare
        // non-reserved word is tried as a <cursor name>).
        pKeyword "FETCH"
        >>. opt (attempt (opt pFetchOrientation .>> pKeyword "FROM"))
        .>>. pLocalQualifiedNameExpression
        .>>. pOutputUsingClause
        |>> fun ((head, cursor), output) -> Fetch(Option.flatten head, cursor, output)

    // 14.6 <close statement> ::= CLOSE <cursor name>
    let pCloseStatement = pKeyword "CLOSE" >>. pLocalQualifiedNameExpression |>> Close

    // 14.7 <select statement: single row>
    // SELECT [ <set quantifier> ] <select list> INTO <select target list>
    //     <table expression>
    // 14.7 <select target list> ::= <target specification> [ { , <target specification> }... ]
    // The <table expression> (FROM/WHERE/GROUP BY/HAVING/WINDOW) reuses the
    // QueryParser clause parsers; INTO sits between the select list and FROM.
    let pSelectStatementSingleRow =
        pKeyword "SELECT" >>. QueryParser.pSetQuantifier
        .>>. sepBy1 QueryParser.pSelectSublist (token (pstring ","))
        >>= fun (dist, cols) ->
            pKeyword "INTO" >>. sepBy1 pTargetSpecification (token (pstring ","))
            >>= fun into ->
                QueryParser.pFromClause
                >>= fun from ->
                    opt (attempt QueryParser.pWhereClause)
                    >>= fun whr ->
                        opt (attempt QueryParser.pGroupByClause)
                        >>= fun grp ->
                            opt (attempt QueryParser.pHavingClause)
                            >>= fun hav ->
                                opt (attempt QueryParser.pWindowClause)
                                |>> fun win ->
                                    let grpDistinct, grpList =
                                        match grp with
                                        | Some(d, l) -> Option.defaultValue false d, l
                                        | None -> false, []

                                    { IsDistinct = Option.defaultValue false dist
                                      Columns = cols
                                      Into = into
                                      From = from
                                      Where = whr
                                      GroupBy = grpList
                                      GroupByDistinct = grpDistinct
                                      Having = hav
                                      Window = Option.defaultValue [] win }
                                    |> SelectInto

    // 7.12 <where clause> (positioned 14.8/14.13, searched 14.9/14.14)
    //   positioned: WHERE CURRENT OF <cursor name>
    //   searched:   WHERE <search condition>
    // Returns (cursor, search condition) — exactly one is Some.
    let private pWhereClause =
        pKeyword "WHERE"
        >>. (attempt (
                 pKeyword "CURRENT" >>. pKeyword "OF" >>. pSchemaQualifiedNameExpression
                 |>> fun c -> Some c, None
             )
             <|> (pExpression |>> fun e -> None, Some e))

    // 14.8/14.9/14.13/14.14 <target table> ::= <table name> | ONLY ( <table name> )
    // Returns (name, isOnly).
    let private pTargetTable =
        attempt (
            pKeyword "ONLY"
            >>. between (token (pstring "(")) (token (pstring ")")) pSchemaQualifiedNameExpression
            |>> fun name -> name, true
        )
        <|> (pSchemaQualifiedNameExpression |>> fun name -> name, false)

    // 14.9/14.14 FOR PORTION OF <application time period name> FROM <point in time 1> TO <point in time 2>
    //     FROM <point in time 1> TO <point in time 2>
    let private pPortionOf =
        pKeyword "FOR"
        >>. pKeyword "PORTION"
        >>. pKeyword "OF"
        >>. pIdentifierExpression
        .>>. (pKeyword "FROM" >>. (pDatetimeValueExpression .>> ws))
        .>>. (pKeyword "TO" >>. (pDatetimeValueExpression .>> ws))
        |>> fun ((period, fromPoint), toPoint) ->
            { PeriodName = period
              From = fromPoint
              To = toPoint }

    // 20.25/20.27 — the omitted target form is only valid when the statement is
    // positioned through a dynamic cursor and carries no <portion of>, correlation
    // name or search condition.
    let private pOmittedTargetGuard clause target cursor where portion alias =
        let isPositioned =
            Option.isSome cursor
            && Option.isNone where
            && Option.isNone portion
            && Option.isNone alias

        match target with
        | DmlTarget.OmittedTarget when not isPositioned ->
            fail (
                "The target table of the "
                + clause
                + " may only be omitted for a positioned statement (WHERE CURRENT OF)"
            )
        | _ -> preturn ()

    // 14.8/14.13 are the positioned forms (WHERE CURRENT OF) and 14.9/14.14 the searched forms
    // (FOR PORTION OF); the two clause sets are mutually exclusive.
    let private pPositionedPortionGuard portion cursor =
        if Option.isSome portion && Option.isSome cursor then
            fail "FOR PORTION OF cannot be combined with WHERE CURRENT OF."
        else
            preturn ()

    // 14.8 <delete statement: positioned> ::= DELETE FROM <target table> [ [ AS ] <correlation name> ] WHERE CURRENT OF <cursor name>
    // 14.9 <delete statement: searched>   ::= DELETE FROM <target table>
    //     [ FOR PORTION OF <application time period name> FROM <point in time 1> TO <point in time 2> ]
    //     [ [ AS ] <correlation name> ] [ WHERE <search condition> ]
    // 20.25 <preparable dynamic delete statement: positioned> ::= DELETE [ FROM <target table> ]
    //     WHERE CURRENT OF <preparable dynamic cursor name>
    let pDeleteStatement =
        pKeyword "DELETE" >>. opt (attempt (pKeyword "FROM" >>. pTargetTable))
        .>>. opt (attempt pPortionOf)
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpression)
        .>>. opt pWhereClause
        >>= fun (((target, portion), alias), whr) ->
            let cursor, where =
                match whr with
                | Some(c, w) -> c, w
                | None -> None, None

            let target =
                match target with
                | Some(name, isOnly) -> DmlTarget.TableTarget(name, isOnly)
                | None -> DmlTarget.OmittedTarget

            let statement =
                { Target = target
                  TableAlias = alias
                  Where = where
                  PortionOf = portion
                  Cursor = cursor }
                |> Delete

            pOmittedTargetGuard
                "preparable dynamic delete statement: positioned (20.25)"
                target
                cursor
                where
                portion
                alias
            >>. pPositionedPortionGuard portion cursor
            >>. preturn statement

    // 14.10 <truncate table statement> ::= TRUNCATE TABLE <target table> [ <identity column restart option> ]
    let pTruncateTableStatement =
        pKeyword "TRUNCATE" >>. pKeyword "TABLE" >>. pSchemaQualifiedNameExpression
        .>>. opt (
            pKeyword "RESTART" >>. pKeyword "IDENTITY" >>% true
            <|> (pKeyword "CONTINUE" >>. pKeyword "IDENTITY" >>% false)
        )
        |>> fun (table, restart) -> Truncate(table, restart)

    // 14.11 <override clause> ::= OVERRIDING USER VALUE | OVERRIDING SYSTEM VALUE
    // (None when absent; Some true = USER, Some false = SYSTEM)
    let private pOverrideClause =
        opt (
            pKeyword "OVERRIDING"
            >>. (pKeyword "USER" >>% true <|> (pKeyword "SYSTEM" >>% false))
            .>> pKeyword "VALUE"
        )

    // 14.11 <insert statement> ::= INSERT INTO <insertion target> <insert columns and source>
    // 14.11 <insertion target> ::= <table name>
    // 14.11 <insert columns and source> ::= <from subquery> | <from constructor> | <from default>
    // 14.11 <from subquery>    ::= [ ( <insert column list> ) ] [ <override clause> ] <query expression>
    // 14.11 <from constructor> ::= [ ( <insert column list> ) ] [ <override clause> ] <contextually typed table value constructor>
    // 14.11 <from default>     ::= DEFAULT VALUES
    let pInsertStatement =
        // 7.3 <contextually typed table value constructor> ::= VALUES <contextually typed row value expression list>
        // Used as <from constructor> of <insert statement> (14.11). Each <contextually typed
        // row value constructor element> also admits a 6.5 <contextually typed value specification>.
        let pContextuallyTypedTableValueConstructor =
            pKeyword "VALUES"
            >>. sepBy1
                    (between
                        (token (pstring "("))
                        (token (pstring ")"))
                        (sepBy1 (pDefaultSpecification <|> pNullSpecification <|> pExpression) (token (pstring ","))))
                    (token (pstring ","))
            |>> Values

        pKeyword "INSERT" >>. pKeyword "INTO" >>. pSchemaQualifiedNameExpression
        .>>. opt (
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpression (token (pstring ",")))
        )
        .>>. pOverrideClause
        .>>. (pContextuallyTypedTableValueConstructor
              <|> (QueryParser.pQueryExpression |>> Query)
              <|> (pKeyword "DEFAULT" >>. pKeyword "VALUES" >>% DefaultValues))
        >>= fun (((table, cols), ovr), source) ->
            // 14.11 <from default> has neither an <insert column list> nor an <override clause>.
            match source with
            | InsertSource.DefaultValues when Option.isSome cols || Option.isSome ovr ->
                fail "DEFAULT VALUES cannot carry an <insert column list> or an <override clause>."
            | InsertSource.DefaultValues ->
                preturn (
                    Insert
                        { Table = table
                          Columns = None
                          Source = InsertSource.DefaultValues
                          Override = None }
                )
            | _ ->
                preturn (
                    Insert
                        { Table = table
                          Columns = cols
                          Source = source
                          Override = ovr }
                )

    // 14.12 <merge statement> ::= MERGE INTO <target table> [ [ AS ] <merge correlation name> ]
    //     USING <table reference> ON <search condition> <merge operation specification>
    // <merge operation specification> ::= <merge when clause>...
    // <merge when clause> ::= <merge when matched clause> | <merge when not matched clause>
    let pMergeStatement =
        // 14.12 <merge update specification> ::= UPDATE SET <set clause list>
        // 14.12 <merge delete specification> ::= DELETE
        let pMatchedAction =
            // <merge update or delete specification> ::= <merge update specification> | <merge delete specification>
            // <set clause> shares 14.15's <update source>, which admits a 6.5
            // <contextually typed value specification> (NULL).
            choice
                [ attempt (pKeyword "UPDATE" >>. pKeyword "SET")
                  >>. sepBy1
                          (pIdentifierExpression .>> token (pstring "=")
                           .>>. (pNullSpecification <|> pExpression))
                          (token (pstring ","))
                  |>> MergeUpdate
                  pKeyword "DELETE" >>% MergeDelete ]

        // 14.12 <merge insert specification> ::= INSERT [ ( <insert column list> ) ] [ <override clause> ] VALUES <merge insert value list>
        let pNotMatchedAction =
            pKeyword "INSERT"
            >>. opt (
                between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpression (token (pstring ",")))
            )
            .>>. pOverrideClause
            .>> pKeyword "VALUES"
            .>>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 (pDefaultSpecification <|> pNullSpecification <|> pExpression) (token (pstring ",")))
            |>> fun ((cols, ovr), values) -> MergeInsert(cols, ovr, values)

        // 14.12 <merge when matched clause>     ::= WHEN MATCHED [ AND <search condition> ] THEN <merge update or delete specification>
        // 14.12 <merge when not matched clause> ::= WHEN NOT MATCHED [ AND <search condition> ] THEN <merge insert specification>
        let pWhenMatch =
            pKeyword "WHEN"
            >>. choice
                    [ attempt (pKeyword "NOT" .>> pKeyword "MATCHED") >>% NotMatched
                      pKeyword "MATCHED" >>% Matched ]
            .>>. opt (pKeyword "AND" >>. pExpression)
            .>> pKeyword "THEN"
            >>= fun (cond, filter) ->
                (match cond with
                 | Matched -> pMatchedAction
                 | NotMatched -> pNotMatchedAction)
                |>> fun action ->
                    { MatchCondition = cond
                      Condition = filter
                      Action = action }

        pKeyword "MERGE" >>. pKeyword "INTO" >>. pTargetTable
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpression)
        .>> pKeyword "USING"
        .>>. QueryParser.pTableReference
        .>> pKeyword "ON"
        .>>. pExpression
        .>>. many1 pWhenMatch
        |>> fun (((((target, targetIsOnly), alias), source), on), whens) ->
            { Target = target
              TargetIsOnly = targetIsOnly
              TargetAlias = alias
              Source = source
              On = on
              WhenClauses = whens }
            |> Merge

    // 14.13 <update statement: positioned> ::= UPDATE <target table> [ [ AS ] <correlation name> ] SET <set clause list> WHERE CURRENT OF <cursor name>
    // 14.14 <update statement: searched>   ::= UPDATE <target table> [ FOR PORTION OF <application time period name> FROM <point in time 1> TO <point in time 2> ]
    //     [ [ AS ] <correlation name> ] SET <set clause list> [ WHERE <search condition> ]
    // 14.15 <set clause list> ::= <set clause> [ { <comma> <set clause> }... ]
    // 20.27 <preparable dynamic update statement: positioned> ::= UPDATE [ <target table> ] SET <set clause list>
    //     WHERE CURRENT OF <preparable dynamic cursor name>
    let pUpdateStatement =
        // 14.15 <set clause> ::= <set target> <equals operator> <update source>
        //     | <multiple column assignment> | <mutated set clause>
        let pSetClause =
            // 14.15 <multiple column assignment> ::= <set target list> <equals operator> <assigned row>
            // <set target list> ::= ( <set target> [ { <comma> <set target> }... ] )
            attempt (
                between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpression (token (pstring ",")))
                .>> token (pstring "=")
                // <assigned row> is a <contextually typed row value expression>: NULL is legal.
                .>>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (sepBy1 (pNullSpecification <|> pExpression) (token (pstring ",")))
                |>> MultipleSet
            )
            <|> attempt (
                // 14.15 <mutated set clause> ::= <mutated target> <period> <method name>
                // <mutated target> ::= <object column> | <mutated set clause>
                // <set clause> ::= <mutated set clause> <equals operator> <update source>
                pIdentifierExpression .>>. many1 (token (pstring ".") >>. pIdentifierExpression)
                .>> token (pstring "=")
                .>>. (pDefaultSpecification <|> pNullSpecification <|> pExpression)
                |>> fun ((first, rest), value) ->
                    // The last segment is the method name; the rest is the
                    // mutated target (folded into a FieldReference chain).
                    let target: Expression =
                        List.fold
                            (fun (acc: Expression) (name: Expression) ->
                                { Kind = FieldReference(acc, name)
                                  Pos = acc.Pos })
                            first
                            (List.take (rest.Length - 1) rest)

                    MutatedSet(target, List.last rest, value)
            )
            <|> ( // 14.15 <set clause> ::= <set target> <equals operator> <update source>
            // <set target> ::= <update target> (<object column>)
            pIdentifierExpression .>> token (pstring "=")
            .>>. (pDefaultSpecification <|> pNullSpecification <|> pExpression)
            |>> SingleSet)

        // 20.25 <preparable dynamic delete statement: positioned> /
        // 20.27 <preparable dynamic update statement: positioned> omit the <target table>.
        let pOptionalDmlTarget =
            attempt pTargetTable |>> DmlTarget.TableTarget
            <|> preturn DmlTarget.OmittedTarget

        pKeyword "UPDATE" >>. pOptionalDmlTarget
        .>>. opt (attempt pPortionOf)
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpression)
        .>> pKeyword "SET"
        .>>. sepBy1 pSetClause (token (pstring ","))
        .>>. opt pWhereClause
        >>= fun ((((target, portion), alias), sets), whr) ->
            let cursor, where =
                match whr with
                | Some(c, w) -> c, w
                | None -> None, None

            let statement =
                { Target = target
                  TableAlias = alias
                  Set = sets
                  Where = where
                  PortionOf = portion
                  Cursor = cursor }
                |> Update

            pOmittedTargetGuard
                "preparable dynamic update statement: positioned (20.27)"
                target
                cursor
                where
                portion
                alias
            >>. pPositionedPortionGuard portion cursor
            >>. preturn statement

    // 14.16 <temporary table declaration> ::= DECLARE LOCAL TEMPORARY TABLE <table name> <table element list>
    //     [ ON COMMIT <table commit action> ROWS ]
    let pTemporaryTableDeclaration =
        // 11.3 <table element> ::= <column definition> | <table constraint definition>
        let pTableElement =
            attempt (SchemaParser.pColumnDefinition |>> Choice1Of2)
            <|> (SchemaParser.pTableConstraintDefinition |>> Choice2Of2)

        // 14.16 <table commit action> ::= PRESERVE | DELETE
        let pTableCommitAction =
            pKeyword "PRESERVE" >>% TableCommitAction.PreserveOnCommit
            <|> (pKeyword "DELETE" >>% TableCommitAction.DeleteOnCommit)

        pKeyword "DECLARE"
        >>. pKeyword "LOCAL"
        >>. pKeyword "TEMPORARY"
        >>. pKeyword "TABLE"
        >>. pSchemaQualifiedNameExpression
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTableElement (token (pstring ",")))
        .>>. opt (attempt (pKeyword "ON" >>. pKeyword "COMMIT" >>. pTableCommitAction .>> pKeyword "ROWS"))
        |>> fun ((name, elements), onCommit) ->
            let cols =
                elements
                |> List.choose (function
                    | Choice1Of2 c -> Some c
                    | _ -> None)

            let cons =
                elements
                |> List.choose (function
                    | Choice2Of2 c -> Some c
                    | _ -> None)

            { Name = name
              Columns = cols
              Constraints = cons
              OnCommit = onCommit }
            |> DeclareTemporaryTable

    // 14.17 <locator reference> ::= <host parameter name> | <embedded variable name> | <dynamic parameter specification>
    // (<embedded variable name> is a host-language construct and is not modelled;
    //  the embedded form degrades to <host parameter name> — see docs/trade-off.md.)
    let private pLocatorReference =
        pQuestionMark >>% "?" <|> pHostParameter |>> Parameter |> withExprPosition

    // 14.17 <free locator statement> ::= FREE LOCATOR <locator reference> [ { <comma> <locator reference> }... ]
    let pFreeLocatorStatement =
        pKeyword "FREE"
        >>. pKeyword "LOCATOR"
        >>. sepBy1 pLocatorReference (token (pstring ","))
        |>> FreeLocator

    // 14.18 <hold locator statement> ::= HOLD LOCATOR <locator reference> [ { <comma> <locator reference> }... ]
    let pHoldLocatorStatement =
        pKeyword "HOLD"
        >>. pKeyword "LOCATOR"
        >>. sepBy1 pLocatorReference (token (pstring ","))
        |>> HoldLocator
