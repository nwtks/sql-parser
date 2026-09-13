namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.QueryParser

module CursorParser =
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

    // 14.2 <cursor properties> ::= [ <cursor sensitivity> ] [ <cursor scrollability> ] CURSOR
    //     [ <cursor holdability> ] [ <cursor returnability> ]
    let pCursorProperties =
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
    // (pQuery already absorbs the trailing [ <updatability clause> ])
    let pDeclareCursorStatement =
        pKeyword "DECLARE" >>. pQualifiedNameExpr .>>. pCursorProperties
        .>> pKeyword "FOR"
        .>>. pQuery
        |>> fun ((name, properties), specification) ->
            { Name = name
              Properties = properties
              Specification = specification }
            |> DeclareCursor

    // 20.11 <input using clause> / 20.12 <output using clause> — shared by
    // 20.19 <dynamic open statement>, 20.20 <dynamic fetch statement> and
    // 20.13 <execute statement>. Defined here (and not in DynamicParser.fs)
    // because CursorParser.fs is compiled before DynamicParser.fs.

    // 20.10 <using descriptor> / 20.12 <into descriptor>
    // The `[ SQL ] DESCRIPTOR <descriptor name>` tail shared by both.
    let pDescriptorName =
        opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR" >>. pQualifiedNameExpr

    // 20.11 <input using clause> ::= <using arguments> | <using input descriptor>
    // <using arguments> ::= USING <using argument> [ { <comma> <using argument> }... ]
    let pUsingClause =
        pKeyword "USING"
        >>. (attempt (pDescriptorName |>> UsingClause.UsingDescriptor)
             <|> (sepBy1 pExpression (token (pstring ",")) |>> UsingClause.UsingArguments))

    // 20.12 <output using clause> ::= <into arguments> | <into descriptor>
    // <into arguments> ::= INTO <into argument> [ { <comma> <into argument> }... ]
    let pIntoClause =
        pKeyword "INTO"
        >>. (attempt (pDescriptorName |>> UsingClause.UsingDescriptor)
             <|> (sepBy1 pQualifiedNameExpr (token (pstring ",")) |>> UsingClause.UsingArguments))

    // 14.4 <open statement> ::= OPEN <cursor name>
    // 20.19 <dynamic open statement> ::= OPEN <conventional dynamic cursor name> [ <input using clause> ]
    let pOpenStatement =
        pKeyword "OPEN" >>. pQualifiedNameExpr .>>. opt (attempt pUsingClause) |>> Open

    // 14.5 <fetch orientation> ::= NEXT | PRIOR | FIRST | LAST | { ABSOLUTE | RELATIVE } <simple value specification>
    let pFetchOrientation =
        pKeyword "NEXT" >>% Next
        <|> (pKeyword "PRIOR" >>% Prior)
        <|> (pKeyword "FIRST" >>% First)
        <|> (pKeyword "LAST" >>% Last)
        <|> (pKeyword "ABSOLUTE" >>. pExpression |>> Absolute)
        <|> (pKeyword "RELATIVE" >>. pExpression |>> Relative)

    // 14.5 <fetch statement> ::= FETCH [ [ <fetch orientation> ] FROM ]
    //                                <cursor name> INTO <fetch target list>
    // 20.20 <dynamic fetch statement> ::= FETCH [ [ <fetch orientation> ] FROM ]
    //                                <dynamic cursor name> <output using clause>
    let pFetchStatement =
        pKeyword "FETCH" >>. opt (attempt pFetchOrientation)
        .>>. opt (attempt (pKeyword "FROM" >>% ()))
        .>>. pQualifiedNameExpr
        .>>. pIntoClause
        |>> fun (((orient, _), cursor), output) -> Fetch(orient, cursor, output)

    // 14.6 <close statement> ::= CLOSE <cursor name>
    let pCloseStatement = pKeyword "CLOSE" >>. pQualifiedNameExpr |>> Close

    // 14.7 <select statement: single row>
    // SELECT [ <set quantifier> ] <select list> INTO <select target list>
    //     <table expression>
    // The <table expression> (FROM/WHERE/GROUP BY/HAVING/WINDOW) reuses the
    // QueryParser clause parsers; INTO sits between the select list and FROM.
    let pSelectIntoStatement =
        pKeyword "SELECT" >>. pSetQuantifier
        .>>. sepBy1 pSelectSublist (token (pstring ","))
        >>= fun (dist, cols) ->
            pKeyword "INTO" >>. sepBy1 pQualifiedNameExpr (token (pstring ","))
            >>= fun into ->
                opt (attempt pFromClause)
                >>= fun from ->
                    opt (attempt pWhereClause)
                    >>= fun whr ->
                        opt (attempt pGroupByClause)
                        >>= fun grp ->
                            opt (attempt pHavingClause)
                            >>= fun hav ->
                                opt (attempt pWindowClause)
                                |>> fun win ->
                                    let grpDistinct, grpList =
                                        match grp with
                                        | Some(d, l) -> Option.defaultValue false d, l
                                        | None -> false, []

                                    { IsDistinct = Option.defaultValue false dist
                                      Columns = cols
                                      Into = into
                                      From = Option.defaultValue [] from
                                      Where = whr
                                      GroupBy = grpList
                                      GroupByDistinct = grpDistinct
                                      Having = hav
                                      Window = Option.defaultValue [] win }
                                    |> SelectInto

    // 14.16 <temporary table declaration> ::= DECLARE LOCAL TEMPORARY TABLE <table name> <table element list>
    //     [ ON COMMIT <table commit action> ROWS ]
    let pTemporaryTableDeclarationStatement =
        // 11.3 <table element> ::= <column definition> | <table constraint definition>
        let pTableElement =
            attempt (DdlParser.pColumnDefinition |>> Choice1Of2)
            <|> (DdlParser.pTableConstraint |>> Choice2Of2)

        // 14.16 <table commit action> ::= PRESERVE | DELETE
        let pTableCommitAction =
            pKeyword "PRESERVE" >>% TableCommitAction.PreserveOnCommit
            <|> (pKeyword "DELETE" >>% TableCommitAction.DeleteOnCommit)

        pKeyword "DECLARE"
        >>. pKeyword "LOCAL"
        >>. pKeyword "TEMPORARY"
        >>. pKeyword "TABLE"
        >>. pQualifiedNameExpr
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
    let pLocatorReference =
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

    // 20.8 <cursor attribute> ::= <cursor sensitivity> | <cursor scrollability>
    //     | <cursor holdability> | <cursor returnability>
    let pCursorAttribute =
        choice
            [ attempt (pCursorSensitivity |>> CursorAttribute.SensitivityAttribute)
              attempt (pCursorScrollability |>> CursorAttribute.ScrollabilityAttribute)
              attempt (pCursorHoldability |>> CursorAttribute.HoldabilityAttribute)
              attempt (pCursorReturnability |>> CursorAttribute.ReturnabilityAttribute) ]

    // 20.8 <cursor attributes> ::= <cursor attribute>...
    // (20.8 is not referenced by any production in sql-2016-grammar.txt; exposed for
    //  library consumers — see docs/trade-off.md.)
    let pCursorAttributes = many1 pCursorAttribute
