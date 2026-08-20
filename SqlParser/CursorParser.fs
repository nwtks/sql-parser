namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.QueryParser

module CursorParser =
    // 14.5 <fetch orientation> ::= NEXT | PRIOR | FIRST | LAST | { ABSOLUTE | RELATIVE } <simple value specification>
    let pFetchOrientation =
        pKeyword "NEXT" >>% Next
        <|> (pKeyword "PRIOR" >>% Prior)
        <|> (pKeyword "FIRST" >>% First)
        <|> (pKeyword "LAST" >>% Last)
        <|> (pKeyword "ABSOLUTE" >>. pExpression |>> Absolute)
        <|> (pKeyword "RELATIVE" >>. pExpression |>> Relative)

    // 14.4 <open statement> ::= OPEN <cursor name>
    let pOpenStatement = pKeyword "OPEN" >>. pQualifiedName |>> Open

    // 14.5 <fetch statement> ::= FETCH [ [ <fetch orientation> ] FROM ]
    //                                <cursor name> INTO <fetch target list>
    let pFetchStatement =
        pKeyword "FETCH" >>. opt (attempt pFetchOrientation)
        .>>. opt (attempt (pKeyword "FROM" >>% ()))
        .>>. pQualifiedName
        .>> pKeyword "INTO"
        .>>. sepBy1 pQualifiedName (token (pstring ","))
        |>> fun (((orient, _), cursor), targets) -> Fetch(orient, cursor, targets)

    // 14.6 <close statement> ::= CLOSE <cursor name>
    let pCloseStatement = pKeyword "CLOSE" >>. pQualifiedName |>> Close

    // 14.7 <select statement: single row>
    // SELECT [ <set quantifier> ] <select list> INTO <select target list>
    //     <table expression>
    // The <table expression> (FROM/WHERE/GROUP BY/HAVING/WINDOW) reuses the
    // QueryParser clause parsers; INTO sits between the select list and FROM.
    let pSelectIntoStatement =
        pKeyword "SELECT" >>. pSetQuantifier
        .>>. sepBy1 pSelectSublist (token (pstring ","))
        >>= fun (dist, cols) ->
            pKeyword "INTO" >>. sepBy1 pQualifiedName (token (pstring ","))
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
