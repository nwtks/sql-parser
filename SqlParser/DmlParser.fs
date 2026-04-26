namespace SqlParser

open FParsec
open SqlParser.Ast
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.QueryParser

module DmlParser =
    let pInsertStatement =
        pKeyword "INSERT" >>. pKeyword "INTO" >>. pIdentifier
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifier (token (pstring ","))))
        .>>. (pKeyword "VALUES"
              >>. sepBy1
                      (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ","))))
                      (token (pstring ","))
              |>> Values
              <|> (pQuery |>> Query))
        |>> fun ((table, cols), source) ->
            { Table = table
              Columns = cols
              Source = source }
            |> Insert

    let pUpdateStatement =
        pKeyword "UPDATE" >>. pIdentifier .>> pKeyword "SET"
        .>>. sepBy1 (pIdentifier .>> token (pstring "=") .>>. pExpression) (token (pstring ","))
        .>>. opt (pKeyword "WHERE" >>. pExpression)
        |>> fun ((table, sets), whr) ->
            { Table = table
              Set = sets
              Where = whr }
            |> Update

    let pDeleteStatement =
        pKeyword "DELETE" >>. pKeyword "FROM" >>. pIdentifier
        .>>. opt (pKeyword "WHERE" >>. pExpression)
        |>> fun (table, whr) -> { Table = table; Where = whr } |> Delete

    let pMergeStatement =
        let pAction =
            choice
                [ attempt (pKeyword "UPDATE" >>. pKeyword "SET")
                  >>. sepBy1 (pIdentifier .>> token (pstring "=") .>>. pExpression) (token (pstring ","))
                  |>> MergeUpdate
                  pKeyword "DELETE" >>% MergeDelete
                  pKeyword "INSERT"
                  >>. opt (
                      between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifier (token (pstring ",")))
                  )
                  .>> pKeyword "VALUES"
                  .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
                  |>> MergeInsert ]

        let pWhenMatch =
            pKeyword "WHEN"
            >>. choice
                    [ attempt (pKeyword "NOT" .>> pKeyword "MATCHED") >>% NotMatched
                      pKeyword "MATCHED" >>% Matched ]
            .>>. opt (pKeyword "AND" >>. pExpression)
            .>> pKeyword "THEN"
            .>>. pAction
            |>> fun ((cond, filter), action) ->
                { MatchCondition = cond
                  Condition = filter
                  Action = action }

        pKeyword "MERGE" >>. pKeyword "INTO" >>. pIdentifier
        .>>. opt (opt (pKeyword "AS") >>. pIdentifier)
        .>> pKeyword "USING"
        .>>. pTableSource
        .>> pKeyword "ON"
        .>>. pExpression
        .>>. many1 pWhenMatch
        |>> fun ((((target, alias), source), on), whens) ->
            { Target = target
              TargetAlias = alias
              Source = source
              On = on
              WhenClauses = whens }
            |> Merge
