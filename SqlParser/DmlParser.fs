namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.QueryParser

module DmlParser =
    // This module implements the data-manipulation statements (section 14 of
    // sql-2016-grammar.txt):
    //
    //   14.8/14.9  <delete statement>       ::= DELETE FROM <target table> [ [ AS ] <correlation name> ]
    //                                             [ WHERE <search condition> ]
    //   14.10      <truncate table statement> ::= TRUNCATE TABLE <target table> [ <identity column restart> ]
    //                                             [ <drop behavior> ]
    //   14.11      <insert statement>       ::= INSERT INTO <insertion target> ... (VALUES | query | DEFAULT VALUES)
    //   14.12      <merge statement>        ::= MERGE INTO <target> [ [ AS ] <correlation name> ]
    //                                             USING <table reference> ON <search condition> ...
    //   14.13/14.14 <update statement>      ::= UPDATE <target table> ... SET <set clause list>
    //                                             [ WHERE <search condition> ]
    //   14.15      <set clause list>
    let pInsertStatement =
        let pOverride =
            opt (
                pKeyword "OVERRIDING"
                >>. (pKeyword "USER" >>% true <|> (pKeyword "SYSTEM" >>% false))
                .>> pKeyword "VALUE"
            )

        // 7.3 <contextually typed table value constructor> — the VALUES clause
        // of an INSERT statement.
        let pContextuallyTypedTableValueConstructor =
            pKeyword "VALUES"
            >>. sepBy1
                    (between
                        (token (pstring "("))
                        (token (pstring ")"))
                        (sepBy1 (pDefaultValue <|> pExpression) (token (pstring ","))))
                    (token (pstring ","))
            |>> Values

        pKeyword "INSERT" >>. pKeyword "INTO" >>. pQualifiedName
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))
        .>>. pOverride
        .>>. (pContextuallyTypedTableValueConstructor
              <|> (pQuery |>> Query)
              <|> (pKeyword "DEFAULT" >>. pKeyword "VALUES" >>% DefaultValues))
        |>> fun (((table, cols), ovr), source) ->
            { Table = table
              Columns = cols
              Source = source
              Override = ovr }
            |> Insert

    let pUpdateStatement =
        let pSetClause =
            attempt (
                between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                .>> token (pstring "=")
                .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
                |>> MultipleSet
            )
            <|> (pIdentifierExpr .>> token (pstring "=") .>>. (pDefaultValue <|> pExpression)
                 |>> SingleSet)

        pKeyword "UPDATE" >>. pQualifiedName
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpr)
        .>> pKeyword "SET"
        .>>. sepBy1 pSetClause (token (pstring ","))
        .>>. opt (pKeyword "WHERE" >>. pExpression)
        |>> fun (((table, alias), sets), whr) ->
            { Table = table
              TableAlias = alias
              Set = sets
              Where = whr }
            |> Update

    let pDeleteStatement =
        pKeyword "DELETE" >>. pKeyword "FROM" >>. pQualifiedName
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpr)
        .>>. opt (pKeyword "WHERE" >>. pExpression)
        |>> fun ((table, alias), whr) ->
            { Table = table
              TableAlias = alias
              Where = whr }
            |> Delete

    let pMergeStatement =
        let pAction =
            choice
                [ attempt (pKeyword "UPDATE" >>. pKeyword "SET")
                  >>. sepBy1 (pIdentifierExpr .>> token (pstring "=") .>>. pExpression) (token (pstring ","))
                  |>> MergeUpdate
                  pKeyword "DELETE" >>% MergeDelete
                  pKeyword "INSERT"
                  >>. opt (
                      between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
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

        pKeyword "MERGE" >>. pKeyword "INTO" >>. pQualifiedName
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpr)
        .>> pKeyword "USING"
        .>>. pTableReference
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
