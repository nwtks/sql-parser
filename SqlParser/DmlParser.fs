namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.QueryParser

module DmlParser =
    // 14.11 <override clause> ::= OVERRIDING USER VALUE | OVERRIDING SYSTEM VALUE
    // (None when absent; Some true = USER, Some false = SYSTEM)
    let pOverride =
        opt (
            pKeyword "OVERRIDING"
            >>. (pKeyword "USER" >>% true <|> (pKeyword "SYSTEM" >>% false))
            .>> pKeyword "VALUE"
        )

    // 14.9/14.14 <application time period specification> ::= FOR PORTION OF <application time period name>
    //     FROM <point in time 1> TO <point in time 2>
    let pPortionOf =
        pKeyword "FOR" >>. pKeyword "PORTION" >>. pKeyword "OF" >>. pIdentifierExpr
        .>>. (pKeyword "FROM" >>. (pValueExpressionNoBoolean .>> ws))
        .>>. (pKeyword "TO" >>. (pValueExpressionNoBoolean .>> ws))
        |>> fun ((period, fromPoint), toPoint) ->
            { PeriodName = period
              From = fromPoint
              To = toPoint }

    // 14.8/14.9/14.13/14.14 <where clause> (positioned vs searched)
    //   positioned: WHERE CURRENT OF <cursor name>
    //   searched:   WHERE <search condition>
    // Returns (cursor, search condition) — exactly one is Some.
    let pWhereClause =
        pKeyword "WHERE"
        >>. (attempt (
                 pKeyword "CURRENT" >>. pKeyword "OF" >>. pQualifiedName
                 |>> fun c -> Some c, None
             )
             <|> (pExpression |>> fun e -> None, Some e))

    // 14.11 <insert statement> ::= INSERT INTO <insertion target> <insert columns and source>
    // 14.11 <insertion target> ::= <table name>
    // 14.11 <insert columns and source> ::= <from subquery> | <from constructor> | <from default>
    // 14.11 <from subquery>    ::= [ ( <insert column list> ) ] [ <override clause> ] <query expression>
    // 14.11 <from constructor> ::= [ ( <insert column list> ) ] [ <override clause> ] <contextually typed table value constructor>
    // 14.11 <from default>     ::= DEFAULT VALUES
    let pInsertStatement =
        // 7.3 <contextually typed table value constructor> ::= VALUES <contextually typed row value expression list>
        // Used as <from constructor> of <insert statement> (14.11).
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

    // 14.13 <update statement: positioned> ::= UPDATE <target table> [ [ AS ] <correlation name> ] SET <set clause list> WHERE CURRENT OF <cursor name>
    // 14.14 <update statement: searched>   ::= UPDATE <target table> [ FOR PORTION OF <application time period name> FROM <point in time 1> TO <point in time 2> ]
    //     [ [ AS ] <correlation name> ] SET <set clause list> [ WHERE <search condition> ]
    // 14.15 <set clause list> ::= <set clause> [ { <comma> <set clause> }... ]
    let pUpdateStatement =
        // 14.15 <set clause> ::= <set target> <equals operator> <update source>
        //     | <multiple column assignment> | <mutated set clause>
        let pSetClause =
            // 14.15 <multiple column assignment> ::= <set target list> <equals operator> <assigned row>
            // <set target list> ::= ( <set target> [ { <comma> <set target> }... ] )
            attempt (
                between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                .>> token (pstring "=")
                .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
                |>> MultipleSet
            )
            <|> attempt (
                // 14.15 <mutated set clause> ::= <mutated target> <period> <method name>
                // <mutated target> ::= <object column> | <mutated set clause>
                // <set clause> ::= <mutated set clause> <equals operator> <update source>
                pIdentifierExpr .>>. many1 (token (pstring ".") >>. pIdentifierExpr)
                .>> token (pstring "=")
                .>>. (pDefaultValue <|> pExpression)
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
            pIdentifierExpr .>> token (pstring "=") .>>. (pDefaultValue <|> pExpression)
            |>> SingleSet)

        pKeyword "UPDATE" >>. pQualifiedName
        .>>. opt (attempt pPortionOf)
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpr)
        .>> pKeyword "SET"
        .>>. sepBy1 pSetClause (token (pstring ","))
        .>>. opt pWhereClause
        |>> fun ((((table, portion), alias), sets), whr) ->
            let cursor, where =
                match whr with
                | Some(c, w) -> c, w
                | None -> None, None

            { Table = table
              TableAlias = alias
              Set = sets
              Where = where
              PortionOf = portion
              Cursor = cursor }
            |> Update

    // 14.8 <delete statement: positioned> ::= DELETE FROM <target table> [ [ AS ] <correlation name> ] WHERE CURRENT OF <cursor name>
    // 14.9 <delete statement: searched>   ::= DELETE FROM <target table>
    //     [ FOR PORTION OF <application time period name> FROM <point in time 1> TO <point in time 2> ]
    //     [ [ AS ] <correlation name> ] [ WHERE <search condition> ]
    let pDeleteStatement =
        pKeyword "DELETE" >>. pKeyword "FROM" >>. pQualifiedName
        .>>. opt (attempt pPortionOf)
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpr)
        .>>. opt pWhereClause
        |>> fun (((table, portion), alias), whr) ->
            let cursor, where =
                match whr with
                | Some(c, w) -> c, w
                | None -> None, None

            { Table = table
              TableAlias = alias
              Where = where
              PortionOf = portion
              Cursor = cursor }
            |> Delete

    // 14.12 <merge statement> ::= MERGE INTO <target table> [ [ AS ] <merge correlation name> ]
    //     USING <table reference> ON <search condition> <merge operation specification>
    // <merge operation specification> ::= <merge when clause>...
    // <merge when clause> ::= <merge when matched clause> | <merge when not matched clause>
    let pMergeStatement =
        // 14.12 <merge update specification> ::= UPDATE SET <set clause list>
        // 14.12 <merge delete specification> ::= DELETE
        // 14.12 <merge insert specification> ::= INSERT [ ( <insert column list> ) ] [ <override clause> ] VALUES <merge insert value list>
        let pAction =
            // <merge update specification> ::= UPDATE SET <set clause list> (14.12)
            // <merge delete specification> ::= DELETE
            // <merge insert specification> ::= INSERT [ ( <insert column list> ) ] [ <override clause> ] VALUES <merge insert value list>
            choice
                [ attempt (pKeyword "UPDATE" >>. pKeyword "SET")
                  >>. sepBy1 (pIdentifierExpr .>> token (pstring "=") .>>. pExpression) (token (pstring ","))
                  |>> MergeUpdate
                  pKeyword "DELETE" >>% MergeDelete
                  pKeyword "INSERT"
                  >>. opt (
                      between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                  )
                  .>>. pOverride
                  .>> pKeyword "VALUES"
                  .>>. between
                      (token (pstring "("))
                      (token (pstring ")"))
                      (sepBy1 (pDefaultValue <|> pExpression) (token (pstring ",")))
                  |>> fun ((cols, ovr), values) -> MergeInsert(cols, ovr, values) ]

        // 14.12 <merge when matched clause>     ::= WHEN MATCHED [ AND <search condition> ] THEN <merge update or delete specification>
        // 14.12 <merge when not matched clause> ::= WHEN NOT MATCHED [ AND <search condition> ] THEN <merge insert specification>
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
