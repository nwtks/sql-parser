namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.QueryParser

module DmlParser =
    // 7.12 <where clause> (positioned 14.8/14.13, searched 14.9/14.14)
    //   positioned: WHERE CURRENT OF <cursor name>
    //   searched:   WHERE <search condition>
    // Returns (cursor, search condition) — exactly one is Some.
    let pWhereClause =
        pKeyword "WHERE"
        >>. (attempt (
                 pKeyword "CURRENT" >>. pKeyword "OF" >>. pQualifiedNameExpr
                 |>> fun c -> Some c, None
             )
             <|> (pExpression |>> fun e -> None, Some e))

    // 14.8/14.9/14.13/14.14 <target table> ::= <table name> | ONLY ( <table name> )
    // Returns (name, isOnly).
    let pTargetTable =
        attempt (
            pKeyword "ONLY"
            >>. between (token (pstring "(")) (token (pstring ")")) pQualifiedNameExpr
            |>> fun name -> name, true
        )
        <|> (pQualifiedNameExpr |>> fun name -> name, false)

    // 14.9/14.14 FOR PORTION OF <application time period name> FROM <point in time 1> TO <point in time 2>
    //     FROM <point in time 1> TO <point in time 2>
    let pPortionOf =
        pKeyword "FOR" >>. pKeyword "PORTION" >>. pKeyword "OF" >>. pIdentifierExpr
        .>>. (pKeyword "FROM" >>. (pDatetimeValueExpression .>> ws))
        .>>. (pKeyword "TO" >>. (pDatetimeValueExpression .>> ws))
        |>> fun ((period, fromPoint), toPoint) ->
            { PeriodName = period
              From = fromPoint
              To = toPoint }

    // 20.25/20.27 — the omitted target form is only valid when the statement is
    // positioned through a dynamic cursor and carries no <portion of>, correlation
    // name or search condition.
    let pOmittedTargetGuard clause target cursor where portion alias =
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

    // 14.8 <delete statement: positioned> ::= DELETE FROM <target table> [ [ AS ] <correlation name> ] WHERE CURRENT OF <cursor name>
    // 14.9 <delete statement: searched>   ::= DELETE FROM <target table>
    //     [ FOR PORTION OF <application time period name> FROM <point in time 1> TO <point in time 2> ]
    //     [ [ AS ] <correlation name> ] [ WHERE <search condition> ]
    // 20.25 <preparable dynamic delete statement: positioned> ::= DELETE [ FROM <target table> ]
    //     WHERE CURRENT OF <preparable dynamic cursor name>
    let pDeleteStatement =
        pKeyword "DELETE" >>. opt (attempt (pKeyword "FROM" >>. pTargetTable))
        .>>. opt (attempt pPortionOf)
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpr)
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
            >>. preturn statement

    // 14.11 <override clause> ::= OVERRIDING USER VALUE | OVERRIDING SYSTEM VALUE
    // (None when absent; Some true = USER, Some false = SYSTEM)
    let pOverride =
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

        pKeyword "INSERT" >>. pKeyword "INTO" >>. pQualifiedNameExpr
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

        pKeyword "MERGE" >>. pKeyword "INTO" >>. pTargetTable
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpr)
        .>> pKeyword "USING"
        .>>. pTableReference
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

    // 20.25 <preparable dynamic delete statement: positioned> /
    // 20.27 <preparable dynamic update statement: positioned> omit the <target table>.
    let pOptionalDmlTarget =
        attempt pTargetTable |>> DmlTarget.TableTarget
        <|> preturn DmlTarget.OmittedTarget

    // 14.13 <update statement: positioned> ::= UPDATE <target table> [ [ AS ] <correlation name> ] SET <set clause list> WHERE CURRENT OF <cursor name>
    // 14.14 <update statement: searched>   ::= UPDATE <target table> [ FOR PORTION OF <application time period name> FROM <point in time 1> TO <point in time 2> ]
    //     [ [ AS ] <correlation name> ] SET <set clause list> [ WHERE <search condition> ]
    // 20.27 <preparable dynamic update statement: positioned> ::= UPDATE [ <target table> ] SET <set clause list>
    //     WHERE CURRENT OF <preparable dynamic cursor name>
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

        pKeyword "UPDATE" >>. pOptionalDmlTarget
        .>>. opt (attempt pPortionOf)
        .>>. opt (opt (pKeyword "AS") >>. pIdentifierExpr)
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
            >>. preturn statement
