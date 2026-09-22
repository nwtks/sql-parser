namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.QueryParser
open SqlParser.SchemaParser
open SqlParser.AccessControlParser
open SqlParser.DataManipulationParser
open SqlParser.ControlParser
open SqlParser.TransactionParser
open SqlParser.ConnectionParser
open SqlParser.SessionParser
open SqlParser.DynamicParser
open SqlParser.DiagnosticsParser

module SqlParser =
    let private withStmtPosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 4 <SQL statement> — the shared top-level forward ref is declared in SchemaParser.fs
    // (`pStatement` / `pStatementRef`) and wired at the bottom of this module.

    // 5.1 <semicolon> ::= ;
    let private pSemicolon = token (pstring ";")

    // <data change statement> (7.6) — the DML statements allowed inside a
    // <data change delta table> (FINAL|NEW|OLD TABLE ( ... )). Wired here because
    // the DML parsers live in DataManipulationParser.fs, which is compiled after QueryParser.fs.
    pDataChangeStatementRef.Value <- choice [ pInsertStatement; pUpdateStatement; pDeleteStatement; pMergeStatement ]

    // 7.17 <with clause> + <query expression> — WITH [ RECURSIVE ] <with list>
    // <query expression body>. The body is a query only: <with clause> is a prefix of
    // <query expression> (7.17), so `WITH ... INSERT/UPDATE/DELETE/MERGE` is never a valid
    // <SQL statement>. A WITH-prefixed query is a <direct select statement: multiple rows>
    // (22.2), so it is reachable only from `parse` (22.1), not from `parseStatement` (13.4).
    let private pWithStatement =
        pWithClause .>>. (pQueryExpression |>> Select |> withStmtPosition)
        |>> fun ((recu, ctes), stmt) ->
            { Kind = WithStatement(recu, ctes, stmt.Kind)
              Pos = stmt.Pos }

    // 8 Predicates — wire the forward refs declared in ExpressionParser.fs to the parsers defined
    // in PredicateParser.fs. Assigning here (rather than in PredicateParser.fs itself) forces
    // PredicateParser's module initialiser to run before the first parse.
    pPredicateRef.Value <- PredicateParser.pPredicate opp.ExpressionParser
    pPredicateNoBooleanTestRef.Value <- PredicateParser.pPredicateNoBooleanTest opp.ExpressionParser
    pWhenOperandPart2Ref.Value <- PredicateParser.pWhenOperandPart2 opp.ExpressionParser

    // 11 <SQL-schema statement> — DDL dispatcher
    // 11.1 <schema element> ::= <table definition> | <view definition> | <domain definition>
    //     | <character set definition> | <collation definition> | <transliteration definition>
    //     | <assertion definition> | <trigger definition> | <user-defined type definition>
    //     | <user-defined cast definition> | <user-defined ordering definition>
    //     | <transform definition> | <schema routine> | <sequence generator definition>
    //     | <grant statement> | <role definition>
    // Only CREATE-family elements and GRANT are schema elements — DROP / ALTER /
    // TRUNCATE / REVOKE are NOT. It is a local binding because the CREATE SCHEMA parser
    // (SchemaParser.fs) takes it as a parameter and is its only consumer.
    let private pSqlSchemaStatement =
        let pSchemaElement =
            choice
                [ attempt pTableDefinition
                  attempt pViewDefinition
                  attempt pCreateRoleStatement
                  attempt pSequenceGeneratorDefinition
                  attempt pDomainDefinition
                  attempt pCharacterSetDefinition
                  attempt pCollationDefinition
                  attempt pTransliterationDefinition
                  attempt pAssertionDefinition
                  attempt pUserDefinedCastDefinition
                  attempt pUserDefinedOrderingDefinition
                  attempt pTransformDefinition
                  attempt pUserDefinedTypeDefinition
                  attempt pSchemaProcedure
                  attempt pSchemaFunction
                  attempt pSchemaMethod
                  attempt pTriggerDefinition
                  attempt pGrantStatement ]

        choice
            [ attempt pTableDefinition
              attempt pViewDefinition
              attempt pCreateRoleStatement
              attempt pSequenceGeneratorDefinition
              attempt pAlterSequenceStatement
              attempt (pCreateSchemaStatement pSchemaElement)
              attempt pDomainDefinition
              attempt pAlterDomainStatement
              attempt pCharacterSetDefinition
              attempt pCollationDefinition
              attempt pTransliterationDefinition
              attempt pAssertionDefinition
              attempt pUserDefinedCastDefinition
              attempt pUserDefinedOrderingDefinition
              attempt pTransformDefinition
              attempt pAlterTransformStatement
              attempt pUserDefinedTypeDefinition
              attempt pSchemaProcedure
              attempt pSchemaFunction
              attempt pSchemaMethod
              attempt pAlterTypeStatement
              attempt pAlterRoutineStatement
              attempt pTriggerDefinition
              attempt pGrantStatement
              attempt pRevokeStatement
              pDropStatement
              pAlterTableStatement
              pTruncateTableStatement ]

    // 13.4 <SQL data statement> ::= <open statement> | <fetch statement> | <close statement>
    //     | <select statement: single row> | <free locator statement> | <hold locator statement>
    //     | <SQL data change statement>
    // <declare cursor> (14.1, an SQL-client module statement) and <temporary table declaration>
    // (14.16, a <direct SQL data statement> only) are deliberately absent — they are not
    // <SQL procedure statement>s.
    let private pSqlDataStatement =
        choice
            [ attempt pFreeLocatorStatement
              attempt pHoldLocatorStatement
              attempt pOpenStatement
              attempt pFetchStatement
              attempt pCloseStatement
              attempt pSelectStatementSingleRow ]

    // 13.4 <SQL data change statement> ::= <delete statement: positioned> | <delete statement: searched>
    //     | <insert statement> | <update statement: positioned> | <update statement: searched>
    //     | <truncate table statement> | <merge statement>
    // TRUNCATE is dispatched by pSqlSchemaStatement (a routing convenience — it is not an
    // 11.x <schema element>, but both entry points share that choice).
    // A bare multi-row <query expression> (22.2) is not a <SQL data change statement>.
    let private pSqlDataChangeStatement =
        choice
            [ pInsertStatement
              pUpdateStatement
              pDeleteStatement
              pMergeStatement ]

    // 16 <SQL control statement> ::= <call statement> | <return statement>
    let private pSqlControlStatement =
        choice [ attempt pCallStatement; attempt pReturnStatement ]

    // 17 <SQL-transaction statement> ::= <start transaction statement> | <set transaction statement>
    //     | <set constraints mode statement> | <savepoint statement> | <release savepoint statement>
    //     | <commit statement> | <rollback statement> — dispatcher
    let private pSqlTransactionStatement =
        choice
            [ attempt pStartTransactionStatement
              attempt pSetTransactionStatement
              attempt pSetConstraintsStatement
              attempt pSavepointStatement
              attempt pReleaseSavepointStatement
              attempt pCommitStatement
              attempt pRollbackStatement ]

    // 18.1 <connect statement> / 18.2 <set connection statement> / 18.3 <disconnect statement>
    let private pSqlConnectionStatement =
        choice
            [ attempt pConnectStatement
              attempt pSetConnectionStatement
              attempt pDisconnectStatement ]

    // 19 <SQL-session statement> — dispatcher
    let private pSqlSessionStatement =
        choice
            [ attempt pSetRoleStatement
              attempt pSetSessionUserIdentifierStatement
              attempt pSetLocalTimeZoneStatement
              attempt pSetSessionCharacteristicsStatement
              attempt pSetCatalogStatement
              attempt pSetSchemaStatement
              attempt pSetNamesStatement
              attempt pSetPathStatement
              attempt pSetTransformGroupStatement
              attempt pSetSessionCollationStatement ]

    // 20 <SQL-dynamic statement> — dispatcher
    let private pSqlDynamicStatement =
        choice
            [ attempt pDynamicDeclareCursorStatement
              attempt pExecuteImmediateStatement
              attempt pExecuteStatement
              attempt pPrepareStatement
              attempt pDeallocatePreparedStatement
              attempt pDescribeStatement
              attempt pAllocateDescriptorStatement
              attempt pAllocateExtendedDynamicCursorStatement
              attempt pAllocateReceivedCursorStatement
              attempt pDeallocateDescriptorStatement
              attempt pGetDescriptorStatement
              attempt pSetDescriptorStatement
              attempt pCopyDescriptorStatement
              attempt pPipeRowStatement ]

    // 22.1 <directly executable statement> — only the *searched* forms of <update statement>
    // (14.14) and <delete statement> (14.9) are directly executable. The positioned forms
    // (14.13 / 14.8, entered through WHERE CURRENT OF) are reachable only from a
    // <SQL procedure statement> (13.4), so they are excluded from pDirectSqlStatement.
    let private pSearchedUpdateStatement =
        pUpdateStatement
        >>= fun stmt ->
            match stmt with
            | Update { Cursor = Some _ } ->
                fail "a positioned <update statement> (14.13) is not directly executable (22.1)."
            | _ -> preturn stmt

    let private pSearchedDeleteStatement =
        pDeleteStatement
        >>= fun stmt ->
            match stmt with
            | Delete { Cursor = Some _ } ->
                fail "a positioned <delete statement> (14.8) is not directly executable (22.1)."
            | _ -> preturn stmt

    // 22.1 <direct SQL statement> ::= <directly executable statement> <semicolon>
    // <directly executable statement> ::= <direct SQL data statement> | <SQL schema statement>
    //     | <SQL transaction statement> | <SQL connection statement> | <SQL session statement>
    //     | <direct implementation-defined statement>
    // <direct SQL data statement> ::= <delete statement: searched> | <direct select statement:
    //     multiple rows> | <insert statement> | <update statement: searched> | <truncate table
    //     statement> | <merge statement> | <temporary table declaration>
    // The final alternative (<direct implementation-defined statement>) is not implemented, so
    // it is omitted. OPEN/FETCH/CLOSE, SELECT INTO, FREE/HOLD LOCATOR, DECLARE CURSOR,
    // CALL/RETURN, GET DIAGNOSTICS and every dynamic-SQL statement are NOT directly executable
    // — use `parseStatement` (13.4) for those.
    let private pDirectSqlStatement =
        choice
            [ attempt pWithStatement
              attempt (pQueryExpression |>> Select |> withStmtPosition)
              attempt (pInsertStatement |> withStmtPosition)
              attempt (pSearchedUpdateStatement |> withStmtPosition)
              attempt (pSearchedDeleteStatement |> withStmtPosition)
              attempt (pMergeStatement |> withStmtPosition)
              attempt (pTemporaryTableDeclaration |> withStmtPosition)
              attempt (pSqlSchemaStatement |> withStmtPosition)
              attempt (pSqlTransactionStatement |> withStmtPosition)
              attempt (pSqlConnectionStatement |> withStmtPosition)
              attempt (pSqlSessionStatement |> withStmtPosition) ]
        .>> pSemicolon

    // 23.1 <get diagnostics statement> ::= GET DIAGNOSTICS <SQL diagnostics information>
    let private pSqlDiagnosticsStatement = choice [ attempt pGetDiagnosticsStatement ]

    // 13.4 <SQL procedure statement> ::= <SQL executable statement>
    // <SQL executable statement> ::= <SQL schema statement> | <SQL data statement>
    //     | <SQL control statement> | <SQL transaction statement> | <SQL connection statement>
    //     | <SQL session statement> | <SQL diagnostics statement> | <SQL dynamic statement>
    // A WITH-prefixed query and a bare multi-row SELECT are 22.x forms, not 13.4 statements.
    pStatementRef.Value <-
        choice
            [ attempt (pSqlDataStatement |> withStmtPosition)
              attempt (pSqlDataChangeStatement |> withStmtPosition)
              attempt (pSqlSchemaStatement |> withStmtPosition)
              attempt (pSqlControlStatement |> withStmtPosition)
              attempt (pSqlTransactionStatement |> withStmtPosition)
              attempt (pSqlConnectionStatement |> withStmtPosition)
              attempt (pSqlSessionStatement |> withStmtPosition)
              attempt (pSqlDynamicStatement |> withStmtPosition)
              attempt (pSqlDiagnosticsStatement |> withStmtPosition) ]

    // Every parser validates its input, so a malformed value yields an FParsec `Failure`. Should an
    // exception still escape (e.g. a numeric conversion a source check missed), it surfaces as
    // `Result.Error` rather than being thrown out of the public API — see docs/gotchas.md.
    let private runParser p sql =
        let toError msg line column =
            Result.Error(ParseError(msg, { Line = line; Column = column }))

        try
            match run p sql with
            | Success(res, _, _) -> Result.Ok res
            | Failure(msg, error, _) -> toError msg (int64 error.Position.Line) (int64 error.Position.Column)
        with ex ->
            toError ex.Message 0L 0L

    /// 22.1 — parses a <direct SQL statement> (the directly executable statement families).
    let parse sql =
        runParser (ws >>. pDirectSqlStatement .>> eof) sql

    /// 13.4 — parses a <SQL procedure statement> (<SQL executable statement>). The trailing
    /// <semicolon> is mandatory. Notably absent: DECLARE CURSOR (14.1, SQL-client modules),
    /// <temporary table declaration> (14.16) and multi-row SELECT / WITH — those are direct
    /// SQL (22.1) forms, reachable only through `parse`.
    let parseStatement sql =
        runParser (ws >>. pStatement .>> pSemicolon .>> eof) sql
