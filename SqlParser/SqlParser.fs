namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.DmlParser
open SqlParser.DdlParser
open SqlParser.ExpressionParser
open SqlParser.QueryParser
open SqlParser.TransactionParser
open SqlParser.ControlParser
open SqlParser.RoutineParser
open SqlParser.TypeParser
open SqlParser.SessionParser
open SqlParser.CursorParser
open SqlParser.ConnectionParser
open SqlParser.DiagnosticsParser
open SqlParser.DynamicParser

module SqlParser =
    // 4 <SQL statement> — top-level dispatcher (wired via pStatementRef)
    let pStatement, pStatementRef = createParserForwardedToRef<Statement, unit> ()

    let withStmtPosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // <data change statement> (7.6) — the DML statements allowed inside a
    // <data change delta table> (FINAL|NEW|OLD TABLE ( ... )). Wired here because
    // the DML parsers live in DmlParser.fs, which is compiled after QueryParser.fs.
    pDataChangeStatementRef.Value <- choice [ pInsertStatement; pUpdateStatement; pDeleteStatement; pMergeStatement ]

    // 11 <SQL-schema statement> — DDL dispatcher
    let pDdl =
        choice
            [ attempt pCreateTableStatement
              attempt pCreateViewStatement
              attempt pCreateRoleStatement
              attempt pCreateSequenceStatement
              attempt pAlterSequenceStatement
              attempt pCreateSchemaStatement
              attempt pCreateDomainStatement
              attempt pAlterDomainStatement
              attempt pCreateCharacterSetStatement
              attempt pCreateCollationStatement
              attempt pCreateTransliterationStatement
              attempt pCreateAssertionStatement
              attempt pCreateCastStatement
              attempt pCreateOrderingStatement
              attempt pCreateTransformStatement
              attempt pAlterTransformStatement
              attempt pCreateTypeStatement
              attempt pCreateProcedureStatement
              attempt pCreateFunctionStatement
              attempt pAlterTypeStatement
              attempt pAlterRoutineStatement
              attempt pCreateTriggerStatement
              attempt pGrantStatement
              attempt pRevokeStatement
              pDropStatement
              pAlterTableStatement
              pTruncateStatement ]

    // 11.1 <schema element> ::= <table definition> | <view definition> | <domain definition>
    //     | <character set definition> | <collation definition> | <transliteration definition>
    //     | <assertion definition> | <trigger definition> | <user-defined type definition>
    //     | <user-defined cast definition> | <user-defined ordering definition>
    //     | <transform definition> | <schema routine> | <sequence generator definition>
    //     | <grant statement> | <role definition>
    // Only CREATE-family elements and GRANT are schema elements — DROP / ALTER /
    // TRUNCATE / REVOKE are NOT (wired here so the CREATE SCHEMA parser defined in
    // DdlParser.fs can consume nested schema elements).
    pSchemaElementImpl.Value <-
        choice
            [ attempt pCreateTableStatement
              attempt pCreateViewStatement
              attempt pCreateRoleStatement
              attempt pCreateSequenceStatement
              attempt pCreateDomainStatement
              attempt pCreateCharacterSetStatement
              attempt pCreateCollationStatement
              attempt pCreateTransliterationStatement
              attempt pCreateAssertionStatement
              attempt pCreateCastStatement
              attempt pCreateOrderingStatement
              attempt pCreateTransformStatement
              attempt pCreateTypeStatement
              attempt pCreateProcedureStatement
              attempt pCreateFunctionStatement
              attempt pCreateTriggerStatement
              attempt pGrantStatement ]

    // <SQL procedure statement> / <triggered SQL statement> used inside routine
    // bodies and triggered actions may be any statement (wired here so the
    // routine/trigger parsers defined in RoutineParser.fs can consume them).
    pRoutineBodyStatementRefImpl.Value <- pStatement

    // 14.1 <declare cursor> / 14.4 <open statement> / 14.5 <fetch statement> / 14.6 <close statement>
    // 14.7 <select statement: single row> / 14.16 <temporary table declaration>
    // 14.17 <free locator statement> / 14.18 <hold locator statement>
    let pCursor =
        choice
            [ attempt pTemporaryTableDeclarationStatement
              attempt pDeclareCursorStatement
              attempt pFreeLocatorStatement
              attempt pHoldLocatorStatement
              attempt pOpenStatement
              attempt pFetchStatement
              attempt pCloseStatement
              attempt pSelectIntoStatement ]

    // 14.8-14.15 <DML statement> ::= <insert statement> | <update statement> | <delete statement> | <merge statement> | <query expression>
    let pDml =
        choice
            [ attempt (pQuery |>> Select)
              pInsertStatement
              pUpdateStatement
              pDeleteStatement
              pMergeStatement ]

    // 22.1 <directly executable statement> — only the *searched* forms of <update statement>
    // (14.14) and <delete statement> (14.9) are directly executable. The positioned forms
    // (14.13 / 14.8, entered through WHERE CURRENT OF) are reachable only from a
    // <SQL procedure statement> (13.4), so they are excluded from pDirectSqlStatement.
    let pSearchedUpdateStatement =
        pUpdateStatement
        >>= fun stmt ->
            match stmt with
            | StatementKind.Update { Cursor = Some _ } ->
                fail "a positioned <update statement> (14.13) is not directly executable (22.1)."
            | _ -> preturn stmt

    let pSearchedDeleteStatement =
        pDeleteStatement
        >>= fun stmt ->
            match stmt with
            | StatementKind.Delete { Cursor = Some _ } ->
                fail "a positioned <delete statement> (14.8) is not directly executable (22.1)."
            | _ -> preturn stmt

    // 16 <SQL control statement> ::= <call statement> | <return statement>
    let pControl = choice [ attempt pCallStatement; attempt pReturnStatement ]

    // 17 <SQL-transaction statement> ::= <start transaction statement> | <set transaction statement>
    //     | <set constraints mode statement> | <savepoint statement> | <release savepoint statement>
    //     | <commit statement> | <rollback statement> — dispatcher
    let pTransactionStatement =
        choice
            [ attempt pStartTransactionStatement
              attempt pSetTransactionStatement
              attempt pSetConstraintsStatement
              attempt pSavepointStatement
              attempt pReleaseSavepointStatement
              attempt pCommitStatement
              attempt pRollbackStatement ]

    // 18.1 <connect statement> / 18.2 <set connection statement> / 18.3 <disconnect statement>
    let pConnection =
        choice
            [ attempt pConnectStatement
              attempt pSetConnectionStatement
              attempt pDisconnectStatement ]

    // 19 <SQL-session statement> — dispatcher
    let pSession =
        choice
            [ attempt pSetRoleStatement
              attempt pSetSessionAuthorizationStatement
              attempt pSetTimeZoneStatement
              attempt pSetSessionCharacteristicsStatement
              attempt pSetCatalogStatement
              attempt pSetSchemaStatement
              attempt pSetNamesStatement
              attempt pSetPathStatement
              attempt pSetTransformGroupStatement
              attempt pSetSessionCollationStatement ]

    // 20 <SQL-dynamic statement> — dispatcher
    let pDynamic =
        choice
            [ attempt pDynamicDeclareCursorStatement
              attempt pExecuteImmediateStatement
              attempt pExecuteStatement
              attempt pPrepareStatement
              attempt pDeallocatePrepareStatement
              attempt pDescribeStatement
              attempt pAllocateDescriptorStatement
              attempt pAllocateExtendedDynamicCursorStatement
              attempt pAllocateReceivedCursorStatement
              attempt pDeallocateDescriptorStatement
              attempt pGetDescriptorStatement
              attempt pSetDescriptorStatement
              attempt pCopyDescriptorStatement
              attempt pPipeRowStatement ]

    // 23.1 <get diagnostics statement> ::= GET DIAGNOSTICS <SQL diagnostics information>
    let pDiagnostics = choice [ attempt pGetDiagnosticsStatement ]

    // 7.17 <with clause> + <query expression> — WITH [ RECURSIVE ] <with list>
    // <query expression body>. The body is a query only: <with clause> is a prefix of
    // <query expression> (7.17), so `WITH ... INSERT/UPDATE/DELETE/MERGE` is not a valid
    // <SQL statement> and is rejected by both entry points.
    let pWithStatement =
        pWithClause .>>. ((pQuery |>> Select) |> withStmtPosition)
        |>> fun ((recu, ctes), stmt) ->
            { Kind = WithStatement(recu, ctes, stmt.Kind)
              Pos = stmt.Pos }

    pStatementRef.Value <-
        choice
            [ attempt pWithStatement
              attempt (pCursor |> withStmtPosition)
              attempt (pDml |> withStmtPosition)
              attempt (pDdl |> withStmtPosition)
              attempt (pControl |> withStmtPosition)
              attempt (pTransactionStatement |> withStmtPosition)
              attempt (pConnection |> withStmtPosition)
              attempt (pSession |> withStmtPosition)
              attempt (pDynamic |> withStmtPosition)
              attempt (pDiagnostics |> withStmtPosition) ]

    // 5.1 <semicolon> ::= ;
    let pSemicolon = token (pstring ";")

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
    let pDirectSqlStatement =
        choice
            [ attempt pWithStatement
              attempt ((pQuery |>> Select) |> withStmtPosition)
              attempt (pInsertStatement |> withStmtPosition)
              attempt (pSearchedUpdateStatement |> withStmtPosition)
              attempt (pSearchedDeleteStatement |> withStmtPosition)
              attempt (pMergeStatement |> withStmtPosition)
              attempt (pTemporaryTableDeclarationStatement |> withStmtPosition)
              attempt (pDdl |> withStmtPosition)
              attempt (pTransactionStatement |> withStmtPosition)
              attempt (pConnection |> withStmtPosition)
              attempt (pSession |> withStmtPosition) ]
        .>> pSemicolon

    let private runParser (p: Parser<Statement, unit>) sql =
        match run p sql with
        | Success(res, _, _) -> Result.Ok res
        | Failure(msg, error, _) ->
            Result.Error(
                ParseError(
                    msg,
                    { Line = int64 error.Position.Line
                      Column = int64 error.Position.Column }
                )
            )

    /// 22.1 — parses a <direct SQL statement> (the directly executable statement families).
    let parse sql =
        runParser (ws >>. pDirectSqlStatement .>> eof) sql

    /// 13.4 — parses any <SQL statement> the library supports: a superset of the grammar's
    /// <SQL executable statement>, which also accepts DECLARE CURSOR (14.1) and
    /// <temporary table declaration> (14.16). The trailing <semicolon> is mandatory.
    let parseStatement sql =
        runParser (ws >>. pStatement .>> pSemicolon .>> eof) sql
