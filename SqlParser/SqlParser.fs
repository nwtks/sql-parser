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

    // 14.4 <open statement> / 14.5 <fetch statement> / 14.6 <close statement> / 14.7 <select statement: single row>
    let pCursor =
        choice
            [ attempt pOpenStatement
              attempt pFetchStatement
              attempt pCloseStatement
              attempt pSelectIntoStatement ]

    // 14.8-14.14 <DML statement> ::= <insert statement> | <update statement> | <delete statement> | <merge statement> | <query expression>
    let pDml =
        choice
            [ attempt (pQuery |>> Select)
              pInsertStatement
              pUpdateStatement
              pDeleteStatement
              pMergeStatement ]

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
            [ attempt pExecuteImmediateStatement
              attempt pExecuteStatement
              attempt pPrepareStatement
              attempt pDeallocatePrepareStatement
              attempt pDescribeStatement
              attempt pAllocateDescriptorStatement
              attempt pDeallocateDescriptorStatement
              attempt pGetDescriptorStatement
              attempt pSetDescriptorStatement
              attempt pCopyDescriptorStatement
              attempt pPipeRowStatement ]

    // 23.1 <get diagnostics statement> ::= GET DIAGNOSTICS <SQL diagnostics information>
    let pDiagnostics = choice [ attempt pGetDiagnosticsStatement ]

    // 7.17 <with clause> + <query expression> — WITH [ RECURSIVE ] <with list> <query expression body>
    let pWithStatement =
        pWithClause .>>. (pDml |> withStmtPosition)
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

    let parse sql =
        match run (ws >>. pStatement .>> eof) sql with
        | Success(res, _, _) -> Result.Ok res
        | Failure(msg, error, _) ->
            Result.Error(
                ParseError(
                    msg,
                    { Line = int64 error.Position.Line
                      Column = int64 error.Position.Column }
                )
            )
