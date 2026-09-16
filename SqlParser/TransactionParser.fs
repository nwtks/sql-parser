namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module TransactionParser =
    // 17.3 <level of isolation> ::= READ UNCOMMITTED | READ COMMITTED | REPEATABLE READ | SERIALIZABLE
    let pLevelOfIsolation =
        choice
            [ attempt (pKeyword "READ" >>. pKeyword "UNCOMMITTED" >>% ReadUncommitted)
              attempt (pKeyword "READ" >>. pKeyword "COMMITTED" >>% ReadCommitted)
              attempt (pKeyword "REPEATABLE" >>. pKeyword "READ" >>% RepeatableRead)
              attempt (pKeyword "SERIALIZABLE" >>% Serializable) ]

    // 17.3 <transaction mode> ::= <isolation level> | <transaction access mode> | <diagnostics size>
    // <isolation level> ::= ISOLATION LEVEL <level of isolation>
    // <transaction access mode> ::= READ ONLY | READ WRITE
    // <diagnostics size> ::= DIAGNOSTICS SIZE <number of conditions>
    let pTransactionMode =
        choice
            [ attempt (pKeyword "ISOLATION" >>. pKeyword "LEVEL" >>. pLevelOfIsolation |>> Isolation)
              attempt (pKeyword "READ" >>. pKeyword "ONLY" >>% AccessMode ReadOnly)
              attempt (pKeyword "READ" >>. pKeyword "WRITE" >>% AccessMode ReadWrite)
              // <number of conditions> ::= <simple value specification> (strict)
              attempt (
                  pKeyword "DIAGNOSTICS" >>. pKeyword "SIZE" >>. pSimpleValueSpecification
                  |>> DiagnosticsSize
              ) ]

    // 17.3 <transaction characteristics> ::= <transaction mode> [ { <comma> <transaction mode> }... ]
    let pTransactionCharacteristics = sepBy1 pTransactionMode (token (pstring ","))

    // 17.1 <start transaction statement> ::= START TRANSACTION [ <transaction characteristics> ]
    let pStartTransactionStatement =
        pKeyword "START" >>. pKeyword "TRANSACTION" >>. opt pTransactionCharacteristics
        |>> fun modes -> StartTransaction(Option.defaultValue [] modes)

    // 17.2 <set transaction statement> ::= SET [ LOCAL ] TRANSACTION <transaction characteristics>
    let pSetTransactionStatement =
        pKeyword "SET" >>. opt (pKeyword "LOCAL") .>> pKeyword "TRANSACTION"
        .>>. pTransactionCharacteristics
        |>> fun (isLocal, modes) -> SetTransaction(Option.isSome isLocal, modes)

    // 17.4 <set constraints mode statement> ::= SET CONSTRAINTS <constraint name list> { DEFERRED | IMMEDIATE }
    // <constraint name list> ::= ALL | <constraint name>  [ { <comma>  <constraint name>  }... ]
    let pSetConstraintsStatement =
        pKeyword "SET" >>. pKeyword "CONSTRAINTS"
        .>>. (attempt (pKeyword "ALL" >>% None)
              <|> (sepBy1 pIdentifierExpression (token (pstring ",")) |>> Some))
        .>>. (attempt (pKeyword "DEFERRED" >>% true) <|> (pKeyword "IMMEDIATE" >>% false))
        |>> fun ((_, names), deferred) -> SetConstraints(names, deferred)

    // 17.5 <savepoint statement> ::= SAVEPOINT <savepoint specifier>
    let pSavepointStatement =
        pKeyword "SAVEPOINT" >>. pIdentifierExpression |>> Savepoint

    // 17.6 <release savepoint statement> ::= RELEASE SAVEPOINT <savepoint specifier>
    let pReleaseSavepointStatement =
        pKeyword "RELEASE" >>. pKeyword "SAVEPOINT" >>. pIdentifierExpression
        |>> ReleaseSavepoint

    // 17.7/17.8 <commit/rollback> chain option ::= AND [ NO ] CHAIN
    let pChain =
        attempt (pKeyword "AND" >>. pKeyword "NO" >>. pKeyword "CHAIN" >>% Some false)
        <|> attempt (pKeyword "AND" >>. pKeyword "CHAIN" >>% Some true)
        <|> preturn None

    // 17.7 <commit statement> ::= COMMIT [ WORK ] [ AND [ NO ] CHAIN ]
    let pCommitStatement =
        pKeyword "COMMIT" >>. opt (pKeyword "WORK") >>. pChain |>> Commit

    // 17.8 <rollback statement> ::= ROLLBACK [ WORK ] [ AND [ NO ] CHAIN ] [ <savepoint clause> ]
    // <savepoint clause> ::= TO SAVEPOINT <savepoint specifier>
    let pRollbackStatement =
        pKeyword "ROLLBACK" >>. opt (pKeyword "WORK") >>. pChain
        .>>. opt (pKeyword "TO" >>. pKeyword "SAVEPOINT" >>. pIdentifierExpression)
        |>> fun (chain, savepoint) -> Rollback(chain, savepoint)
