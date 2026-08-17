namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module TransactionParser =
    let pIsolationLevel =
        choice
            [ attempt (pKeyword "READ" >>. pKeyword "UNCOMMITTED" >>% ReadUncommitted)
              attempt (pKeyword "READ" >>. pKeyword "COMMITTED" >>% ReadCommitted)
              attempt (pKeyword "REPEATABLE" >>. pKeyword "READ" >>% RepeatableRead)
              attempt (pKeyword "SERIALIZABLE" >>% Serializable) ]

    let pTransactionMode =
        choice
            [ attempt (pKeyword "ISOLATION" >>. pKeyword "LEVEL" >>. pIsolationLevel |>> Isolation)
              attempt (pKeyword "READ" >>. pKeyword "ONLY" >>% AccessMode ReadOnly)
              attempt (pKeyword "READ" >>. pKeyword "WRITE" >>% AccessMode ReadWrite) ]

    let pTransactionCharacteristics = sepBy1 pTransactionMode (token (pstring ","))

    let pStartTransactionStatement =
        pKeyword "START" >>. pKeyword "TRANSACTION" >>. opt pTransactionCharacteristics
        |>> fun modes -> StartTransaction(Option.defaultValue [] modes)

    let pSetTransactionStatement =
        pKeyword "SET" >>. opt (pKeyword "LOCAL") .>> pKeyword "TRANSACTION"
        .>>. pTransactionCharacteristics
        |>> fun (isLocal, modes) -> SetTransaction(Option.isSome isLocal, modes)

    let pSetConstraintsStatement =
        pKeyword "SET" >>. pKeyword "CONSTRAINTS"
        .>>. (attempt (pKeyword "ALL" >>% None)
              <|> (sepBy1 pIdentifierExpr (token (pstring ",")) |>> Some))
        .>>. (attempt (pKeyword "DEFERRED" >>% true) <|> (pKeyword "IMMEDIATE" >>% false))
        |>> fun ((_, names), deferred) -> SetConstraints(names, deferred)

    let pSavepointStatement = pKeyword "SAVEPOINT" >>. pIdentifierExpr |>> Savepoint

    let pReleaseSavepointStatement =
        pKeyword "RELEASE" >>. pKeyword "SAVEPOINT" >>. pIdentifierExpr
        |>> ReleaseSavepoint

    let pChain =
        attempt (pKeyword "AND" >>. pKeyword "NO" >>. pKeyword "CHAIN" >>% Some false)
        <|> attempt (pKeyword "AND" >>. pKeyword "CHAIN" >>% Some true)
        <|> preturn None

    let pCommitStatement =
        pKeyword "COMMIT" >>. opt (pKeyword "WORK") >>. pChain |>> Commit

    let pRollbackStatement =
        pKeyword "ROLLBACK" >>. opt (pKeyword "WORK") >>. pChain
        .>>. opt (pKeyword "TO" >>. pKeyword "SAVEPOINT" >>. pIdentifierExpr)
        |>> fun (chain, savepoint) -> Rollback(chain, savepoint)

    let pTransactionStatement =
        choice
            [ attempt pStartTransactionStatement
              attempt pSetTransactionStatement
              attempt pSetConstraintsStatement
              attempt pSavepointStatement
              attempt pReleaseSavepointStatement
              attempt pCommitStatement
              attempt pRollbackStatement ]
