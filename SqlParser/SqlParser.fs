namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.DmlParser
open SqlParser.DdlParser
open SqlParser.ExpressionParser
open SqlParser.QueryParser

module SqlParser =
    let pStatement, pStatementRef = createParserForwardedToRef<Statement, unit> ()

    let withStmtPosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    let pDml =
        choice
            [ attempt (pQuery |>> Select)
              pInsertStatement
              pUpdateStatement
              pDeleteStatement
              pMergeStatement ]

    let pDdl =
        choice
            [ attempt pCreateTableStatement
              attempt pCreateIndexStatement
              attempt pCreateViewStatement
              pDropStatement
              pAlterTableStatement
              pTruncateStatement ]

    let pWithStatement =
        pWithClause .>>. (pDml |> withStmtPosition)
        |>> fun ((recu, ctes), stmt) ->
            { Kind = WithStatement(recu, ctes, stmt.Kind)
              Pos = stmt.Pos }

    pStatementRef.Value <-
        choice
            [ attempt pWithStatement
              attempt (pDml |> withStmtPosition)
              attempt (pDdl |> withStmtPosition) ]

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
