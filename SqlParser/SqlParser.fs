namespace SqlParser

open FParsec
open SqlParser.Ast
open SqlParser.Lexer
open SqlParser.QueryParser
open SqlParser.DmlParser
open SqlParser.DdlParser

module SqlParser =
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
        let pCte =
            pIdentifier
            .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifier (token (pstring ","))))
            .>> pKeyword "AS"
            .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
            |>> fun ((name, cols), q) ->
                { Name = name
                  Columns = cols
                  Query = q }

        pKeyword "WITH" >>. opt (pKeyword "RECURSIVE" >>% true)
        .>>. sepBy1 pCte (token (pstring ","))
        .>>. (pDml |> withStmtPosition)
        |>> fun ((recu, ctes), stmt) ->
            { Kind = WithStatement(Option.defaultValue false recu, ctes, stmt.Kind)
              Pos = stmt.Pos }

    let pStatement, pStatementRef = createParserForwardedToRef<Statement, unit> ()

    pStatementRef.Value <-
        choice
            [ attempt pWithStatement
              attempt (pDml |> withStmtPosition)
              attempt (pDdl |> withStmtPosition) ]

    let parse sql =
        match run (ws >>. pStatement .>> eof) sql with
        | Success(res, _, _) -> Choice1Of2 res
        | Failure(msg, error, _) ->
            Choice2Of2(
                ParseError(
                    msg,
                    { Line = int64 error.Position.Line
                      Column = int64 error.Position.Column }
                )
            )
