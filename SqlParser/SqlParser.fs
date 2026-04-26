namespace SqlParser

open FParsec
open SqlParser.Ast
open SqlParser.Lexer
open SqlParser.QueryParser
open SqlParser.DmlParser
open SqlParser.DdlParser

module SqlParser =
    let withStmtPosition (p: Parser<StatementKind, unit>) : Parser<Statement, unit> =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    let pStatement, pStatementRef = createParserForwardedToRef<Statement, unit> ()

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
        .>>. pStatement
        |>> fun ((recu, ctes), stmt) ->
            { Kind = WithStatement(Option.defaultValue false recu, ctes, stmt.Kind)
              Pos = stmt.Pos }

    pStatementRef.Value <-
        choice
            [ attempt pWithStatement
              attempt (pQuery |>> Select |> withStmtPosition)
              attempt (pInsertStatement |> withStmtPosition)
              attempt (pUpdateStatement |> withStmtPosition)
              attempt (pDeleteStatement |> withStmtPosition)
              attempt (pMergeStatement |> withStmtPosition)
              attempt (pCreateTableStatement |> withStmtPosition)
              attempt (pCreateIndexStatement |> withStmtPosition)
              attempt (pCreateViewStatement |> withStmtPosition)
              attempt (pDropStatement |> withStmtPosition)
              attempt (pAlterTableStatement |> withStmtPosition)
              attempt (pTruncateStatement |> withStmtPosition) ]

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
