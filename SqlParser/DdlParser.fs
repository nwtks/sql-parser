namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.Types

module DdlParser =
    let pColumnDefinition =
        pipe5
            pIdentifierExpr
            pDataType
            (opt (pKeyword "NOT" >>. pKeyword "NULL" >>% false <|> (pKeyword "NULL" >>% true)))
            (opt (pKeyword "PRIMARY" >>. pKeyword "KEY") |>> Option.isSome)
            (opt (pKeyword "DEFAULT" >>. pExpression))
            (fun name typ isNull pri def ->
                { Name = name
                  DataType = typ
                  IsNullable = isNull
                  IsPrimaryKey = pri
                  DefaultValue = def })

    let pCreateTableStatement =
        pKeyword "CREATE" >>. pKeyword "TABLE" >>. pIdentifierExpr
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pColumnDefinition (token (pstring ",")))
        |>> fun (name, cols) -> { Table = name; Columns = cols } |> CreateTable

    let pCreateIndexStatement =
        pKeyword "CREATE" >>. opt (pKeyword "UNIQUE" >>% true) .>> pKeyword "INDEX"
        .>>. pIdentifierExpr
        .>> pKeyword "ON"
        .>>. pIdentifierExpr
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
        |>> fun (((unique, name), table), cols) ->
            { Name = name
              Table = table
              Columns = cols
              Unique = Option.defaultValue false unique }
            |> CreateIndex

    let pCreateViewStatement =
        pKeyword "CREATE" >>. pKeyword "VIEW" >>. pIdentifierExpr .>> pKeyword "AS"
        .>>. pQuery
        |>> fun (name, query) -> { Name = name; Query = query } |> CreateView

    let pDropStatement =
        pKeyword "DROP"
        >>. choice
                [ attempt (pKeyword "TABLE" >>. pIdentifierExpr) |>> DropTable
                  attempt (pKeyword "INDEX" >>. pIdentifierExpr) |>> DropIndex
                  attempt (pKeyword "VIEW" >>. pIdentifierExpr) |>> DropView ]
        |>> Drop

    let pAlterTableStatement =
        let pAction =
            choice
                [ attempt (pKeyword "ADD" >>. opt (pKeyword "COLUMN") >>. pColumnDefinition)
                  |>> AddColumn
                  attempt (pKeyword "DROP" >>. opt (pKeyword "COLUMN") >>. pIdentifierExpr)
                  |>> DropColumn ]

        pKeyword "ALTER" >>. pKeyword "TABLE" >>. pIdentifierExpr .>>. pAction
        |>> fun (name, action) -> { Table = name; Action = action } |> AlterTable

    let pTruncateStatement =
        pKeyword "TRUNCATE" >>. opt (pKeyword "TABLE") >>. pIdentifierExpr |>> Truncate
