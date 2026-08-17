namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.Types

module DdlParser =
    // This module implements the data-definition and authorization statements of the
    // SQL-2016 grammar:
    //
    //   11.1  <table definition> (CREATE TABLE)
    //   11.2  <column definition> / <column constraint definition>
    //   11.6  <table constraint definition>
    //   11.10 <alter table statement>
    //   11.31 <drop table statement>
    //   11.32 <view definition> (CREATE VIEW)
    //   12.2  <grant privilege statement> / 12.5 <grant role statement>
    //   12.4  <role definition> / 12.6 <drop role statement>
    //   12.7  <revoke statement>
    //   14.10 <truncate table statement>
    type ColumnConstraintKind =
        | NotNull
        | Null
        | PrimaryKey
        | Unique
        | References of ForeignKeyConstraint
        | Check of Expression
        | Default of Expression

    let pReferentialAction =
        choice
            [ pKeyword "CASCADE" >>% ReferentialAction.Cascade
              attempt (pKeyword "SET" >>. pKeyword "NULL" >>% ReferentialAction.SetNull)
              attempt (pKeyword "SET" >>. pKeyword "DEFAULT" >>% ReferentialAction.SetDefault)
              attempt (pKeyword "RESTRICT" >>% ReferentialAction.Restrict)
              attempt (pKeyword "NO" >>. pKeyword "ACTION" >>% ReferentialAction.NoAction) ]

    let pReferentialTriggeredAction =
        many (
            choice
                [ attempt (
                      pKeyword "ON" >>. pKeyword "UPDATE" >>. pReferentialAction
                      |>> fun a -> (Some a, None)
                  )
                  attempt (
                      pKeyword "ON" >>. pKeyword "DELETE" >>. pReferentialAction
                      |>> fun a -> (None, Some a)
                  ) ]
        )
        |>> fun acts -> (List.tryPick fst acts, List.tryPick snd acts)

    let pColumnConstraint =
        choice
            [ pKeyword "NOT" >>. pKeyword "NULL" >>% NotNull
              pKeyword "NULL" >>% Null
              pKeyword "PRIMARY" >>. pKeyword "KEY" >>% PrimaryKey
              pKeyword "UNIQUE" >>% Unique
              attempt (
                  pKeyword "REFERENCES" >>. pIdentifierExpr
                  .>>. opt (
                      between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                  )
                  .>>. pReferentialTriggeredAction
                  |>> fun ((table, refCols), (onUpd, onDel)) ->
                      References
                          { Name = None
                            Columns = []
                            Table = table
                            RefColumns = refCols
                            OnUpdate = onUpd
                            OnDelete = onDel }
              )
              attempt (
                  pKeyword "CHECK"
                  >>. between (token (pstring "(")) (token (pstring ")")) pExpression
                  |>> Check
              )
              attempt (pKeyword "DEFAULT" >>. pExpression |>> Default) ]

    let pColumnDefinition =
        pIdentifierExpr .>>. pDataType .>>. many pColumnConstraint
        |>> fun ((name, typ), cons) ->
            { Name = name
              DataType = typ
              IsNullable =
                cons
                |> List.tryPick (function
                    | NotNull -> Some false
                    | Null -> Some true
                    | _ -> None)
              IsPrimaryKey =
                cons
                |> List.exists (function
                    | PrimaryKey -> true
                    | _ -> false)
              DefaultValue =
                cons
                |> List.tryPick (function
                    | Default e -> Some e
                    | _ -> None)
              IsUnique =
                cons
                |> List.exists (function
                    | Unique -> true
                    | _ -> false)
              References =
                cons
                |> List.tryPick (function
                    | References r -> Some r
                    | _ -> None)
              Check =
                cons
                |> List.tryPick (function
                    | Check e -> Some e
                    | _ -> None) }

    let pForeignKeyConstraint =
        pKeyword "FOREIGN"
        >>. pKeyword "KEY"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
        .>> pKeyword "REFERENCES"
        .>>. pIdentifierExpr
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))
        .>>. pReferentialTriggeredAction
        |>> fun (((cols, table), refCols), (onUpd, onDel)) ->
            { Name = None
              Columns = cols
              Table = table
              RefColumns = refCols
              OnUpdate = onUpd
              OnDelete = onDel }
            : ForeignKeyConstraint

    let pTableConstraint =
        let pName = opt (pKeyword "CONSTRAINT" >>. pIdentifierExpr)

        let pColumnList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))

        choice
            [ attempt (
                  pName .>> pKeyword "PRIMARY" .>> pKeyword "KEY" .>>. pColumnList
                  |>> fun (n, cols) -> TableConstraint.PrimaryKey(n, cols)
              )
              attempt (
                  pName .>> pKeyword "UNIQUE" .>>. pColumnList
                  |>> fun (n, cols) -> TableConstraint.Unique(n, cols)
              )
              attempt (
                  pName .>>. pForeignKeyConstraint
                  |>> fun (n, fk) -> TableConstraint.ForeignKey { fk with Name = n }
              )
              attempt (
                  pName .>> pKeyword "CHECK"
                  .>>. between (token (pstring "(")) (token (pstring ")")) pExpression
                  |>> fun (n, e) -> TableConstraint.Check(n, e)
              ) ]

    let pCreateTableStatement =
        let pTableElement =
            attempt (pColumnDefinition |>> Choice1Of2) <|> (pTableConstraint |>> Choice2Of2)

        let pAsSubquery =
            pKeyword "AS" >>. pQuery
            .>>. opt (pKeyword "WITH" >>. opt (pKeyword "NO") .>> pKeyword "DATA" |>> Option.isNone)
            |>> fun (q, withData) -> (q, withData)

        pKeyword "CREATE" >>. pKeyword "TABLE" >>. pQualifiedName
        .>>. (attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTableElement (token (pstring ",")))
                  |>> fun elems -> (elems, None, None)
              )
              <|> attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                  .>>. pAsSubquery
                  |>> fun (cols, (q, withData)) -> ([], Some cols, Some(q, withData))
              )
              <|> (pAsSubquery |>> fun (q, withData) -> ([], None, Some(q, withData))))
        |>> fun (name, (elems, asCols, asQuery)) ->
            let cols =
                elems
                |> List.choose (function
                    | Choice1Of2 c -> Some c
                    | _ -> None)

            let cons =
                elems
                |> List.choose (function
                    | Choice2Of2 c -> Some c
                    | _ -> None)

            { Table = name
              Columns = cols
              Constraints = cons
              AsQuery = asQuery |> Option.map fst
              AsColumns = asCols
              WithData = asQuery |> Option.bind snd }
            |> CreateTable

    let pCreateViewStatement =
        pKeyword "CREATE" >>. pKeyword "VIEW" >>. pQualifiedName
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))
        .>> pKeyword "AS"
        .>>. pQuery
        |>> fun ((name, cols), query) ->
            { Name = name
              Columns = cols
              Query = query }
            |> CreateView

    // <grantor> ::= CURRENT_USER | CURRENT_ROLE
    let pGrantor =
        (pIdentifierExpr |>> ignore)
        <|> (pKeyword "CURRENT_USER" |>> ignore)
        <|> (pKeyword "CURRENT_ROLE" |>> ignore)

    // 12.4 <role definition> ::= CREATE ROLE <role name> [ WITH ADMIN <grantor> ]
    let pCreateRoleStatement =
        pKeyword "CREATE" >>. pKeyword "ROLE" >>. pIdentifierExpr
        .>>. opt (pKeyword "WITH" >>. pKeyword "ADMIN" >>. pGrantor)
        |>> fun (name, _) -> CreateRole name

    let pPrivilegeColumnList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))

    // <action> ::= SELECT [ <privilege column list> ] | INSERT [ <privilege column list> ]
    //             | UPDATE [ <privilege column list> ] | DELETE | REFERENCES [ <privilege column list> ]
    //             | USAGE | TRIGGER | UNDER | EXECUTE
    let pPrivilegeAction =
        choice
            [ attempt (pKeyword "SELECT" >>. opt pPrivilegeColumnList |>> PrivilegeAction.Select)
              attempt (pKeyword "INSERT" >>. opt pPrivilegeColumnList |>> PrivilegeAction.Insert)
              attempt (pKeyword "UPDATE" >>. opt pPrivilegeColumnList |>> PrivilegeAction.Update)
              attempt (pKeyword "DELETE" >>% PrivilegeAction.Delete)
              attempt (
                  pKeyword "REFERENCES" >>. opt pPrivilegeColumnList
                  |>> PrivilegeAction.References
              )
              attempt (pKeyword "USAGE" >>% PrivilegeAction.Usage)
              attempt (pKeyword "TRIGGER" >>% PrivilegeAction.Trigger)
              attempt (pKeyword "UNDER" >>% PrivilegeAction.Under)
              attempt (pKeyword "EXECUTE" >>% PrivilegeAction.Execute) ]

    let pPrivileges =
        choice
            [ attempt (pKeyword "ALL" >>. pKeyword "PRIVILEGES" >>% Privileges.AllPrivileges)
              attempt (sepBy1 pPrivilegeAction (token (pstring ",")) |>> Privileges.Actions) ]

    let pObjectName =
        let pKind =
            choice
                [ pKeyword "TABLE"
                  pKeyword "DOMAIN"
                  pKeyword "COLLATION"
                  attempt (pKeyword "CHARACTER" >>. pKeyword "SET")
                  pKeyword "TYPE"
                  pKeyword "SEQUENCE"
                  pKeyword "TRANSLATION" ]

        opt pKind >>. pQualifiedName

    // <drop behavior> ::= CASCADE | RESTRICT   (true = CASCADE, false = RESTRICT)
    let pDropBehavior = pKeyword "CASCADE" >>% true <|> (pKeyword "RESTRICT" >>% false)

    // 12.2 <grant privilege statement> ::= GRANT <privileges> TO <grantee> [ { , <grantee> }... ]
    //     [ WITH HIERARCHY OPTION ] [ WITH GRANT OPTION ] [ GRANTED BY <grantor> ]
    let pGrantStatement =
        pKeyword "GRANT"
        >>. choice
                [ attempt (
                      pPrivileges .>> pKeyword "ON" .>>. pObjectName .>> pKeyword "TO"
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "HIERARCHY" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "GRANT" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      |>> fun (((((privs, obj), grantees), withHier), withOpt), _) ->
                          GrantStatement.GrantPrivileges(
                              privs,
                              obj,
                              grantees,
                              Option.isSome withHier,
                              Option.isSome withOpt
                          )
                  )
                  // 12.5 <grant role statement> ::= GRANT <role granted> [ { , <role granted> }... ]
                  //     TO <grantee> [ { , <grantee> }... ] [ WITH ADMIN OPTION ] [ GRANTED BY <grantor> ]
                  attempt (
                      sepBy1 pIdentifierExpr (token (pstring ",")) .>> pKeyword "TO"
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "ADMIN" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      |>> fun (((roles, grantees), withAdm), _) ->
                          GrantStatement.GrantRoles(roles, grantees, Option.isSome withAdm)
                  ) ]
        |>> Grant

    // 12.7 <revoke statement> ::= <revoke privilege statement> | <revoke role statement>
    // <revoke option extension> ::= GRANT OPTION FOR | HIERARCHY OPTION FOR
    let pRevokeOptionExtension =
        choice
            [ attempt (pKeyword "GRANT" >>. pKeyword "OPTION" >>. pKeyword "FOR" >>% GrantOptionFor)
              attempt (
                  pKeyword "HIERARCHY" >>. pKeyword "OPTION" >>. pKeyword "FOR"
                  >>% HierarchyOptionFor
              ) ]

    // <revoke privilege statement> ::= REVOKE [ <revoke option extension> ] <privileges>
    //     FROM <grantee> [ { , <grantee> }... ] [ GRANTED BY <grantor> ] <drop behavior>
    let pRevokeStatement =
        pKeyword "REVOKE"
        >>. choice
                [ attempt (
                      opt (attempt pRevokeOptionExtension) .>>. pPrivileges .>> pKeyword "ON"
                      .>>. pObjectName
                      .>> pKeyword "FROM"
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      .>>. pDropBehavior
                      |>> fun (((((optOpt, privs), obj), grantees), _), cascade) ->
                          RevokeStatement.RevokePrivileges(
                              privs,
                              obj,
                              grantees,
                              Option.defaultValue NoOption optOpt,
                              cascade
                          )
                  )
                  // <revoke role statement> ::= REVOKE [ ADMIN OPTION FOR ] <role revoked>
                  //     [ { , <role revoked> }... ] FROM <grantee> [ { , <grantee> }... ]
                  //     [ GRANTED BY <grantor> ] <drop behavior>
                  attempt (
                      opt (attempt (pKeyword "ADMIN" >>. pKeyword "OPTION" >>. pKeyword "FOR" >>% true))
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>> pKeyword "FROM"
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      .>>. pDropBehavior
                      |>> fun ((((adminFor, roles), grantees), _), cascade) ->
                          RevokeStatement.RevokeRoles(roles, grantees, Option.defaultValue false adminFor, cascade)
                  ) ]
        |>> Revoke

    // 11.31 <drop table statement> ::= DROP TABLE <table name> <drop behavior>
    let pDropStatement =
        pKeyword "DROP"
        >>. choice
                [ attempt (pKeyword "TABLE" >>. pQualifiedName .>>. pDropBehavior) |>> DropTable
                  attempt (pKeyword "VIEW" >>. pIdentifierExpr) |>> DropView
                  attempt (pKeyword "ROLE" >>. pIdentifierExpr) |>> DropStatement.DropRole ]
        |>> Drop

    let pAlterTableStatement =
        let pColumnAction =
            choice
                [ attempt (
                      pKeyword "SET" >>. pKeyword "DEFAULT" >>. pExpression
                      |>> ColumnAlteration.SetDefault
                  )
                  attempt (pKeyword "DROP" >>. pKeyword "DEFAULT" >>% ColumnAlteration.DropDefault)
                  attempt (
                      pKeyword "SET" >>. pKeyword "NOT" >>. pKeyword "NULL"
                      >>% ColumnAlteration.SetNotNull
                  )
                  attempt (
                      pKeyword "DROP" >>. pKeyword "NOT" >>. pKeyword "NULL"
                      >>% ColumnAlteration.DropNotNull
                  )
                  attempt (
                      pKeyword "SET" >>. pKeyword "DATA" >>. pKeyword "TYPE" >>. pDataType
                      |>> ColumnAlteration.SetDataType
                  ) ]

        let pAction =
            choice
                [ attempt (pKeyword "ADD" >>. opt (pKeyword "COLUMN") >>. pColumnDefinition |>> AddColumn)
                  attempt (pKeyword "ADD" >>. pTableConstraint |>> AddConstraint)
                  attempt (pKeyword "DROP" >>. opt (pKeyword "COLUMN") >>. pIdentifierExpr |>> DropColumn)
                  attempt (pKeyword "DROP" >>. pKeyword "CONSTRAINT" >>. pIdentifierExpr |>> DropConstraint)
                  attempt (
                      pKeyword "ALTER" >>. opt (pKeyword "COLUMN") >>. pIdentifierExpr
                      .>>. pColumnAction
                      |>> AlterColumn
                  ) ]

        pKeyword "ALTER" >>. pKeyword "TABLE" >>. pQualifiedName .>>. pAction
        |>> fun (name, action) -> { Table = name; Action = action } |> AlterTable

    let pTruncateStatement =
        pKeyword "TRUNCATE" >>. opt (pKeyword "TABLE") >>. pQualifiedName
        .>>. opt (
            pKeyword "RESTART" >>. pKeyword "IDENTITY" >>% true
            <|> (pKeyword "CONTINUE" >>. pKeyword "IDENTITY" >>% false)
        )
        |>> fun (table, restart) -> Truncate(table, restart)
