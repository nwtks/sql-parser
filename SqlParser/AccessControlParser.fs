namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.SchemaParser

module AccessControlParser =
    // 12.2 <grantor> ::= CURRENT_USER | CURRENT_ROLE
    let pGrantor =
        pIdentifierExpr |>> ignore
        <|> (pKeyword "CURRENT_USER" |>> ignore)
        <|> (pKeyword "CURRENT_ROLE" |>> ignore)

    // 12.3 <privileges> ::= ALL PRIVILEGES | <action> [ { <comma> <action> }... ]
    // The three 12.3 sub-rules below are local because <privileges> is their only consumer.
    let pPrivileges =
        // 12.3 <privilege column list> ::= ( <column name list> )
        let pPrivilegeColumnList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))

        // 12.3 <privilege method list> ::= <specific routine designator> [ { , <specific routine designator> }... ]
        let pPrivilegeMethodList = sepBy1 pSpecificRoutineDesignator (token (pstring ","))

        // 12.3 <action> ::= SELECT | SELECT ( <privilege column list> )
        //                 | SELECT ( <privilege method list> ) | INSERT [ <column list> ] ...
        let pPrivilegeAction =
            choice
                [ // SELECT ( <privilege method list> ) must be tried before the plain
                  // SELECT [ <privilege column list> ] alternative, otherwise the
                  // leading SELECT would consume the input and leave the '(' behind.
                  attempt (
                      pKeyword "SELECT"
                      >>. between (token (pstring "(")) (token (pstring ")")) pPrivilegeMethodList
                      |>> fun methods -> PrivilegeAction.Select(Some(PrivilegeMethods methods))
                  )
                  attempt (
                      pKeyword "SELECT" >>. opt pPrivilegeColumnList
                      |>> fun cols -> PrivilegeAction.Select(Option.map PrivilegeColumns cols)
                  )
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

        choice
            [ attempt (pKeyword "ALL" >>. pKeyword "PRIVILEGES" >>% Privileges.AllPrivileges)
              attempt (sepBy1 pPrivilegeAction (token (pstring ",")) |>> Privileges.Actions) ]

    // 12.3 <object name> / 12.2 <grant privilege statement> — <object name> ::= [ <object kind> ] <qualified name> | <specific routine designator>
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

        choice
            [ attempt (opt pKind >>. pQualifiedNameExpr)
              attempt pRoutineDesignatorWithType ]

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

    // 12.4 <role definition> ::= CREATE ROLE <role name> [ WITH ADMIN <grantor> ]
    let pCreateRoleStatement =
        pKeyword "CREATE" >>. pKeyword "ROLE" >>. pIdentifierExpr
        .>>. opt (pKeyword "WITH" >>. pKeyword "ADMIN" >>. pGrantor)
        |>> fun (name, _) -> CreateRole name

    // 12.7 <revoke statement> ::= <revoke privilege statement> | <revoke role statement>
    // 12.7 <revoke option extension> ::= GRANT OPTION FOR | HIERARCHY OPTION FOR
    let pRevokeOptionExtension =
        choice
            [ attempt (pKeyword "GRANT" >>. pKeyword "OPTION" >>. pKeyword "FOR" >>% GrantOptionFor)
              attempt (
                  pKeyword "HIERARCHY" >>. pKeyword "OPTION" >>. pKeyword "FOR"
                  >>% HierarchyOptionFor
              ) ]

    // 12.7 <revoke privilege statement> ::= REVOKE [ <revoke option extension> ] <privileges>
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
                  // 12.7 <revoke role statement> ::= REVOKE [ ADMIN OPTION FOR ] <role revoked>
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
