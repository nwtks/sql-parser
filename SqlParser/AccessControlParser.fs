namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.SchemaParser

module AccessControlParser =
    // 12.3 <grantor> ::= CURRENT_USER | CURRENT_ROLE
    // (an <authorization identifier> is also accepted in the <grantor> position — see
    //  docs/trade-off.md; the keywords are reserved words, so pIdentifierExpression fails on
    //  them and the keyword alternatives are reachable)
    let pGrantor =
        pIdentifierExpression |>> Grantor.AuthorizationId
        <|> (pKeyword "CURRENT_USER" >>% Grantor.CurrentUser)
        <|> (pKeyword "CURRENT_ROLE" >>% Grantor.CurrentRole)

    // 12.3 <grantee> ::= PUBLIC | <authorization identifier>
    let pGrantee =
        (pKeyword "PUBLIC" >>% Grantee.Public)
        <|> (pIdentifierExpression |>> Grantee.AuthorizationId)

    // 12.3 <privileges> ::= ALL PRIVILEGES | <action> [ { <comma> <action> }... ]
    // The three 12.3 sub-rules below are local because <privileges> is their only consumer.
    let pPrivileges =
        // 12.3 <privilege column list> ::= ( <column name list> )
        let pPrivilegeColumnList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpression (token (pstring ",")))

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

    // 12.3 <object name> ::= [ TABLE ] <table name> | DOMAIN <domain name> | COLLATION <collation name>
    //     | CHARACTER SET <character set name> | TRANSLATION <transliteration name>
    //     | TYPE <schema-resolved user-defined type name> | SEQUENCE <sequence generator name>
    //     | <specific routine designator>
    // Returns the kind keyword (None = the optional [ TABLE ] is absent), the 10.6 <routine type>
    // of the routine-designator alternative (None = absent) and the qualified name. The two
    // options are mutually exclusive; pGrantStatement / pRevokeStatement turn them into the
    // flat StatementKind cases.
    let pObjectName =
        let pKind =
            choice
                [ pKeyword "TABLE" >>% ObjectKind.Table
                  pKeyword "DOMAIN" >>% ObjectKind.Domain
                  pKeyword "COLLATION" >>% ObjectKind.Collation
                  attempt (pKeyword "CHARACTER" >>. pKeyword "SET" >>% ObjectKind.CharacterSet)
                  pKeyword "TRANSLATION" >>% ObjectKind.Translation
                  pKeyword "TYPE" >>% ObjectKind.Type
                  pKeyword "SEQUENCE" >>% ObjectKind.Sequence ]

        // The <specific routine designator> branch must be tried first: ROUTINE is a
        // non-reserved word, so the kind branch below would otherwise consume it as a
        // plain <qualified name>. A local variant of pRoutineDesignator (which
        // pDropStatement shares) is used so the <routine type> is kept in the AST.
        choice
            [ attempt (
                  pRoutineType .>>. pSchemaQualifiedNameExpression
                  |>> fun (rt, name) -> (None, Some rt, name)
              )
              attempt (
                  opt pKind .>>. pSchemaQualifiedNameExpression
                  |>> fun (kind, name) -> (kind, None, name)
              ) ]

    // 12.2 <grant privilege statement> ::= GRANT <privileges> TO <grantee> [ { , <grantee> }... ]
    //     [ WITH HIERARCHY OPTION ] [ WITH GRANT OPTION ] [ GRANTED BY <grantor> ]
    // (12.3 <privileges> ::= <object privileges> ON <object name>)
    let pGrantStatement =
        pKeyword "GRANT"
        >>. choice
                [ attempt (
                      pPrivileges .>> pKeyword "ON" .>>. pObjectName .>> pKeyword "TO"
                      .>>. sepBy1 pGrantee (token (pstring ","))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "HIERARCHY" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "GRANT" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      |>> fun (((((privs, (kind, rt, name)), grantees), withHier), withOpt), grantor) ->
                          let stmt: GrantPrivilegeStatement =
                              { Privileges = privs
                                Object = name
                                Grantees = grantees
                                WithHierarchyOption = Option.isSome withHier
                                WithGrantOption = Option.isSome withOpt
                                Grantor = grantor }

                          match kind, rt with
                          | _, Some rt -> GrantRoutine(rt, stmt)
                          | Some ObjectKind.Table, _ -> GrantTable stmt
                          | Some ObjectKind.Domain, _ -> GrantDomain stmt
                          | Some ObjectKind.Collation, _ -> GrantCollation stmt
                          | Some ObjectKind.CharacterSet, _ -> GrantCharacterSet stmt
                          | Some ObjectKind.Translation, _ -> GrantTranslation stmt
                          | Some ObjectKind.Type, _ -> GrantType stmt
                          | Some ObjectKind.Sequence, _ -> GrantSequence stmt
                          | None, None -> GrantObject stmt
                  )
                  // 12.5 <grant role statement> ::= GRANT <role granted> [ { , <role granted> }... ]
                  //     TO <grantee> [ { , <grantee> }... ] [ WITH ADMIN OPTION ] [ GRANTED BY <grantor> ]
                  attempt (
                      sepBy1 pIdentifierExpression (token (pstring ",")) .>> pKeyword "TO"
                      .>>. sepBy1 pGrantee (token (pstring ","))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "ADMIN" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      |>> fun (((roles, grantees), withAdm), grantor) ->
                          GrantRoles(roles, grantees, Option.isSome withAdm, grantor)
                  ) ]

    // 12.4 <role definition> ::= CREATE ROLE <role name> [ WITH ADMIN <grantor> ]
    let pCreateRoleStatement =
        pKeyword "CREATE" >>. pKeyword "ROLE" >>. pIdentifierExpression
        .>>. opt (pKeyword "WITH" >>. pKeyword "ADMIN" >>. pGrantor)
        |>> fun (name, grantor) -> CreateRole(name, grantor)

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
    // (12.3 <privileges> ::= <object privileges> ON <object name>)
    let pRevokeStatement =
        pKeyword "REVOKE"
        >>. choice
                [ attempt (
                      opt (attempt pRevokeOptionExtension) .>>. pPrivileges .>> pKeyword "ON"
                      .>>. pObjectName
                      .>> pKeyword "FROM"
                      .>>. sepBy1 pGrantee (token (pstring ","))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      .>>. pDropBehavior
                      |>> fun (((((optOpt, privs), (kind, rt, name)), grantees), grantor), cascade) ->
                          let stmt: RevokePrivilegeStatement =
                              { Privileges = privs
                                Object = name
                                Grantees = grantees
                                Option = Option.defaultValue NoOption optOpt
                                Grantor = grantor
                                DropBehavior = cascade }

                          match kind, rt with
                          | _, Some rt -> RevokeRoutine(rt, stmt)
                          | Some ObjectKind.Table, _ -> RevokeTable stmt
                          | Some ObjectKind.Domain, _ -> RevokeDomain stmt
                          | Some ObjectKind.Collation, _ -> RevokeCollation stmt
                          | Some ObjectKind.CharacterSet, _ -> RevokeCharacterSet stmt
                          | Some ObjectKind.Translation, _ -> RevokeTranslation stmt
                          | Some ObjectKind.Type, _ -> RevokeType stmt
                          | Some ObjectKind.Sequence, _ -> RevokeSequence stmt
                          | None, None -> RevokeObject stmt
                  )
                  // 12.7 <revoke role statement> ::= REVOKE [ ADMIN OPTION FOR ] <role revoked>
                  //     [ { , <role revoked> }... ] FROM <grantee> [ { , <grantee> }... ]
                  //     [ GRANTED BY <grantor> ] <drop behavior>
                  attempt (
                      opt (attempt (pKeyword "ADMIN" >>. pKeyword "OPTION" >>. pKeyword "FOR" >>% true))
                      .>>. sepBy1 pIdentifierExpression (token (pstring ","))
                      .>> pKeyword "FROM"
                      .>>. sepBy1 pGrantee (token (pstring ","))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      .>>. pDropBehavior
                      |>> fun ((((adminFor, roles), grantees), grantor), cascade) ->
                          RevokeRoles(roles, grantees, Option.defaultValue false adminFor, grantor, cascade)
                  ) ]
