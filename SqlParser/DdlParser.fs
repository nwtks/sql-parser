namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.Types

module DdlParser =
    type ColumnConstraintKind =
        | NotNull
        | Null
        | PrimaryKey
        | Unique
        | References of ForeignKeyConstraint
        | Check of Expression
        | Default of Expression

    // 11.8 <referential action> ::= CASCADE | SET NULL | SET DEFAULT | RESTRICT | NO ACTION
    let pReferentialAction =
        choice
            [ pKeyword "CASCADE" >>% ReferentialAction.Cascade
              attempt (pKeyword "SET" >>. pKeyword "NULL" >>% ReferentialAction.SetNull)
              attempt (pKeyword "SET" >>. pKeyword "DEFAULT" >>% ReferentialAction.SetDefault)
              attempt (pKeyword "RESTRICT" >>% ReferentialAction.Restrict)
              attempt (pKeyword "NO" >>. pKeyword "ACTION" >>% ReferentialAction.NoAction) ]

    // 11.8 <referential triggered action> ::= [ <update rule> ] [ <delete rule> ] | [ <delete rule> ] [ <update rule> ] — <update rule> ::= ON UPDATE <referential action>
    let pReferentialTriggeredAction =
        many (
            choice
                [ attempt (
                      pKeyword "ON" >>. pKeyword "UPDATE" >>. pReferentialAction
                      |>> fun a -> Some a, None
                  )
                  attempt (
                      pKeyword "ON" >>. pKeyword "DELETE" >>. pReferentialAction
                      |>> fun a -> None, Some a
                  ) ]
        )
        |>> fun acts -> List.tryPick fst acts, List.tryPick snd acts

    // 11.4 <column constraint definition> — NOT NULL | NULL | PRIMARY KEY | UNIQUE | REFERENCES <table> | CHECK ( <search condition> ) | DEFAULT <value expression>
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

    // 11.72 <sequence generator option> — shared by CREATE/ALTER SEQUENCE and
    // the <identity column specification> (11.2).
    let pSequenceOption =
        choice
            [ attempt (pKeyword "AS" >>. pDataType |>> DataTypeOption)
              attempt (pKeyword "START" >>. pKeyword "WITH" >>. pSignedNumericLiteral |>> StartWith)
              attempt (pKeyword "INCREMENT" >>. pKeyword "BY" >>. pSignedNumericLiteral |>> IncrementBy)
              attempt (pKeyword "MAXVALUE" >>. pSignedNumericLiteral |>> fun v -> MaxValue(Some v))
              attempt (pKeyword "NO" >>. pKeyword "MAXVALUE" >>% MaxValue None)
              attempt (pKeyword "MINVALUE" >>. pSignedNumericLiteral |>> fun v -> MinValue(Some v))
              attempt (pKeyword "NO" >>. pKeyword "MINVALUE" >>% MinValue None)
              attempt (pKeyword "CYCLE" >>% Cycle true)
              attempt (pKeyword "NO" >>. pKeyword "CYCLE" >>% Cycle false)
              attempt (
                  pKeyword "RESTART" >>. opt (pKeyword "WITH" >>. pSignedNumericLiteral)
                  |>> Restart
              ) ]

    // 11.2 <identity column specification> ::= GENERATED { ALWAYS | BY DEFAULT }
    //     AS IDENTITY [ ( <common sequence generator options> ) ]
    let pIdentitySpec =
        pKeyword "GENERATED"
        >>. (pKeyword "ALWAYS" >>% true <|> (pKeyword "BY" >>. pKeyword "DEFAULT" >>% false))
        .>> pKeyword "AS"
        .>> pKeyword "IDENTITY"
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (many pSequenceOption))
        |>> fun (isAlways, opts) ->
            { IsAlways = isAlways
              Options = Option.defaultValue [] opts }

    // 11.4 <column definition> ::= <column name> <data type> [ <default clause> ] [ <column constraint definition>... ] [ <collate clause> ]
    let pColumnDefinition =
        pIdentifierExpr
        .>>. pDataType
        .>>. opt (attempt pIdentitySpec)
        .>>. many pColumnConstraint
        |>> fun (((name, typ), identity), cons) ->
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
                    | _ -> None)
              Identity = identity }

    // 11.8 <referential constraint definition> ::= FOREIGN KEY ( <column list> ) REFERENCES <table> [ ( <column list> ) ] [ <referential triggered action> ]
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

    // 11.6 <table constraint definition> ::= [ <constraint name definition> ] <table constraint> — <table constraint> ::= PRIMARY KEY | UNIQUE | FOREIGN KEY | CHECK
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

    // 11.72 <sequence generator definition> ::= CREATE SEQUENCE <sequence generator name> [ <sequence generator options> ]
    let pCreateSequenceStatement =
        pKeyword "CREATE" >>. pKeyword "SEQUENCE" >>. pQualifiedNameExpr
        .>>. many pSequenceOption
        |>> fun (name, opts) -> CreateSequence(name, opts)

    // 11.73 <alter sequence generator statement> ::= ALTER SEQUENCE <name> <options>
    let pAlterSequenceStatement =
        pKeyword "ALTER" >>. pKeyword "SEQUENCE" >>. pQualifiedNameExpr
        .>>. many1 pSequenceOption
        |>> fun (name, opts) -> AlterSequence(name, opts)

    // 11.1 <table definition> ::= CREATE [ <table scope> ] TABLE <table name> <table contents source> [ <typed table clause> ]
    let pCreateTableStatement =
        // 11.1 <table element> ::= <column definition> | <table constraint definition>
        let pTableElement =
            attempt (pColumnDefinition |>> Choice1Of2) <|> (pTableConstraint |>> Choice2Of2)

        // 11.1 <as subquery clause> ::= AS <query expression> [ WITH [ NO ] DATA ]
        let pAsSubquery =
            pKeyword "AS" >>. pQuery
            .>>. opt (pKeyword "WITH" >>. opt (pKeyword "NO") .>> pKeyword "DATA" |>> Option.isNone)
            |>> fun (q, withData) -> q, withData

        // 11.3 <table scope> ::= GLOBAL TEMPORARY | LOCAL TEMPORARY
        let pTableScope =
            attempt (pKeyword "GLOBAL" >>. pKeyword "TEMPORARY" >>% TableScope.Global)
            <|> (pKeyword "LOCAL" >>. pKeyword "TEMPORARY" >>% TableScope.Local)

        // 11.1 <typed table clause> ::= OF <UDT name> [ UNDER <supertable> ]
        // (the <subtable clause> is parsed and discarded; the <typed table element
        // list> is not supported — see docs/trade-off.md)
        let pTypedTableClause =
            pKeyword "OF" >>. pQualifiedNameExpr
            .>>. opt (pKeyword "UNDER" >>. pQualifiedNameExpr)
            |>> fun (typ, _sub) -> typ

        pKeyword "CREATE" >>. opt pTableScope .>> pKeyword "TABLE"
        .>>. pQualifiedNameExpr
        .>>. (attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTableElement (token (pstring ",")))
                  |>> fun elems -> elems, None, None, None
              )
              <|> attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                  .>>. pAsSubquery
                  |>> fun (cols, (q, withData)) -> [], Some cols, Some(q, withData), None
              )
              <|> (pAsSubquery |>> fun (q, withData) -> [], None, Some(q, withData), None)
              <|> (pTypedTableClause |>> fun typ -> [], None, None, Some typ))
        |>> fun ((scope, name), (elems, asCols, asQuery, ofType)) ->
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
              TableScope = scope
              Columns = cols
              Constraints = cons
              AsQuery = asQuery |> Option.map fst
              AsColumns = asCols
              WithData = asQuery |> Option.bind snd
              OfType = ofType }
            |> CreateTable

    // 11.32 <levels clause> ::= CASCADED | LOCAL   (default is CASCADED)
    let pCheckOption =
        pKeyword "WITH"
        >>. opt (pKeyword "CASCADED" >>% true <|> (pKeyword "LOCAL" >>% false))
        .>> pKeyword "CHECK"
        .>> pKeyword "OPTION"
        |>> Option.defaultValue true

    // 11.32 <view definition> ::= CREATE VIEW <table name> [ <view column list> ] [ <referenceable view specification> ] AS <query expression> [ <view check option> ]
    let pCreateViewStatement =
        // 11.32 <view specification> ::= <regular view specification> | <referenceable view specification>
        // (regular = column list; referenceable = OF <UDT name> — the <subview clause>
        // and <view element list> are not captured — see docs/trade-off.md)
        let pViewSpecification =
            choice
                [ attempt (
                      between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                      |>> Choice1Of2
                  )
                  attempt (pKeyword "OF" >>. pQualifiedNameExpr |>> Choice2Of2) ]

        pKeyword "CREATE" >>. pKeyword "VIEW" >>. pQualifiedNameExpr
        .>>. opt pViewSpecification
        .>> pKeyword "AS"
        .>>. pQuery
        .>>. opt (attempt pCheckOption)
        |>> fun (((name, spec), query), checkOpt) ->
            let cols, ofType =
                match spec with
                | Some(Choice1Of2 c) -> Some c, None
                | Some(Choice2Of2 t) -> None, Some t
                | None -> None, None

            { Name = name
              Columns = cols
              Query = query
              CheckOption = checkOpt
              OfType = ofType }
            |> CreateView

    // 12.2 <grantor> ::= CURRENT_USER | CURRENT_ROLE
    let pGrantor =
        pIdentifierExpr |>> ignore
        <|> (pKeyword "CURRENT_USER" |>> ignore)
        <|> (pKeyword "CURRENT_ROLE" |>> ignore)

    // 12.4 <role definition> ::= CREATE ROLE <role name> [ WITH ADMIN <grantor> ]
    let pCreateRoleStatement =
        pKeyword "CREATE" >>. pKeyword "ROLE" >>. pIdentifierExpr
        .>>. opt (pKeyword "WITH" >>. pKeyword "ADMIN" >>. pGrantor)
        |>> fun (name, _) -> CreateRole name

    // 12.3 <privilege column list> ::= ( <column name list> )
    let pPrivilegeColumnList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))

    // 10.6 <specific routine designator> ::= SPECIFIC <routine type> <specific name> | <routine designator>
    // (simplified: the trailing [ FOR <user-defined type name> ] clause is not captured)
    let pRoutineType =
        choice
            [ pKeyword "ROUTINE" >>% ()
              pKeyword "FUNCTION" >>% ()
              pKeyword "PROCEDURE" >>% ()
              attempt (
                  opt (
                      choice
                          [ pKeyword "INSTANCE" >>% ()
                            pKeyword "STATIC" >>% ()
                            pKeyword "CONSTRUCTOR" >>% () ]
                  )
                  >>. pKeyword "METHOD"
                  >>% ()
              ) ]

    // 10.6 <routine designator> ::= [ <routine type> ] <qualified identifier>
    let pRoutineDesignatorWithType =
        choice [ attempt (pRoutineType >>. pQualifiedNameExpr); pQualifiedNameExpr ]

    // 10.6 <specific routine designator> ::= SPECIFIC <routine type> <specific name> | <routine designator>
    let pSpecificRoutineDesignator =
        choice
            [ attempt (pKeyword "SPECIFIC" >>. pRoutineType >>. pQualifiedNameExpr)
              attempt pRoutineDesignatorWithType ]

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

    // 12.3 <privileges> ::= ALL PRIVILEGES | <action> [ { <comma> <action> }... ]
    let pPrivileges =
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

    // 11.2 <drop behavior> ::= CASCADE | RESTRICT   (true = CASCADE, false = RESTRICT)
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

    // 11.71 <transforms to be dropped> ::= ALL | <transform group element>
    let pTransformsToBeDropped =
        pKeyword "ALL" >>% TransformDropTarget.AllTransforms
        <|> (pQualifiedNameExpr |>> TransformDropTarget.TransformGroup)

    // 11.31 <drop table statement> ::= DROP TABLE <table name> <drop behavior>
    // 11.33 <drop view statement> ::= DROP VIEW <table name> <drop behavior>
    // 11.74 <drop sequence generator statement> ::= DROP SEQUENCE <sequence generator name> <drop behavior>
    // 12.6 <drop role statement> ::= DROP ROLE <role name>
    // 11.2 <drop schema statement> ::= DROP SCHEMA <schema name> <drop behavior>
    // 11.40 <drop domain statement> ::= DROP DOMAIN <domain name> <drop behavior>
    // 11.44 <drop collation statement> ::= DROP COLLATION <collation name> <drop behavior>
    // 11.42 <drop character set statement> ::= DROP CHARACTER SET <character set name>
    // 11.46 <drop transliteration statement> ::= DROP TRANSLATION <transliteration name>
    // 11.48 <drop assertion statement> ::= DROP ASSERTION <constraint name> [ <drop behavior> ]
    // 11.64 <drop user-defined cast statement> ::= DROP CAST (<source data type> AS <target data type>) <drop behavior>
    // 11.66 <drop user-defined ordering statement> ::= DROP ORDERING FOR <schema-resolved user-defined type name> <drop behavior>
    // 11.71 <drop transform statement> ::= DROP { TRANSFORM | TRANSFORMS } <transforms to be dropped> FOR <schema-resolved user-defined type name> <drop behavior>
    // 11.50 <drop trigger statement> ::= DROP TRIGGER <trigger name>
    // 11.62 <drop routine statement> ::= DROP <specific routine designator> <drop behavior>
    // 11.59 <drop data type statement> ::= DROP TYPE <schema-resolved user-defined type name> <drop behavior>
    let pDropStatement =
        pKeyword "DROP"
        >>. choice
                [ attempt (pKeyword "TABLE" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropTable
                  attempt (pKeyword "VIEW" >>. pQualifiedNameExpr .>>. pDropBehavior) |>> DropView
                  attempt (pKeyword "SEQUENCE" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropSequence
                  attempt (pKeyword "ROLE" >>. pIdentifierExpr) |>> DropStatement.DropRole
                  attempt (pKeyword "SCHEMA" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropSchema
                  attempt (pKeyword "DOMAIN" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropDomain
                  attempt (pKeyword "COLLATION" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropCollation
                  attempt (pKeyword "CHARACTER" >>. pKeyword "SET" >>. pQualifiedNameExpr)
                  |>> DropCharacterSet
                  attempt (pKeyword "TRANSLATION" >>. pQualifiedNameExpr) |>> DropTransliteration
                  attempt (pKeyword "ASSERTION" >>. pQualifiedNameExpr .>>. opt pDropBehavior)
                  |>> DropAssertion
                  attempt (
                      pKeyword "CAST"
                      >>. between
                              (token (pstring "("))
                              (token (pstring ")"))
                              (pDataType .>>. (pKeyword "AS" >>. pDataType))
                      .>>. pDropBehavior
                  )
                  |>> fun ((source, target), behavior) -> DropCast(source, target, behavior)
                  attempt (pKeyword "ORDERING" >>. pKeyword "FOR" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropOrdering
                  attempt (
                      pKeyword "TRANSFORM" <|> pKeyword "TRANSFORMS" >>. pTransformsToBeDropped
                      .>>. (pKeyword "FOR" >>. pQualifiedNameExpr)
                      .>>. pDropBehavior
                  )
                  |>> fun ((target, forName), behavior) -> DropTransform(forName, target, behavior)
                  // 11.62 <drop routine statement> ::= DROP <specific routine designator> <drop behavior>
                  attempt (pKeyword "TRIGGER" >>. pQualifiedNameExpr) |>> DropTrigger
                  attempt (pRoutineDesignatorWithType .>>. pDropBehavior) |>> DropRoutine
                  // 11.59 <drop data type statement> ::= DROP TYPE <name> <drop behavior>
                  attempt (pKeyword "TYPE" >>. pQualifiedNameExpr .>>. pDropBehavior) |>> DropType ]
        |>> Drop

    // 11.10 <alter table statement> ::= ALTER TABLE <table name> <alter table action>
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

        // 11.10 <alter table action> ::= ADD COLUMN <column definition> | ADD <table constraint>
        //     | DROP COLUMN <column name> | DROP CONSTRAINT <constraint name> | ALTER COLUMN ...
        let pAction =
            choice
                [ attempt (pKeyword "ADD" >>. opt (pKeyword "COLUMN") >>. pColumnDefinition |>> AddColumn)
                  attempt (pKeyword "ADD" >>. pTableConstraint |>> AlterTableAction.AddConstraint)
                  attempt (pKeyword "DROP" >>. opt (pKeyword "COLUMN") >>. pIdentifierExpr |>> DropColumn)
                  attempt (
                      pKeyword "DROP" >>. pKeyword "CONSTRAINT" >>. pIdentifierExpr
                      |>> AlterTableAction.DropConstraint
                  )
                  attempt (
                      pKeyword "ALTER" >>. opt (pKeyword "COLUMN") >>. pIdentifierExpr
                      .>>. pColumnAction
                      |>> AlterColumn
                  ) ]

        pKeyword "ALTER" >>. pKeyword "TABLE" >>. pQualifiedNameExpr .>>. pAction
        |>> fun (name, action) -> { Table = name; Action = action } |> AlterTable

    // 17.12 <truncate table statement> ::= TRUNCATE TABLE <target table> [ <identity column restart option> ]
    let pTruncateStatement =
        pKeyword "TRUNCATE" >>. opt (pKeyword "TABLE") >>. pQualifiedNameExpr
        .>>. opt (
            pKeyword "RESTART" >>. pKeyword "IDENTITY" >>% true
            <|> (pKeyword "CONTINUE" >>. pKeyword "IDENTITY" >>% false)
        )
        |>> fun (table, restart) -> Truncate(table, restart)

    // 10.8 <constraint characteristics> ::=
    //     <constraint check time> [ [ NOT ] DEFERRABLE ] [ <constraint enforcement> ]
    //   | [ [ NOT ] DEFERRABLE ] <constraint check time> [ <constraint enforcement> ]
    //   | <constraint enforcement>
    let pConstraintCharacteristics =
        // 10.8 <constraint check time> ::= INITIALLY DEFERRED | INITIALLY IMMEDIATE
        // NOTE: both alternatives are parenthesized — `<|>` binds tighter than `>>.`/`>>%`,
        // so an unparenthesized `INITIALLY >>. DEFERRED >>% true <|> (...)` would group as
        // `INITIALLY >>. (DEFERRED >>% (true <|> ...))` and never try IMMEDIATE.
        let pCheckTime: Parser<bool, unit> =
            attempt (pKeyword "INITIALLY" >>. pKeyword "DEFERRED" >>% true)
            <|> (pKeyword "INITIALLY" >>. pKeyword "IMMEDIATE" >>% false)

        // 10.8 <constraint deferrability> ::= [ NOT ] DEFERRABLE
        let pDeferrable: Parser<bool, unit> =
            attempt (pKeyword "NOT" >>. pKeyword "DEFERRABLE" >>% false)
            <|> (pKeyword "DEFERRABLE" >>% true)

        // 10.8 <constraint enforcement> ::= [ NOT ] ENFORCED
        let pEnforced: Parser<bool, unit> =
            attempt (pKeyword "NOT" >>. pKeyword "ENFORCED" >>% false)
            <|> (pKeyword "ENFORCED" >>% true)

        let mk (initiallyDeferred: bool option) (deferrable: bool option) (enforced: bool option) =
            { InitiallyDeferred = initiallyDeferred
              Deferrable = deferrable
              Enforced = enforced }

        choice
            [ // <check time> [ <deferrability> ] [ <enforcement> ]
              attempt (
                  pCheckTime .>>. opt pDeferrable .>>. opt pEnforced
                  |>> fun ((ct, d), e) -> mk (Some ct) d e
              )
              // [ <deferrability> ] [ <check time> ] [ <enforcement> ]
              // — only the deferrability is required, so a bare `[ NOT ] DEFERRABLE`
              // is valid and any following keyword (e.g. a domain's COLLATE clause)
              // is left for the enclosing production.
              attempt (
                  pDeferrable .>>. opt pCheckTime .>>. opt pEnforced
                  |>> fun ((d, ct), e) -> mk ct (Some d) e
              )
              // <enforcement> alone
              attempt (pEnforced |>> fun e -> mk None None (Some e))
              // <constraint characteristics> is optional in its enclosing production
              preturn (mk None None None) ]

    // Forward reference to the full DDL statement set; a <schema element> is any
    // DDL statement. Wired to pDdl in SqlParser.fs (which also contains the
    // CREATE SCHEMA parser that consumes these elements).
    // 11.1 <schema element> ::= <table definition> | <view definition> | <domain definition> | ...
    let pSchemaElement, pSchemaElementImpl =
        createParserForwardedToRef<StatementKind, unit> ()

    // 11.1 <schema definition> ::= CREATE SCHEMA <schema name clause> [ <schema character set or path> ] [ <schema element>... ]
    let pCreateSchemaStatement =
        let pNameClause =
            choice
                [ attempt (
                      pQualifiedNameExpr .>>. opt (pKeyword "AUTHORIZATION" >>. pIdentifierExpr)
                      |>> fun (name, auth) -> Some name, auth
                  )
                  pKeyword "AUTHORIZATION" >>. pIdentifierExpr |>> fun auth -> None, Some auth ]

        // 11.1 <schema character set or path> ::=
        //     <schema character set specification>
        //   | <schema path specification>
        //   | <character set specification> <path specification>
        //   | <path specification> <character set specification>
        let pSchemaCharsetOrPath =
            let pCharset =
                pKeyword "DEFAULT"
                >>. pKeyword "CHARACTER"
                >>. pKeyword "SET"
                >>. pQualifiedNameExpr

            // 11.1 <path specification> ::= PATH <path-resolved user-defined type name> [ { <comma> ... }... ]
            let pPath = pKeyword "PATH" >>. sepBy1 pQualifiedNameExpr (token (pstring ","))

            choice
                [ attempt (pCharset .>>. opt (attempt pPath) |>> fun (c, p) -> Some c, p)
                  attempt (pPath .>>. opt (attempt pCharset) |>> fun (p, c) -> c, Some p)
                  preturn (None, None) ]

        pKeyword "CREATE" >>. pKeyword "SCHEMA" >>. pNameClause
        .>>. pSchemaCharsetOrPath
        .>>. many pSchemaElement
        |>> fun ((nameClause, (charset, path)), elements) ->
            CreateSchema
                { Name = fst nameClause
                  Authorization = snd nameClause
                  CharacterSet = charset
                  Path = path
                  Elements = elements }

    // 11.34 <domain constraint> ::= [ <constraint name definition> ] CHECK ( <search condition> ) [ <constraint characteristics> ]
    let pDomainConstraint =
        opt (pKeyword "CONSTRAINT" >>. pQualifiedNameExpr)
        .>>. (pKeyword "CHECK"
              >>. between (token (pstring "(")) (token (pstring ")")) pExpression)
        .>>. pConstraintCharacteristics
        |>> fun ((name, check), chars) ->
            { Name = name
              Check = check
              Characteristics = chars }

    // 11.34 <domain definition> ::= CREATE DOMAIN <domain name> [ AS ] <data type> [ <default clause> ] [ <domain constraint>... ] [ <collate clause> ]
    let pCreateDomainStatement =
        pKeyword "CREATE" >>. pKeyword "DOMAIN" >>. pQualifiedNameExpr
        .>>. opt (pKeyword "AS")
        .>>. pDataType
        .>>. opt (pKeyword "DEFAULT" >>. pExpression)
        .>>. many pDomainConstraint
        .>>. opt (pKeyword "COLLATE" >>. pQualifiedNameExpr)
        |>> fun (((((name, _), dataType), def), constraints), collation) ->
            CreateDomain
                { Name = name
                  DataType = dataType
                  Default = def
                  Constraints = constraints
                  Collation = collation }

    // 11.35 <alter domain statement> ::= ALTER DOMAIN <domain name> <alter domain action>
    let pAlterDomainStatement =
        let pAction =
            choice
                [ attempt (
                      pKeyword "SET" >>. pKeyword "DEFAULT" >>. pExpression
                      |>> DomainAlteration.SetDefault
                  )
                  attempt (pKeyword "DROP" >>. pKeyword "DEFAULT" >>% DomainAlteration.DropDefault)
                  attempt (pKeyword "ADD" >>. pDomainConstraint |>> DomainAlteration.AddConstraint)
                  attempt (
                      pKeyword "DROP" >>. pKeyword "CONSTRAINT" >>. pQualifiedNameExpr
                      |>> DomainAlteration.DropConstraint
                  ) ]

        pKeyword "ALTER" >>. pKeyword "DOMAIN" >>. pQualifiedNameExpr .>>. pAction
        |>> fun (name, action) -> AlterDomain(name, action)

    // 11.41 <character set definition> ::= CREATE CHARACTER SET <character set name> [ AS GET <character set name> ] [ <collate clause> ]
    let pCreateCharacterSetStatement =
        pKeyword "CREATE"
        >>. pKeyword "CHARACTER"
        >>. pKeyword "SET"
        >>. pQualifiedNameExpr
        .>>. opt (pKeyword "AS")
        .>>. (pKeyword "GET" >>. pQualifiedNameExpr)
        .>>. opt (pKeyword "COLLATE" >>. pQualifiedNameExpr)
        |>> fun (((name, _), source), collate) -> CreateCharacterSet(name, source, collate)

    // 11.43 <collation definition> ::= CREATE COLLATION <collation name> FOR <character set name> FROM <collation name> [ <pad characteristic> ]
    let pCreateCollationStatement =
        // 11.43 <pad characteristic> ::= NO PAD | PAD SPACE
        let pPadCharacteristic =
            pKeyword "NO" >>. pKeyword "PAD" >>% true
            <|> (pKeyword "PAD" >>. pKeyword "SPACE" >>% false)

        pKeyword "CREATE" >>. pKeyword "COLLATION" >>. pQualifiedNameExpr
        .>>. (pKeyword "FOR" >>. pQualifiedNameExpr)
        .>>. (pKeyword "FROM" >>. pQualifiedNameExpr)
        .>>. opt pPadCharacteristic
        |>> fun (((name, cs), existing), pad) -> CreateCollation(name, cs, existing, pad)

    // 11.45 <transliteration definition> ::= CREATE TRANSLATION <transliteration name> FOR <source character set> TO <target character set> FROM <transliteration source>
    let pCreateTransliterationStatement =
        pKeyword "CREATE" >>. pKeyword "TRANSLATION" >>. pQualifiedNameExpr
        .>>. (pKeyword "FOR" >>. pQualifiedNameExpr)
        .>>. (pKeyword "TO" >>. pQualifiedNameExpr)
        .>>. (pKeyword "FROM" >>. pQualifiedNameExpr)
        |>> fun (((name, source), target), trSource) -> CreateTransliteration(name, source, target, trSource)

    // 11.47 <assertion definition> ::= CREATE ASSERTION <constraint name> CHECK ( <search condition> ) [ <constraint characteristics> ]
    let pCreateAssertionStatement =
        pKeyword "CREATE" >>. pKeyword "ASSERTION" >>. pQualifiedNameExpr
        .>>. (pKeyword "CHECK"
              >>. between (token (pstring "(")) (token (pstring ")")) pExpression)
        .>>. pConstraintCharacteristics
        |>> fun ((name, check), chars) -> CreateAssertion(name, check, chars)

    // 11.63 <user-defined cast definition> ::= CREATE CAST ( <source data type> AS <target data type> ) WITH <cast function> [ AS ASSIGNMENT ]
    let pCreateCastStatement =
        pKeyword "CREATE"
        >>. pKeyword "CAST"
        >>. between (token (pstring "(")) (token (pstring ")")) (pDataType .>>. (pKeyword "AS" >>. pDataType))
        .>>. (pKeyword "WITH" >>. pSpecificRoutineDesignator)
        .>>. opt (pKeyword "AS" >>. pKeyword "ASSIGNMENT" >>% true)
        |>> fun (((source, target), fn), assignment) ->
            CreateCast(source, target, fn, Option.defaultValue false assignment)

    // 11.65 <ordering category> ::= RELATIVE WITH <specific routine designator> | MAP WITH <specific routine designator> | STATE [ <data type> ]
    let pOrderingCategory =
        choice
            [ attempt (
                  pKeyword "RELATIVE" >>. pKeyword "WITH" >>. pSpecificRoutineDesignator
                  |>> OrderingCategory.Relative
              )
              attempt (
                  pKeyword "MAP" >>. pKeyword "WITH" >>. pSpecificRoutineDesignator
                  |>> OrderingCategory.Map
              )
              attempt (pKeyword "STATE" >>. opt pQualifiedNameExpr |>> OrderingCategory.State) ]

    // 11.65 <ordering form> ::= EQUALS ONLY BY <ordering category> | ORDER FULL BY <ordering category>
    let pOrderingForm =
        pKeyword "EQUALS" >>. pKeyword "ONLY" >>. pKeyword "BY" >>. pOrderingCategory
        |>> OrderingForm.EqualsOnlyBy
        <|> (pKeyword "ORDER" >>. pKeyword "FULL" >>. pKeyword "BY" >>. pOrderingCategory
             |>> OrderingForm.OrderFullBy)

    // 11.65 <user-defined ordering definition> ::= CREATE ORDERING FOR <schema-resolved user-defined type name> <ordering form>
    let pCreateOrderingStatement =
        pKeyword "CREATE"
        >>. pKeyword "ORDERING"
        >>. pKeyword "FOR"
        >>. pQualifiedNameExpr
        .>>. pOrderingForm
        |>> fun (name, form) -> CreateOrdering(name, form)

    // 11.67 <transform element> ::= TO SQL WITH <specific routine designator> | FROM SQL WITH <specific routine designator>
    let pTransformElement =
        pKeyword "TO"
        >>. pKeyword "SQL"
        >>. pKeyword "WITH"
        >>. pSpecificRoutineDesignator
        |>> TransformElement.ToSql
        <|> (pKeyword "FROM"
             >>. pKeyword "SQL"
             >>. pKeyword "WITH"
             >>. pSpecificRoutineDesignator
             |>> TransformElement.FromSql)

    // 11.67 <transform group> ::= <group name> ( <transform element> [ { <comma> <transform element> }... ] )
    let pTransformGroup =
        pQualifiedNameExpr
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTransformElement (token (pstring ",")))
        |>> fun (name, elements) -> { Name = name; Elements = elements }

    // 11.67 <transform definition> ::= CREATE { TRANSFORM | TRANSFORMS } FOR <schema-resolved user-defined type name> <transform group> [ { <comma> <transform group> }... ]
    let pCreateTransformStatement =
        pKeyword "CREATE"
        >>. (pKeyword "TRANSFORM" <|> pKeyword "TRANSFORMS")
        >>. pKeyword "FOR"
        >>. pQualifiedNameExpr
        .>>. many1 pTransformGroup
        |>> fun (name, groups) -> CreateTransform(name, groups)

    // 11.68 <transform kind> ::= TO SQL | FROM SQL
    let pTransformKind =
        pKeyword "TO" >>. pKeyword "SQL" >>% TransformKind.ToSqlKind
        <|> (pKeyword "FROM" >>. pKeyword "SQL" >>% TransformKind.FromSqlKind)

    // 11.68 <alter transform action> ::= ADD <transform element list> | DROP ( <transform kind list> ) <drop behavior>
    let pAlterTransformAction =
        pKeyword "ADD"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTransformElement (token (pstring ",")))
        |>> TransformAlteration.AddTransformElements
        <|> (pKeyword "DROP"
             >>. between
                     (token (pstring "("))
                     (token (pstring ")"))
                     (sepBy1 pTransformKind (token (pstring ",")) .>>. pDropBehavior)
             |>> TransformAlteration.DropTransformElements)

    // 11.68 <alter transform group> ::= <group name> ( <alter transform action> [ { <comma> <alter transform action> }... ] )
    let pAlterTransformGroup =
        pQualifiedNameExpr
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pAlterTransformAction (token (pstring ",")))
        |>> fun (name, actions) -> { Name = name; Actions = actions }

    // 11.68 <alter transform statement> ::= ALTER { TRANSFORM | TRANSFORMS } FOR <schema-resolved user-defined type name> <alter transform group> [ { <comma> <alter transform group> }... ]
    let pAlterTransformStatement =
        pKeyword "ALTER"
        >>. (pKeyword "TRANSFORM" <|> pKeyword "TRANSFORMS")
        >>. pKeyword "FOR"
        >>. pQualifiedNameExpr
        .>>. many1 pAlterTransformGroup
        |>> fun (name, groups) -> AlterTransform(name, groups)
