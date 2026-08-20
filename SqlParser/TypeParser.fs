namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.Types
open SqlParser.RoutineParser

module TypeParser =
    // 11.52 <attribute definition> ::= <attribute name> <data type>
    //     [ <attribute default> ] [ <collate clause> ]
    let pAttributeDefinition =
        pIdentifierExpr
        .>>. pDataType
        .>>. opt (pKeyword "DEFAULT" >>. pExpression)
        .>>. opt (pKeyword "COLLATE" >>. pQualifiedName)
        |>> fun (((name, dataType), def), collate) ->
            { Name = name
              DataType = dataType
              Default = def
              Collate = collate }

    // 11.51 <member list> ::= ( <attribute definition> { , <attribute definition> } )
    let pMemberList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy1 pAttributeDefinition (token (pstring ",")))

    // 11.51 <representation> ::= <predefined type> | <collection type> | <member list>
    // (the <predefined type> alternative is tried first so that AS ROW ( ... ) /
    // AS INT ARRAY etc. parse as data types, not member lists)
    let pRepresentation =
        choice
            [ attempt (pDataType |>> TypeRepresentation.Predefined)
              attempt (pMemberList |>> TypeRepresentation.MemberList) ]

    // 11.51 <user-defined type option> ::= <instantiable clause> | <finality> | <reference type specification> | <cast to ref> | <cast to type> | <cast to distinct> | <cast to source>
    let pTypeOption =
        choice
            [ // 11.51 <instantiable clause> ::= INSTANTIABLE | NOT INSTANTIABLE
              attempt (pKeyword "INSTANTIABLE" >>% TypeOption.Instantiable true)
              attempt (pKeyword "NOT" >>. pKeyword "INSTANTIABLE" >>% TypeOption.Instantiable false)
              // 11.51 <finality> ::= FINAL | NOT FINAL
              attempt (pKeyword "FINAL" >>% TypeOption.Final true)
              attempt (pKeyword "NOT" >>. pKeyword "FINAL" >>% TypeOption.Final false)
              // 11.51 <reference type specification> — <user-defined representation> ::= REF USING <predefined type>
              attempt (pKeyword "REF" >>. pKeyword "USING" >>. pDataType |>> TypeOption.RefUsing)
              // 11.51 <derived representation> ::= REF FROM <list of attributes>
              attempt (
                  pKeyword "REF"
                  >>. pKeyword "FROM"
                  >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                  |>> TypeOption.RefFrom
              )
              // 11.51 <system-generated representation> ::= REF IS SYSTEM GENERATED
              attempt (
                  pKeyword "REF" >>. pKeyword "IS" >>. pKeyword "SYSTEM" >>. pKeyword "GENERATED"
                  >>% TypeOption.RefIsSystemGenerated
              )
              // 11.51 <cast to ref> ::= CAST ( SOURCE AS REF ) WITH <cast to ref identifier>
              attempt (
                  pKeyword "CAST"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pKeyword "SOURCE" >>. pKeyword "AS" >>. pKeyword "REF" >>% ())
                  .>> pKeyword "WITH"
                  >>. pIdentifierExpr
                  |>> TypeOption.CastToRef
              )
              // 11.51 <cast to type> ::= CAST ( REF AS SOURCE ) WITH <cast to type identifier>
              attempt (
                  pKeyword "CAST"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pKeyword "REF" >>. pKeyword "AS" >>. pKeyword "SOURCE" >>% ())
                  .>> pKeyword "WITH"
                  >>. pIdentifierExpr
                  |>> TypeOption.CastToType
              )
              // 11.51 <cast to distinct> ::= CAST ( SOURCE AS DISTINCT ) WITH <cast to distinct identifier>
              attempt (
                  pKeyword "CAST"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pKeyword "SOURCE" >>. pKeyword "AS" >>. pKeyword "DISTINCT" >>% ())
                  .>> pKeyword "WITH"
                  >>. pIdentifierExpr
                  |>> TypeOption.CastToDistinct
              )
              // 11.51 <cast to source> ::= CAST ( DISTINCT AS SOURCE ) WITH <cast to source identifier>
              attempt (
                  pKeyword "CAST"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pKeyword "DISTINCT" >>. pKeyword "AS" >>. pKeyword "SOURCE" >>% ())
                  .>> pKeyword "WITH"
                  >>. pIdentifierExpr
                  |>> TypeOption.CastToSource
              ) ]

    // 11.51 <partial method specification> — [ INSTANCE | STATIC | CONSTRUCTOR ]
    let pMethodKind =
        choice
            [ attempt (pKeyword "INSTANCE" >>% MethodKind.Instance)
              attempt (pKeyword "STATIC" >>% MethodKind.Static)
              attempt (pKeyword "CONSTRUCTOR" >>% MethodKind.Constructor) ]

    // 11.51 <partial method specification> ::= [ INSTANCE | STATIC | CONSTRUCTOR ]
    //     METHOD <method name> <SQL parameter declaration list> <returns clause>
    //     [ SPECIFIC <specific method name> ]
    let pPartialMethodSpecification =
        opt pMethodKind
        .>>. (pKeyword "METHOD" >>. pIdentifierExpr)
        .>>. pParameterDeclarationList
        .>>. opt (pKeyword "RETURNS" >>. pDataType)
        .>>. opt (pKeyword "SPECIFIC" >>. pQualifiedName)
        |>> fun ((((kind, name), parameters), returns), specific) ->
            { Kind = kind
              Name = name
              Parameters = parameters
              Returns = returns
              Specific = specific
              SelfAsResult = false
              SelfAsLocator = false
              Characteristics = [] }

    // 11.51 <method characteristic> ::= <language clause> | <parameter style clause> | <deterministic characteristic> | <SQL-data access indication> | <null-call clause>
    let pMethodCharacteristic =
        choice
            [ // 11.51 <language clause> ::= LANGUAGE <language name>
              attempt (pKeyword "LANGUAGE" >>. pIdentifierRaw |>> Language)
              // 11.51 <parameter style clause> ::= PARAMETER STYLE <parameter style>
              attempt (pKeyword "PARAMETER" >>. pKeyword "STYLE" >>. pIdentifierRaw |>> ParameterStyle)
              // 11.51 <deterministic characteristic> ::= DETERMINISTIC | NOT DETERMINISTIC
              attempt (pKeyword "NOT" >>. pKeyword "DETERMINISTIC" >>% Deterministic false)
              attempt (pKeyword "DETERMINISTIC" >>% Deterministic true)
              // 11.51 <SQL-data access indication> ::= NO SQL | CONTAINS SQL | READS SQL DATA | MODIFIES SQL DATA
              attempt (pKeyword "NO" >>. pKeyword "SQL" >>% SqlDataAccess NoSql)
              attempt (pKeyword "CONTAINS" >>. pKeyword "SQL" >>% SqlDataAccess ContainsSql)
              attempt (
                  pKeyword "READS" >>. pKeyword "SQL" >>. pKeyword "DATA"
                  >>% SqlDataAccess ReadsSqlData
              )
              attempt (
                  pKeyword "MODIFIES" >>. pKeyword "SQL" >>. pKeyword "DATA"
                  >>% SqlDataAccess ModifiesSqlData
              )
              // 11.51 <null-call clause> ::= RETURNS NULL ON NULL INPUT | CALLED ON NULL INPUT
              attempt (
                  pKeyword "RETURNS"
                  >>. pKeyword "NULL"
                  >>. pKeyword "ON"
                  >>. pKeyword "NULL"
                  >>. pKeyword "INPUT"
                  >>% NullCall true
              )
              attempt (
                  pKeyword "CALLED" >>. pKeyword "ON" >>. pKeyword "NULL" >>. pKeyword "INPUT"
                  >>% NullCall false
              ) ]

    // 11.51 <original method specification> ::= <partial method specification>
    //     [ SELF AS RESULT ] [ SELF AS LOCATOR ] [ <method characteristics> ]
    let pOriginalMethodSpecification =
        pPartialMethodSpecification
        .>>. opt (attempt (pKeyword "SELF" >>. pKeyword "AS" >>. pKeyword "RESULT" >>% true))
        .>>. opt (attempt (pKeyword "SELF" >>. pKeyword "AS" >>. pKeyword "LOCATOR" >>% true))
        .>>. many pMethodCharacteristic
        |>> fun (((baseSpec, selfAsResult), selfAsLocator), characteristics) ->
            { baseSpec with
                SelfAsResult = Option.isSome selfAsResult
                SelfAsLocator = Option.isSome selfAsLocator
                Characteristics = characteristics }

    // 11.51 <method specification> ::= <original method specification>
    //     | OVERRIDING <partial method specification>
    let pMethodSpecification =
        choice
            [ attempt (pKeyword "OVERRIDING" >>. pPartialMethodSpecification)
              attempt pOriginalMethodSpecification ]

    // 11.51 <method specification list> ::= <method specification> [ { <comma> <method specification> }... ]
    let pMethodSpecificationList = sepBy1 pMethodSpecification (token (pstring ","))

    // 11.51 <user-defined type definition> ::= CREATE TYPE <user-defined type body>
    // 11.51 <user-defined type body> ::= <schema-resolved user-defined type name> [ <subtype clause> ] [ AS <representation> ] [ <user-defined type option list> ] [ <method specification list> ]
    // 11.51 <subtype clause> ::= UNDER <supertype name>
    let pCreateTypeStatement =
        pKeyword "CREATE" >>. pKeyword "TYPE" >>. pQualifiedName
        .>>. opt (pKeyword "UNDER" >>. pQualifiedName)
        .>>. opt (pKeyword "AS" >>. pRepresentation)
        .>>. many pTypeOption
        .>>. opt pMethodSpecificationList
        |>> fun ((((name, under), repr), options), methods) ->
            CreateType
                { Name = name
                  Under = under
                  Representation = repr
                  Options = options
                  Methods = Option.defaultValue [] methods }

    // 11.53 <alter type action> ::= <add attribute definition> | <drop attribute definition> | <add original method specification> | <add overriding method specification> | <drop method specification>
    let pAlterTypeAction =
        choice
            [ // 11.54 <add attribute definition> ::= ADD ATTRIBUTE <attribute definition>
              attempt (
                  pKeyword "ADD" >>. pKeyword "ATTRIBUTE" >>. pAttributeDefinition
                  |>> AlterTypeAction.AddAttribute
              )
              // 11.55 <drop attribute definition> ::= DROP ATTRIBUTE <attribute name> RESTRICT
              attempt (
                  pKeyword "DROP" >>. pKeyword "ATTRIBUTE" >>. pIdentifierExpr
                  .>> pKeyword "RESTRICT"
                  |>> AlterTypeAction.DropAttribute
              )
              // 11.56/11.57 <add [overriding] method specification>
              attempt (
                  pKeyword "ADD"
                  >>. choice
                          [ attempt (pKeyword "OVERRIDING" >>. pPartialMethodSpecification |>> fun s -> (s, true))
                            attempt (pOriginalMethodSpecification |>> fun s -> s, false) ]
                  |>> fun (spec, ovr) -> AlterTypeAction.AddMethod(spec, ovr)
              )
              // 11.58 <drop method specification> ::= DROP
              //     [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD <method name>
              //     <data type list> RESTRICT
              attempt (
                  pKeyword "DROP" >>. opt pMethodKind .>> pKeyword "METHOD"
                  .>>. pIdentifierExpr
                  .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pDataType (token (pstring ",")))
                  .>> pKeyword "RESTRICT"
                  |>> fun ((kind, name), types) -> AlterTypeAction.DropMethod(kind, name, types)
              ) ]

    // 11.53 <alter type statement> ::= ALTER TYPE <schema-resolved user-defined type name> <alter type action>
    let pAlterTypeStatement =
        pKeyword "ALTER" >>. pKeyword "TYPE" >>. pQualifiedName .>>. pAlterTypeAction
        |>> fun (name, action) -> AlterType { Name = name; Action = action }
