namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.TransactionParser

module SessionParser =
    //   17.9 <set role statement> ::= SET ROLE <role specification>
    //   <role specification> ::= <value specification> | NONE
    let pSetRoleStatement =
        pKeyword "SET"
        >>. pKeyword "ROLE"
        >>. (attempt (pKeyword "NONE" >>% None) <|> (pExpression |>> Some))
        |>> SetRole

    // 19.1 <set session characteristics statement>
    // ::= SET SESSION CHARACTERISTICS AS <session characteristic list>
    // <session characteristic> ::= TRANSACTION <transaction mode> [ , ... ]
    let pSetSessionCharacteristicsStatement =
        pKeyword "SET"
        >>. pKeyword "SESSION"
        >>. pKeyword "CHARACTERISTICS"
        >>. pKeyword "AS"
        >>. pKeyword "TRANSACTION"
        >>. sepBy1 pTransactionMode (token (pstring ","))
        |>> SetSessionCharacteristics

    // 19.2 <set session user identifier statement>
    // ::= SET SESSION AUTHORIZATION <value specification>
    let pSetSessionAuthorizationStatement =
        pKeyword "SET"
        >>. pKeyword "SESSION"
        >>. pKeyword "AUTHORIZATION"
        >>. pExpression
        |>> SetSessionAuthorization

    // 19.4 <set local time zone statement>
    // ::= SET TIME ZONE <set time zone value>
    // <set time zone value> ::= <interval value expression> | LOCAL
    // None = LOCAL.
    let pSetTimeZoneStatement =
        pKeyword "SET"
        >>. pKeyword "TIME"
        >>. pKeyword "ZONE"
        >>. (attempt (pKeyword "LOCAL" >>% None) <|> (pExpression |>> Some))
        |>> SetTimeZone

    // 19.5 <set catalog statement> ::= SET CATALOG <value specification>
    let pSetCatalogStatement =
        pKeyword "SET" >>. pKeyword "CATALOG" >>. pExpression |>> SetCatalog

    // 19.6 <set schema statement> ::= SET SCHEMA <value specification>
    let pSetSchemaStatement =
        pKeyword "SET" >>. pKeyword "SCHEMA" >>. pExpression |>> SetSchema

    // 19.7 <set names statement> ::= SET NAMES <value specification>
    let pSetNamesStatement =
        pKeyword "SET" >>. pKeyword "NAMES" >>. pExpression |>> SetNames

    // 19.8 <set path statement> ::= SET PATH <value specification>
    let pSetPathStatement =
        pKeyword "SET" >>. pKeyword "PATH" >>. pExpression |>> SetPath

    // 19.9 <set transform group statement> ::= SET <transform group characteristic>
    // 19.9 <transform group characteristic> ::=
    //     DEFAULT TRANSFORM GROUP <value specification>
    //   | TRANSFORM GROUP FOR TYPE <path-resolved user-defined type name>
    //       <value specification>
    // SetTransformGroup (group value, optional FOR TYPE name).
    let pSetTransformGroupStatement =
        pKeyword "SET"
        >>. (attempt (
                 pKeyword "DEFAULT" >>. pKeyword "TRANSFORM" >>. pKeyword "GROUP" >>. pExpression
                 |>> fun g -> g, None
             )
             <|> (pKeyword "TRANSFORM"
                  >>. pKeyword "GROUP"
                  >>. pKeyword "FOR"
                  >>. pKeyword "TYPE"
                  >>. pQualifiedName
                  .>>. pExpression
                  |>> fun (t, g) -> g, Some t))
        |>> fun (group, typeName) -> SetTransformGroup(group, typeName)

    // 19.10 <set session collation statement>
    // ::= SET COLLATION <collation specification> [ FOR <character set specification list> ]
    //   | SET NO COLLATION [ FOR <character set specification list> ]
    // SetSessionCollation (collation option, FOR charset list option).
    // None collation = NO COLLATION.
    let pSetSessionCollationStatement =
        pKeyword "SET"
        >>. (attempt (pKeyword "NO" >>. pKeyword "COLLATION" >>% None)
             <|> (pKeyword "COLLATION" >>. pExpression |>> Some))
        .>>. opt (attempt (pKeyword "FOR" >>. sepBy1 pExpression (token (pstring ","))))
        |>> fun (collation, charsets) -> SetSessionCollation(collation, charsets)
