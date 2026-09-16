namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module ConnectionParser =
    // 18.1 <connect statement> ::= CONNECT TO <connection target>
    // <connection target> ::= <SQL-server name> [ AS <connection name> ] [ USER <connection user name> ] | DEFAULT
    // Server = None means CONNECT TO DEFAULT.
    let pConnectStatement =
        pKeyword "CONNECT"
        >>. pKeyword "TO"
        >>. (attempt (pKeyword "DEFAULT" >>% None)
             <|> (pSimpleValueSpecificationCompatibility
                  .>>. opt (attempt (pKeyword "AS" >>. pSimpleValueSpecificationCompatibility))
                  .>>. opt (attempt (pKeyword "USER" >>. pSimpleValueSpecificationCompatibility))
                  |>> fun ((server, name), user) -> Some(server, name, user)))
        |>> fun conn ->
            match conn with
            | None ->
                Connect
                    { Server = None
                      ConnectionName = None
                      User = None }
            | Some(server, name, user) ->
                Connect
                    { Server = Some server
                      ConnectionName = name
                      User = user }

    // 18.2 <set connection statement> ::= SET CONNECTION <connection object>
    // <connection object> ::= DEFAULT | <connection name>
    // None = DEFAULT.
    let pSetConnectionStatement =
        pKeyword "SET"
        >>. pKeyword "CONNECTION"
        >>. (attempt (pKeyword "DEFAULT" >>% None)
             <|> (pSimpleValueSpecificationCompatibility |>> Some))
        |>> SetConnection

    // 18.3 <disconnect statement> ::= DISCONNECT <disconnect object>
    // <disconnect object> ::= <connection object> | ALL | CURRENT
    // <connection object> ::= DEFAULT | <connection name>
    let pDisconnectStatement =
        pKeyword "DISCONNECT"
        >>. (attempt (pKeyword "ALL" >>% DisconnectAll)
             <|> attempt (pKeyword "CURRENT" >>% DisconnectCurrent)
             <|> attempt (pKeyword "DEFAULT" >>% DisconnectDefault)
             <|> (pSimpleValueSpecificationCompatibility |>> DisconnectName))
        |>> Disconnect
