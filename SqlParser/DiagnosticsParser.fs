namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module DiagnosticsParser =
    // 23.1 <statement information item> ::= <simple target specification> <equals operator> <statement information item name>
    // 23.1 <condition information item> ::= <simple target specification> <equals operator> <condition information item name>
    // 23.1 <statement information item> / <condition information item> — <target> = <item name> (shorthand)
    let pInfoItem =
        pQualifiedName .>> token (pstring "=") .>>. pIdentifierRaw
        |>> fun (target, name) -> target, name

    // 23.1 <get diagnostics statement> ::= GET DIAGNOSTICS <SQL diagnostics information>
    // <SQL diagnostics information> ::= <statement information> | <condition information> | <all information>
    // <statement information> ::= <statement information item> [ { <comma> <statement information item> }... ]
    // <condition information> ::= CONDITION <condition number> <condition information item> [ { <comma> <condition information item> }... ]
    // <all information> ::= <all info target> <equals operator> ALL [ <all qualifier> ]
    let pGetDiagnosticsStatement =
        pKeyword "GET"
        >>. pKeyword "DIAGNOSTICS"
        >>. (attempt (
                 pKeyword "CONDITION" >>. pExpression .>>. sepBy1 pInfoItem (token (pstring ","))
                 |>> fun (num, items) -> ConditionInfo(num, items)
             )
             <|> attempt (
                 pQualifiedName .>> token (pstring "=") .>> pKeyword "ALL"
                 .>>. opt (
                     attempt (pKeyword "STATEMENT" >>% AllStatement)
                     <|> (pKeyword "CONDITION" >>. opt pExpression |>> AllCondition)
                 )
                 |>> fun (target, qual) -> AllInfo(target, qual)
             )
             <|> (sepBy1 pInfoItem (token (pstring ",")) |>> StatementInfo))
        |>> GetDiagnostics
