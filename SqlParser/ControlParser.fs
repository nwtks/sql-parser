namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module ControlParser =
    // 16.1 <call statement> ::= CALL <routine invocation>
    // <routine invocation> ::= <routine name> <SQL argument list>
    // <routine name> ::= [ <schema name> <period> ] <qualified identifier>
    let pCallStatement =
        pKeyword "CALL" >>. pQualifiedNameExpr
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy pExpression (token (pstring ",")))
        |>> fun (name, args) -> Call(name, args)

    // 16.2 <return statement> ::= RETURN <return value>
    // <return value> ::= <value expression> | NULL
    let pReturnStatement = pKeyword "RETURN" >>. pExpression |>> Return
