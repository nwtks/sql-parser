namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module ControlParser =
    // 16.1 <call statement> ::= CALL <routine invocation> — <SQL argument> admits a
    // 6.5 <contextually typed value specification> (so CALL f(NULL) is legal) and the
    // 10.4 <descriptor argument> (pSqlArgument).
    // <routine invocation> ::= <routine name> <SQL argument list>
    // <routine name> ::= [ <schema name> <period> ] <qualified identifier>
    let pCallStatement =
        pKeyword "CALL" >>. pSchemaQualifiedNameExpression .>>. pSqlArgumentList
        |>> fun (name, args) -> Call(name, args)

    // 16.2 <return statement> ::= RETURN <return value>
    // <return value> ::= <value expression> | NULL — NULL is an explicit alternative.
    let pReturnStatement =
        pKeyword "RETURN" >>. (pExpression <|> pNullSpecification) |>> Return
