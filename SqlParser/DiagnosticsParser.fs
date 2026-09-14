namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module DiagnosticsParser =
    // 23.1 <statement information item name> — closed enumeration of reserved keywords.
    let pStatementInfoItemName =
        choice
            [ pKeyword "NUMBER"
              pKeyword "MORE"
              pKeyword "COMMAND_FUNCTION"
              pKeyword "COMMAND_FUNCTION_CODE"
              pKeyword "DYNAMIC_FUNCTION"
              pKeyword "DYNAMIC_FUNCTION_CODE"
              pKeyword "ROW_COUNT"
              pKeyword "TRANSACTIONS_COMMITTED"
              pKeyword "TRANSACTIONS_ROLLED_BACK"
              pKeyword "TRANSACTION_ACTIVE" ]

    // 23.1 <condition information item name> — closed enumeration of reserved keywords.
    let pConditionInfoItemName =
        choice
            [ pKeyword "CATALOG_NAME"
              pKeyword "CLASS_ORIGIN"
              pKeyword "COLUMN_NAME"
              pKeyword "CONDITION_NUMBER"
              pKeyword "CONNECTION_NAME"
              pKeyword "CONSTRAINT_CATALOG"
              pKeyword "CONSTRAINT_NAME"
              pKeyword "CONSTRAINT_SCHEMA"
              pKeyword "CURSOR_NAME"
              pKeyword "MESSAGE_LENGTH"
              pKeyword "MESSAGE_OCTET_LENGTH"
              pKeyword "MESSAGE_TEXT"
              pKeyword "PARAMETER_MODE"
              pKeyword "PARAMETER_NAME"
              pKeyword "PARAMETER_ORDINAL_POSITION"
              pKeyword "RETURNED_SQLSTATE"
              pKeyword "ROUTINE_CATALOG"
              pKeyword "ROUTINE_NAME"
              pKeyword "ROUTINE_SCHEMA"
              pKeyword "SCHEMA_NAME"
              pKeyword "SERVER_NAME"
              pKeyword "SPECIFIC_NAME"
              pKeyword "SUBCLASS_ORIGIN"
              pKeyword "TABLE_NAME"
              pKeyword "TRIGGER_CATALOG"
              pKeyword "TRIGGER_NAME"
              pKeyword "TRIGGER_SCHEMA" ]

    // 23.1 <statement information item> ::= <simple target specification> <equals operator> <statement information item name>
    let pStatementInfoItem =
        pQualifiedNameExpr .>> token (pstring "=") .>>. pStatementInfoItemName

    // 23.1 <condition information item> ::= <simple target specification> <equals operator> <condition information item name>
    let pConditionInfoItem =
        pQualifiedNameExpr .>> token (pstring "=") .>>. pConditionInfoItemName

    // 23.1 <get diagnostics statement> ::= GET DIAGNOSTICS <SQL diagnostics information>
    // <SQL diagnostics information> ::= <statement information> | <condition information> | <all information>
    // <statement information> ::= <statement information item> [ { <comma> <statement information item> }... ]
    // <condition information> ::= CONDITION <condition number> <condition information item> [ { <comma> <condition information item> }... ]
    // <all information> ::= <all info target> <equals operator> ALL [ <all qualifier> ]
    let pGetDiagnosticsStatement =
        pKeyword "GET"
        >>. pKeyword "DIAGNOSTICS"
        >>. (attempt (
                 pKeyword "CONDITION" >>. pExpression
                 .>>. sepBy1 pConditionInfoItem (token (pstring ","))
                 |>> fun (num, items) -> ConditionInfo(num, items)
             )
             <|> attempt (
                 pQualifiedNameExpr .>> token (pstring "=") .>> pKeyword "ALL"
                 .>>. opt (
                     attempt (pKeyword "STATEMENT" >>% AllStatement)
                     <|> (pKeyword "CONDITION" >>. opt pExpression |>> AllCondition)
                 )
                 |>> fun (target, qual) -> AllInfo(target, qual)
             )
             <|> (sepBy1 pStatementInfoItem (token (pstring ",")) |>> StatementInfo))
        |>> GetDiagnostics
