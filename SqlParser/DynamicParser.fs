namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.DataManipulationParser

module DynamicParser =
    // 20.2 <allocate descriptor statement> ::= ALLOCATE [ SQL ] DESCRIPTOR <descriptor name> [ WITH MAX <occurrences> ]
    let pAllocateDescriptorStatement =
        pKeyword "ALLOCATE" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. pQualifiedNameExpr
        .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "MAX" >>. pExpression))
        |>> fun (name, max) -> AllocateDescriptor(name, max)

    // 20.3 <deallocate descriptor statement> ::= DEALLOCATE [ SQL ] DESCRIPTOR <descriptor name>
    let pDeallocateDescriptorStatement =
        pKeyword "DEALLOCATE" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. pQualifiedNameExpr
        |>> DeallocateDescriptor

    // 20.4 <header item name> — closed enumeration.
    let pHeaderItemName =
        choice
            [ pKeyword "COUNT"
              pKeyword "KEY_TYPE"
              pKeyword "DYNAMIC_FUNCTION"
              pKeyword "DYNAMIC_FUNCTION_CODE"
              pKeyword "TOP_LEVEL_COUNT" ]

    // 20.4/20.5 <descriptor item name> — closed enumeration.
    let pDescriptorItemName =
        choice
            [ pKeyword "CARDINALITY"
              pKeyword "CHARACTER_SET_CATALOG"
              pKeyword "CHARACTER_SET_NAME"
              pKeyword "CHARACTER_SET_SCHEMA"
              pKeyword "COLLATION_CATALOG"
              pKeyword "COLLATION_NAME"
              pKeyword "COLLATION_SCHEMA"
              pKeyword "DATA"
              pKeyword "DATETIME_INTERVAL_CODE"
              pKeyword "DATETIME_INTERVAL_PRECISION"
              pKeyword "DEGREE"
              pKeyword "INDICATOR"
              pKeyword "KEY_MEMBER"
              pKeyword "LENGTH"
              pKeyword "LEVEL"
              pKeyword "NAME"
              pKeyword "NULLABLE"
              pKeyword "NULL_ORDERING"
              pKeyword "OCTET_LENGTH"
              pKeyword "PARAMETER_MODE"
              pKeyword "PARAMETER_ORDINAL_POSITION"
              pKeyword "PARAMETER_SPECIFIC_CATALOG"
              pKeyword "PARAMETER_SPECIFIC_NAME"
              pKeyword "PARAMETER_SPECIFIC_SCHEMA"
              pKeyword "PRECISION"
              pKeyword "RETURNED_CARDINALITY"
              pKeyword "RETURNED_LENGTH"
              pKeyword "RETURNED_OCTET_LENGTH"
              pKeyword "SCALE"
              pKeyword "SCOPE_CATALOG"
              pKeyword "SCOPE_NAME"
              pKeyword "SCOPE_SCHEMA"
              pKeyword "SORT_DIRECTION"
              pKeyword "TYPE"
              pKeyword "UNNAMED"
              pKeyword "USER_DEFINED_TYPE_CATALOG"
              pKeyword "USER_DEFINED_TYPE_NAME"
              pKeyword "USER_DEFINED_TYPE_SCHEMA"
              pKeyword "USER_DEFINED_TYPE_CODE" ]

    // 20.4 <get header information> ::= <target> <equals operator> <header item name>
    let pHeaderInfoItem =
        pQualifiedNameExpr .>> token (pstring "=") .>>. pHeaderItemName
        |>> fun (target, name) -> target, name

    // 20.4 <get item information> ::= <target> <equals operator> <descriptor item name> (item form)
    let pDescriptorInfoItem =
        pQualifiedNameExpr .>> token (pstring "=") .>>. pDescriptorItemName
        |>> fun (target, name) -> target, name

    // 20.4 <get descriptor statement> ::= GET [ SQL ] DESCRIPTOR <descriptor name> <get descriptor information>
    let pGetDescriptorStatement =
        pKeyword "GET" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. pQualifiedNameExpr
        .>>. (attempt (
                  pKeyword "VALUE" >>. pExpression
                  .>>. sepBy1 pDescriptorInfoItem (token (pstring ","))
                  |>> fun (num, items) -> GetItem(num, items)
              )
              <|> (sepBy1 pHeaderInfoItem (token (pstring ",")) |>> GetHeader))
        |>> fun (name, info) -> GetDescriptor(name, info)

    // 20.5 <set header information> ::= <header item name> <equals operator> <value>
    let pSetHeaderInfoItem =
        pHeaderItemName .>> token (pstring "=") .>>. pExpression
        |>> fun (name, value) -> name, value

    // 20.5 <set item information> ::= <descriptor item name> <equals operator> <value> (item form)
    let pSetDescriptorInfoItem =
        pDescriptorItemName .>> token (pstring "=") .>>. pExpression
        |>> fun (name, value) -> name, value

    // 20.5 <set descriptor statement> ::= SET [ SQL ] DESCRIPTOR <descriptor name> <set descriptor information>
    let pSetDescriptorStatement =
        pKeyword "SET" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. pQualifiedNameExpr
        .>>. (attempt (
                  pKeyword "VALUE" >>. pExpression
                  .>>. sepBy1 pSetDescriptorInfoItem (token (pstring ","))
                  |>> fun (num, items) -> SetItem(num, items)
              )
              <|> (sepBy1 pSetHeaderInfoItem (token (pstring ",")) |>> SetHeader))
        |>> fun (name, info) -> SetDescriptor(name, info)

    // 20.6 <copy descriptor options> ::= NAME | TYPE | NAME , TYPE | DATA
    let pCopyDescriptorOptions =
        choice
            [ attempt (
                  pKeyword "NAME" >>. token (pstring ",") >>. pKeyword "TYPE"
                  >>% [ "NAME"; "TYPE" ]
              )
              pKeyword "NAME" >>% [ "NAME" ]
              pKeyword "TYPE" >>% [ "TYPE" ]
              pKeyword "DATA" >>% [ "DATA" ] ]

    // 20.6 <copy descriptor statement> ::= COPY <source> TO <target> | COPY <source> VALUE <n> ( <options> ) TO <target> VALUE <n>
    let pCopyDescriptorStatement =
        pKeyword "COPY" >>. pQualifiedNameExpr
        >>= fun source ->
            attempt (
                pKeyword "VALUE" >>. pExpression
                >>= fun srcItem ->
                    between (token (pstring "(")) (token (pstring ")")) pCopyDescriptorOptions
                    >>= fun opts ->
                        pKeyword "TO" >>. pQualifiedNameExpr
                        >>= fun target ->
                            pKeyword "VALUE" >>. pExpression
                            |>> fun tgtItem ->
                                CopyDescriptor
                                    { Source = source
                                      SourceItem = Some srcItem
                                      Options = Some opts
                                      Target = target
                                      TargetItem = Some tgtItem }
            )
            <|> (pKeyword "TO" >>. pQualifiedNameExpr
                 |>> fun target ->
                     CopyDescriptor
                         { Source = source
                           SourceItem = None
                           Options = None
                           Target = target
                           TargetItem = None })

    // 20.7 <prepare statement> ::= PREPARE <SQL statement name> [ <attributes specification> ] FROM <SQL statement variable>
    let pPrepareStatement =
        pKeyword "PREPARE" >>. pQualifiedNameExpr
        .>>. opt (attempt (pKeyword "ATTRIBUTES" >>. pExpression))
        .>> pKeyword "FROM"
        .>>. pExpression
        |>> fun ((name, attrs), stmt) -> Prepare(name, attrs, stmt)

    // 20.8 <cursor attribute> ::= <cursor sensitivity> | <cursor scrollability>
    //     | <cursor holdability> | <cursor returnability>
    let pCursorAttribute =
        choice
            [ attempt (pCursorSensitivity |>> CursorAttribute.SensitivityAttribute)
              attempt (pCursorScrollability |>> CursorAttribute.ScrollabilityAttribute)
              attempt (pCursorHoldability |>> CursorAttribute.HoldabilityAttribute)
              attempt (pCursorReturnability |>> CursorAttribute.ReturnabilityAttribute) ]

    // 20.8 <cursor attributes> ::= <cursor attribute>...
    // (20.8 is not referenced by any production in sql-2016-grammar.txt; exposed for
    //  library consumers — see docs/trade-off.md.)
    let pCursorAttributes = many1 pCursorAttribute

    // 20.9 <deallocate prepared statement> ::= DEALLOCATE PREPARE <SQL statement name>
    let pDeallocatePrepareStatement =
        pKeyword "DEALLOCATE" >>. pKeyword "PREPARE" >>. pQualifiedNameExpr
        |>> DeallocatePrepare

    // 20.10 <nesting option> ::= WITH NESTING | WITHOUT NESTING
    let pNestingOption =
        pKeyword "WITH" >>. pKeyword "NESTING" >>% true
        <|> (pKeyword "WITHOUT" >>. pKeyword "NESTING" >>% false)

    // 20.10 <using descriptor> ::= USING [ SQL ] DESCRIPTOR <descriptor name> (DESCRIBE <using descriptor>)
    let pUsingDescriptor =
        pKeyword "USING" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. pQualifiedNameExpr

    // 20.10 <describe statement> ::= DESCRIBE [ INPUT | OUTPUT ] <name> <using descriptor> [ <nesting option> ]
    //                            | DESCRIBE CURSOR <cursor> STRUCTURE <using descriptor> [ <nesting option> ]
    let pDescribeStatement =
        pKeyword "DESCRIBE"
        >>= fun _ ->
            attempt (
                pKeyword "INPUT" >>. pQualifiedNameExpr
                >>= fun name ->
                    pUsingDescriptor
                    >>= fun desc ->
                        opt (attempt pNestingOption)
                        |>> fun nesting ->
                            { IsInput = true
                              IsCursor = false
                              Name = name
                              Descriptor = desc
                              Nesting = nesting }
            )
            <|> (opt (pKeyword "OUTPUT" >>% ())
                 >>= fun _ ->
                     attempt (
                         pKeyword "CURSOR" >>. pQualifiedNameExpr .>> pKeyword "STRUCTURE"
                         |>> fun c -> true, c
                     )
                     <|> (pQualifiedNameExpr |>> fun n -> false, n)
                     >>= fun (isCursor, name) ->
                         pUsingDescriptor
                         >>= fun desc ->
                             opt (attempt pNestingOption)
                             |>> fun nesting ->
                                 { IsInput = false
                                   IsCursor = isCursor
                                   Name = name
                                   Descriptor = desc
                                   Nesting = nesting })
            |>> Describe

    // 20.11 <input using clause> / 20.12 <output using clause> are shared with
    // 20.19 <dynamic open statement> / 20.20 <dynamic fetch statement> and are
    // therefore defined in DataManipulationParser.fs (compiled before this module).

    // 20.13 <execute statement> ::= EXECUTE <SQL statement name> [ <output using clause> ] [ <input using clause> ]
    let pExecuteStatement =
        pKeyword "EXECUTE" >>. pQualifiedNameExpr
        .>>. opt (attempt pIntoClause)
        .>>. opt (attempt pUsingClause)
        |>> fun ((name, result), param) -> Execute(name, result, param)

    // 20.14 <execute immediate statement> ::= EXECUTE IMMEDIATE <SQL statement variable>
    // Must be tried before <execute statement> so EXECUTE IMMEDIATE isn't
    // consumed as EXECUTE <name> with name = IMMEDIATE.
    let pExecuteImmediateStatement =
        pKeyword "EXECUTE" >>. pKeyword "IMMEDIATE" >>. pExpression |>> ExecuteImmediate

    // 20.15 <statement name>
    // 20.17 <extended statement name>
    // 20.17 <extended cursor name>
    //     ::= [ <scope option> ] <simple value specification>
    let pExtendedName =
        opt (attempt pScopeOption) .>>. pSimpleValueSpecification
        |>> fun (scope, simpleValue) ->
            { Scope = scope
              SimpleValue = simpleValue }

    // 20.15 <dynamic declare cursor> ::= DECLARE <cursor name> <cursor properties> FOR <statement name>
    let pDynamicDeclareCursorStatement =
        pKeyword "DECLARE" >>. pQualifiedNameExpr .>>. pCursorProperties
        .>> pKeyword "FOR"
        .>>. pExtendedName
        |>> fun ((name, properties), statement) ->
            { Name = name
              Properties = properties
              Statement = statement }
            |> DynamicDeclareCursor

    // 20.17 <allocate extended dynamic cursor statement> ::= ALLOCATE <extended cursor name>
    //     <cursor properties> FOR <extended statement name>
    let pAllocateExtendedDynamicCursorStatement =
        pKeyword "ALLOCATE" >>. pExtendedName .>>. pCursorProperties .>> pKeyword "FOR"
        .>>. pExtendedName
        |>> fun ((cursor, properties), statement) ->
            { Cursor = cursor
              Properties = properties
              Statement = statement }
            |> AllocateExtendedDynamicCursor

    // 20.18 <allocate received cursor statement> ::= ALLOCATE <cursor name> [ CURSOR ]
    //     FOR PROCEDURE <specific routine designator>
    let pAllocateReceivedCursorStatement =
        pKeyword "ALLOCATE" >>. pQualifiedNameExpr .>>. opt (pKeyword "CURSOR" >>% ())
        .>> pKeyword "FOR"
        .>> pKeyword "PROCEDURE"
        .>>. SchemaParser.pSpecificRoutineDesignator
        |>> fun ((name, _), routine) -> { Name = name; Routine = routine } |> AllocateReceivedCursor

    // 20.28 <pipe row statement> ::= PIPE ROW (<row value expression>)
    let pPipeRowStatement =
        pKeyword "PIPE" >>. pKeyword "ROW" >>. pQualifiedNameExpr |>> PipeRow
