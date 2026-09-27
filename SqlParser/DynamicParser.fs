namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module DynamicParser =
    // `pExtendedName` (5.4 <extended descriptor name>, 20.17 <extended statement name>,
    // 20.17 <extended cursor name>) lives in DataManipulationParser.fs because the
    // 20.10/20.11 <using descriptor> tail in this module compiles later than that
    // module; the shared parser is consumed by both.

    // 20.2 <allocate descriptor statement> ::= ALLOCATE [ SQL ] DESCRIPTOR <conventional descriptor name> [ WITH MAX <occurrences> ]
    // <conventional descriptor name> ::= <non-extended descriptor name> | <extended descriptor name>
    // <non-extended descriptor name> ::= <identifier> — a single part, with no catalog or
    // schema qualifier. The extended form (20.17) is `[ <scope option> ] <simple value specification>`.
    let pAllocateDescriptorStatement =
        pKeyword "ALLOCATE" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. DataManipulationParser.pExtendedName
        // <occurrences> ::= <simple value specification> (strict)
        .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "MAX" >>. pSimpleValueSpecification))
        |>> fun (name, max) -> AllocateDescriptor(name, max)

    // 20.3 <deallocate descriptor statement> ::= DEALLOCATE [ SQL ] DESCRIPTOR <conventional descriptor name>
    let pDeallocateDescriptorStatement =
        pKeyword "DEALLOCATE" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. DataManipulationParser.pExtendedName
        |>> DeallocateDescriptor

    // 20.4 <header item name> — closed enumeration.
    let private pHeaderItemName =
        choice
            [ pKeyword "COUNT"
              pKeyword "KEY_TYPE"
              pKeyword "DYNAMIC_FUNCTION"
              pKeyword "DYNAMIC_FUNCTION_CODE"
              pKeyword "TOP_LEVEL_COUNT" ]

    // 20.4/20.5 <descriptor item name> — closed enumeration.
    let private pDescriptorItemName =
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

    // 20.4 <get descriptor statement> ::= GET [ SQL ] DESCRIPTOR <descriptor name> <get descriptor information>
    let pGetDescriptorStatement =
        // 20.4 <get header information> ::= <simple target specification 1> <equals operator> <header item name>
        // 20.4 <simple target specification 1> ::= <simple target specification> (6.4), so a host
        // parameter (`:x`) is a legal target — not just a column reference.
        let pGetHeaderInformation =
            DataManipulationParser.pSimpleTargetSpecification .>> token (pstring "=")
            .>>. pHeaderItemName
            |>> fun (target, name) -> target, name

        // 20.4 <get item information> ::= <simple target specification 2> <equals operator> <descriptor item name>
        let pGetItemInformation =
            DataManipulationParser.pSimpleTargetSpecification .>> token (pstring "=")
            .>>. pDescriptorItemName
            |>> fun (target, name) -> target, name

        pKeyword "GET" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. DataManipulationParser.pExtendedName
        .>>. (attempt (
                  pKeyword "VALUE" >>. pSimpleValueSpecification
                  .>>. sepBy1 pGetItemInformation (token (pstring ","))
                  |>> fun (num, items) -> GetItem(num, items)
              )
              <|> (sepBy1 pGetHeaderInformation (token (pstring ",")) |>> GetHeader))
        |>> fun (name, info) -> GetDescriptor(name, info)

    // 20.5 <set descriptor statement> ::= SET [ SQL ] DESCRIPTOR <descriptor name> <set descriptor information>
    let pSetDescriptorStatement =
        // 20.5 <set header information> ::= <header item name> <equals operator> <value>
        let pSetHeaderInformation =
            pHeaderItemName .>> token (pstring "=") .>>. pSimpleValueSpecification
            |>> fun (name, value) -> name, value

        // 20.5 <set item information> ::= <descriptor item name> <equals operator> <value> (item form)
        let pSetItemInformation =
            pDescriptorItemName .>> token (pstring "=") .>>. pSimpleValueSpecification
            |>> fun (name, value) -> name, value

        pKeyword "SET" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. DataManipulationParser.pExtendedName
        .>>. (attempt (
                  pKeyword "VALUE" >>. pSimpleValueSpecification
                  .>>. sepBy1 pSetItemInformation (token (pstring ","))
                  |>> fun (num, items) -> SetItem(num, items)
              )
              <|> (sepBy1 pSetHeaderInformation (token (pstring ",")) |>> SetHeader))
        |>> fun (name, info) -> SetDescriptor(name, info)

    let pCopyDescriptorStatement =
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

        // 20.6 <copy descriptor statement> ::= COPY <source descriptor name> TO <target descriptor name>
        //     | COPY <source descriptor name> VALUE <item number 1> ( <options> ) TO <target descriptor name> VALUE <item number 2>
        // 20.6 <source descriptor name> ::= <descriptor name> — admits the 5.4 extended form via `pExtendedName`.
        // 20.6 <target descriptor name> ::= <PTF descriptor name> ::= PTF <simple value specification>.
        let pTargetDescriptorName = pKeyword "PTF" >>. pSimpleValueSpecification

        pKeyword "COPY" >>. DataManipulationParser.pExtendedName
        >>= fun source ->
            attempt (
                pKeyword "VALUE" >>. pSimpleValueSpecification
                >>= fun srcItem ->
                    between (token (pstring "(")) (token (pstring ")")) pCopyDescriptorOptions
                    >>= fun opts ->
                        pKeyword "TO" >>. pTargetDescriptorName
                        >>= fun target ->
                            pKeyword "VALUE" >>. pSimpleValueSpecification
                            |>> fun tgtItem ->
                                CopyDescriptor
                                    { Source = source
                                      SourceItem = Some srcItem
                                      Options = Some opts
                                      Target = target
                                      TargetItem = Some tgtItem }
            )
            <|> (pKeyword "TO" >>. pTargetDescriptorName
                 |>> fun target ->
                     CopyDescriptor
                         { Source = source
                           SourceItem = None
                           Options = None
                           Target = target
                           TargetItem = None })

    // 20.7 <prepare statement> ::= PREPARE <SQL statement name> [ <attributes specification> ] FROM <SQL statement variable>
    // <SQL statement name> ::= <statement name> | <extended statement name> (20.17) — the extended
    // form is `[ <scope option> ] <simple value specification>`.
    let pPrepareStatement =
        pKeyword "PREPARE" >>. DataManipulationParser.pExtendedName
        .>>. opt (attempt (pKeyword "ATTRIBUTES" >>. pSimpleValueSpecification))
        .>> pKeyword "FROM"
        .>>. pSimpleValueSpecification
        |>> fun ((name, attrs), stmt) -> Prepare(name, attrs, stmt)

    // 20.9 <deallocate prepared statement> ::= DEALLOCATE PREPARE <SQL statement name>
    let pDeallocatePreparedStatement =
        pKeyword "DEALLOCATE"
        >>. pKeyword "PREPARE"
        >>. DataManipulationParser.pExtendedName
        |>> DeallocatePrepare

    // 20.10 <describe statement> ::= <describe input statement> | <describe output statement>
    // <describe input statement>  ::= DESCRIBE INPUT <SQL statement name> <using descriptor> [ <nesting option> ]
    // <describe output statement> ::= DESCRIBE [ OUTPUT ] <described object> <using descriptor> [ <nesting option> ]
    // <described object>           ::= <SQL statement name> | CURSOR <cursor name> STRUCTURE
    // Both <SQL statement name> and <cursor name> admit the 20.17 extended form
    // `[ <scope option> ] <simple value specification>`. INPUT commits to <describe input
    // statement> (no backtracking) so DESCRIBE INPUT CURSOR ... is rejected.
    let pDescribeStatement =
        // 20.10 <nesting option> ::= WITH NESTING | WITHOUT NESTING
        let pNestingOption =
            pKeyword "WITH" >>. pKeyword "NESTING" >>% true
            <|> (pKeyword "WITHOUT" >>. pKeyword "NESTING" >>% false)

        // 20.10 <using descriptor> ::= USING [ SQL ] DESCRIPTOR <descriptor name>
        let pUsingDescriptor =
            pKeyword "USING" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
            >>. DataManipulationParser.pExtendedName

        // 20.10 <cursor name> in the CURSOR branch is the strict 5.4 <local qualified name>
        // (at most two parts, MODULE the only <local qualifier>); the 20.17 extended form
        // is reserved for 20.17 ALLOCATE / 20.15 DECLARE.
        let pDescribeCursorName =
            // Wrapped as an ExtendedName so the AST shape is uniform.
            pLocalQualifiedNameExpression |>> fun n -> { Scope = None; SimpleValue = n }

        pKeyword "DESCRIBE"
        >>= fun _ ->
            (pKeyword "INPUT" >>. DataManipulationParser.pExtendedName
             >>= fun name ->
                 pUsingDescriptor
                 >>= fun desc ->
                     opt (attempt pNestingOption)
                     |>> fun nesting ->
                         { IsInput = true
                           IsCursor = false
                           Name = name
                           Descriptor = desc
                           Nesting = nesting })
            <|> (opt (pKeyword "OUTPUT" >>% ())
                 >>= fun _ ->
                     attempt (
                         pKeyword "CURSOR" >>. pDescribeCursorName .>> pKeyword "STRUCTURE"
                         |>> fun c -> true, c
                     )
                     <|> (DataManipulationParser.pExtendedName |>> fun n -> false, n)
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

    // 20.13 <execute statement> ::= EXECUTE <SQL statement name> [ <output using clause> ] [ <input using clause> ]
    // <SQL statement name> admits the 20.17 extended form.
    let pExecuteStatement =
        pKeyword "EXECUTE" >>. DataManipulationParser.pExtendedName
        .>>. opt (attempt DataManipulationParser.pOutputUsingClause)
        .>>. opt (attempt DataManipulationParser.pInputUsingClause)
        |>> fun ((name, result), param) -> Execute(name, result, param)

    // 20.14 <execute immediate statement> ::= EXECUTE IMMEDIATE <SQL statement variable>
    // Must be tried before <execute statement> so EXECUTE IMMEDIATE isn't
    // consumed as EXECUTE <name> with name = IMMEDIATE.
    let pExecuteImmediateStatement =
        pKeyword "EXECUTE" >>. pKeyword "IMMEDIATE" >>. pSimpleValueSpecification
        |>> ExecuteImmediate

    // 5.4 <extended descriptor name> / 20.17 <extended statement name> /
    // 20.17 <extended cursor name> are now defined in DataManipulationParser.fs
    // (`pExtendedName`).

    // 20.15 <dynamic declare cursor> ::= DECLARE <cursor name> <cursor properties> FOR <statement name>
    // <statement name> ::= <identifier> — the plain form, NOT the 20.17
    // <extended statement name> (`[ <scope option> ] <simple value specification>`).
    let pDynamicDeclareCursorStatement =
        pKeyword "DECLARE" >>. pLocalQualifiedNameExpression
        .>>. DataManipulationParser.pCursorProperties
        .>> pKeyword "FOR"
        .>>. pIdentifierNameExpression
        |>> fun ((name, properties), statement) ->
            // A 20.15 <statement name> has no <scope option>, so the <extended statement name>
            // record is filled with the absent scope. The intermediate binding is required:
            // an inline record inside a record literal does not parse here.
            let statementName: ExtendedName =
                { Scope = None
                  SimpleValue = statement }

            { Name = name
              Properties = properties
              Statement = statementName }
            |> DynamicDeclareCursor

    // 20.17 <allocate extended dynamic cursor statement> ::= ALLOCATE <extended cursor name>
    //     <cursor properties> FOR <extended statement name>
    let pAllocateExtendedDynamicCursorStatement =
        pKeyword "ALLOCATE" >>. DataManipulationParser.pExtendedName
        .>>. DataManipulationParser.pCursorProperties
        .>> pKeyword "FOR"
        .>>. DataManipulationParser.pExtendedName
        |>> fun ((cursor, properties), statement) ->
            { Cursor = cursor
              Properties = properties
              Statement = statement }
            |> AllocateExtendedDynamicCursor

    // 20.18 <allocate received cursor statement> ::= ALLOCATE <cursor name> [ CURSOR ]
    //     FOR PROCEDURE <specific routine designator>
    let pAllocateReceivedCursorStatement =
        pKeyword "ALLOCATE" >>. pLocalQualifiedNameExpression
        .>>. opt (pKeyword "CURSOR" >>% ())
        .>> pKeyword "FOR"
        .>> pKeyword "PROCEDURE"
        .>>. SchemaParser.pSpecificRoutineDesignator
        |>> fun ((name, _), routine) -> { Name = name; Routine = routine } |> AllocateReceivedCursor

    // 20.19 <dynamic open statement> ::= OPEN <extended cursor name> [ <input using clause> ]
    // The 20.19 form uses the 20.17 <extended cursor name> `[ <scope option> ] <simple value
    // specification>` rather than the 5.4 <cursor name> of 14.4 <open statement>.
    let pDynamicOpenStatement =
        pKeyword "OPEN" >>. DataManipulationParser.pExtendedName
        .>>. opt (attempt DataManipulationParser.pInputUsingClause)
        |>> DynamicOpen

    // 20.20 <dynamic fetch statement> ::=
    //     FETCH [ [ <fetch orientation> ] FROM ] <extended cursor name> <output using clause>
    // The <output using clause> accepts both <into arguments> and <into descriptor>; the
    // <fetch orientation> is shared with 14.5 <fetch statement>.
    let pDynamicFetchStatement =
        let pFetchOrientation =
            pKeyword "NEXT" >>% Next
            <|> (pKeyword "PRIOR" >>% Prior)
            <|> (pKeyword "FIRST" >>% First)
            <|> (pKeyword "LAST" >>% Last)
            <|> (pKeyword "ABSOLUTE" >>. pSimpleValueSpecification |>> Absolute)
            <|> (pKeyword "RELATIVE" >>. pSimpleValueSpecification |>> Relative)

        pKeyword "FETCH" >>. opt (attempt (opt pFetchOrientation .>> pKeyword "FROM"))
        .>>. DataManipulationParser.pExtendedName
        .>>. DataManipulationParser.pOutputUsingClause
        |>> fun ((head, cursor), output) -> DynamicFetch(Option.flatten head, cursor, output)

    // 20.22 <dynamic close statement> ::= CLOSE <extended cursor name>
    let pDynamicCloseStatement =
        pKeyword "CLOSE" >>. DataManipulationParser.pExtendedName |>> DynamicClose

    // 20.28 <pipe row statement> ::= PIPE ROW <PTF descriptor name>
    // 20.28 <PTF descriptor name> ::= PTF <simple value specification>
    let pPipeRowStatement =
        pKeyword "PIPE"
        >>. pKeyword "ROW"
        >>. pKeyword "PTF"
        >>. pSimpleValueSpecification
        |>> PipeRow
