namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module DynamicParser =
    // 20.10 <using descriptor> ::= USING [ SQL ] DESCRIPTOR <descriptor name> (DESCRIBE <using descriptor>)
    let pUsingDescriptor =
        pKeyword "USING" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. pQualifiedNameExpr

    // 20.10 <nesting option> ::= WITH NESTING | WITHOUT NESTING
    let pNestingOption =
        pKeyword "WITH" >>. pKeyword "NESTING" >>% true
        <|> (pKeyword "WITHOUT" >>. pKeyword "NESTING" >>% false)

    // 20.4 <get descriptor information> ::= <get header information> | VALUE <item number> <get item information>
    // <get item information> ::= <target> <equals operator> <descriptor item name> (item form)
    let pDescriptorInfoItem =
        pQualifiedNameExpr .>> token (pstring "=") .>>. pIdentifierRaw
        |>> fun (target, name) -> target, name

    // 20.5 <set descriptor information> ::= <set header information> | VALUE <item number> <set item information>
    // <set item information> ::= <descriptor item name> <equals operator> <value> (item form)
    let pSetDescriptorInfoItem =
        pIdentifierRaw .>> token (pstring "=") .>>. pExpression
        |>> fun (name, value) -> name, value

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

    // 20.4 <get descriptor statement> ::= GET [ SQL ] DESCRIPTOR <descriptor name> <get descriptor information>
    let pGetDescriptorStatement =
        pKeyword "GET" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. pQualifiedNameExpr
        .>>. (attempt (
                  pKeyword "VALUE" >>. pExpression
                  .>>. sepBy1 pDescriptorInfoItem (token (pstring ","))
                  |>> fun (num, items) -> GetItem(num, items)
              )
              <|> (sepBy1 pDescriptorInfoItem (token (pstring ",")) |>> GetHeader))
        |>> fun (name, info) -> GetDescriptor(name, info)

    // 20.5 <set descriptor statement> ::= SET [ SQL ] DESCRIPTOR <descriptor name> <set descriptor information>
    let pSetDescriptorStatement =
        pKeyword "SET" >>. opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR"
        >>. pQualifiedNameExpr
        .>>. (attempt (
                  pKeyword "VALUE" >>. pExpression
                  .>>. sepBy1 pSetDescriptorInfoItem (token (pstring ","))
                  |>> fun (num, items) -> SetItem(num, items)
              )
              <|> (sepBy1 pSetDescriptorInfoItem (token (pstring ",")) |>> SetHeader))
        |>> fun (name, info) -> SetDescriptor(name, info)

    // 20.6 <copy descriptor statement> ::= COPY <source> TO <target> | COPY <source> VALUE <n> ( <options> ) TO <target> VALUE <n>
    let pCopyDescriptorStatement =
        pKeyword "COPY" >>. pQualifiedNameExpr
        >>= fun source ->
            attempt (
                pKeyword "VALUE" >>. pExpression
                >>= fun srcItem ->
                    between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierRaw (token (pstring ",")))
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

    // 20.9 <deallocate prepared statement> ::= DEALLOCATE PREPARE <SQL statement name>
    let pDeallocatePrepareStatement =
        pKeyword "DEALLOCATE" >>. pKeyword "PREPARE" >>. pQualifiedNameExpr
        |>> DeallocatePrepare

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

    // 20.13 <output using clause> ::= INTO <into argument> [ { <comma> <into argument> }... ] | INTO [ SQL ] DESCRIPTOR <descriptor name>
    let pIntoClause =
        pKeyword "INTO"
        >>. (attempt (
                 pKeyword "SQL" >>. pKeyword "DESCRIPTOR" >>. pQualifiedNameExpr
                 |>> UsingDescriptor
             )
             <|> (sepBy1 pQualifiedNameExpr (token (pstring ",")) |>> UsingArguments))

    // 20.13 <input using clause> ::= USING <using argument> [ { <comma> <using argument> }... ] | USING [ SQL ] DESCRIPTOR <descriptor name>
    let pUsingClause =
        pKeyword "USING"
        >>. (attempt (
                 pKeyword "SQL" >>. pKeyword "DESCRIPTOR" >>. pQualifiedNameExpr
                 |>> UsingDescriptor
             )
             <|> (sepBy1 pExpression (token (pstring ",")) |>> UsingArguments))

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

    // 20.28 <pipe row statement> ::= PIPE ROW (<row value expression>)
    let pPipeRowStatement =
        pKeyword "PIPE" >>. pKeyword "ROW" >>. pQualifiedNameExpr |>> PipeRow
