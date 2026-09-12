namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.Types
open SqlParser.DdlParser

module RoutineParser =
    // 13.4 <SQL procedure statement> / 11.49 <triggered SQL statement> — forward ref
    // Forward reference to the full statement parser, wired in SqlParser.fs so
    // that <SQL procedure statement> / <triggered SQL statement> can contain any
    // statement (including nested routine/trigger definitions).
    let pRoutineBodyStatementRef, pRoutineBodyStatementRefImpl =
        createParserForwardedToRef<Statement, unit> ()

    // 10.2 <language name> ::= ADA | C | COBOL | FORTRAN | M | MUMPS | PASCAL | PLI | SQL
    // Shared with 11.51 <method characteristic> (TypeParser.fs) — TypeParser.fs is compiled
    // after RoutineParser.fs, so it reuses these two parsers (see docs/trade-off.md).
    let pLanguageName: Parser<string, unit> =
        choice
            [ attempt (pKeyword "FORTRAN" >>% "FORTRAN")
              attempt (pKeyword "MUMPS" >>% "MUMPS")
              attempt (pKeyword "PASCAL" >>% "PASCAL")
              attempt (pKeyword "COBOL" >>% "COBOL")
              attempt (pKeyword "ADA" >>% "ADA")
              attempt (pKeyword "PLI" >>% "PLI")
              attempt (pKeyword "SQL" >>% "SQL")
              attempt (pKeyword "M" >>% "M")
              attempt (pKeyword "C" >>% "C") ]

    // 10.2 <language clause> ::= LANGUAGE <language name>
    let pLanguageClause: Parser<string, unit> =
        attempt (pKeyword "LANGUAGE" >>. pLanguageName)

    // 11.60 <parameter style clause> ::= PARAMETER STYLE <parameter style>
    // 11.60 <parameter style> ::= SQL | GENERAL
    let pParameterStyleClause: Parser<string, unit> =
        pKeyword "PARAMETER"
        >>. pKeyword "STYLE"
        >>. ((pKeyword "SQL" >>% "SQL") <|> (pKeyword "GENERAL" >>% "GENERAL"))

    // 11.60 <parameter mode> ::= IN | OUT | INOUT
    let pParameterMode =
        choice
            [ attempt (pKeyword "INOUT" >>% ParameterMode.InOut)
              attempt (pKeyword "IN" >>% ParameterMode.In)
              attempt (pKeyword "OUT" >>% ParameterMode.Out) ]

    // 20.16 <descriptor value constructor> ::= DESCRIPTOR ( <descriptor column list> )
    // 20.16 <descriptor column specification> ::= <column name> [ <data type> ]
    // (only reachable here from <parameter default>; <descriptor argument> / PTF
    //  copartition is not implemented — see docs/trade-off.md.)
    let pDescriptorValueConstructor =
        pKeyword "DESCRIPTOR"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 (pIdentifierExpr .>>. opt pDataType) (token (pstring ",")))
        |>> DescriptorValueConstructor
        |> withExprPosition

    // 11.60 <locator indication> ::= AS LOCATOR
    let pLocatorIndication = pKeyword "AS" >>. pKeyword "LOCATOR" >>% true

    // 11.60 <pass through option> ::= PASS THROUGH | NO PASS THROUGH
    let pPassThroughOption =
        attempt (pKeyword "PASS" >>. pKeyword "THROUGH" >>% PassThroughOption.PassThrough)
        <|> (pKeyword "NO" >>. pKeyword "PASS" >>. pKeyword "THROUGH"
             >>% PassThroughOption.NoPassThrough)

    // 11.60 <generic table pruning> ::= PRUNE ON EMPTY | KEEP ON EMPTY
    let pGenericTablePruning =
        attempt (
            pKeyword "PRUNE" >>. pKeyword "ON" >>. pKeyword "EMPTY"
            >>% GenericTablePruning.PruneOnEmpty
        )
        <|> (pKeyword "KEEP" >>. pKeyword "ON" >>. pKeyword "EMPTY"
             >>% GenericTablePruning.KeepOnEmpty)

    // 11.60 <generic table semantics> ::= WITH ROW SEMANTICS
    //     | WITH SET SEMANTICS [ <generic table pruning> ]
    let pGenericTableSemantics =
        pKeyword "WITH"
        >>. choice
                [ attempt (pKeyword "ROW" >>. pKeyword "SEMANTICS" >>% GenericTableSemantics.RowSemantics)
                  attempt (
                      pKeyword "SET" >>. pKeyword "SEMANTICS" >>. opt (attempt pGenericTablePruning)
                      |>> GenericTableSemantics.SetSemantics
                  ) ]

    // 11.60 <parameter type> ::= <data type> [ <locator indication> ]
    //     | <generic table parameter type> | <descriptor parameter type>
    // 11.60 <generic table parameter type> ::= TABLE [ <pass through option> ] [ <generic table semantics> ]
    // NOTE: the two keyword-led alternatives are tried BEFORE <data type>, because their
    // leading keywords would otherwise be consumed as a user-defined type name
    // (TABLE is reserved, but DESCRIPTOR is a non-reserved keyword).
    let pParameterType =
        choice
            [ attempt (
                  pKeyword "TABLE" >>. opt (attempt pPassThroughOption)
                  .>>. opt (attempt pGenericTableSemantics)
                  |>> GenericTableParameter
              )
              attempt (pKeyword "DESCRIPTOR" >>% DescriptorParameter)
              attempt (
                  pDataType .>>. opt pLocatorIndication
                  |>> fun (dataType, locator) -> DataTypeParameter(dataType, Option.isSome locator)
              ) ]

    // 11.60 <SQL parameter declaration> ::= [ <parameter mode> ] [ <SQL parameter name> ]
    //     <parameter type> [ RESULT ] [ DEFAULT <parameter default> ]
    // The optional <SQL parameter name> must backtrack: for `IN mytype` the identifier after
    // the mode could be either the parameter name (followed by a type) or the type itself.
    let pParameterDeclaration =
        let pWithName: Parser<ParameterMode option * Expression option * ParameterType, unit> =
            opt pParameterMode .>>. pIdentifierExpr .>>. pParameterType
            |>> fun ((mode, name), paramType) -> mode, Some name, paramType

        let pWithoutName: Parser<ParameterMode option * Expression option * ParameterType, unit> =
            opt pParameterMode .>>. pParameterType
            |>> fun (mode, paramType) -> mode, None, paramType

        attempt (
            (attempt pWithName <|> pWithoutName)
            .>>. opt (pKeyword "RESULT")
            .>>. opt (pKeyword "DEFAULT" >>. (attempt pDescriptorValueConstructor <|> pExpression))
            |>> fun (((mode, name, paramType), isResult), defaultVal) ->
                { Mode = mode
                  Name = name
                  ParameterType = paramType
                  IsResult = Option.isSome isResult
                  Default = defaultVal }
        )

    // 11.60 <SQL parameter declaration list> ::= ( [ <SQL parameter declaration> [ { , <SQL parameter declaration> }... ] ] )
    let pParameterDeclarationList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy pParameterDeclaration (token (pstring ",")))

    // 11.60 <rights clause> ::= SQL SECURITY INVOKER | SQL SECURITY DEFINER
    let pRightsClause: Parser<RightsClause, unit> =
        pKeyword "SQL"
        >>. pKeyword "SECURITY"
        >>. ((pKeyword "INVOKER" >>% RightsClause.SqlSecurityInvoker)
             <|> (pKeyword "DEFINER" >>% RightsClause.SqlSecurityDefiner))

    // 11.60 <external security clause> ::= EXTERNAL SECURITY DEFINER
    //     | EXTERNAL SECURITY INVOKER | EXTERNAL SECURITY IMPLEMENTATION DEFINED
    let pExternalSecurityClause: Parser<ExternalSecurity, unit> =
        pKeyword "EXTERNAL"
        >>. pKeyword "SECURITY"
        >>. choice
                [ attempt (
                      pKeyword "IMPLEMENTATION" >>. pKeyword "DEFINED"
                      >>% ExternalSecurity.ImplementationDefined
                  )
                  pKeyword "DEFINER" >>% ExternalSecurity.Definer
                  pKeyword "INVOKER" >>% ExternalSecurity.Invoker ]

    // 11.60 <transform group specification> ::= TRANSFORM GROUP { <single group specification> | <multiple group specification> }
    // 11.60 <single group specification> ::= <group name>
    // 11.60 <multiple group specification> ::= <group specification> [ { <comma> <group specification> }... ]
    // 11.60 <group specification> ::= <group name> FOR TYPE <path-resolved user-defined type name>
    // A lone <group name> with no FOR TYPE is syntactically identical to a one-element
    // <multiple group specification>, so it is reported as <single group specification>
    // (see docs/trade-off.md).
    let pTransformGroupSpecification: Parser<TransformGroupSpecification, unit> =
        pKeyword "TRANSFORM"
        >>. pKeyword "GROUP"
        >>. sepBy1
                (pIdentifierExpr
                 .>>. opt (attempt (pKeyword "FOR" >>. pKeyword "TYPE" >>. pQualifiedNameExpr)))
                (token (pstring ","))
        |>> fun groups ->
            match groups with
            | [ (name, None) ] -> TransformGroupSpecification.SingleTransformGroup name
            | _ -> TransformGroupSpecification.MultipleTransformGroups groups

    // 11.60 <external body reference> ::= EXTERNAL [ NAME <external routine name> ]
    //     [ <parameter style clause> ] [ <transform group specification> ] [ <external security clause> ]
    let pExternalBodyReference: Parser<ExternalBodyReference, unit> =
        pKeyword "EXTERNAL" >>. opt (pKeyword "NAME" >>. pQualifiedNameExpr)
        .>>. opt (attempt pParameterStyleClause)
        .>>. opt (attempt pTransformGroupSpecification)
        .>>. opt (attempt pExternalSecurityClause)
        |>> fun (((name, style), transform), security) ->
            { Name = name
              ParameterStyle = style
              TransformGroup = transform
              ExternalSecurity = security }

    // 11.60 <PTF private parameters> ::= PRIVATE [ DATA ] <private parameter declaration list>
    // 11.60 <private parameter declaration list> ::= ( [ <SQL parameter declaration> [ { <comma> <SQL parameter declaration> }... ] ] )
    let pPtfPrivateParameters: Parser<PtfPrivateParameters, unit> =
        pKeyword "PRIVATE" >>. opt (pKeyword "DATA") .>>. pParameterDeclarationList
        |>> fun (data, declarations) ->
            { HasData = Option.isSome data
              Declarations = declarations }

    // 11.60 <polymorphic table function body> ::= [ <PTF private parameters> ]
    //     [ DESCRIBE WITH <PTF describe component procedure> ]
    //     [ START WITH <PTF start component procedure> ]
    //     FULFILL WITH <PTF fulfill component procedure>
    //     [ FINISH WITH <PTF finish component procedure> ]
    // <PTF {describe|start|fulfill|finish} component procedure> (11.60) is a
    // <specific routine designator> (10.6); only FULFILL is mandatory.
    let pPolymorphicTableFunctionBody: Parser<PolymorphicTableFunctionBody, unit> =
        opt (attempt pPtfPrivateParameters)
        .>>. opt (attempt (pKeyword "DESCRIBE" >>. pKeyword "WITH" >>. pSpecificRoutineDesignator))
        .>>. opt (attempt (pKeyword "START" >>. pKeyword "WITH" >>. pSpecificRoutineDesignator))
        .>>. (pKeyword "FULFILL" >>. pKeyword "WITH" >>. pSpecificRoutineDesignator)
        .>>. opt (attempt (pKeyword "FINISH" >>. pKeyword "WITH" >>. pSpecificRoutineDesignator))
        |>> fun ((((privateParams, describe), start), fulfill), finish) ->
            { PrivateParameters = privateParams
              Describe = describe
              Start = start
              Fulfill = fulfill
              Finish = finish }

    // 11.60 <routine characteristic> ::= <language clause> | <parameter style clause> | SPECIFIC <specific name> | <deterministic characteristic> | <SQL-data access indication> | <null-call clause> | <returned result sets characteristic> | <savepoint level indication>
    let pRoutineCharacteristic =
        choice
            [ attempt (pLanguageClause |>> Language)
              attempt (pParameterStyleClause |>> ParameterStyle)
              attempt (pKeyword "SPECIFIC" >>. pQualifiedNameExpr |>> SpecificName)
              attempt (pKeyword "NOT" >>. pKeyword "DETERMINISTIC" >>% Deterministic false)
              attempt (pKeyword "DETERMINISTIC" >>% Deterministic true)
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
              )
              attempt (
                  pKeyword "DYNAMIC"
                  >>. pKeyword "RESULT"
                  >>. pKeyword "SETS"
                  >>. (pUnsignedInteger .>> ws)
                  |>> DynamicResultSets
              )
              attempt (
                  pKeyword "NEW" >>. pKeyword "SAVEPOINT" >>. pKeyword "LEVEL"
                  >>% SavepointLevel true
              )
              attempt (
                  pKeyword "OLD" >>. pKeyword "SAVEPOINT" >>. pKeyword "LEVEL"
                  >>% SavepointLevel false
              ) ]

    // ISO 9075-2 11.60 SR: each <routine characteristic> may appear at most once in a
    // given routine definition — reject duplicates (the BNF's "[ <routine characteristic>... ]"
    // alone would allow them). Categories: Language / ParameterStyle / SpecificName /
    // Deterministic / SqlDataAccess / NullCall / DynamicResultSets / SavepointLevel / ExternalName.
    let routineCharacteristicCategory (c: RoutineCharacteristic) =
        match c with
        | Language _ -> "Language"
        | ParameterStyle _ -> "ParameterStyle"
        | SpecificName _ -> "SpecificName"
        | Deterministic _ -> "Deterministic"
        | SqlDataAccess _ -> "SqlDataAccess"
        | NullCall _ -> "NullCall"
        | DynamicResultSets _ -> "DynamicResultSets"
        | SavepointLevel _ -> "SavepointLevel"
        | ExternalName _ -> "ExternalName"

    let private rejectDuplicateCharacteristics
        (chars: RoutineCharacteristic list)
        : Parser<RoutineCharacteristic list, unit> =
        let dup =
            chars
            |> List.groupBy routineCharacteristicCategory
            |> List.tryFind (fun (_, g) -> List.length g > 1)

        match dup with
        | Some(cat, _) -> fail (sprintf "duplicate routine characteristic: %s" cat)
        | None -> preturn chars

    let pRoutineCharacteristics =
        many pRoutineCharacteristic >>= rejectDuplicateCharacteristics

    // 11.61 <alter routine characteristic> ::= <language clause> | <parameter style clause>
    //     | <SQL-data access indication> | <null-call clause> | <returned result sets characteristic>
    //     | NAME <external routine name>
    // 11.61's set is narrower than 11.60's — no SPECIFIC / <deterministic characteristic> /
    // <savepoint level indication> — but NAME <external routine name> is allowed here and is
    // NOT a <routine characteristic> of 11.60 (it used to be accepted in CREATE by mistake).
    let pAlterRoutineCharacteristic =
        choice
            [ attempt (pLanguageClause |>> Language)
              attempt (pParameterStyleClause |>> ParameterStyle)
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
              )
              attempt (
                  pKeyword "DYNAMIC"
                  >>. pKeyword "RESULT"
                  >>. pKeyword "SETS"
                  >>. (pUnsignedInteger .>> ws)
                  |>> DynamicResultSets
              )
              attempt (pKeyword "NAME" >>. pQualifiedNameExpr |>> ExternalName) ]

    let pAlterRoutineCharacteristics =
        many pAlterRoutineCharacteristic >>= rejectDuplicateCharacteristics

    // 11.60 <routine body> ::= <SQL routine spec> | <external body reference> | <polymorphic table function body>
    // 11.60 <SQL routine spec> ::= [ <rights clause> ] <SQL routine body>
    // The PTF branch is tried first: `DESCRIBE WITH ...` would otherwise be consumed by the
    // dynamic <describe statement> (20.10) — see docs/gotchas.md.
    let pRoutineBody =
        choice
            [ attempt (pPolymorphicTableFunctionBody |>> RoutineBody.PolymorphicTableFunction)
              attempt (pExternalBodyReference |>> RoutineBody.ExternalRoutine)
              attempt (
                  pKeyword "BEGIN"
                  >>. pKeyword "ATOMIC"
                  >>. sepEndBy1 pRoutineBodyStatementRef (token (pstring ";"))
                  .>> pKeyword "END"
                  |>> fun stmts -> RoutineBody.BeginAtomic(List.map (fun s -> s.Kind) stmts)
              )
              attempt (
                  opt pRightsClause .>>. pRoutineBodyStatementRef
                  |>> fun (rights, s) -> RoutineBody.SqlRoutine(rights, s.Kind)
              ) ]

    // ISO 9075-2 11.60 SR: <parameter style clause> may appear in <routine characteristics> and
    // again in <external body reference>, but a routine has at most one parameter style.
    let private validateRoutine (routine: CreateRoutine) : Parser<CreateRoutine, unit> =
        let inCharacteristics =
            routine.Characteristics
            |> List.exists (function
                | ParameterStyle _ -> true
                | _ -> false)

        let inExternalBody =
            match routine.Body with
            | RoutineBody.ExternalRoutine { ParameterStyle = Some _ } -> true
            | _ -> false

        if inCharacteristics && inExternalBody then
            fail "duplicate <parameter style clause> (11.60)"
        else
            preturn routine

    // 11.60 <schema procedure> ::= CREATE <SQL-invoked procedure> — <SQL-invoked procedure> ::= PROCEDURE <schema qualified routine name> <SQL parameter declaration list> <routine characteristics> <routine body>
    let pCreateProcedureStatement =
        pKeyword "CREATE" >>. pKeyword "PROCEDURE" >>. pQualifiedNameExpr
        .>>. pParameterDeclarationList
        .>>. pRoutineCharacteristics
        .>>. pRoutineBody
        |>> (fun (((name, parameters), characteristics), body) ->
            { Name = name
              Parameters = parameters
              Returns = None
              Characteristics = characteristics
              // 11.60 <dispatch clause> is a <function specification> suffix only.
              Dispatch = false
              Body = body })
        >>= validateRoutine
        |>> CreateProcedure

    // 11.60 <returns clause> ::= RETURNS <returns type>
    // 11.60 <returns data type> ::= <data type> [ <locator indication> ]
    // 11.60 <result cast> ::= CAST FROM <result cast from type>
    // 11.60 <returns table type> ::= TABLE [ <table function column list> ] | ONLY PASS THROUGH
    // 11.60 <table function column list element> ::= <column name> <data type>
    let pReturnsType =
        let pReturnsDataType =
            pDataType
            .>>. opt pLocatorIndication
            .>>. opt (
                attempt (
                    pKeyword "CAST" >>. pKeyword "FROM" >>. pDataType .>>. opt pLocatorIndication
                    |>> fun (dataType, locator) -> dataType, Option.isSome locator
                )
            )
            |>> fun ((dataType, locator), castFrom) ->
                { ReturnsDataType.DataType = dataType
                  AsLocator = Option.isSome locator
                  CastFrom = castFrom }

        let pTableFunctionColumnList =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1
                    (pIdentifierExpr .>>. pDataType
                     |>> fun (name, dataType) ->
                         { TableFunctionColumn.Name = name
                           DataType = dataType })
                    (token (pstring ",")))

        choice
            [ attempt (pReturnsDataType |>> ReturnsData)
              attempt (pKeyword "TABLE" >>. opt (attempt pTableFunctionColumnList) |>> ReturnsTable)
              attempt (
                  pKeyword "ONLY" >>. pKeyword "PASS" >>. pKeyword "THROUGH"
                  >>% ReturnsOnlyPassThrough
              ) ]

    // 11.60 <schema function> ::= CREATE <SQL-invoked function>
    // 11.60 <SQL-invoked function> ::= { <function specification> | <method specification designator> } <routine body>
    // 11.60 <function specification> ::= FUNCTION <schema qualified routine name> <SQL parameter declaration list>
    //     <returns clause> <routine characteristics> [ <dispatch clause> ]
    let pCreateFunctionStatement =
        pKeyword "CREATE" >>. pKeyword "FUNCTION" >>. pQualifiedNameExpr
        .>>. pParameterDeclarationList
        .>>. (pKeyword "RETURNS" >>. pReturnsType)
        .>>. pRoutineCharacteristics
        .>>. opt (pKeyword "STATIC" >>. pKeyword "DISPATCH")
        .>>. pRoutineBody
        |>> (fun (((((name, parameters), returns), characteristics), dispatch), body) ->
            { Name = name
              Parameters = parameters
              Returns = Some returns
              Characteristics = characteristics
              Dispatch = Option.isSome dispatch
              Body = body })
        >>= validateRoutine
        |>> CreateFunction

    // 11.60 <method specification designator> ::= SPECIFIC METHOD <specific method name>
    //     | [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD <method name> <SQL parameter declaration list>
    //         [ <returns clause> ] FOR <schema-resolved user-defined type name>
    // A <method specification designator> has no <routine characteristics> slot (11.60).
    let pMethodSpecificationDesignator: Parser<MethodSpecificationDesignator, unit> =
        choice
            [ attempt (
                  pKeyword "SPECIFIC" >>. pKeyword "METHOD" >>. pQualifiedNameExpr
                  |>> MethodSpecificationDesignator.SpecificMethod
              )
              attempt (
                  opt pMethodKind
                  .>>. (pKeyword "METHOD" >>. pIdentifierExpr)
                  .>>. pParameterDeclarationList
                  .>>. opt (pKeyword "RETURNS" >>. pReturnsType)
                  .>>. (pKeyword "FOR" >>. pQualifiedNameExpr)
                  |>> fun ((((kind, name), parameters), returns), forType) ->
                      MethodSpecificationDesignator.MethodDeclaration
                          { Kind = kind
                            Name = name
                            Parameters = parameters
                            Returns = returns
                            ForType = forType }
              ) ]

    // 11.60 <schema function> ::= CREATE <SQL-invoked function>, <method specification designator> form
    let pCreateMethodStatement =
        pKeyword "CREATE" >>. pMethodSpecificationDesignator .>>. pRoutineBody
        |>> fun (designator, body) -> CreateMethod { Designator = designator; Body = body }

    // 11.61 <alter routine statement> ::= ALTER <specific routine designator> <alter routine characteristic>... [ RESTRICT ]
    let pAlterRoutineStatement =
        pKeyword "ALTER" >>. pSpecificRoutineDesignator
        .>>. pAlterRoutineCharacteristics
        .>>. opt (pKeyword "RESTRICT")
        |>> fun ((routine, characteristics), _) ->
            AlterRoutine
                { Routine = routine
                  Characteristics = characteristics }

    // 11.49 <trigger action time> ::= BEFORE | AFTER | INSTEAD OF
    let pTriggerActionTime =
        choice
            [ attempt (pKeyword "BEFORE" >>% TriggerActionTime.Before)
              attempt (pKeyword "AFTER" >>% TriggerActionTime.After)
              attempt (pKeyword "INSTEAD" >>. pKeyword "OF" >>% TriggerActionTime.InsteadOf) ]

    // 11.49 <trigger event> ::= INSERT | DELETE | UPDATE [ OF <trigger column list> ]
    let pTriggerEvent =
        choice
            [ attempt (pKeyword "INSERT" >>% TriggerEvent.Insert)
              attempt (pKeyword "DELETE" >>% TriggerEvent.Delete)
              attempt (
                  pKeyword "UPDATE"
                  >>. opt (pKeyword "OF" >>. sepBy1 pIdentifierExpr (token (pstring ",")))
                  |>> TriggerEvent.Update
              ) ]

    // 11.49 <transition table or variable> ::= OLD TABLE [ AS ] <transition table name> | NEW TABLE [ AS ] <transition table name> | OLD [ ROW ] [ AS ] <old row variable> | NEW [ ROW ] [ AS ] <new row variable>
    let pTransitionTableOrVariable =
        choice
            [ attempt (
                  pKeyword "OLD" >>. pKeyword "TABLE" >>. opt (pKeyword "AS") >>. pIdentifierExpr
                  |>> TransitionTableOrVariable.OldTable
              )
              attempt (
                  pKeyword "NEW" >>. pKeyword "TABLE" >>. opt (pKeyword "AS") >>. pIdentifierExpr
                  |>> TransitionTableOrVariable.NewTable
              )
              attempt (
                  pKeyword "OLD"
                  >>. opt (pKeyword "ROW")
                  >>. opt (pKeyword "AS")
                  >>. pIdentifierExpr
                  |>> TransitionTableOrVariable.OldRow
              )
              attempt (
                  pKeyword "NEW"
                  >>. opt (pKeyword "ROW")
                  >>. opt (pKeyword "AS")
                  >>. pIdentifierExpr
                  |>> TransitionTableOrVariable.NewRow
              ) ]

    // 11.49 <triggered SQL statement> ::= <SQL procedure statement> | BEGIN ATOMIC { <SQL procedure statement>; }... END
    let pTriggeredStatement =
        choice
            [ attempt (
                  pKeyword "BEGIN"
                  >>. pKeyword "ATOMIC"
                  >>. sepEndBy1 pRoutineBodyStatementRef (token (pstring ";"))
                  .>> pKeyword "END"
                  |>> fun stmts -> TriggeredStatement.BeginAtomic(List.map (fun s -> s.Kind) stmts)
              )
              attempt (pRoutineBodyStatementRef |>> fun s -> SingleStatement s.Kind) ]

    // 11.49 <triggered action> ::= [ FOR EACH { ROW | STATEMENT } ] [ WHEN ( <search condition> ) ] <triggered SQL statement>
    let pTriggeredAction =
        opt (
            pKeyword "FOR"
            >>. pKeyword "EACH"
            >>. ((pKeyword "ROW" >>% true) <|> (pKeyword "STATEMENT" >>% false))
        )
        .>>. opt (
            pKeyword "WHEN"
            >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        )
        .>>. pTriggeredStatement
        |>> fun ((forEach, whenCond), statement) ->
            { ForEach = forEach
              When = whenCond
              Statement = statement }

    // 11.49 <trigger definition> ::= CREATE TRIGGER <trigger name> <trigger action time> <trigger event> ON <table name> [ REFERENCING <transition table or variable list> ] <triggered action>
    let pCreateTriggerStatement =
        pKeyword "CREATE" >>. pKeyword "TRIGGER" >>. pQualifiedNameExpr
        .>>. pTriggerActionTime
        .>>. pTriggerEvent
        .>>. (pKeyword "ON" >>. pQualifiedNameExpr)
        .>>. opt (pKeyword "REFERENCING" >>. many pTransitionTableOrVariable)
        .>>. pTriggeredAction
        |>> fun (((((name, actionTime), event), table), transitions), action) ->
            CreateTrigger
                { Name = name
                  ActionTime = actionTime
                  Event = event
                  Table = table
                  Transitions = Option.defaultValue [] transitions
                  Action = action }
