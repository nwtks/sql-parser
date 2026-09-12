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

    // 11.60 <parameter mode> ::= IN | OUT | INOUT
    let pParameterMode =
        choice
            [ attempt (pKeyword "INOUT" >>% ParameterMode.InOut)
              attempt (pKeyword "IN" >>% ParameterMode.In)
              attempt (pKeyword "OUT" >>% ParameterMode.Out) ]

    // 11.60 <SQL parameter declaration> ::= [ <parameter mode> ] [ <SQL parameter name> ] <parameter type> [ RESULT ] [ DEFAULT <parameter default> ]
    let pParameterDeclaration =
        opt pParameterMode
        .>>. opt pIdentifierExpr
        .>>. pDataType
        .>>. opt (pKeyword "RESULT")
        .>>. opt (pKeyword "DEFAULT" >>. pExpression)
        |>> fun ((((mode, name), dataType), isResult), defaultVal) ->
            { Mode = mode
              Name = name
              DataType = dataType
              IsResult = Option.isSome isResult
              Default = defaultVal }

    // 11.60 <SQL parameter declaration list> ::= ( [ <SQL parameter declaration> [ { , <SQL parameter declaration> }... ] ] )
    let pParameterDeclarationList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy pParameterDeclaration (token (pstring ",")))

    // 11.60 <routine characteristic> ::= <language clause> | <parameter style clause> | SPECIFIC <specific name> | <deterministic characteristic> | <SQL-data access indication> | <null-call clause> | <returned result sets characteristic> | <savepoint level indication>
    let pRoutineCharacteristic =
        choice
            [ attempt (pKeyword "LANGUAGE" >>. pIdentifierRaw |>> Language)
              attempt (pKeyword "PARAMETER" >>. pKeyword "STYLE" >>. pIdentifierRaw |>> ParameterStyle)
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
              )
              attempt (pKeyword "NAME" >>. pQualifiedNameExpr |>> ExternalName) ]

    // ISO 9075-2 11.60 SR: each <routine characteristic> may appear at most once in a
    // given routine definition — reject duplicates (the BNF's "<characteristic>..."
    // alone would allow them). Categories: Language / ParameterStyle / SpecificName /
    // Deterministic / SqlDataAccess / NullCall / DynamicResultSets / SavepointLevel / ExternalName.
    let pRoutineCharacteristics =
        many pRoutineCharacteristic
        >>= fun chars ->
            let dup =
                chars
                |> List.groupBy (fun c ->
                    match c with
                    | Language _ -> "Language"
                    | ParameterStyle _ -> "ParameterStyle"
                    | SpecificName _ -> "SpecificName"
                    | Deterministic _ -> "Deterministic"
                    | SqlDataAccess _ -> "SqlDataAccess"
                    | NullCall _ -> "NullCall"
                    | DynamicResultSets _ -> "DynamicResultSets"
                    | SavepointLevel _ -> "SavepointLevel"
                    | ExternalName _ -> "ExternalName")
                |> List.tryFind (fun (_, g) -> List.length g > 1)

            match dup with
            | Some (cat, _) -> fail (sprintf "duplicate routine characteristic: %s" cat)
            | None -> preturn chars

    // 11.60 <routine body> ::= <SQL routine spec> | <external body reference>
    let pRoutineBody =
        choice
            [ attempt (
                  pKeyword "EXTERNAL" >>. opt (pKeyword "NAME" >>. pQualifiedNameExpr)
                  |>> ExternalRoutine
              )
              attempt (
                  pKeyword "BEGIN"
                  >>. pKeyword "ATOMIC"
                  >>. sepEndBy1 pRoutineBodyStatementRef (token (pstring ";"))
                  .>> pKeyword "END"
                  |>> fun stmts -> RoutineBody.BeginAtomic(List.map (fun s -> s.Kind) stmts)
              )
              attempt (pRoutineBodyStatementRef |>> fun s -> SqlRoutine s.Kind) ]

    // 11.60 <schema procedure> ::= CREATE <SQL-invoked procedure> — <SQL-invoked procedure> ::= PROCEDURE <schema qualified routine name> <SQL parameter declaration list> <routine characteristics> <routine body>
    let pCreateProcedureStatement =
        pKeyword "CREATE" >>. pKeyword "PROCEDURE" >>. pQualifiedNameExpr
        .>>. pParameterDeclarationList
        .>>. pRoutineCharacteristics
        .>>. pRoutineBody
        |>> fun (((name, parameters), characteristics), body) ->
            CreateProcedure
                { Name = name
                  Parameters = parameters
                  Returns = None
                  Characteristics = characteristics
                  Body = body }

    // 11.60 <schema function> ::= CREATE <SQL-invoked function> — <SQL-invoked function> ::= { <function specification> | <method specification designator> } <routine body>
    let pCreateFunctionStatement =
        pKeyword "CREATE" >>. pKeyword "FUNCTION" >>. pQualifiedNameExpr
        .>>. pParameterDeclarationList
        .>>. (pKeyword "RETURNS" >>. pDataType)
        .>>. pRoutineCharacteristics
        .>>. pRoutineBody
        |>> fun ((((name, parameters), returns), characteristics), body) ->
            CreateFunction
                { Name = name
                  Parameters = parameters
                  Returns = Some returns
                  Characteristics = characteristics
                  Body = body }

    // 11.61 <alter routine statement> ::= ALTER <specific routine designator> <routine characteristic>... [ RESTRICT ]
    let pAlterRoutineStatement =
        pKeyword "ALTER" >>. pSpecificRoutineDesignator
        .>>. pRoutineCharacteristics
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
