namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module ControlParser =
    // 10.4 <table argument> ::= <table argument proper>
    //     [ [ AS ] <table argument correlation name> [ ( <derived column list> ) ] ]
    //     [ PARTITION BY <table argument partitioning list> ]
    //     [ PRUNE WHEN EMPTY | KEEP WHEN EMPTY ]
    //     [ ORDER BY <table argument ordering list> ]
    // The `<table function invocation>` and `TABLE ( <query> )` forms are also <value
    // expression>s, so they are only read as a <table argument> when a table-argument clause
    // is present (the unambiguous `TABLE ( <name> )` form needs none) — see docs/trade-off.md.
    let private pTableArgument =
        // 10.4 <table argument correlation name> ::= <correlation name> — the optional AS
        // is not kept (the AST models correlations as a name + optional column list).
        // COPARTITION starts the <copartition clause> and is never a correlation.
        let pCorrelationName =
            pIdentifierExpression
            >>= fun name ->
                match name.Kind with
                | Identifier "COPARTITION" -> fail "COPARTITION starts a <copartition clause> (10.4)"
                | _ -> preturn name

        // 10.4 <table argument parenthesized derived column list>
        let pDerivedColumnList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpression (token (pstring ",")))

        // 10.4 <table argument proper> ::= TABLE ( <table or query name> ) | TABLE <table subquery>
        //     | <table function invocation>
        let pTableArgumentProper =
            choice
                [ // TABLE <table subquery> ::= TABLE ( <query expression> )
                  attempt (
                      pKeyword "TABLE"
                      >>. between (token (pstring "(")) (token (pstring ")")) QueryParser.pQueryExpression
                  )
                  |>> TableArgumentTableQuery
                  // TABLE ( <table or query name> )
                  pKeyword "TABLE"
                  >>. between (token (pstring "(")) (token (pstring ")")) pSchemaQualifiedNameExpression
                  |>> TableArgumentName
                  // <table function invocation> ::= <routine invocation>
                  pRoutineInvocation |>> TableArgumentInvocation ]

        // 10.4 <table argument partitioning list> ::= <column reference>
        //     | ( [ <column reference> [ { <comma> <column reference> }... ] ] )
        let pPartitioning =
            pKeyword "PARTITION"
            >>. pKeyword "BY"
            >>. (attempt (
                     between
                         (token (pstring "("))
                         (token (pstring ")"))
                         (sepBy pColumnReferenceExpression (token (pstring ",")))
                 )
                 <|> (pColumnReferenceExpression |>> fun c -> [ c ]))

        // 10.4 <table argument pruning> ::= PRUNE WHEN EMPTY | KEEP WHEN EMPTY
        let pPruning =
            pKeyword "PRUNE" >>. pKeyword "WHEN" >>. pKeyword "EMPTY" >>% PruneWhenEmpty
            <|> (pKeyword "KEEP" >>. pKeyword "WHEN" >>. pKeyword "EMPTY" >>% KeepWhenEmpty)

        // 10.4 <table argument ordering column> ::= <column reference> [ <ordering specification> ] [ <null ordering> ]
        // 10.4 <table argument ordering list> ::= <table argument ordering column>
        //     | ( <table argument ordering column> [ { <comma> ... }... ] )
        let pOrdering =
            pKeyword "ORDER"
            >>. pKeyword "BY"
            >>. (attempt (
                     between
                         (token (pstring "("))
                         (token (pstring ")"))
                         (sepBy1 QueryParser.pSortSpecification (token (pstring ",")))
                 )
                 <|> (QueryParser.pSortSpecification |>> fun s -> [ s ]))

        let pCorrelation =
            attempt (opt (pKeyword "AS") >>. pCorrelationName .>>. opt (attempt pDerivedColumnList))

        let mkTableArgument proper correlation partitioning pruning ordering =
            { Table = proper
              Correlation = correlation
              PartitionBy = partitioning
              Pruning = pruning
              OrderBy = ordering }
            : TableArgument

        attempt (
            pTableArgumentProper
            .>>. opt pCorrelation
            .>>. opt (attempt pPartitioning)
            .>>. opt (attempt pPruning)
            .>>. opt (attempt pOrdering)
            >>= fun ((((proper, correlation), partitioning), pruning), ordering) ->
                let hasClause =
                    Option.isSome correlation
                    || Option.isSome partitioning
                    || Option.isSome pruning
                    || Option.isSome ordering

                match proper, hasClause with
                | TableArgumentName _, _ -> preturn (mkTableArgument proper correlation partitioning pruning ordering)
                | _, true -> preturn (mkTableArgument proper correlation partitioning pruning ordering)
                | _ -> fail "a <table argument> needs a table-argument clause (10.4)"
        )

    // 10.4 <SQL argument list> ::=
    //     ( [ <SQL argument> [ { <comma> <SQL argument> }... ] [ <copartition clause> ] ] )
    // (plain — no DISTINCT/ALL; used by <routine invocation>, <method invocation>,
    // <static method invocation>, <new specification> and 16.1 CALL.)
    let private pSqlArgumentListBody =
        // 10.4 <descriptor argument> ::= <descriptor value constructor> | CAST ( NULL AS DESCRIPTOR )
        let pDescriptorArgument =
            let pDescriptorCast =
                attempt (
                    pKeyword "CAST"
                    >>. between
                            (token (pstring "("))
                            (token (pstring ")"))
                            (pKeyword "NULL" >>. pKeyword "AS" >>. pKeyword "DESCRIPTOR")
                    >>% DescriptorCast
                )
                |> withExprPosition

            attempt pDescriptorCast <|> SchemaParser.pDescriptorValueConstructor

        // 10.4 <generalized expression> ::= <value expression> AS <path-resolved user-defined type name>
        let pGeneralizedExpressionArgument =
            pExpression .>> pKeyword "AS" .>>. pSchemaQualifiedNameExpression
            |>> fun (e, name) -> SqlArgumentGeneralized(e, UserDefinedType name)

        // 10.4 <named argument SQL argument> ::= <value expression> | <target specification>
        //     | <contextually typed value specification> | <table argument> | <descriptor argument>
        let pNamedArgumentValue =
            choice
                [ attempt pDescriptorArgument |>> SqlArgumentDescriptor
                  attempt pTableArgument |>> SqlArgumentTable
                  pNullSpecification |>> SqlArgumentValue
                  pExpression |>> SqlArgumentValue ]

        // 10.4 <copartition clause> ::= COPARTITION <copartition list>
        let pCopartition =
            // 10.4 <copartition specification> ::= ( <range variable> [ { <comma> <range variable> }... ] )
            // 10.4 <range variable> ::= <table name> | <query name> | <correlation name>
            let pCopartitionSpecification =
                between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (sepBy1 pSchemaQualifiedNameExpression (token (pstring ",")))

            pKeyword "COPARTITION"
            >>. sepBy1 pCopartitionSpecification (token (pstring ","))

        // 10.4 <named argument specification> ::=
        //     <SQL parameter name> <named argument assignment token> <named argument SQL argument>
        // <named argument assignment token> ::= `=>` (5.2).
        let pNamedArgument =
            pIdentifierExpression .>> token (pstring "=>") .>>. pNamedArgumentValue
            |>> SqlArgumentNamed

        // The delimiter that ends an <SQL argument>: `,`, `)` or the <copartition clause>.
        let pSqlArgumentEnd =
            token (pstring ",") >>% ()
            <|> (token (pstring ")") >>% ())
            <|> (pKeyword "COPARTITION" >>% ())

        // 10.4 <SQL argument> ::= <value expression> | <generalized expression>
        //     | <target specification> | <contextually typed value specification>
        //     | <named argument specification> | <table argument> | <descriptor argument>
        let pSqlArgument =
            choice
                [ attempt pDescriptorArgument |>> SqlArgumentDescriptor
                  attempt pNamedArgument
                  // A <generalized expression> is only taken when the argument ends there:
                  // `f(x) AS t PARTITION BY a` is a <table argument> (10.4).
                  attempt (pGeneralizedExpressionArgument .>> followedBy pSqlArgumentEnd)
                  attempt pTableArgument |>> SqlArgumentTable
                  pNullSpecification |>> SqlArgumentValue
                  pExpression |>> SqlArgumentValue ]

        // A leading COPARTITION starts the <copartition clause>, not a routine call named
        // COPARTITION (the keyword is not in the 5.2 reserved set).
        let pCopartitionOnlyArgumentList =
            pCopartition
            |>> fun copartition ->
                { Arguments = []
                  Copartition = Some copartition }

        choice
            [ attempt pCopartitionOnlyArgumentList
              sepBy pSqlArgument (token (pstring ",")) .>>. opt (attempt pCopartition)
              |>> fun (arguments, copartition) ->
                  { Arguments = arguments
                    Copartition = copartition } ]

    pSqlArgumentListBodyRef.Value <- pSqlArgumentListBody

    let private pSqlArgumentList =
        between (token (pstring "(")) (token (pstring ")")) pSqlArgumentListBody

    pSqlArgumentListRef.Value <- pSqlArgumentList

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
