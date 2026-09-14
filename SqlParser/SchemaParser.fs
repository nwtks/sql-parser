namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module SchemaParser =
    // 10.2 <language name> ::= ADA | C | COBOL | FORTRAN | M | MUMPS | PASCAL | PLI | SQL
    // Shared with 11.51 <method characteristic> and 11.60 <routine characteristic>, which
    // live in this module too (see docs/trade-off.md).
    let pLanguageName =
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
    let pLanguageClause = attempt (pKeyword "LANGUAGE" >>. pLanguageName)

    // 10.6 <routine type> ::= ROUTINE | FUNCTION | PROCEDURE
    //     | [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD
    let pRoutineType =
        choice
            [ pKeyword "ROUTINE" >>% RoutineType.Routine
              pKeyword "FUNCTION" >>% RoutineType.Function
              pKeyword "PROCEDURE" >>% RoutineType.Procedure
              // 10.6 pMethodKind also serves 11.51 / 11.60 — it lives in ExpressionParser.fs, which is compiled first.
              attempt (
                  opt pMethodKind .>>. pKeyword "METHOD"
                  |>> fun (methodKind, _) -> RoutineType.Method methodKind
              ) ]

    // 10.6 <routine designator> ::= [ <routine type> ] <qualified identifier>
    // (<object name> in 12.2 / 12.3 — kept separate from <specific routine designator>
    //  because <object name> carries a plain name, not a designator)
    let pRoutineDesignatorWithType =
        choice [ attempt (pRoutineType >>. pQualifiedNameExpr); pQualifiedNameExpr ]

    // 10.6 <specific routine designator> ::=
    //       SPECIFIC <routine type> <specific name>
    //     | <routine type> <member name> [ FOR <schema-resolved user-defined type name> ]
    // 10.6 <member name> ::= <member name alternatives> [ <data type list> ]
    // A bare <schema qualified routine name> is also accepted (RoutineType = None) so that
    // callers such as `ALTER ROUTINE add` keep working — see docs/trade-off.md.
    let pSpecificRoutineDesignator =
        // 10.6 <data type list> ::= ( [ <data type> [ { <comma> <data type> }... ] ] )
        let pDataTypeList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy pDataType (token (pstring ",")))

        let mk isSpecific routineType name dataTypeList forType =
            { IsSpecific = isSpecific
              RoutineType = routineType
              Name = name
              DataTypeList = dataTypeList
              ForType = forType }

        choice
            [ attempt (
                  pKeyword "SPECIFIC" >>. pRoutineType .>>. pQualifiedNameExpr
                  |>> fun (routineType, name) -> mk true (Some routineType) name None None
              )
              attempt (
                  opt pRoutineType
                  .>>. pQualifiedNameExpr
                  .>>. opt (attempt pDataTypeList)
                  .>>. opt (attempt (pKeyword "FOR" >>. pQualifiedNameExpr))
                  |>> fun (((routineType, name), dataTypeList), forType) ->
                      mk false routineType name dataTypeList forType
              ) ]

    // 10.7 <collate clause> ::= COLLATE <collation name>
    let pCollateClause = pKeyword "COLLATE" >>. pQualifiedNameExpr

    // 10.8 <constraint enforcement> ::= [ NOT ] ENFORCED   (true = ENFORCED, false = NOT ENFORCED)
    // Also used by 11.25 <alter table constraint definition>, and by the
    // <column constraint definition> (11.4) / <table constraint definition> (11.6).
    // It is defined at the top of the module so both can reuse it.
    let pConstraintEnforcement =
        attempt (pKeyword "NOT" >>. pKeyword "ENFORCED" >>% false)
        <|> (pKeyword "ENFORCED" >>% true)

    // 10.8 <constraint characteristics> ::=
    //     <constraint check time> [ [ NOT ] DEFERRABLE ] [ <constraint enforcement> ]
    //   | [ [ NOT ] DEFERRABLE ] <constraint check time> [ <constraint enforcement> ]
    //   | <constraint enforcement>
    let pConstraintCharacteristics =
        // 10.8 <constraint check time> ::= INITIALLY DEFERRED | INITIALLY IMMEDIATE
        // NOTE: both alternatives are parenthesized — `<|>` binds tighter than `>>.`/`>>%`,
        // so an unparenthesized `INITIALLY >>. DEFERRED >>% true <|> (...)` would group as
        // `INITIALLY >>. (DEFERRED >>% (true <|> ...))` and never try IMMEDIATE.
        let pCheckTime =
            attempt (pKeyword "INITIALLY" >>. pKeyword "DEFERRED" >>% true)
            <|> (pKeyword "INITIALLY" >>. pKeyword "IMMEDIATE" >>% false)

        // 10.8 <constraint deferrability> ::= [ NOT ] DEFERRABLE
        let pDeferrable =
            attempt (pKeyword "NOT" >>. pKeyword "DEFERRABLE" >>% false)
            <|> (pKeyword "DEFERRABLE" >>% true)

        // 10.8 <constraint enforcement> ::= [ NOT ] ENFORCED — shared with 11.25
        let pEnforced = pConstraintEnforcement

        let mk initiallyDeferred deferrable enforced =
            { InitiallyDeferred = initiallyDeferred
              Deferrable = deferrable
              Enforced = enforced }

        choice
            [ // <check time> [ <deferrability> ] [ <enforcement> ]
              attempt (
                  pCheckTime .>>. opt pDeferrable .>>. opt pEnforced
                  |>> fun ((ct, d), e) -> mk (Some ct) d e
              )
              // [ <deferrability> ] [ <check time> ] [ <enforcement> ]
              // — only the deferrability is required, so a bare `[ NOT ] DEFERRABLE`
              // is valid and any following keyword (e.g. a domain's COLLATE clause)
              // is left for the enclosing production.
              attempt (
                  pDeferrable .>>. opt pCheckTime .>>. opt pEnforced
                  |>> fun ((d, ct), e) -> mk ct (Some d) e
              )
              // <enforcement> alone
              attempt (pEnforced |>> fun e -> mk None None (Some e))
              // <constraint characteristics> is optional in its enclosing production
              preturn (mk None None None) ]

    // 11.1 <schema element> ::= <table definition> | <view definition> | <domain definition> | ...
    // It is passed in as a parameter (wired in SqlParser.fs, which also defines the
    // CREATE SCHEMA parser that consumes these elements) so no forward reference is needed.
    // 11.1 <schema definition> ::= CREATE SCHEMA <schema name clause> [ <schema character set or path> ] [ <schema element>... ]
    let pCreateSchemaStatement (pSchemaElement: Parser<StatementKind, unit>) =
        let pNameClause =
            choice
                [ attempt (
                      pQualifiedNameExpr .>>. opt (pKeyword "AUTHORIZATION" >>. pIdentifierExpr)
                      |>> fun (name, auth) -> Some name, auth
                  )
                  pKeyword "AUTHORIZATION" >>. pIdentifierExpr |>> fun auth -> None, Some auth ]

        // 11.1 <schema character set or path> ::=
        //     <schema character set specification>
        //   | <schema path specification>
        //   | <character set specification> <path specification>
        //   | <path specification> <character set specification>
        let pSchemaCharsetOrPath =
            let pCharset =
                pKeyword "DEFAULT"
                >>. pKeyword "CHARACTER"
                >>. pKeyword "SET"
                >>. pQualifiedNameExpr

            // 10.3 <path specification> ::= PATH <path-resolved user-defined type name> [ { <comma> ... }... ]
            let pPath = pKeyword "PATH" >>. sepBy1 pQualifiedNameExpr (token (pstring ","))

            choice
                [ attempt (pCharset .>>. opt (attempt pPath) |>> fun (c, p) -> Some c, p)
                  attempt (pPath .>>. opt (attempt pCharset) |>> fun (p, c) -> c, Some p)
                  preturn (None, None) ]

        pKeyword "CREATE" >>. pKeyword "SCHEMA" >>. pNameClause
        .>>. pSchemaCharsetOrPath
        .>>. many pSchemaElement
        |>> fun ((nameClause, (charset, path)), elements) ->
            CreateSchema
                { Name = fst nameClause
                  Authorization = snd nameClause
                  CharacterSet = charset
                  Path = path
                  Elements = elements }

    // 11.2 <drop behavior> ::= CASCADE | RESTRICT   (true = CASCADE, false = RESTRICT)
    let pDropBehavior = pKeyword "CASCADE" >>% true <|> (pKeyword "RESTRICT" >>% false)

    // 11.8 <referential triggered action> ::= [ <update rule> ] [ <delete rule> ] | [ <delete rule> ] [ <update rule> ] — <update rule> ::= ON UPDATE <referential action>
    let pReferentialTriggeredAction =
        // 11.8 <referential action> ::= CASCADE | SET NULL | SET DEFAULT | RESTRICT | NO ACTION
        let pReferentialAction =
            choice
                [ pKeyword "CASCADE" >>% ReferentialAction.Cascade
                  attempt (pKeyword "SET" >>. pKeyword "NULL" >>% ReferentialAction.SetNull)
                  attempt (pKeyword "SET" >>. pKeyword "DEFAULT" >>% ReferentialAction.SetDefault)
                  attempt (pKeyword "RESTRICT" >>% ReferentialAction.Restrict)
                  attempt (pKeyword "NO" >>. pKeyword "ACTION" >>% ReferentialAction.NoAction) ]

        many (
            choice
                [ attempt (
                      pKeyword "ON" >>. pKeyword "UPDATE" >>. pReferentialAction
                      |>> fun a -> Some a, None
                  )
                  attempt (
                      pKeyword "ON" >>. pKeyword "DELETE" >>. pReferentialAction
                      |>> fun a -> None, Some a
                  ) ]
        )
        |>> fun acts -> List.tryPick fst acts, List.tryPick snd acts

    // 11.4 <column constraint definition> ::=
    //     [ <constraint name definition> ] <column constraint> [ <constraint characteristics> ]
    // 11.4 <column constraint> ::= NOT NULL | <unique specification>
    //     | <references specification> | <check constraint definition>
    // (the <default clause> is NOT a column constraint — see pDefaultClause below)
    let pColumnConstraint =
        let pName = opt (pKeyword "CONSTRAINT" >>. pIdentifierExpr)

        let pKind =
            choice
                [ attempt (pKeyword "NOT" >>. pKeyword "NULL" >>% ColumnConstraintKind.NotNull)
                  attempt (pKeyword "PRIMARY" >>. pKeyword "KEY" >>% ColumnConstraintKind.PrimaryKey)
                  attempt (pKeyword "UNIQUE" >>% ColumnConstraintKind.Unique)
                  attempt (
                      pKeyword "REFERENCES" >>. pIdentifierExpr
                      .>>. opt (
                          between
                              (token (pstring "("))
                              (token (pstring ")"))
                              (sepBy1 pIdentifierExpr (token (pstring ",")))
                      )
                      .>>. pReferentialTriggeredAction
                      |>> fun ((table, refCols), (onUpd, onDel)) ->
                          ColumnConstraintKind.References
                              { Name = None
                                Columns = []
                                Table = table
                                RefColumns = refCols
                                OnUpdate = onUpd
                                OnDelete = onDel }
                  )
                  attempt (
                      pKeyword "CHECK"
                      >>. between (token (pstring "(")) (token (pstring ")")) pExpression
                      |>> ColumnConstraintKind.Check
                  ) ]

        pName .>>. pKind .>>. pConstraintCharacteristics
        |>> fun ((name, kind), characteristics) ->
            { Name = name
              Kind = kind
              Characteristics = characteristics }

    // 11.5 <default option> ::= <literal> | <datetime value function> | USER
    //     | CURRENT_USER | CURRENT_ROLE | SESSION_USER | SYSTEM_USER | CURRENT_CATALOG
    //     | CURRENT_SCHEMA | CURRENT_PATH | <implicitly typed value specification>
    // NOTE: deliberately NOT `pGeneralValueSpecification` — that also accepts `VALUE`,
    // `?` / `:name` and `COLLATION FOR (...)`, none of which are <default option>s.
    let pDefaultOption =
        // 11.5 <implicitly typed value specification> ::= <null specification> | <empty specification>
        // (<null specification> is covered by pLiteralExpr; <empty specification> is
        //  ARRAY[] / MULTISET[] — see 6.42 / 6.45)
        let pEmptySpecification =
            attempt (
                pKeyword "ARRAY" >>. token pLeftBracket .>> token pRightBracket
                >>% ArrayConstructor []
            )
            <|> (pKeyword "MULTISET" >>. token pLeftBracket .>> token pRightBracket
                 >>% MultisetConstructor [])
            |> withExprPosition

        choice
            [ attempt pLiteralExpr
              // 5.3 <signed numeric literal> — pLiteralExpr only accepts the unsigned form
              attempt (pSignedNumericLiteral |>> Number |>> Literal |> withExprPosition)
              attempt pDateTimeValueFunction
              attempt (pKeyword "USER" >>% User |> withExprPosition)
              attempt (pKeyword "CURRENT_USER" >>% ExpressionKind.CurrentUser |> withExprPosition)
              attempt (pKeyword "CURRENT_ROLE" >>% ExpressionKind.CurrentRole |> withExprPosition)
              attempt (pKeyword "SESSION_USER" >>% SessionUser |> withExprPosition)
              attempt (pKeyword "SYSTEM_USER" >>% SystemUser |> withExprPosition)
              attempt (pKeyword "CURRENT_CATALOG" >>% CurrentCatalog |> withExprPosition)
              attempt (pKeyword "CURRENT_SCHEMA" >>% CurrentSchema |> withExprPosition)
              attempt (pKeyword "CURRENT_PATH" >>% CurrentPath |> withExprPosition)
              attempt pEmptySpecification ]

    // 11.5 <default clause> ::= DEFAULT <default option>
    let pDefaultClause = pKeyword "DEFAULT" >>. pDefaultOption

    // 11.72 <basic sequence generator option> ::= <sequence generator increment by option>
    //     | <sequence generator maxvalue option> | <sequence generator minvalue option>
    //     | <sequence generator cycle option>
    let pBasicSequenceGeneratorOption =
        choice
            [ attempt (pKeyword "INCREMENT" >>. pKeyword "BY" >>. pSignedNumericLiteral |>> IncrementBy)
              attempt (pKeyword "MAXVALUE" >>. pSignedNumericLiteral |>> fun v -> MaxValue(Some v))
              attempt (pKeyword "NO" >>. pKeyword "MAXVALUE" >>% MaxValue None)
              attempt (pKeyword "MINVALUE" >>. pSignedNumericLiteral |>> fun v -> MinValue(Some v))
              attempt (pKeyword "NO" >>. pKeyword "MINVALUE" >>% MinValue None)
              attempt (pKeyword "CYCLE" >>% Cycle true)
              attempt (pKeyword "NO" >>. pKeyword "CYCLE" >>% Cycle false) ]

    // 11.72 <sequence generator start with option> ::= START WITH <sequence generator start value>
    let pSequenceGeneratorStartWithOption =
        pKeyword "START" >>. pKeyword "WITH" >>. pSignedNumericLiteral |>> StartWith

    // 11.73 <alter sequence generator restart option> ::= RESTART [ WITH <sequence generator restart value> ]
    let pAlterSequenceGeneratorRestartOption =
        pKeyword "RESTART" >>. opt (pKeyword "WITH" >>. pSignedNumericLiteral)
        |>> Restart

    // 11.72 <sequence generator option> — shared by CREATE/ALTER SEQUENCE and
    // the <identity column specification> (11.4).
    let pSequenceOption =
        choice
            [ attempt (pKeyword "AS" >>. pDataType |>> DataTypeOption)
              attempt pSequenceGeneratorStartWithOption
              attempt pBasicSequenceGeneratorOption
              attempt pAlterSequenceGeneratorRestartOption ]

    // 11.4 <identity column specification> ::= GENERATED { ALWAYS | BY DEFAULT }
    //     AS IDENTITY [ ( <common sequence generator options> ) ]
    let pIdentitySpec =
        pKeyword "GENERATED"
        >>. (pKeyword "ALWAYS" >>% true <|> (pKeyword "BY" >>. pKeyword "DEFAULT" >>% false))
        .>> pKeyword "AS"
        .>> pKeyword "IDENTITY"
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (many pSequenceOption))
        |>> fun (isAlways, opts) ->
            { IsAlways = isAlways
              Options = Option.defaultValue [] opts }

    // 11.4 <generation clause> ::= GENERATED ALWAYS AS ( <value expression> )
    let pGenerationClause =
        pKeyword "GENERATED"
        >>. pKeyword "ALWAYS"
        >>. pKeyword "AS"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> GeneratedColumn

    // 11.4 <system time period start column specification> ::= GENERATED ALWAYS AS ROW START
    // 11.4 <system time period end column specification>   ::= GENERATED ALWAYS AS ROW END
    let pSystemTimePeriodColumn =
        pKeyword "GENERATED"
        >>. pKeyword "ALWAYS"
        >>. pKeyword "AS"
        >>. pKeyword "ROW"
        >>. (pKeyword "START" >>% SystemTimePeriodKind.RowStart
             <|> (pKeyword "END" >>% SystemTimePeriodKind.RowEnd))
        |>> SystemTimePeriodColumn

    // 11.4 the single optional value clause of a <column definition>:
    //     <default clause> | <identity column specification> | <generation clause>
    //     | <system time period start column specification> | <system time period end column specification>
    // All three GENERATED alternatives start with `GENERATED ALWAYS AS`, so each is `attempt`ed.
    let pColumnGeneration =
        choice
            [ attempt (pIdentitySpec |>> IdentityColumn)
              attempt pGenerationClause
              attempt pSystemTimePeriodColumn ]

    // 11.4 <column definition> ::= <column name> [ <data type or domain name> ]
    //       [ <default clause> | <identity column specification> | <generation clause>
    //       | <system time period start column specification> | <system time period end column specification> ]
    //       [ <column constraint definition>... ] [ <collate clause> ]
    let pColumnDefinition =
        pIdentifierExpr
        .>>. pDataType
        .>>. opt (
            attempt (pDefaultClause |>> Choice1Of2)
            <|> attempt (pColumnGeneration |>> Choice2Of2)
        )
        .>>. many (attempt pColumnConstraint)
        .>>. opt (attempt pCollateClause)
        |>> fun ((((name, typ), valueClause), constraints), collation) ->
            let defaultValue, identity, generation, systemTimePeriod =
                match valueClause with
                | Some(Choice1Of2 d) -> Some d, None, None, None
                | Some(Choice2Of2(IdentityColumn spec)) -> None, Some spec, None, None
                | Some(Choice2Of2(GeneratedColumn expr)) -> None, None, Some expr, None
                | Some(Choice2Of2(SystemTimePeriodColumn kind)) -> None, None, None, Some kind
                | None -> None, None, None, None

            let kinds = constraints |> List.map (fun c -> c.Kind)

            { Name = name
              DataType = typ
              IsNullable =
                kinds
                |> List.tryPick (function
                    | ColumnConstraintKind.NotNull -> Some false
                    | _ -> None)
              IsPrimaryKey =
                kinds
                |> List.exists (function
                    | ColumnConstraintKind.PrimaryKey -> true
                    | _ -> false)
              DefaultValue = defaultValue
              IsUnique =
                kinds
                |> List.exists (function
                    | ColumnConstraintKind.Unique -> true
                    | _ -> false)
              References =
                kinds
                |> List.tryPick (function
                    | ColumnConstraintKind.References r -> Some r
                    | _ -> None)
              Check =
                kinds
                |> List.tryPick (function
                    | ColumnConstraintKind.Check e -> Some e
                    | _ -> None)
              Identity = identity
              Generation = generation
              SystemTimePeriod = systemTimePeriod
              Collation = collation
              Constraints = constraints }

    // 11.6 <table constraint definition> ::=
    //     [ <constraint name definition> ] <table constraint> [ <constraint characteristics> ]
    // 11.6 <table constraint> ::= PRIMARY KEY | UNIQUE | FOREIGN KEY | CHECK
    let pTableConstraint =
        // 11.8 <referential constraint definition> ::= FOREIGN KEY ( <column list> ) REFERENCES <table> [ ( <column list> ) ] [ <referential triggered action> ]
        let pForeignKeyConstraint =
            pKeyword "FOREIGN"
            >>. pKeyword "KEY"
            >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
            .>> pKeyword "REFERENCES"
            .>>. pIdentifierExpr
            .>>. opt (
                between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
            )
            .>>. pReferentialTriggeredAction
            |>> fun (((cols, table), refCols), (onUpd, onDel)) ->
                { Name = None
                  Columns = cols
                  Table = table
                  RefColumns = refCols
                  OnUpdate = onUpd
                  OnDelete = onDel }
                : ForeignKeyConstraint

        let pName = opt (pKeyword "CONSTRAINT" >>. pIdentifierExpr)

        let pColumnList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))

        let pConstraint =
            choice
                [ attempt (
                      pName .>> pKeyword "PRIMARY" .>> pKeyword "KEY" .>>. pColumnList
                      |>> fun (n, cols) -> TableConstraint.PrimaryKey(n, cols)
                  )
                  attempt (
                      pName .>> pKeyword "UNIQUE" .>>. pColumnList
                      |>> fun (n, cols) -> TableConstraint.Unique(n, cols)
                  )
                  attempt (
                      pName .>>. pForeignKeyConstraint
                      |>> fun (n, fk) -> TableConstraint.ForeignKey { fk with Name = n }
                  )
                  attempt (
                      pName .>> pKeyword "CHECK"
                      .>>. between (token (pstring "(")) (token (pstring ")")) pExpression
                      |>> fun (n, e) -> TableConstraint.Check(n, e)
                  ) ]

        pConstraint .>>. pConstraintCharacteristics
        |>> fun (body, characteristics) ->
            { Constraint = body
              Characteristics = characteristics }

    // 11.3 <system or application time period specification>
    //     ::= PERIOD FOR SYSTEM_TIME | PERIOD FOR <application time period name>
    let pTimePeriodSpecification =
        attempt (
            pKeyword "PERIOD" >>. pKeyword "FOR" >>. pKeyword "SYSTEM_TIME"
            >>% TimePeriodSpecification.SystemTimePeriod
        )
        <|> (pKeyword "PERIOD" >>. pKeyword "FOR" >>. pIdentifierExpr
             |>> TimePeriodSpecification.ApplicationTimePeriod)

    // 11.3 <table period definition> ::= <system or application time period specification>
    //     <left paren> <period begin column name> <comma> <period end column name> <right paren>
    let pTablePeriodDefinition =
        pTimePeriodSpecification
        .>>. between
            (token (pstring "("))
            (token (pstring ")"))
            (pIdentifierExpr .>>. (token (pstring ",") >>. pIdentifierExpr))
        |>> fun (specification, (beginColumn, endColumn)) ->
            { TablePeriodDefinition.Specification = specification
              BeginColumn = beginColumn
              EndColumn = endColumn }

    // 11.3 <reference generation> ::= SYSTEM GENERATED | USER GENERATED | DERIVED
    let pReferenceGeneration =
        choice
            [ attempt (
                  pKeyword "SYSTEM" >>. pKeyword "GENERATED"
                  >>% ReferenceGeneration.SystemGenerated
              )
              attempt (pKeyword "USER" >>. pKeyword "GENERATED" >>% ReferenceGeneration.UserGenerated)
              attempt (pKeyword "DERIVED" >>% ReferenceGeneration.Derived) ]

    // 11.3 <self-referencing column specification> ::=
    //     REF IS <self-referencing column name> [ <reference generation> ]
    // Shared by the <typed table element list> (11.3) and the <view element list> (11.32).
    let pSelfReferencingColumn =
        pKeyword "REF" >>. pKeyword "IS" >>. pIdentifierExpr
        .>>. opt pReferenceGeneration
        |>> fun (name, generation) -> { Name = name; Generation = generation }

    // 11.3 <table definition> ::= CREATE [ <table scope> ] TABLE <table name> <table contents source>
    //       [ WITH <system versioning clause> ] [ ON COMMIT <table commit action> ROWS ]
    // 11.3 <table contents source> ::= <table element list> | <typed table clause> | <as subquery clause>
    let pCreateTableStatement =
        // 11.3 <like clause> ::= LIKE <table name> [ <like option>... ]
        // 11.3 <like option> ::= <identity option> | <column default option> | <generation option>
        let pLikeClause =
            let pLikeOption =
                choice
                    [ attempt (pKeyword "INCLUDING" >>. pKeyword "IDENTITY" >>% LikeOption.IncludingIdentity)
                      attempt (pKeyword "EXCLUDING" >>. pKeyword "IDENTITY" >>% LikeOption.ExcludingIdentity)
                      attempt (pKeyword "INCLUDING" >>. pKeyword "DEFAULTS" >>% LikeOption.IncludingDefaults)
                      attempt (pKeyword "EXCLUDING" >>. pKeyword "DEFAULTS" >>% LikeOption.ExcludingDefaults)
                      attempt (pKeyword "INCLUDING" >>. pKeyword "GENERATED" >>% LikeOption.IncludingGenerated)
                      attempt (pKeyword "EXCLUDING" >>. pKeyword "GENERATED" >>% LikeOption.ExcludingGenerated) ]

            pKeyword "LIKE" >>. pQualifiedNameExpr .>>. many pLikeOption

        // 11.3 <table element> ::= <column definition> | <table period definition>
        //     | <table constraint definition> | <like clause>
        let pTableElement =
            choice
                [ attempt (pColumnDefinition |>> Choice1Of4)
                  attempt (pTablePeriodDefinition |>> Choice2Of4)
                  attempt (pTableConstraint |>> Choice3Of4)
                  attempt (pLikeClause |>> Choice4Of4) ]

        // 11.3 <as subquery clause> ::= [ ( <column name list> ) ] AS <table subquery> <with or without data>
        // 11.3 <with or without data> ::= WITH NO DATA | WITH DATA
        // The <with or without data> clause is mandatory (true = WITH DATA) — see docs/trade-off.md.
        let pAsSubquery =
            pKeyword "AS" >>. pQuery
            .>>. (pKeyword "WITH" >>. opt (pKeyword "NO") .>> pKeyword "DATA" |>> Option.isNone)

        // 11.3 <table scope> ::= GLOBAL TEMPORARY | LOCAL TEMPORARY
        let pTableScope =
            attempt (pKeyword "GLOBAL" >>. pKeyword "TEMPORARY" >>% TableScope.Global)
            <|> (pKeyword "LOCAL" >>. pKeyword "TEMPORARY" >>% TableScope.Local)

        // 11.3 <column option list> ::=
        //     [ <scope clause> ] [ <default clause> ] [ <column constraint definition>... ]
        let pColumnOptionList =
            opt pScopeClause .>>. opt pDefaultClause .>>. many (attempt pColumnConstraint)

        // 11.3 <column options> ::= <column name> WITH OPTIONS <column option list>
        // NOTE: `OPTIONS` is not a reserved word, so the mandatory `WITH OPTIONS` is
        // what tells a <column options> element from a <table constraint definition>.
        let pColumnOptions =
            pIdentifierExpr .>> pKeyword "WITH" .>> pKeyword "OPTIONS"
            .>>. pColumnOptionList
            |>> fun (name, ((scope, defaultValue), constraints)) ->
                { Name = name
                  Scope = scope
                  DefaultValue = defaultValue
                  Constraints = constraints }

        // 11.3 <typed table element> ::= <column options> | <table constraint definition>
        //     | <self-referencing column specification>
        let pTypedTableElement =
            choice
                [ attempt (pColumnOptions |>> TypedTableElement.TypedColumnOptions)
                  attempt (pSelfReferencingColumn |>> TypedTableElement.TypedSelfReference)
                  attempt (pTableConstraint |>> TypedTableElement.TypedTableConstraint) ]

        // 11.3 <typed table element list> ::=
        //     <left paren> <typed table element> [ { <comma> <typed table element> }... ] <right paren>
        let pTypedTableElementList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTypedTableElement (token (pstring ",")))

        // 11.3 <typed table clause> ::= OF <path-resolved user-defined type name>
        //     [ <subtable clause> ] [ <typed table element list> ]
        let pTypedTableClause =
            pKeyword "OF" >>. pQualifiedNameExpr
            .>>. opt (pKeyword "UNDER" >>. pQualifiedNameExpr)
            .>>. opt pTypedTableElementList

        // 11.3 <system versioning clause> ::= SYSTEM VERSIONING
        let pWithSystemVersioning =
            attempt (pKeyword "WITH" >>. pKeyword "SYSTEM" >>. pKeyword "VERSIONING" >>% true)

        // 11.3 <table commit action> ::= PRESERVE | DELETE
        let pTableCommitAction =
            attempt (pKeyword "PRESERVE" >>% TableCommitAction.PreserveOnCommit)
            <|> (pKeyword "DELETE" >>% TableCommitAction.DeleteOnCommit)

        let pOnCommit =
            attempt (pKeyword "ON" >>. pKeyword "COMMIT" >>. pTableCommitAction .>> pKeyword "ROWS")

        pKeyword "CREATE" >>. opt pTableScope .>> pKeyword "TABLE"
        .>>. pQualifiedNameExpr
        .>>. (attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTableElement (token (pstring ",")))
                  |>> fun elems -> elems, None, None, None, None, []
              )
              <|> attempt (
                  between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                  .>>. pAsSubquery
                  |>> fun (cols, (q, withData)) -> [], Some cols, Some(q, withData), None, None, []
              )
              <|> (pAsSubquery |>> fun (q, withData) -> [], None, Some(q, withData), None, None, [])
              <|> (pTypedTableClause
                   |>> fun ((typ, supertable), typedElements) ->
                       [], None, None, Some typ, supertable, Option.defaultValue [] typedElements))
        .>>. opt (attempt pWithSystemVersioning)
        .>>. opt (attempt pOnCommit)
        |>> fun
                ((((scope, name), (elems, asCols, asQuery, ofType, under, typedElements)), withSystemVersioning),
                 onCommit) ->
            let cols =
                elems
                |> List.choose (function
                    | Choice1Of4 c -> Some c
                    | _ -> None)

            let periods =
                elems
                |> List.choose (function
                    | Choice2Of4 p -> Some p
                    | _ -> None)

            let cons =
                elems
                |> List.choose (function
                    | Choice3Of4 c -> Some c
                    | _ -> None)

            let like =
                elems
                |> List.tryPick (function
                    | Choice4Of4 l -> Some l
                    | _ -> None)

            { Table = name
              TableScope = scope
              Columns = cols
              Constraints = cons
              AsQuery = asQuery |> Option.map fst
              AsColumns = asCols
              WithData = asQuery |> Option.map snd
              OfType = ofType
              Under = under
              TypedElements = typedElements
              Like = like
              WithSystemVersioning = Option.defaultValue false withSystemVersioning
              OnCommit = onCommit
              Periods = periods }
            |> CreateTable

    // 11.10 <alter table statement> ::= ALTER TABLE <table name> <alter table action>
    let pAlterTableStatement =
        // 11.27 <add system time period column list>
        //     ::= ADD [ COLUMN ] <column definition 1> ADD [ COLUMN ] <column definition 2>
        // Both columns are required by the grammar, so this parser yields exactly two entries.
        // Local because pAlterTableStatement is its only consumer.
        let pAddSystemTimePeriodColumnList =
            pKeyword "ADD" >>. opt (pKeyword "COLUMN") >>. pColumnDefinition
            .>>. (pKeyword "ADD" >>. opt (pKeyword "COLUMN") >>. pColumnDefinition)
            |>> fun (first, second) -> [ first; second ]

        // 11.20 <set identity column generation clause> ::= SET GENERATED { ALWAYS | BY DEFAULT }
        let pSetIdentityColumnGeneration =
            pKeyword "SET"
            >>. pKeyword "GENERATED"
            >>. (pKeyword "ALWAYS" >>% true <|> (pKeyword "BY" >>. pKeyword "DEFAULT" >>% false))

        // 11.20 <alter identity column option>
        //     ::= <alter sequence generator restart option> | SET <basic sequence generator option>
        let pAlterIdentityColumnOption =
            attempt pAlterSequenceGeneratorRestartOption
            <|> (pKeyword "SET" >>. pBasicSequenceGeneratorOption)

        // 11.20 <alter identity column specification> ::=
        //         <set identity column generation clause> [ <alter identity column option>... ]
        //       | <alter identity column option>...
        // At least one of the two alternatives must match, so the parser never succeeds on empty
        // input (which would otherwise shadow the remaining <alter column action> alternatives).
        let pAlterIdentityColumnSpecification =
            attempt (
                pSetIdentityColumnGeneration .>>. many pAlterIdentityColumnOption
                |>> fun (generation, opts) ->
                    { AlterIdentityColumnSpecification.Generation = Some generation
                      Options = opts }
            )
            <|> (many1 pAlterIdentityColumnOption
                 |>> fun opts ->
                     { AlterIdentityColumnSpecification.Generation = None
                       Options = opts })

        let pColumnAction =
            choice
                [ attempt (pKeyword "SET" >>. pDefaultClause |>> ColumnAlteration.SetDefault)
                  attempt (pKeyword "DROP" >>. pKeyword "DEFAULT" >>% ColumnAlteration.DropDefault)
                  attempt (
                      pKeyword "SET" >>. pKeyword "NOT" >>. pKeyword "NULL"
                      >>% ColumnAlteration.SetNotNull
                  )
                  attempt (
                      pKeyword "DROP" >>. pKeyword "NOT" >>. pKeyword "NULL"
                      >>% ColumnAlteration.DropNotNull
                  )
                  // 11.17 <add column scope clause> ::= ADD <scope clause>
                  attempt (pKeyword "ADD" >>. pScopeClause |>> ColumnAlteration.AddColumnScope)
                  // 11.18 <drop column scope clause> ::= DROP SCOPE <drop behavior>
                  attempt (
                      pKeyword "DROP" >>. pKeyword "SCOPE" >>. pDropBehavior
                      |>> ColumnAlteration.DropColumnScope
                  )
                  attempt (
                      pKeyword "SET" >>. pKeyword "DATA" >>. pKeyword "TYPE" >>. pDataType
                      |>> ColumnAlteration.SetDataType
                  )
                  // 11.20 <alter identity column specification>
                  attempt (pAlterIdentityColumnSpecification |>> ColumnAlteration.AlterIdentityColumn)
                  // 11.21 <drop identity property clause> ::= DROP IDENTITY
                  attempt (pKeyword "DROP" >>. pKeyword "IDENTITY" >>% ColumnAlteration.DropIdentity)
                  // 11.22 <drop column generation expression clause> ::= DROP EXPRESSION
                  // NOTE: EXPRESSION is not a reserved word, but at this level there is no
                  // `DROP <column name>` alternative to shadow, so the branch is unambiguous.
                  attempt (pKeyword "DROP" >>. pKeyword "EXPRESSION" >>% ColumnAlteration.DropExpression) ]

        // 11.10 <alter table action> ::= <add column definition> | <alter column definition>
        //     | <drop column definition> | <add table constraint definition>
        //     | <alter table constraint definition> | <drop table constraint definition>
        //     | <add table period definition> | <drop table period definition>
        //     | <add system versioning clause> | <drop system versioning clause>
        let pAction =
            choice
                [ attempt (pKeyword "ADD" >>. opt (pKeyword "COLUMN") >>. pColumnDefinition |>> AddColumn)
                  attempt (pKeyword "ADD" >>. pTableConstraint |>> AlterTableAction.AddConstraint)
                  // 11.27 <add table period definition> ::= ADD <table period definition>
                  //     [ <add system time period column list> ]
                  attempt (
                      pKeyword "ADD" >>. pTablePeriodDefinition
                      .>>. opt (attempt pAddSystemTimePeriodColumnList)
                      |>> fun (period, columns) -> AddTablePeriod(period, Option.defaultValue [] columns)
                  )
                  // 11.29 <add system versioning clause> ::= ADD <system versioning clause>
                  attempt (
                      pKeyword "ADD" >>. pKeyword "SYSTEM" >>. pKeyword "VERSIONING"
                      >>% AddSystemVersioning
                  )
                  // 11.26 <drop table constraint definition>
                  //     ::= DROP CONSTRAINT <constraint name> <drop behavior>
                  attempt (
                      pKeyword "DROP" >>. pKeyword "CONSTRAINT" >>. pIdentifierExpr .>>. pDropBehavior
                      |>> AlterTableAction.DropConstraint
                  )
                  // 11.23 <drop column definition> ::= DROP [ COLUMN ] <column name> <drop behavior>
                  attempt (
                      pKeyword "DROP" >>. opt (pKeyword "COLUMN") >>. pIdentifierExpr
                      .>>. pDropBehavior
                      |>> DropColumn
                  )
                  // 11.28 <drop table period definition>
                  //     ::= DROP <system or application time period specification> <drop behavior>
                  attempt (
                      pKeyword "DROP" >>. pTimePeriodSpecification .>>. pDropBehavior
                      |>> DropTablePeriod
                  )
                  // 11.30 <drop system versioning clause> ::= DROP SYSTEM VERSIONING <drop behavior>
                  attempt (
                      pKeyword "DROP"
                      >>. pKeyword "SYSTEM"
                      >>. pKeyword "VERSIONING"
                      >>. pDropBehavior
                      |>> DropSystemVersioning
                  )
                  // 11.25 <alter table constraint definition>
                  //     ::= ALTER CONSTRAINT <constraint name> <constraint enforcement>
                  attempt (
                      pKeyword "ALTER" >>. pKeyword "CONSTRAINT" >>. pIdentifierExpr
                      .>>. pConstraintEnforcement
                      |>> AlterTableAction.AlterConstraint
                  )
                  attempt (
                      pKeyword "ALTER" >>. opt (pKeyword "COLUMN") >>. pIdentifierExpr
                      .>>. pColumnAction
                      |>> AlterColumn
                  ) ]

        pKeyword "ALTER" >>. pKeyword "TABLE" >>. pQualifiedNameExpr .>>. pAction
        |>> fun (name, action) -> { Table = name; Action = action } |> AlterTable

    // 11.32 <levels clause> ::= CASCADED | LOCAL   (default is CASCADED)
    let pCheckOption =
        pKeyword "WITH"
        >>. opt (pKeyword "CASCADED" >>% true <|> (pKeyword "LOCAL" >>% false))
        .>> pKeyword "CHECK"
        .>> pKeyword "OPTION"
        |>> Option.defaultValue true

    // 11.32 <view definition> ::= CREATE [ RECURSIVE ] VIEW <table name> <view specification>
    //       AS <query expression> [ WITH [ <levels clause> ] CHECK OPTION ]
    let pCreateViewStatement =
        // 11.32 <view column option> ::= <column name> WITH OPTIONS <scope clause>
        // (the <scope clause> is mandatory here, unlike in 11.3's <column option list>)
        let pViewColumnOption =
            pIdentifierExpr .>> pKeyword "WITH" .>> pKeyword "OPTIONS" .>>. pScopeClause
            |>> fun (name, scope) -> { Name = name; Scope = scope }

        // 11.32 <view element> ::= <self-referencing column specification> | <view column option>
        let pViewElement =
            choice
                [ attempt (pSelfReferencingColumn |>> ViewElement.ViewSelfReference)
                  attempt (pViewColumnOption |>> ViewElement.ViewColumnOption) ]

        // 11.32 <view element list> ::=
        //     <left paren> <view element> [ { <comma> <view element> }... ] <right paren>
        let pViewElementList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pViewElement (token (pstring ",")))

        // 11.32 <view specification> ::= <regular view specification> | <referenceable view specification>
        // 11.32 <regular view specification> ::= [ ( <view column list> ) ]
        // 11.32 <referenceable view specification> ::= OF <path-resolved user-defined type name>
        //     [ <subview clause> ] [ <view element list> ]
        let pViewSpecification =
            choice
                [ attempt (
                      between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                      |>> Choice1Of2
                  )
                  attempt (
                      pKeyword "OF" >>. pQualifiedNameExpr
                      .>>. opt (pKeyword "UNDER" >>. pQualifiedNameExpr)
                      .>>. opt pViewElementList
                      |>> Choice2Of2
                  ) ]

        pKeyword "CREATE" >>. opt (pKeyword "RECURSIVE" >>% true) .>> pKeyword "VIEW"
        .>>. pQualifiedNameExpr
        .>>. opt pViewSpecification
        .>> pKeyword "AS"
        .>>. pQuery
        .>>. opt (attempt pCheckOption)
        |>> fun ((((isRecursive, name), spec), query), checkOpt) ->
            let cols, ofType, under, viewElements =
                match spec with
                | Some(Choice1Of2 c) -> Some c, None, None, []
                | Some(Choice2Of2((typeName, subview), elements)) ->
                    None, Some typeName, subview, Option.defaultValue [] elements
                | None -> None, None, None, []

            { Name = name
              IsRecursive = Option.defaultValue false isRecursive
              Columns = cols
              Query = query
              CheckOption = checkOpt
              OfType = ofType
              Under = under
              ViewElements = viewElements }
            |> CreateView

    // 11.34 <domain constraint> ::= [ <constraint name definition> ] CHECK ( <search condition> ) [ <constraint characteristics> ]
    let pDomainConstraint =
        opt (pKeyword "CONSTRAINT" >>. pQualifiedNameExpr)
        .>>. (pKeyword "CHECK"
              >>. between (token (pstring "(")) (token (pstring ")")) pExpression)
        .>>. pConstraintCharacteristics
        |>> fun ((name, check), chars) ->
            { Name = name
              Check = check
              Characteristics = chars }

    // 11.34 <domain definition> ::= CREATE DOMAIN <domain name> [ AS ] <data type> [ <default clause> ] [ <domain constraint>... ] [ <collate clause> ]
    let pCreateDomainStatement =
        pKeyword "CREATE" >>. pKeyword "DOMAIN" >>. pQualifiedNameExpr
        .>>. opt (pKeyword "AS")
        .>>. pDataType
        .>>. opt pDefaultClause
        .>>. many pDomainConstraint
        .>>. opt (pKeyword "COLLATE" >>. pQualifiedNameExpr)
        |>> fun (((((name, _), dataType), def), constraints), collation) ->
            CreateDomain
                { Name = name
                  DataType = dataType
                  Default = def
                  Constraints = constraints
                  Collation = collation }

    // 11.35 <alter domain statement> ::= ALTER DOMAIN <domain name> <alter domain action>
    let pAlterDomainStatement =
        let pAction =
            choice
                [ attempt (pKeyword "SET" >>. pDefaultClause |>> DomainAlteration.SetDefault)
                  attempt (pKeyword "DROP" >>. pKeyword "DEFAULT" >>% DomainAlteration.DropDefault)
                  attempt (pKeyword "ADD" >>. pDomainConstraint |>> DomainAlteration.AddConstraint)
                  attempt (
                      pKeyword "DROP" >>. pKeyword "CONSTRAINT" >>. pQualifiedNameExpr
                      |>> DomainAlteration.DropConstraint
                  ) ]

        pKeyword "ALTER" >>. pKeyword "DOMAIN" >>. pQualifiedNameExpr .>>. pAction
        |>> fun (name, action) -> AlterDomain(name, action)

    // 11.41 <character set definition> ::= CREATE CHARACTER SET <character set name> [ AS GET <character set name> ] [ <collate clause> ]
    let pCreateCharacterSetStatement =
        pKeyword "CREATE"
        >>. pKeyword "CHARACTER"
        >>. pKeyword "SET"
        >>. pQualifiedNameExpr
        .>>. opt (pKeyword "AS")
        .>>. (pKeyword "GET" >>. pQualifiedNameExpr)
        .>>. opt (pKeyword "COLLATE" >>. pQualifiedNameExpr)
        |>> fun (((name, _), source), collate) -> CreateCharacterSet(name, source, collate)

    // 11.43 <collation definition> ::= CREATE COLLATION <collation name> FOR <character set name> FROM <collation name> [ <pad characteristic> ]
    let pCreateCollationStatement =
        // 11.43 <pad characteristic> ::= NO PAD | PAD SPACE
        let pPadCharacteristic =
            pKeyword "NO" >>. pKeyword "PAD" >>% true
            <|> (pKeyword "PAD" >>. pKeyword "SPACE" >>% false)

        pKeyword "CREATE" >>. pKeyword "COLLATION" >>. pQualifiedNameExpr
        .>>. (pKeyword "FOR" >>. pQualifiedNameExpr)
        .>>. (pKeyword "FROM" >>. pQualifiedNameExpr)
        .>>. opt pPadCharacteristic
        |>> fun (((name, cs), existing), pad) -> CreateCollation(name, cs, existing, pad)

    // 11.45 <transliteration definition> ::= CREATE TRANSLATION <transliteration name> FOR <source character set> TO <target character set> FROM <transliteration source>
    // 11.45 <transliteration source> ::= <existing transliteration name> | <transliteration routine>
    // 11.45 <transliteration routine> ::= <specific routine designator>
    let pCreateTransliterationStatement =
        pKeyword "CREATE" >>. pKeyword "TRANSLATION" >>. pQualifiedNameExpr
        .>>. (pKeyword "FOR" >>. pQualifiedNameExpr)
        .>>. (pKeyword "TO" >>. pQualifiedNameExpr)
        .>>. (pKeyword "FROM" >>. pSpecificRoutineDesignator)
        |>> fun (((name, source), target), trSource) -> CreateTransliteration(name, source, target, trSource)

    // 11.47 <assertion definition> ::= CREATE ASSERTION <constraint name> CHECK ( <search condition> ) [ <constraint characteristics> ]
    let pCreateAssertionStatement =
        pKeyword "CREATE" >>. pKeyword "ASSERTION" >>. pQualifiedNameExpr
        .>>. (pKeyword "CHECK"
              >>. between (token (pstring "(")) (token (pstring ")")) pExpression)
        .>>. pConstraintCharacteristics
        |>> fun ((name, check), chars) -> CreateAssertion(name, check, chars)

    // 13.4 <SQL procedure statement> / 11.49 <triggered SQL statement> — the shared top-level
    // statement forward ref. Declared here because SchemaParser is the first module that
    // needs "any statement" (routine bodies / triggered statements); SqlParser.fs wires it
    // and reuses the same ref.
    let pStatement, pStatementRef = createParserForwardedToRef<Statement, unit> ()

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
                  >>. sepEndBy1 pStatement (token (pstring ";"))
                  .>> pKeyword "END"
                  |>> fun stmts -> TriggeredStatement.BeginAtomic(List.map (fun s -> s.Kind) stmts)
              )
              attempt (pStatement |>> fun s -> SingleStatement s.Kind) ]

    // 11.49 <triggered action> ::= [ FOR EACH { ROW | STATEMENT } ] [ WHEN ( <search condition> ) ] <triggered SQL statement>
    let pTriggeredAction =
        opt (
            pKeyword "FOR"
            >>. pKeyword "EACH"
            >>. (pKeyword "ROW" >>% true <|> (pKeyword "STATEMENT" >>% false))
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

    // 11.60 <parameter mode> ::= IN | OUT | INOUT
    let pParameterMode =
        choice
            [ attempt (pKeyword "INOUT" >>% ParameterMode.InOut)
              attempt (pKeyword "IN" >>% ParameterMode.In)
              attempt (pKeyword "OUT" >>% ParameterMode.Out) ]

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

        let pWithName =
            opt pParameterMode .>>. pIdentifierExpr .>>. pParameterType
            |>> fun ((mode, name), paramType) -> mode, Some name, paramType

        let pWithoutName =
            opt pParameterMode .>>. pParameterType
            |>> fun (mode, paramType) -> mode, None, paramType

        attempt (
            attempt pWithName <|> pWithoutName
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

    // 11.60 <parameter style clause> ::= PARAMETER STYLE <parameter style>
    // 11.60 <parameter style> ::= SQL | GENERAL
    let pParameterStyleClause =
        pKeyword "PARAMETER"
        >>. pKeyword "STYLE"
        >>. (pKeyword "SQL" >>% "SQL" <|> (pKeyword "GENERAL" >>% "GENERAL"))

    // 11.52 <attribute definition> ::= <attribute name> <data type>
    //     [ <attribute default> ] [ <collate clause> ]
    let pAttributeDefinition =
        pIdentifierExpr
        .>>. pDataType
        .>>. opt (pKeyword "DEFAULT" >>. pExpression)
        .>>. opt (pKeyword "COLLATE" >>. pQualifiedNameExpr)
        |>> fun (((name, dataType), def), collate) ->
            { Name = name
              DataType = dataType
              Default = def
              Collate = collate }

    // 11.51 <representation> ::= <predefined type> | <collection type> | <member list>
    // (the <predefined type> alternative is tried first so that AS ROW ( ... ) /
    // AS INT ARRAY etc. parse as data types, not member lists)
    let pRepresentation =
        // 11.51 <member list> ::= ( <attribute definition> { , <attribute definition> } )
        let pMemberList =
            between (token (pstring "(")) (token (pstring ")")) (sepBy1 pAttributeDefinition (token (pstring ",")))

        choice
            [ attempt (pDataType |>> TypeRepresentation.Predefined)
              attempt (pMemberList |>> TypeRepresentation.MemberList) ]

    // 11.51 <user-defined type option> ::= <instantiable clause> | <finality> | <reference type specification> | <cast to ref> | <cast to type> | <cast to distinct> | <cast to source>
    let pTypeOption =
        choice
            [ // 11.51 <instantiable clause> ::= INSTANTIABLE | NOT INSTANTIABLE
              attempt (pKeyword "INSTANTIABLE" >>% TypeOption.Instantiable true)
              attempt (pKeyword "NOT" >>. pKeyword "INSTANTIABLE" >>% TypeOption.Instantiable false)
              // 11.51 <finality> ::= FINAL | NOT FINAL
              attempt (pKeyword "FINAL" >>% TypeOption.Final true)
              attempt (pKeyword "NOT" >>. pKeyword "FINAL" >>% TypeOption.Final false)
              // 11.51 <reference type specification> — <user-defined representation> ::= REF USING <predefined type>
              attempt (pKeyword "REF" >>. pKeyword "USING" >>. pDataType |>> TypeOption.RefUsing)
              // 11.51 <derived representation> ::= REF FROM <list of attributes>
              attempt (
                  pKeyword "REF"
                  >>. pKeyword "FROM"
                  >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
                  |>> TypeOption.RefFrom
              )
              // 11.51 <system-generated representation> ::= REF IS SYSTEM GENERATED
              attempt (
                  pKeyword "REF" >>. pKeyword "IS" >>. pKeyword "SYSTEM" >>. pKeyword "GENERATED"
                  >>% TypeOption.RefIsSystemGenerated
              )
              // 11.51 <cast to ref> ::= CAST ( SOURCE AS REF ) WITH <cast to ref identifier>
              attempt (
                  pKeyword "CAST"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pKeyword "SOURCE" >>. pKeyword "AS" >>. pKeyword "REF" >>% ())
                  .>> pKeyword "WITH"
                  >>. pIdentifierExpr
                  |>> TypeOption.CastToRef
              )
              // 11.51 <cast to type> ::= CAST ( REF AS SOURCE ) WITH <cast to type identifier>
              attempt (
                  pKeyword "CAST"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pKeyword "REF" >>. pKeyword "AS" >>. pKeyword "SOURCE" >>% ())
                  .>> pKeyword "WITH"
                  >>. pIdentifierExpr
                  |>> TypeOption.CastToType
              )
              // 11.51 <cast to distinct> ::= CAST ( SOURCE AS DISTINCT ) WITH <cast to distinct identifier>
              attempt (
                  pKeyword "CAST"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pKeyword "SOURCE" >>. pKeyword "AS" >>. pKeyword "DISTINCT" >>% ())
                  .>> pKeyword "WITH"
                  >>. pIdentifierExpr
                  |>> TypeOption.CastToDistinct
              )
              // 11.51 <cast to source> ::= CAST ( DISTINCT AS SOURCE ) WITH <cast to source identifier>
              attempt (
                  pKeyword "CAST"
                  >>. between
                          (token (pstring "("))
                          (token (pstring ")"))
                          (pKeyword "DISTINCT" >>. pKeyword "AS" >>. pKeyword "SOURCE" >>% ())
                  .>> pKeyword "WITH"
                  >>. pIdentifierExpr
                  |>> TypeOption.CastToSource
              ) ]

    // 11.51 <partial method specification> ::= [ INSTANCE | STATIC | CONSTRUCTOR ]
    //     METHOD <method name> <SQL parameter declaration list> <returns clause>
    //     [ SPECIFIC <specific method name> ]
    // NOTE: pMethodKind (10.6 / 11.51 / 11.60) lives in ExpressionParser.fs, which is compiled
    // before this module, so the routine and type parsers here can share it.
    let pPartialMethodSpecification =
        opt pMethodKind
        .>>. (pKeyword "METHOD" >>. pIdentifierExpr)
        .>>. pParameterDeclarationList
        .>>. opt (pKeyword "RETURNS" >>. pDataType)
        .>>. opt (pKeyword "SPECIFIC" >>. pQualifiedNameExpr)
        |>> fun ((((kind, name), parameters), returns), specific) ->
            { Kind = kind
              Name = name
              Parameters = parameters
              Returns = returns
              Specific = specific
              SelfAsResult = false
              SelfAsLocator = false
              Characteristics = [] }

    // 11.51 <method characteristic> ::= <language clause> | <parameter style clause> | <deterministic characteristic> | <SQL-data access indication> | <null-call clause>
    let pMethodCharacteristic =
        choice
            [ // 10.2 <language clause> / 11.51 <parameter style clause> — shared with 11.60
              attempt (pLanguageClause |>> Language)
              attempt (pParameterStyleClause |>> ParameterStyle)
              // 11.51 <deterministic characteristic> ::= DETERMINISTIC | NOT DETERMINISTIC
              attempt (pKeyword "NOT" >>. pKeyword "DETERMINISTIC" >>% Deterministic false)
              attempt (pKeyword "DETERMINISTIC" >>% Deterministic true)
              // 11.51 <SQL-data access indication> ::= NO SQL | CONTAINS SQL | READS SQL DATA | MODIFIES SQL DATA
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
              // 11.51 <null-call clause> ::= RETURNS NULL ON NULL INPUT | CALLED ON NULL INPUT
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
              ) ]

    // ISO 9075-2 SR: each <method characteristic> may appear at most once — reject duplicates.
    let pMethodCharacteristics =
        many pMethodCharacteristic
        >>= fun chars ->
            let dup =
                chars
                |> List.groupBy (fun c ->
                    match c with
                    | Language _ -> "Language"
                    | ParameterStyle _ -> "ParameterStyle"
                    | Deterministic _ -> "Deterministic"
                    | SqlDataAccess _ -> "SqlDataAccess"
                    | NullCall _ -> "NullCall"
                    | _ -> "Other")
                |> List.tryFind (fun (_, g) -> List.length g > 1)

            match dup with
            | Some(cat, _) -> fail (sprintf "duplicate method characteristic: %s" cat)
            | None -> preturn chars

    // 11.51 <original method specification> ::= <partial method specification>
    //     [ SELF AS RESULT ] [ SELF AS LOCATOR ] [ <method characteristics> ]
    let pOriginalMethodSpecification =
        pPartialMethodSpecification
        .>>. opt (attempt (pKeyword "SELF" >>. pKeyword "AS" >>. pKeyword "RESULT" >>% true))
        .>>. opt (attempt (pKeyword "SELF" >>. pKeyword "AS" >>. pKeyword "LOCATOR" >>% true))
        .>>. pMethodCharacteristics
        |>> fun (((baseSpec, selfAsResult), selfAsLocator), characteristics) ->
            { baseSpec with
                SelfAsResult = Option.isSome selfAsResult
                SelfAsLocator = Option.isSome selfAsLocator
                Characteristics = characteristics }

    // 11.51 <method specification> ::= <original method specification>
    //     | OVERRIDING <partial method specification>
    let pMethodSpecification =
        choice
            [ attempt (pKeyword "OVERRIDING" >>. pPartialMethodSpecification)
              attempt pOriginalMethodSpecification ]

    // 11.51 <method specification list> ::= <method specification> [ { <comma> <method specification> }... ]
    let pMethodSpecificationList = sepBy1 pMethodSpecification (token (pstring ","))

    // 11.51 <user-defined type definition> ::= CREATE TYPE <user-defined type body>
    // 11.51 <user-defined type body> ::= <schema-resolved user-defined type name> [ <subtype clause> ] [ AS <representation> ] [ <user-defined type option list> ] [ <method specification list> ]
    // 11.51 <subtype clause> ::= UNDER <supertype name>
    let pCreateTypeStatement =
        pKeyword "CREATE" >>. pKeyword "TYPE" >>. pQualifiedNameExpr
        .>>. opt (pKeyword "UNDER" >>. pQualifiedNameExpr)
        .>>. opt (pKeyword "AS" >>. pRepresentation)
        .>>. many pTypeOption
        .>>. opt pMethodSpecificationList
        |>> fun ((((name, under), repr), options), methods) ->
            CreateType
                { Name = name
                  Under = under
                  Representation = repr
                  Options = options
                  Methods = Option.defaultValue [] methods }

    // 11.53 <alter type action> ::= <add attribute definition> | <drop attribute definition> | <add original method specification> | <add overriding method specification> | <drop method specification>
    let pAlterTypeAction =
        choice
            [ // 11.54 <add attribute definition> ::= ADD ATTRIBUTE <attribute definition>
              attempt (
                  pKeyword "ADD" >>. pKeyword "ATTRIBUTE" >>. pAttributeDefinition
                  |>> AlterTypeAction.AddAttribute
              )
              // 11.55 <drop attribute definition> ::= DROP ATTRIBUTE <attribute name> RESTRICT
              attempt (
                  pKeyword "DROP" >>. pKeyword "ATTRIBUTE" >>. pIdentifierExpr
                  .>> pKeyword "RESTRICT"
                  |>> AlterTypeAction.DropAttribute
              )
              // 11.56/11.57 <add [overriding] method specification>
              attempt (
                  pKeyword "ADD"
                  >>. choice
                          [ attempt (pKeyword "OVERRIDING" >>. pPartialMethodSpecification |>> fun s -> (s, true))
                            attempt (pOriginalMethodSpecification |>> fun s -> s, false) ]
                  |>> fun (spec, ovr) -> AlterTypeAction.AddMethod(spec, ovr)
              )
              // 11.58 <drop method specification> ::= DROP
              //     [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD <method name>
              //     <data type list> RESTRICT
              attempt (
                  pKeyword "DROP" >>. opt pMethodKind .>> pKeyword "METHOD"
                  .>>. pIdentifierExpr
                  .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pDataType (token (pstring ",")))
                  .>> pKeyword "RESTRICT"
                  |>> fun ((kind, name), types) -> AlterTypeAction.DropMethod(kind, name, types)
              ) ]

    // 11.53 <alter type statement> ::= ALTER TYPE <schema-resolved user-defined type name> <alter type action>
    let pAlterTypeStatement =
        pKeyword "ALTER" >>. pKeyword "TYPE" >>. pQualifiedNameExpr
        .>>. pAlterTypeAction
        |>> fun (name, action) -> AlterType { Name = name; Action = action }

    // 11.60 <rights clause> ::= SQL SECURITY INVOKER | SQL SECURITY DEFINER
    let pRightsClause =
        pKeyword "SQL"
        >>. pKeyword "SECURITY"
        >>. (pKeyword "INVOKER" >>% RightsClause.SqlSecurityInvoker
             <|> (pKeyword "DEFINER" >>% RightsClause.SqlSecurityDefiner))

    // 11.60 <external security clause> ::= EXTERNAL SECURITY DEFINER
    //     | EXTERNAL SECURITY INVOKER | EXTERNAL SECURITY IMPLEMENTATION DEFINED
    let pExternalSecurityClause =
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
    let pTransformGroupSpecification =
        pKeyword "TRANSFORM"
        >>. pKeyword "GROUP"
        >>. sepBy1
                (pIdentifierExpr
                 .>>. opt (attempt (pKeyword "FOR" >>. pKeyword "TYPE" >>. pQualifiedNameExpr)))
                (token (pstring ","))
        |>> fun groups ->
            match groups with
            | [ name, None ] -> TransformGroupSpecification.SingleTransformGroup name
            | _ -> TransformGroupSpecification.MultipleTransformGroups groups

    // 11.60 <external body reference> ::= EXTERNAL [ NAME <external routine name> ]
    //     [ <parameter style clause> ] [ <transform group specification> ] [ <external security clause> ]
    let pExternalBodyReference =
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
    let pPtfPrivateParameters =
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
    let pPolymorphicTableFunctionBody =
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
    let routineCharacteristicCategory c =
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

    let private rejectDuplicateCharacteristics chars : Parser<RoutineCharacteristic list, unit> =
        let dup =
            chars
            |> List.groupBy routineCharacteristicCategory
            |> List.tryFind (fun (_, g) -> List.length g > 1)

        match dup with
        | Some(cat, _) -> fail (sprintf "duplicate routine characteristic: %s" cat)
        | None -> preturn chars

    let pRoutineCharacteristics =
        many pRoutineCharacteristic >>= rejectDuplicateCharacteristics

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
                  >>. sepEndBy1 pStatement (token (pstring ";"))
                  .>> pKeyword "END"
                  |>> fun stmts -> RoutineBody.BeginAtomic(List.map (fun s -> s.Kind) stmts)
              )
              attempt (
                  opt pRightsClause .>>. pStatement
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
    let pMethodSpecificationDesignator =
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

    // 11.61 <alter routine statement> ::= ALTER <specific routine designator> <alter routine characteristic>... [ RESTRICT ]
    let pAlterRoutineStatement =
        pKeyword "ALTER" >>. pSpecificRoutineDesignator
        .>>. pAlterRoutineCharacteristics
        .>>. opt (pKeyword "RESTRICT")
        |>> fun ((routine, characteristics), _) ->
            AlterRoutine
                { Routine = routine
                  Characteristics = characteristics }

    // 11.63 <user-defined cast definition> ::= CREATE CAST ( <source data type> AS <target data type> ) WITH <cast function> [ AS ASSIGNMENT ]
    let pCreateCastStatement =
        pKeyword "CREATE"
        >>. pKeyword "CAST"
        >>. between (token (pstring "(")) (token (pstring ")")) (pDataType .>>. (pKeyword "AS" >>. pDataType))
        .>>. (pKeyword "WITH" >>. pSpecificRoutineDesignator)
        .>>. opt (pKeyword "AS" >>. pKeyword "ASSIGNMENT" >>% true)
        |>> fun (((source, target), fn), assignment) ->
            CreateCast(source, target, fn, Option.defaultValue false assignment)

    // 11.65 <ordering category> ::= RELATIVE WITH <specific routine designator> | MAP WITH <specific routine designator> | STATE [ <data type> ]
    let pOrderingCategory =
        choice
            [ attempt (
                  pKeyword "RELATIVE" >>. pKeyword "WITH" >>. pSpecificRoutineDesignator
                  |>> OrderingCategory.Relative
              )
              attempt (
                  pKeyword "MAP" >>. pKeyword "WITH" >>. pSpecificRoutineDesignator
                  |>> OrderingCategory.Map
              )
              attempt (pKeyword "STATE" >>. opt pQualifiedNameExpr |>> OrderingCategory.State) ]

    // 11.65 <ordering form> ::= EQUALS ONLY BY <ordering category> | ORDER FULL BY <ordering category>
    let pOrderingForm =
        pKeyword "EQUALS" >>. pKeyword "ONLY" >>. pKeyword "BY" >>. pOrderingCategory
        |>> OrderingForm.EqualsOnlyBy
        <|> (pKeyword "ORDER" >>. pKeyword "FULL" >>. pKeyword "BY" >>. pOrderingCategory
             |>> OrderingForm.OrderFullBy)

    // 11.65 <user-defined ordering definition> ::= CREATE ORDERING FOR <schema-resolved user-defined type name> <ordering form>
    let pCreateOrderingStatement =
        pKeyword "CREATE"
        >>. pKeyword "ORDERING"
        >>. pKeyword "FOR"
        >>. pQualifiedNameExpr
        .>>. pOrderingForm
        |>> fun (name, form) -> CreateOrdering(name, form)

    // 11.67 <transform element> ::= TO SQL WITH <specific routine designator> | FROM SQL WITH <specific routine designator>
    let pTransformElement =
        pKeyword "TO"
        >>. pKeyword "SQL"
        >>. pKeyword "WITH"
        >>. pSpecificRoutineDesignator
        |>> TransformElement.ToSql
        <|> (pKeyword "FROM"
             >>. pKeyword "SQL"
             >>. pKeyword "WITH"
             >>. pSpecificRoutineDesignator
             |>> TransformElement.FromSql)

    // 11.67 <transform group> ::= <group name> ( <transform element> [ { <comma> <transform element> }... ] )
    let pTransformGroup =
        pQualifiedNameExpr
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTransformElement (token (pstring ",")))
        |>> fun (name, elements) -> { Name = name; Elements = elements }

    // 11.67 <transform definition> ::= CREATE { TRANSFORM | TRANSFORMS } FOR <schema-resolved user-defined type name> <transform group> [ { <comma> <transform group> }... ]
    let pCreateTransformStatement =
        pKeyword "CREATE"
        >>. (pKeyword "TRANSFORM" <|> pKeyword "TRANSFORMS")
        >>. pKeyword "FOR"
        >>. pQualifiedNameExpr
        .>>. many1 pTransformGroup
        |>> fun (name, groups) -> CreateTransform(name, groups)

    // 11.68 <alter transform action> ::= ADD <transform element list> | DROP ( <transform kind list> ) <drop behavior>
    let pAlterTransformAction =
        // 11.70 <transform kind> ::= TO SQL | FROM SQL
        // Local because pAlterTransformAction is its only consumer.
        let pTransformKind =
            pKeyword "TO" >>. pKeyword "SQL" >>% TransformKind.ToSqlKind
            <|> (pKeyword "FROM" >>. pKeyword "SQL" >>% TransformKind.FromSqlKind)

        pKeyword "ADD"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTransformElement (token (pstring ",")))
        |>> TransformAlteration.AddTransformElements
        <|> (pKeyword "DROP"
             >>. between
                     (token (pstring "("))
                     (token (pstring ")"))
                     (sepBy1 pTransformKind (token (pstring ",")) .>>. pDropBehavior)
             |>> TransformAlteration.DropTransformElements)

    // 11.68 <alter transform group> ::= <group name> ( <alter transform action> [ { <comma> <alter transform action> }... ] )
    let pAlterTransformGroup =
        pQualifiedNameExpr
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pAlterTransformAction (token (pstring ",")))
        |>> fun (name, actions) -> { Name = name; Actions = actions }

    // 11.68 <alter transform statement> ::= ALTER { TRANSFORM | TRANSFORMS } FOR <schema-resolved user-defined type name> <alter transform group> [ { <comma> <alter transform group> }... ]
    let pAlterTransformStatement =
        pKeyword "ALTER"
        >>. (pKeyword "TRANSFORM" <|> pKeyword "TRANSFORMS")
        >>. pKeyword "FOR"
        >>. pQualifiedNameExpr
        .>>. many1 pAlterTransformGroup
        |>> fun (name, groups) -> AlterTransform(name, groups)

    // 11.2 <drop schema statement> ::= DROP SCHEMA <schema name> <drop behavior>
    // 11.31 <drop table statement> ::= DROP TABLE <table name> <drop behavior>
    // 11.33 <drop view statement> ::= DROP VIEW <table name> <drop behavior>
    // 11.40 <drop domain statement> ::= DROP DOMAIN <domain name> <drop behavior>
    // 11.42 <drop character set statement> ::= DROP CHARACTER SET <character set name>
    // 11.44 <drop collation statement> ::= DROP COLLATION <collation name> <drop behavior>
    // 11.46 <drop transliteration statement> ::= DROP TRANSLATION <transliteration name>
    // 11.48 <drop assertion statement> ::= DROP ASSERTION <constraint name> [ <drop behavior> ]
    // 11.50 <drop trigger statement> ::= DROP TRIGGER <trigger name>
    // 11.59 <drop data type statement> ::= DROP TYPE <schema-resolved user-defined type name> <drop behavior>
    // 11.64 <drop user-defined cast statement> ::= DROP CAST (<source data type> AS <target data type>) <drop behavior>
    // 11.66 <drop user-defined ordering statement> ::= DROP ORDERING FOR <schema-resolved user-defined type name> <drop behavior>
    // 11.71 <drop transform statement> ::= DROP { TRANSFORM | TRANSFORMS } <transforms to be dropped> FOR <schema-resolved user-defined type name> <drop behavior>
    // 11.74 <drop sequence generator statement> ::= DROP SEQUENCE <sequence generator name> <drop behavior>
    // 12.6 <drop role statement> ::= DROP ROLE <role name>
    // 11.62 <drop routine statement> ::= DROP <specific routine designator> <drop behavior>
    let pDropStatement =
        // 11.71 <transforms to be dropped> ::= ALL | <transform group element>
        // Local because pDropStatement is its only consumer.
        let pTransformsToBeDropped =
            pKeyword "ALL" >>% TransformDropTarget.AllTransforms
            <|> (pQualifiedNameExpr |>> TransformDropTarget.TransformGroup)

        pKeyword "DROP"
        >>. choice
                [ attempt (pKeyword "SCHEMA" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropSchema
                  attempt (pKeyword "TABLE" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropTable
                  attempt (pKeyword "VIEW" >>. pQualifiedNameExpr .>>. pDropBehavior) |>> DropView
                  attempt (pKeyword "DOMAIN" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropDomain
                  attempt (pKeyword "CHARACTER" >>. pKeyword "SET" >>. pQualifiedNameExpr)
                  |>> DropCharacterSet
                  attempt (pKeyword "COLLATION" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropCollation
                  attempt (pKeyword "TRANSLATION" >>. pQualifiedNameExpr) |>> DropTransliteration
                  attempt (pKeyword "ASSERTION" >>. pQualifiedNameExpr .>>. opt pDropBehavior)
                  |>> DropAssertion
                  attempt (pKeyword "TRIGGER" >>. pQualifiedNameExpr) |>> DropTrigger
                  attempt (pKeyword "TYPE" >>. pQualifiedNameExpr .>>. pDropBehavior) |>> DropType
                  attempt (
                      pKeyword "CAST"
                      >>. between
                              (token (pstring "("))
                              (token (pstring ")"))
                              (pDataType .>>. (pKeyword "AS" >>. pDataType))
                      .>>. pDropBehavior
                  )
                  |>> fun ((source, target), behavior) -> DropCast(source, target, behavior)
                  attempt (pKeyword "ORDERING" >>. pKeyword "FOR" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropOrdering
                  attempt (
                      pKeyword "TRANSFORM" <|> pKeyword "TRANSFORMS" >>. pTransformsToBeDropped
                      .>>. (pKeyword "FOR" >>. pQualifiedNameExpr)
                      .>>. pDropBehavior
                  )
                  |>> fun ((target, forName), behavior) -> DropTransform(forName, target, behavior)
                  attempt (pKeyword "SEQUENCE" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropSequence
                  attempt (pKeyword "ROLE" >>. pIdentifierExpr) |>> DropRole
                  attempt (pRoutineDesignatorWithType .>>. pDropBehavior) |>> DropRoutine ]

    // 11.72 <sequence generator definition> ::= CREATE SEQUENCE <sequence generator name> [ <sequence generator options> ]
    let pCreateSequenceStatement =
        pKeyword "CREATE" >>. pKeyword "SEQUENCE" >>. pQualifiedNameExpr
        .>>. many pSequenceOption
        |>> fun (name, opts) -> CreateSequence(name, opts)

    // 11.73 <alter sequence generator statement> ::= ALTER SEQUENCE <name> <options>
    let pAlterSequenceStatement =
        pKeyword "ALTER" >>. pKeyword "SEQUENCE" >>. pQualifiedNameExpr
        .>>. many1 pSequenceOption
        |>> fun (name, opts) -> AlterSequence(name, opts)
