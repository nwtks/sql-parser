namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser
open SqlParser.Types

module DdlParser =
    // 10.8 <constraint enforcement> ::= [ NOT ] ENFORCED   (true = ENFORCED, false = NOT ENFORCED)
    // Also used by 11.25 <alter table constraint definition>, and by the
    // <column constraint definition> (11.4) / <table constraint definition> (11.6).
    // It is defined at the top of the module so both can reuse it.
    let pConstraintEnforcement: Parser<bool, unit> =
        attempt (pKeyword "NOT" >>. pKeyword "ENFORCED" >>% false)
        <|> (pKeyword "ENFORCED" >>% true)

    // 10.8 <constraint characteristics> ::=
    //     <constraint check time> [ [ NOT ] DEFERRABLE ] [ <constraint enforcement> ]
    //   | [ [ NOT ] DEFERRABLE ] <constraint check time> [ <constraint enforcement> ]
    //   | <constraint enforcement>
    let pConstraintCharacteristics: Parser<ConstraintCharacteristics, unit> =
        // 10.8 <constraint check time> ::= INITIALLY DEFERRED | INITIALLY IMMEDIATE
        // NOTE: both alternatives are parenthesized — `<|>` binds tighter than `>>.`/`>>%`,
        // so an unparenthesized `INITIALLY >>. DEFERRED >>% true <|> (...)` would group as
        // `INITIALLY >>. (DEFERRED >>% (true <|> ...))` and never try IMMEDIATE.
        let pCheckTime: Parser<bool, unit> =
            attempt (pKeyword "INITIALLY" >>. pKeyword "DEFERRED" >>% true)
            <|> (pKeyword "INITIALLY" >>. pKeyword "IMMEDIATE" >>% false)

        // 10.8 <constraint deferrability> ::= [ NOT ] DEFERRABLE
        let pDeferrable: Parser<bool, unit> =
            attempt (pKeyword "NOT" >>. pKeyword "DEFERRABLE" >>% false)
            <|> (pKeyword "DEFERRABLE" >>% true)

        // 10.8 <constraint enforcement> ::= [ NOT ] ENFORCED — shared with 11.25
        let pEnforced = pConstraintEnforcement

        let mk (initiallyDeferred: bool option) (deferrable: bool option) (enforced: bool option) =
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

    // 11.8 <referential action> ::= CASCADE | SET NULL | SET DEFAULT | RESTRICT | NO ACTION
    let pReferentialAction =
        choice
            [ pKeyword "CASCADE" >>% ReferentialAction.Cascade
              attempt (pKeyword "SET" >>. pKeyword "NULL" >>% ReferentialAction.SetNull)
              attempt (pKeyword "SET" >>. pKeyword "DEFAULT" >>% ReferentialAction.SetDefault)
              attempt (pKeyword "RESTRICT" >>% ReferentialAction.Restrict)
              attempt (pKeyword "NO" >>. pKeyword "ACTION" >>% ReferentialAction.NoAction) ]

    // 11.8 <referential triggered action> ::= [ <update rule> ] [ <delete rule> ] | [ <delete rule> ] [ <update rule> ] — <update rule> ::= ON UPDATE <referential action>
    let pReferentialTriggeredAction =
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
              attempt (pKeyword "CURRENT_USER" >>% CurrentUser |> withExprPosition)
              attempt (pKeyword "CURRENT_ROLE" >>% CurrentRole |> withExprPosition)
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

    // 10.7 <collate clause> ::= COLLATE <collation name>
    let pCollateClause = pKeyword "COLLATE" >>. pQualifiedNameExpr

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

    // 11.8 <referential constraint definition> ::= FOREIGN KEY ( <column list> ) REFERENCES <table> [ ( <column list> ) ] [ <referential triggered action> ]
    let pForeignKeyConstraint =
        pKeyword "FOREIGN"
        >>. pKeyword "KEY"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
        .>> pKeyword "REFERENCES"
        .>>. pIdentifierExpr
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))
        .>>. pReferentialTriggeredAction
        |>> fun (((cols, table), refCols), (onUpd, onDel)) ->
            { Name = None
              Columns = cols
              Table = table
              RefColumns = refCols
              OnUpdate = onUpd
              OnDelete = onDel }
            : ForeignKeyConstraint

    // 11.6 <table constraint definition> ::=
    //     [ <constraint name definition> ] <table constraint> [ <constraint characteristics> ]
    // 11.6 <table constraint> ::= PRIMARY KEY | UNIQUE | FOREIGN KEY | CHECK
    let pTableConstraint =
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
    let pSelfReferencingColumn: Parser<SelfReferencingColumnSpecification, unit> =
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
        let pColumnOptions: Parser<ColumnOptions, unit> =
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
        let pViewColumnOption: Parser<ViewColumnOptions, unit> =
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

    // 12.2 <grantor> ::= CURRENT_USER | CURRENT_ROLE
    let pGrantor =
        pIdentifierExpr |>> ignore
        <|> (pKeyword "CURRENT_USER" |>> ignore)
        <|> (pKeyword "CURRENT_ROLE" |>> ignore)

    // 12.4 <role definition> ::= CREATE ROLE <role name> [ WITH ADMIN <grantor> ]
    let pCreateRoleStatement =
        pKeyword "CREATE" >>. pKeyword "ROLE" >>. pIdentifierExpr
        .>>. opt (pKeyword "WITH" >>. pKeyword "ADMIN" >>. pGrantor)
        |>> fun (name, _) -> CreateRole name

    // 12.3 <privilege column list> ::= ( <column name list> )
    let pPrivilegeColumnList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))

    // 10.6 <routine type> ::= ROUTINE | FUNCTION | PROCEDURE
    //     | [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD
    let pRoutineType: Parser<RoutineType, unit> =
        choice
            [ pKeyword "ROUTINE" >>% RoutineType.Routine
              pKeyword "FUNCTION" >>% RoutineType.Function
              pKeyword "PROCEDURE" >>% RoutineType.Procedure
              // 10.6 pMethodKind also serves 11.51 / 11.60 — it lives in Types.fs.
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
    let pSpecificRoutineDesignator: Parser<SpecificRoutineDesignator, unit> =
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

    // 12.3 <privilege method list> ::= <specific routine designator> [ { , <specific routine designator> }... ]
    let pPrivilegeMethodList = sepBy1 pSpecificRoutineDesignator (token (pstring ","))

    // 12.3 <action> ::= SELECT | SELECT ( <privilege column list> )
    //                 | SELECT ( <privilege method list> ) | INSERT [ <column list> ] ...
    let pPrivilegeAction =
        choice
            [ // SELECT ( <privilege method list> ) must be tried before the plain
              // SELECT [ <privilege column list> ] alternative, otherwise the
              // leading SELECT would consume the input and leave the '(' behind.
              attempt (
                  pKeyword "SELECT"
                  >>. between (token (pstring "(")) (token (pstring ")")) pPrivilegeMethodList
                  |>> fun methods -> PrivilegeAction.Select(Some(PrivilegeMethods methods))
              )
              attempt (
                  pKeyword "SELECT" >>. opt pPrivilegeColumnList
                  |>> fun cols -> PrivilegeAction.Select(Option.map PrivilegeColumns cols)
              )
              attempt (pKeyword "INSERT" >>. opt pPrivilegeColumnList |>> PrivilegeAction.Insert)
              attempt (pKeyword "UPDATE" >>. opt pPrivilegeColumnList |>> PrivilegeAction.Update)
              attempt (pKeyword "DELETE" >>% PrivilegeAction.Delete)
              attempt (
                  pKeyword "REFERENCES" >>. opt pPrivilegeColumnList
                  |>> PrivilegeAction.References
              )
              attempt (pKeyword "USAGE" >>% PrivilegeAction.Usage)
              attempt (pKeyword "TRIGGER" >>% PrivilegeAction.Trigger)
              attempt (pKeyword "UNDER" >>% PrivilegeAction.Under)
              attempt (pKeyword "EXECUTE" >>% PrivilegeAction.Execute) ]

    // 12.3 <privileges> ::= ALL PRIVILEGES | <action> [ { <comma> <action> }... ]
    let pPrivileges =
        choice
            [ attempt (pKeyword "ALL" >>. pKeyword "PRIVILEGES" >>% Privileges.AllPrivileges)
              attempt (sepBy1 pPrivilegeAction (token (pstring ",")) |>> Privileges.Actions) ]

    // 12.3 <object name> / 12.2 <grant privilege statement> — <object name> ::= [ <object kind> ] <qualified name> | <specific routine designator>
    let pObjectName =
        let pKind =
            choice
                [ pKeyword "TABLE"
                  pKeyword "DOMAIN"
                  pKeyword "COLLATION"
                  attempt (pKeyword "CHARACTER" >>. pKeyword "SET")
                  pKeyword "TYPE"
                  pKeyword "SEQUENCE"
                  pKeyword "TRANSLATION" ]

        choice
            [ attempt (opt pKind >>. pQualifiedNameExpr)
              attempt pRoutineDesignatorWithType ]

    // 11.2 <drop behavior> ::= CASCADE | RESTRICT   (true = CASCADE, false = RESTRICT)
    let pDropBehavior = pKeyword "CASCADE" >>% true <|> (pKeyword "RESTRICT" >>% false)

    // 12.2 <grant privilege statement> ::= GRANT <privileges> TO <grantee> [ { , <grantee> }... ]
    //     [ WITH HIERARCHY OPTION ] [ WITH GRANT OPTION ] [ GRANTED BY <grantor> ]
    let pGrantStatement =
        pKeyword "GRANT"
        >>. choice
                [ attempt (
                      pPrivileges .>> pKeyword "ON" .>>. pObjectName .>> pKeyword "TO"
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "HIERARCHY" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "GRANT" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      |>> fun (((((privs, obj), grantees), withHier), withOpt), _) ->
                          GrantStatement.GrantPrivileges(
                              privs,
                              obj,
                              grantees,
                              Option.isSome withHier,
                              Option.isSome withOpt
                          )
                  )
                  // 12.5 <grant role statement> ::= GRANT <role granted> [ { , <role granted> }... ]
                  //     TO <grantee> [ { , <grantee> }... ] [ WITH ADMIN OPTION ] [ GRANTED BY <grantor> ]
                  attempt (
                      sepBy1 pIdentifierExpr (token (pstring ",")) .>> pKeyword "TO"
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>>. opt (attempt (pKeyword "WITH" >>. pKeyword "ADMIN" >>. pKeyword "OPTION"))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      |>> fun (((roles, grantees), withAdm), _) ->
                          GrantStatement.GrantRoles(roles, grantees, Option.isSome withAdm)
                  ) ]
        |>> Grant

    // 12.7 <revoke statement> ::= <revoke privilege statement> | <revoke role statement>
    // 12.7 <revoke option extension> ::= GRANT OPTION FOR | HIERARCHY OPTION FOR
    let pRevokeOptionExtension =
        choice
            [ attempt (pKeyword "GRANT" >>. pKeyword "OPTION" >>. pKeyword "FOR" >>% GrantOptionFor)
              attempt (
                  pKeyword "HIERARCHY" >>. pKeyword "OPTION" >>. pKeyword "FOR"
                  >>% HierarchyOptionFor
              ) ]

    // 12.7 <revoke privilege statement> ::= REVOKE [ <revoke option extension> ] <privileges>
    //     FROM <grantee> [ { , <grantee> }... ] [ GRANTED BY <grantor> ] <drop behavior>
    let pRevokeStatement =
        pKeyword "REVOKE"
        >>. choice
                [ attempt (
                      opt (attempt pRevokeOptionExtension) .>>. pPrivileges .>> pKeyword "ON"
                      .>>. pObjectName
                      .>> pKeyword "FROM"
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      .>>. pDropBehavior
                      |>> fun (((((optOpt, privs), obj), grantees), _), cascade) ->
                          RevokeStatement.RevokePrivileges(
                              privs,
                              obj,
                              grantees,
                              Option.defaultValue NoOption optOpt,
                              cascade
                          )
                  )
                  // 12.7 <revoke role statement> ::= REVOKE [ ADMIN OPTION FOR ] <role revoked>
                  //     [ { , <role revoked> }... ] FROM <grantee> [ { , <grantee> }... ]
                  //     [ GRANTED BY <grantor> ] <drop behavior>
                  attempt (
                      opt (attempt (pKeyword "ADMIN" >>. pKeyword "OPTION" >>. pKeyword "FOR" >>% true))
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>> pKeyword "FROM"
                      .>>. sepBy1 pIdentifierExpr (token (pstring ","))
                      .>>. opt (attempt (pKeyword "GRANTED" >>. pKeyword "BY" >>. pGrantor))
                      .>>. pDropBehavior
                      |>> fun ((((adminFor, roles), grantees), _), cascade) ->
                          RevokeStatement.RevokeRoles(roles, grantees, Option.defaultValue false adminFor, cascade)
                  ) ]
        |>> Revoke

    // 11.71 <transforms to be dropped> ::= ALL | <transform group element>
    let pTransformsToBeDropped =
        pKeyword "ALL" >>% TransformDropTarget.AllTransforms
        <|> (pQualifiedNameExpr |>> TransformDropTarget.TransformGroup)

    // 11.31 <drop table statement> ::= DROP TABLE <table name> <drop behavior>
    // 11.33 <drop view statement> ::= DROP VIEW <table name> <drop behavior>
    // 11.74 <drop sequence generator statement> ::= DROP SEQUENCE <sequence generator name> <drop behavior>
    // 12.6 <drop role statement> ::= DROP ROLE <role name>
    // 11.2 <drop schema statement> ::= DROP SCHEMA <schema name> <drop behavior>
    // 11.40 <drop domain statement> ::= DROP DOMAIN <domain name> <drop behavior>
    // 11.44 <drop collation statement> ::= DROP COLLATION <collation name> <drop behavior>
    // 11.42 <drop character set statement> ::= DROP CHARACTER SET <character set name>
    // 11.46 <drop transliteration statement> ::= DROP TRANSLATION <transliteration name>
    // 11.48 <drop assertion statement> ::= DROP ASSERTION <constraint name> [ <drop behavior> ]
    // 11.64 <drop user-defined cast statement> ::= DROP CAST (<source data type> AS <target data type>) <drop behavior>
    // 11.66 <drop user-defined ordering statement> ::= DROP ORDERING FOR <schema-resolved user-defined type name> <drop behavior>
    // 11.71 <drop transform statement> ::= DROP { TRANSFORM | TRANSFORMS } <transforms to be dropped> FOR <schema-resolved user-defined type name> <drop behavior>
    // 11.50 <drop trigger statement> ::= DROP TRIGGER <trigger name>
    // 11.62 <drop routine statement> ::= DROP <specific routine designator> <drop behavior>
    // 11.59 <drop data type statement> ::= DROP TYPE <schema-resolved user-defined type name> <drop behavior>
    let pDropStatement =
        pKeyword "DROP"
        >>. choice
                [ attempt (pKeyword "TABLE" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropTable
                  attempt (pKeyword "VIEW" >>. pQualifiedNameExpr .>>. pDropBehavior) |>> DropView
                  attempt (pKeyword "SEQUENCE" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropSequence
                  attempt (pKeyword "ROLE" >>. pIdentifierExpr) |>> DropStatement.DropRole
                  attempt (pKeyword "SCHEMA" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropSchema
                  attempt (pKeyword "DOMAIN" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropDomain
                  attempt (pKeyword "COLLATION" >>. pQualifiedNameExpr .>>. pDropBehavior)
                  |>> DropCollation
                  attempt (pKeyword "CHARACTER" >>. pKeyword "SET" >>. pQualifiedNameExpr)
                  |>> DropCharacterSet
                  attempt (pKeyword "TRANSLATION" >>. pQualifiedNameExpr) |>> DropTransliteration
                  attempt (pKeyword "ASSERTION" >>. pQualifiedNameExpr .>>. opt pDropBehavior)
                  |>> DropAssertion
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
                  // 11.62 <drop routine statement> ::= DROP <specific routine designator> <drop behavior>
                  attempt (pKeyword "TRIGGER" >>. pQualifiedNameExpr) |>> DropTrigger
                  attempt (pRoutineDesignatorWithType .>>. pDropBehavior) |>> DropRoutine
                  // 11.59 <drop data type statement> ::= DROP TYPE <name> <drop behavior>
                  attempt (pKeyword "TYPE" >>. pQualifiedNameExpr .>>. pDropBehavior) |>> DropType ]
        |>> Drop

    // 11.27 <add system time period column list>
    //     ::= ADD [ COLUMN ] <column definition 1> ADD [ COLUMN ] <column definition 2>
    // Both columns are required by the grammar, so this parser yields exactly two entries.
    let pAddSystemTimePeriodColumnList =
        (pKeyword "ADD" >>. opt (pKeyword "COLUMN") >>. pColumnDefinition)
        .>>. (pKeyword "ADD" >>. opt (pKeyword "COLUMN") >>. pColumnDefinition)
        |>> fun (first, second) -> [ first; second ]

    // 11.10 <alter table statement> ::= ALTER TABLE <table name> <alter table action>
    let pAlterTableStatement =
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
        let pAlterIdentityColumnSpecification: Parser<AlterIdentityColumnSpecification, unit> =
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

    // 14.10 <truncate table statement> ::= TRUNCATE TABLE <target table> [ <identity column restart option> ]
    let pTruncateStatement =
        pKeyword "TRUNCATE" >>. opt (pKeyword "TABLE") >>. pQualifiedNameExpr
        .>>. opt (
            pKeyword "RESTART" >>. pKeyword "IDENTITY" >>% true
            <|> (pKeyword "CONTINUE" >>. pKeyword "IDENTITY" >>% false)
        )
        |>> fun (table, restart) -> Truncate(table, restart)

    // Forward reference to the full DDL statement set; a <schema element> is any
    // DDL statement. Wired to pDdl in SqlParser.fs (which also contains the
    // CREATE SCHEMA parser that consumes these elements).
    // 11.1 <schema element> ::= <table definition> | <view definition> | <domain definition> | ...
    let pSchemaElement, pSchemaElementImpl =
        createParserForwardedToRef<StatementKind, unit> ()

    // 11.1 <schema definition> ::= CREATE SCHEMA <schema name clause> [ <schema character set or path> ] [ <schema element>... ]
    let pCreateSchemaStatement =
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

    // 11.70 <transform kind> ::= TO SQL | FROM SQL
    let pTransformKind =
        pKeyword "TO" >>. pKeyword "SQL" >>% TransformKind.ToSqlKind
        <|> (pKeyword "FROM" >>. pKeyword "SQL" >>% TransformKind.FromSqlKind)

    // 11.68 <alter transform action> ::= ADD <transform element list> | DROP ( <transform kind list> ) <drop behavior>
    let pAlterTransformAction =
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
