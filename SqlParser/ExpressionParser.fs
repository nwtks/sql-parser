namespace SqlParser

open FParsec
open SqlParser.Lexer

module ExpressionParser =
    // 6.1 <data type> — forward ref (wired below in this module; used by CAST / JSON returning)
    let pDataType, pDataTypeRef = createParserForwardedToRef<DataType, unit> ()
    // 6.28 <value expression> without boolean operators — forward ref (wired to
    // opp.ExpressionParser after the operator-precedence parser is built below).
    // Used where the grammar requires a non-boolean <value expression> (JSON slots,
    // <point in time>, etc.).
    let pNonBooleanValueExpression, pValueExpressionNoBooleanRef =
        createParserForwardedToRef<Expression, unit> ()

    // 6.29 <numeric value expression> — forward ref (wired after pValueExpressionPrimary is defined
    // to break the cycle: pValueExpressionPrimaryImpl → pArrayElementReference → pNumericValueExpression
    // → pValueExpressionPrimary → pValueExpressionPrimaryImpl).
    let pNumericValueExpression, pNumericValueExpressionRef =
        createParserForwardedToRef<Expression, unit> ()

    // 6.35 <datetime value expression> — forward ref. Defined after pTimeZoneSuffix,
    // but needed by the 6.37 <interval value expression> 4th alternative.
    let pDatetimeValueExpression, pDatetimeValueExpressionRef =
        createParserForwardedToRef<Expression, unit> ()

    // 6.39 <boolean value expression> / 6.28 <value expression> — forward ref (central expression parser)
    let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit> ()

    // 6.43 <multiset value expression> / 6.44 <multiset set function> — forward ref.
    // Defined after pValueExpressionPrimary, but needed by the 6.44 SET (...) parser
    // (which is itself a <value expression primary>), hence the indirection.
    let pMultisetValueExpression, pMultisetValueExpressionRef =
        createParserForwardedToRef<Expression, unit> ()

    // 7.17 <query expression> — forward ref (defined in QueryParser; used by scalar/quantified subqueries)
    let pQuery, pQueryRef = createParserForwardedToRef<Query, unit> ()

    // 8 Predicates — forward refs (defined in PredicateParser.fs, compiled after QueryParser.fs;
    // wired in SqlParser.fs). §6.3 <value expression primary> and §6.39 <boolean test> consume
    // them, so they must be referenced before PredicateParser is compiled.
    // 8.1 <predicate> — the postfix predicate suffix (Expression -> Expression)
    let pPredicate, pPredicateRef =
        createParserForwardedToRef<Expression -> Expression, unit> ()
    // 8.9 ANY/SOME/ALL (subquery) / 8.10 EXISTS / 8.11 UNIQUE / 8.20 PERIOD () / 8.23
    // JSON_EXISTS — the §8 parsers that are also <value expression primary> alternatives,
    // bundled into one parser.
    let pPredicatePrimary, pPredicatePrimaryRef =
        createParserForwardedToRef<Expression, unit> ()

    // 6.3 <value expression primary> helper — attaches source position to an ExpressionKind
    let withExprPosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Expression.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 5.3 <literal> ::= NULL | <character string literal> | <numeric literal>
    //     | <boolean literal> | <datetime literal> | <interval literal> | <hex string literal>
    let pLiteralExpression = pLiteral |> withExprPosition

    // 5.4 <schema qualified name>
    let pSchemaQualifiedNameExpression =
        pSchemaQualifiedName
        |>> function
            | [ s ] -> Identifier s
            | parts -> ColumnReference parts
        |> withExprPosition

    // 6.1 <character string type> ::= CHARACTER [ ( <character length> ) ] | CHAR [ ( <length> ) ] | CHARACTER VARYING ( <length> ) | VARCHAR ( <length> ) | <character large object type>
    let pCharacterStringType =
        choice
            [ attempt (pKeyword "CHARACTER" .>> pKeyword "VARYING") >>% Varchar
              attempt (pKeyword "CHAR" .>> pKeyword "VARYING") >>% Varchar
              pKeyword "VARCHAR" >>% Varchar
              attempt (pKeyword "CHARACTER" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
              >>% CharacterLargeObject
              attempt (pKeyword "CHAR" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
              >>% CharacterLargeObject
              pKeyword "CLOB" >>% CharacterLargeObject
              pKeyword "CHARACTER" >>% Character
              pKeyword "CHAR" >>% Character ]
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)
        |>> fun (typ, len) -> typ len

    // 6.1 <national character string type> ::= NATIONAL CHARACTER [ ( <character length> ) ] | NCHAR [ ... ] | NATIONAL CHARACTER VARYING ... | <national character large object type>
    let pNationalCharacterStringType =
        choice
            [ attempt (pKeyword "NATIONAL" .>> pKeyword "CHARACTER" .>> pKeyword "VARYING")
              >>% NationalVarchar
              attempt (pKeyword "NATIONAL" .>> pKeyword "CHAR" .>> pKeyword "VARYING")
              >>% NationalVarchar
              attempt (pKeyword "NCHAR" .>> pKeyword "VARYING") >>% NationalVarchar
              attempt (
                  pKeyword "NATIONAL"
                  .>> pKeyword "CHARACTER"
                  .>> pKeyword "LARGE"
                  .>> pKeyword "OBJECT"
              )
              >>% NationalCharacterLargeObject
              attempt (pKeyword "NCHAR" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
              >>% NationalCharacterLargeObject
              pKeyword "NCLOB" >>% NationalCharacterLargeObject
              attempt (pKeyword "NATIONAL" .>> pKeyword "CHARACTER") >>% NationalCharacter
              attempt (pKeyword "NATIONAL" .>> pKeyword "CHAR") >>% NationalCharacter
              pKeyword "NCHAR" >>% NationalCharacter ]
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)
        |>> fun (typ, len) -> typ len

    // 6.1 <binary string type> ::= BINARY [ ( <length> ) ] | BINARY VARYING ( <length> ) | VARBINARY ( <length> ) | <binary large object string type>
    let pBinaryStringType =
        choice
            [ attempt (pKeyword "BINARY" .>> pKeyword "VARYING") >>% VarBinary
              pKeyword "VARBINARY" >>% VarBinary
              attempt (pKeyword "BINARY" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
              >>% BinaryLargeObject
              pKeyword "BLOB" >>% BinaryLargeObject
              pKeyword "BINARY" >>% Binary ]
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)
        |>> fun (typ, len) -> typ len

    // 6.1 <exact numeric type> ::= NUMERIC [ ( <precision> [ , <scale> ] ) ] | DECIMAL [ ... ] | DEC [ ... ] | SMALLINT | INTEGER | INT | BIGINT  —  <decimal floating-point type> ::= DECFLOAT [ ( <precision> ) ]
    let pNumericType =
        let pPrecScale =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (pUnsignedIntegerAsInt .>>. opt (token (pstring ",") >>. pUnsignedIntegerAsInt))
            |>> fun (p, s) -> Some p, s

        choice
            [ pKeyword "NUMERIC" >>. opt pPrecScale
              |>> fun ps ->
                  Numeric(
                      match ps with
                      | Some(p, s) -> p, s
                      | None -> None, None
                  )
              pKeyword "DECIMAL" >>. opt pPrecScale
              |>> fun ps ->
                  Decimal(
                      match ps with
                      | Some(p, s) -> p, s
                      | None -> None, None
                  )
              pKeyword "DEC" >>. opt pPrecScale
              |>> fun ps ->
                  Decimal(
                      match ps with
                      | Some(p, s) -> p, s
                      | None -> None, None
                  )
              pKeyword "DECFLOAT"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)
              |>> DecFloat
              pKeyword "SMALLINT" >>% SmallInt
              pKeyword "INTEGER" >>% Integer
              pKeyword "INT" >>% Integer
              pKeyword "BIGINT" >>% BigInt ]

    // 6.1 <approximate numeric type> ::= FLOAT [ ( <precision> ) ] | REAL | DOUBLE PRECISION
    let pApproximateNumericType =
        choice
            [ pKeyword "FLOAT"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)
              |>> Float
              pKeyword "REAL" >>% Real
              attempt (pKeyword "DOUBLE" .>> pKeyword "PRECISION") >>% DoublePrecision ]

    // 6.1 <datetime type> ::= DATE | TIME [ ( <time precision> ) ] [ <with or without time zone> ] | TIMESTAMP [ ( <timestamp precision> ) ] [ <with or without time zone> ]
    let pDateTimeType =
        let pTz =
            opt (pKeyword "WITH" <|> pKeyword "WITHOUT" .>> pKeyword "TIME" .>> pKeyword "ZONE")
            |>> function
                | Some "WITH" -> true
                | _ -> false

        choice
            [ pKeyword "DATE" >>% DateType
              pKeyword "TIME"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)
              .>>. pTz
              |>> fun (p, tz) -> TimeType(p, tz)
              pKeyword "TIMESTAMP"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)
              .>>. pTz
              |>> fun (p, tz) -> TimestampType(p, tz) ]

    // 6.1 <interval type> ::= INTERVAL <interval qualifier>
    let pIntervalType = pKeyword "INTERVAL" >>. pIntervalQualifier |>> IntervalType

    // 6.7 <column reference> / 5.4 <identifier> — <identifier> | <column reference>
    let pIdentifierExpression = pIdentifier |>> Identifier |> withExprPosition

    // 6.1 <row type> ::= ROW <row type body> — <row type body> ::= ( <field definition> [ { , <field definition> }... ] )
    let pRowType =
        pKeyword "ROW"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 (pIdentifierExpression .>>. pDataType) (token (pstring ",")))
        |>> RowType

    // 6.1 <scope clause> ::= SCOPE <table name>
    // Shared by <reference type> (6.1), <column option list> (11.3) and
    // <add column scope clause> (11.17); it lives here because ExpressionParser.fs is
    // compiled before SchemaParser.fs.
    let pScopeClause = pKeyword "SCOPE" >>. pSchemaQualifiedNameExpression

    // 6.1 <data type> — element type parser: all types EXCEPT collection types (to avoid
    // left recursion). The recursive REF / row-field positions use the `pDataType`
    // forward ref, so this parser can be defined before `pCollectionType`.
    let pDataTypeElement =
        choice
            [ attempt pCharacterStringType
              attempt pNationalCharacterStringType
              attempt pBinaryStringType
              attempt pNumericType
              attempt pApproximateNumericType
              pKeyword "BOOLEAN" >>% Boolean
              attempt pDateTimeType
              attempt pIntervalType
              attempt pRowType
              // 6.1 <reference type> ::= REF ( <referenced type> ) [ SCOPE <table name> ]
              // (REF is a reserved word, so it must be tried before the
              // <path-resolved user-defined type name> fallback below)
              attempt (
                  pKeyword "REF" >>. between (token (pstring "(")) (token (pstring ")")) pDataType
                  .>>. opt pScopeClause
                  |>> fun (t, scope) -> ReferenceType(t, scope)
              )
              // 6.1 <path-resolved user-defined type name> ::= [ <schema name> <period> ] <qualified identifier>
              // A UDT name is an identifier (optionally schema-qualified), but it must not be
              // followed by another identifier — that would indicate a misparse (e.g. the
              // NESTED PATH column form of JSON_TABLE, where NESTED would be read as a
              // column name and PATH as a UDT).
              attempt (
                  pSchemaQualifiedName .>>? notFollowedBy pIdentifier
                  >>= fun parts ->
                      getPosition
                      |>> fun pos ->
                          let expr =
                              match parts with
                              | [ s ] -> Identifier s
                              | ps -> ColumnReference ps

                          UserDefinedType
                              { Kind = expr
                                Pos = { Line = pos.Line; Column = pos.Column } }
              ) ]

    // 6.1 <predefined type> — the built-in alternatives of <data type>: no
    // <reference type> and no <path-resolved user-defined type name>. Used by the
    // slots the grammar restricts to <predefined type> (11.34 <domain definition>,
    // 11.51 <representation> / <user-defined representation>).
    let pPredefinedType =
        choice
            [ attempt pCharacterStringType
              attempt pNationalCharacterStringType
              attempt pBinaryStringType
              attempt pNumericType
              attempt pApproximateNumericType
              pKeyword "BOOLEAN" >>% Boolean
              attempt pDateTimeType
              attempt pIntervalType
              attempt pRowType ]

    // 6.1 <collection type> ::= <array type> | <multiset type> — <array type> ::= <data type> ARRAY [ [ <maximum cardinality> ] ] — <multiset type> ::= <data type> MULTISET
    // The suffixes are applied left-to-right and may nest (`INT ARRAY ARRAY` =
    // `ArrayType(ArrayType(Integer, None), None)`), because <data type> on the left of
    // ARRAY/MULTISET may itself be a collection type.
    let pArraySuffix =
        pKeyword "ARRAY"
        .>>. opt (between (token (pstring "[")) (token (pstring "]")) pUnsignedInteger)
        |>> fun (_, len) -> fun t -> ArrayType(t, Option.map int len)

    let pMultisetSuffix = pKeyword "MULTISET" >>% fun t -> MultisetType t

    let pCollectionSuffixes =
        many (choice [ pArraySuffix; pMultisetSuffix ])
        |>> fun suffixes t -> List.fold (fun acc f -> f acc) t suffixes

    let pCollectionType =
        pDataTypeElement .>>. pCollectionSuffixes |>> fun (t, fold) -> fold t

    // 6.1 <collection type> requiring at least ONE suffix — used by the 11.51
    // <representation> slot, where a bare UDT name is NOT a <predefined type> but
    // `<udt> ARRAY` still is a valid <collection type>.
    let pCollectionTypeStrict =
        pDataTypeElement .>>. many1 (choice [ pArraySuffix; pMultisetSuffix ])
        |>> fun (t, suffixes) -> List.fold (fun acc f -> f acc) t suffixes

    pDataTypeRef.Value <- choice [ attempt pCollectionType; pDataTypeElement ]

    // 6.4 <dynamic parameter specification> ::= <question mark>
    let pQuestionMark = pchar '?' .>> ws

    // Literals are handled separately by pLiteralExpression above.
    // 6.4 <general value specification> — parameter forms (`?` dynamic parameter, `:name` host parameter) plus the keyword forms (CURRENT_USER, SESSION_USER, ...).
    let pGeneralValueSpecification =
        choice
            [ pQuestionMark >>% "?" <|> pHostParameter |>> Parameter |> withExprPosition
              pKeyword "CURRENT_CATALOG" >>% CurrentCatalog |> withExprPosition
              pKeyword "CURRENT_DEFAULT_TRANSFORM_GROUP" >>% CurrentDefaultTransformGroup
              |> withExprPosition
              pKeyword "CURRENT_PATH" >>% CurrentPath |> withExprPosition
              pKeyword "CURRENT_ROLE" >>% ExpressionKind.CurrentRole |> withExprPosition
              pKeyword "CURRENT_SCHEMA" >>% CurrentSchema |> withExprPosition
              pKeyword "CURRENT_USER" >>% ExpressionKind.CurrentUser |> withExprPosition
              pKeyword "SESSION_USER" >>% SessionUser |> withExprPosition
              pKeyword "SYSTEM_USER" >>% SystemUser |> withExprPosition
              pKeyword "USER" >>% User |> withExprPosition
              pKeyword "VALUE" >>% Value |> withExprPosition
              pKeyword "CURRENT_TRANSFORM_GROUP_FOR_TYPE" >>. pSchemaQualifiedNameExpression
              |>> CurrentTransformGroupForType
              |> withExprPosition
              // <current collation specification> ::= COLLATION FOR ( <string value expression> )
              pKeyword "COLLATION"
              >>. pKeyword "FOR"
              >>. between (token (pstring "(")) (token (pstring ")")) pExpression
              |>> CollationFor
              |> withExprPosition ]

    // 6.4 <simple value specification> ::= <literal> | <host parameter name>
    //     | <SQL parameter reference> | <embedded variable name>
    // (<embedded variable name> is a host-language construct and is not modelled;
    //  it degrades to <host parameter name> — see docs/trade-off.md.)
    // 5.3 <literal> also admits <signed numeric literal>, which pLiteral does not cover.
    let pSimpleValueSpecification =
        choice
            [ pLiteralExpression
              pSignedNumericLiteral |>> Number |>> Literal |> withExprPosition
              pQuestionMark >>% "?" <|> pHostParameter |>> Parameter |> withExprPosition ]

    // 6.4 <value specification> ::= <literal> | <general value specification>
    // Used where the grammar requires a <value specification> (SET CATALOG/SCHEMA/NAMES/PATH,
    // SET SESSION AUTHORIZATION, SET ROLE, CONNECT TO, ALLOCATE DESCRIPTOR WITH MAX, etc.).
    // pLiteralExpression covers <unsigned literal> (unsigned numeric + general literals + datetime +
    // interval + boolean + binary); pGeneralValueSpecification covers host params and keyword forms.
    let pValueSpecification = choice [ pLiteralExpression; pGeneralValueSpecification ]

    // 6.5 <default specification> ::= DEFAULT — only valid in specific contexts (INSERT VALUES, UPDATE SET), not as a general expression. This parser is used by the DML parser for those contexts.
    let pDefaultSpecification: Parser<Expression, unit> =
        pKeyword "DEFAULT" >>% Default |> withExprPosition

    // A dotted identifier chain stops before '. <identifier> ( ... )' so that a
    // method invocation like a.b.method(x) parses as a column reference (a.b)
    // followed by a method-invocation postfix, while a.b.c (no parens) still
    // parses as a single ColumnReference.
    // 6.7 <column reference> ::= [ <table name> <period> ] <column name>
    let pColumnReferenceExpression =
        pIdentifier
        .>>. many (attempt (token (pstring ".") >>. pIdentifier .>>? notFollowedBy (token (pstring "("))))
        |>> function
            | id, [] -> Identifier id
            | first, rest -> ColumnReference(first :: rest)
        |> withExprPosition

    // 6.7 <column reference> [ <collate clause> ] — the shape shared by
    // 7.13 <grouping column reference>, 7.15 <window partition column reference> and
    // 7.7 <row pattern partition column>.
    let pColumnReferenceWithCollate =
        pColumnReferenceExpression
        .>>. opt (pKeyword "COLLATE" >>. pSchemaQualifiedNameExpression)
        |>> fun (e, collation) ->
            match collation with
            | Some c ->
                { Expression.Kind = Collate(e, c)
                  Pos = e.Pos }
            | None -> e

    // 5.4 <local qualified name> ::= [ <local qualifier> <period> ] <qualified identifier>
    // 14.1 <cursor name> ::= <local qualified name>; <local qualifier> ::= MODULE is the only
    // qualifier a cursor name admits, so `DECLARE a.b CURSOR ...` is rejected.
    let pLocalQualifiedNameExpression =
        getPosition
        .>>. opt (attempt (pKeyword "MODULE" >>. token (pstring ".")))
        .>>. pIdentifier
        |>> fun ((pos, qualifier), name) ->
            { Expression.Kind =
                (match qualifier with
                 | Some _ -> ColumnReference [ "MODULE"; name ]
                 | None -> Identifier name)
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 20.4/23.1 <simple target specification> ::= <host parameter name> | <SQL parameter reference>
    //     | <column reference> | <embedded variable name>
    // (<embedded variable name> is a host-language construct and degrades to <host parameter name>,
    //  as elsewhere — see docs/trade-off.md.)
    let pSimpleTargetSpecification =
        choice
            [ pColumnReferenceExpression
              getPosition .>>. (pQuestionMark >>% "?" <|> pHostParameter)
              |>> fun (pos, name) ->
                  { Expression.Kind = Parameter name
                    Pos = { Line = pos.Line; Column = pos.Column } } ]

    // 6.9 <grouping operation> ::= GROUPING ( <column reference> [ , <column reference> ]... )
    // Plain column references — no <collate clause> slot.
    let pGroupingOperation =
        pKeyword "GROUPING"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 pColumnReferenceExpression (token (pstring ",")))
        |>> Grouping
        |> withExprPosition

    // 6.9 / 6.26 <running or final> ::= RUNNING | FINAL
    let pRunningOrFinal =
        choice
            [ pKeyword "RUNNING" >>% RunningOrFinal.Running
              pKeyword "FINAL" >>% RunningOrFinal.Final ]

    // 6.11 <row marker delta> / 7.9 <row pattern quantifier> — unsigned integer as <value expression> (row marker offsets / quantifier bounds)
    let pUnsignedIntegerExpr: Parser<Expression, unit> =
        getPosition .>>. token pUnsignedInteger
        |>> fun (pos, n) ->
            { Kind = Literal(Number(decimal n))
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 7.8 <row pattern measures> / 7.9 <row pattern common syntax> — defined before the
    // 7.15 <window frame clause> that consumes them, because they are also used by
    // QueryParser's MATCH_RECOGNIZE. They depend only on `pExpression` (forward ref) and
    // Lexer terminals.

    // 7.8 <row pattern measures> ::= MEASURES <row pattern measure list>
    let pRowPatternMeasures =
        // 7.8 <row pattern measure definition> ::= <row pattern measure expression> AS <measure name>
        let pRowPatternMeasure =
            pExpression .>> pKeyword "AS" .>>. pIdentifierExpression
            |>> fun (expr, name) ->
                { RowPatternMeasure.Expression = expr
                  Name = name }

        pKeyword "MEASURES" >>. sepBy1 pRowPatternMeasure (token (pstring ","))

    // 7.9 <row pattern> — forward ref (recursive)
    let pRowPattern, pRowPatternRef = createParserForwardedToRef<RowPattern, unit> ()

    // 7.9 <row pattern term> ::= <row pattern factor> | <row pattern term> <row pattern factor>
    let pRowPatternTerm =
        // 7.9 <row pattern quantifier>
        let pRowPatternQuantifier =
            choice
                [ attempt (
                      token (pstring "*") >>. opt (token (pstring "?"))
                      |>> fun q -> RowPatternQuantifier.Star(Option.isSome q)
                  )
                  attempt (
                      token (pstring "+") >>. opt (token (pstring "?"))
                      |>> fun q -> RowPatternQuantifier.Plus(Option.isSome q)
                  )
                  attempt (
                      token (pstring "?") >>. opt (token (pstring "?"))
                      |>> fun q -> RowPatternQuantifier.Question(Option.isSome q)
                  )
                  attempt (
                      between
                          (token pLeftBrace)
                          (token pRightBrace)
                          (opt pUnsignedIntegerExpr .>> token (pstring ",") .>>. opt pUnsignedIntegerExpr)
                      .>>. opt (token (pstring "?"))
                      |>> fun ((lo, hi), q) -> RowPatternQuantifier.Brace(lo, hi, Option.isSome q)
                  )
                  attempt (
                      between (token pLeftBrace) (token pRightBrace) pUnsignedIntegerExpr
                      |>> RowPatternQuantifier.BraceExact
                  ) ]

        // 7.9 <row pattern primary>
        let pRowPatternPrimary =
            choice
                [ attempt (
                      token pLeftBraceMinus >>. pRowPattern .>> token pRightMinusBrace
                      |>> RowPatternExclude
                  )
                  attempt (token pCircumflex >>% RowPatternAnchorStart)
                  attempt (token pDollarSign >>% RowPatternAnchorEnd)
                  attempt (
                      pKeyword "PERMUTE"
                      >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pRowPattern (token (pstring ",")))
                      |>> RowPatternPermute
                  )
                  attempt (
                      between (token (pstring "(")) (token (pstring ")")) (opt pRowPattern)
                      |>> RowPatternGroup
                  )
                  pIdentifierExpression |>> RowPatternVariable ]

        // 7.9 <row pattern factor> ::= <row pattern primary> [ <row pattern quantifier> ]
        let pRowPatternFactor =
            pRowPatternPrimary .>>. opt (attempt pRowPatternQuantifier)
            |>> fun (primary, quant) ->
                { Primary = primary
                  Quantifier = quant }

        many1 pRowPatternFactor |>> fun factors -> { Factors = factors }

    // 7.9 <row pattern> ::= <row pattern term> | <row pattern alternation>
    pRowPatternRef.Value <- sepBy1 pRowPatternTerm (token pVerticalBar) |>> fun terms -> { Terms = terms }

    // 7.9 <row pattern common syntax> ::= [ AFTER MATCH <skip to> ] [ INITIAL | SEEK ]
    //     PATTERN ( <row pattern> ) [ <subset clause> ] DEFINE <definition list>
    let pRowPatternCommon =
        // 7.9 <row pattern skip to>
        let pRowPatternSkipTo =
            pKeyword "SKIP"
            >>. pKeyword "TO"
            >>. choice
                    [ attempt (pKeyword "NEXT" >>. pKeyword "ROW" >>% SkipToNextRow)
                      attempt (pKeyword "PAST" >>. pKeyword "LAST" >>. pKeyword "ROW" >>% SkipPastLastRow)
                      attempt (pKeyword "FIRST" >>. pIdentifierExpression |>> SkipToFirst)
                      attempt (pKeyword "LAST" >>. pIdentifierExpression |>> SkipToLast)
                      pIdentifierExpression |>> SkipTo ]

        // 7.9 <row pattern subset item> ::= <var> = ( <var> [ , <var> ]... )
        let pRowPatternSubset =
            pIdentifierExpression .>> token (pstring "=")
            .>>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 pIdentifierExpression (token (pstring ",")))
            |>> fun (name, vars) -> { Name = name; Variables = vars }

        // 7.9 <row pattern definition> ::= <var> AS <search condition>
        let pRowPatternDefinition =
            pIdentifierExpression .>> pKeyword "AS" .>>. pExpression
            |>> fun (name, cond) -> { Name = name; Condition = cond }

        opt (attempt (pKeyword "AFTER" >>. pKeyword "MATCH" >>. pRowPatternSkipTo))
        .>>. opt (attempt (pKeyword "INITIAL" >>% true <|> (pKeyword "SEEK" >>% false)))
        .>> pKeyword "PATTERN"
        .>>. between (token (pstring "(")) (token (pstring ")")) pRowPattern
        .>>. opt (attempt (pKeyword "SUBSET" >>. sepBy1 pRowPatternSubset (token (pstring ","))))
        .>> pKeyword "DEFINE"
        .>>. sepBy1 pRowPatternDefinition (token (pstring ","))
        |>> fun ((((after, ios), pattern), subset), define) ->
            { AfterMatch = after
              InitialOrSeek = ios
              Pattern = pattern
              Subset = Option.defaultValue [] subset
              Define = define }

    // 7.15 <window frame clause> — consumed by the 6.10 <window name or specification>
    // below (define-before-use): F#'s `let` bindings are not recursive, so the 6.10 body
    // references this binding directly. Its optional leading <row pattern measures> and
    // trailing <row pattern common syntax> (7.8/7.9) are defined immediately above.
    // 7.15 <window frame clause> ::= [ <row pattern measures> ] <window frame units> <window frame extent> [ <window frame exclusion> ] [ <row pattern common syntax> ]
    let pWindowFrameClause =
        // 7.15 <window frame units> ::= ROWS | RANGE | GROUPS
        let pUnit =
            pKeyword "ROWS" >>% Rows
            <|> (pKeyword "RANGE" >>% Range)
            <|> (pKeyword "GROUPS" >>% Groups)

        // 7.15 <window frame bound> ::= UNBOUNDED PRECEDING | UNBOUNDED FOLLOWING | CURRENT ROW | <unsigned value specification> PRECEDING | <unsigned value specification> FOLLOWING
        // <unsigned value specification> = <unsigned literal> | <general value specification>; pValueSpecification models this exactly.
        let pBound =
            choice
                [ attempt (pKeyword "UNBOUNDED" >>. pKeyword "PRECEDING" >>% UnboundedPreceding)
                  attempt (pKeyword "UNBOUNDED" >>. pKeyword "FOLLOWING" >>% UnboundedFollowing)
                  attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% CurrentRow)
                  attempt (pValueSpecification .>> pKeyword "PRECEDING" |>> Preceding)
                  attempt (pValueSpecification .>> pKeyword "FOLLOWING" |>> Following) ]

        // 7.15 <window frame start> ::= UNBOUNDED PRECEDING | <window frame preceding> | CURRENT ROW
        // — a frame that is not a BETWEEN (and the first bound of one) cannot start at FOLLOWING.
        let pBoundStart =
            choice
                [ attempt (pKeyword "UNBOUNDED" >>. pKeyword "PRECEDING" >>% UnboundedPreceding)
                  attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% CurrentRow)
                  attempt (pValueSpecification .>> pKeyword "PRECEDING" |>> Preceding) ]

        // 7.15 <window frame bound 1> ::= <window frame start> | UNBOUNDED FOLLOWING
        let pBound1 =
            pBoundStart
            <|> (attempt (pKeyword "UNBOUNDED" >>. pKeyword "FOLLOWING") >>% UnboundedFollowing)

        // 7.15 <window frame exclusion> ::= EXCLUDE CURRENT ROW | EXCLUDE GROUP | EXCLUDE TIES | EXCLUDE NO OTHERS
        let pExclusion =
            pKeyword "EXCLUDE"
            >>. choice
                    [ attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% ExcludeCurrentRow)
                      attempt (pKeyword "GROUP" >>% ExcludeGroup)
                      attempt (pKeyword "TIES" >>% ExcludeTies)
                      attempt (pKeyword "NO" >>. pKeyword "OTHERS" >>% ExcludeNoOthers) ]

        // 7.15 <window frame clause> ::= [ <row pattern measures> ] <units> <extent>
        //     [ <exclusion> ] [ <row pattern common syntax> ]
        opt (attempt pRowPatternMeasures)
        .>>. pUnit
        .>>. choice
            [ attempt (
                  pKeyword "BETWEEN" >>. pBound1 .>> pKeyword "AND" .>>. pBound
                  |>> fun (s, e) -> s, Some e
              )
              pBoundStart |>> fun s -> s, None ]
        .>>. opt pExclusion
        .>>. opt (attempt pRowPatternCommon)
        |>> fun ((((measures, unit), (start, endBound)), exclusion), rowPattern) ->
            { Unit = unit
              Start = start
              End = endBound
              Exclusion = exclusion
              Measures = measures
              RowPattern = rowPattern }

    // 10.10 <sort specification> — same non-recursive-`let` constraint as 7.15 above:
    // referenced by the 6.10 body and by the later 10.4 / 10.11 parsers, so it must be
    // defined before them.
    // 10.10 <sort specification> ::= <sort key> [ <ordering specification> ] [ <null ordering> ]
    // 10.4 <ordering specification> ::= ASC | DESC — 10.10 <null ordering> ::= NULLS FIRST | NULLS LAST
    let pSortSpecification =
        let pNullsOrder =
            pKeyword "NULLS"
            >>. (pKeyword "FIRST" >>% NullsFirst <|> (pKeyword "LAST" >>% NullsLast))

        pExpression
        .>>. opt (attempt (pKeyword "ASC" >>% true) <|> attempt (pKeyword "DESC" >>% false))
        .>>. opt (attempt pNullsOrder)
        |>> fun ((expr, asc), nulls) -> expr, Option.defaultValue true asc, nulls

    // — the OVER (...) clause attached to a window function (also parsed at the query level for the WINDOW clause).
    // 6.10 <window name or specification> ::= <window name> | <window specification>
    let pWindowNameOrSpecification =
        let pPartitionBy =
            // 7.15 <window partition column reference> ::= <column reference> [ <collate clause> ]
            pKeyword "PARTITION"
            >>. pKeyword "BY"
            >>. sepBy1 pColumnReferenceWithCollate (token (pstring ","))

        let pOrderBy =
            pKeyword "ORDER"
            >>. pKeyword "BY"
            >>. sepBy1 pSortSpecification (token (pstring ","))

        // 7.15 — an <existing window name> cannot be MEASURES when MEASURES starts a
        // <row pattern measures> clause (the optional leading clause of the
        // <window frame clause>). Reject MEASURES followed by <expr> AS <name> so the
        // frame's MEASURES clause is not swallowed as a window name. The whole parser
        // is wrapped in attempt because notFollowedBy marks its failure as fatal, which
        // would otherwise escape the surrounding opt.
        let pExistingWindowName =
            attempt (
                pIdentifierExpression
                >>= fun name ->
                    match name.Kind with
                    | Identifier "MEASURES" -> notFollowedBy (pExpression .>> pKeyword "AS") >>% name
                    | _ -> preturn name
            )

        pKeyword "OVER"
        >>. (between
                 (token (pstring "("))
                 (token (pstring ")"))
                 (opt pExistingWindowName
                  .>>. opt pPartitionBy
                  .>>. opt pOrderBy
                  .>>. opt pWindowFrameClause
                  |>> fun (((name, pb), ob), frame) ->
                      { ExistingWindowName = name
                        PartitionBy = Option.defaultValue [] pb
                        OrderBy = Option.defaultValue [] ob
                        Frame = frame })
             <|> (pIdentifierExpression
                  |>> fun name ->
                      { ExistingWindowName = Some name
                        PartitionBy = []
                        OrderBy = []
                        Frame = None }))

    // 6.11 <row marker> ::= BEGIN_PARTITION | BEGIN_FRAME | CURRENT_ROW | FRAME_ROW | END_FRAME | END_PARTITION
    let pRowMarker =
        choice
            [ pKeyword "BEGIN_PARTITION" >>% RowMarker.BeginPartition
              pKeyword "BEGIN_FRAME" >>% RowMarker.BeginFrame
              pKeyword "CURRENT_ROW" >>% RowMarker.CurrentRow
              pKeyword "FRAME_ROW" >>% RowMarker.FrameRow
              pKeyword "END_FRAME" >>% RowMarker.EndFrame
              pKeyword "END_PARTITION" >>% RowMarker.EndPartition ]

    // 6.11 <row marker expression> ::= <row marker> [ <row marker delta> ]
    let pRowMarkerExpression =
        pRowMarker
        .>>. opt (
            attempt (token (pstring "+") >>% true <|> (token (pstring "-") >>% false))
            .>>. pUnsignedIntegerExpr
        )
        |>> fun (marker, delta) ->
            { Marker = marker
              Delta = delta |> Option.map (fun (plus, n) -> plus, n) }

    // 6.11 <nested row number function> ::= ROW_NUMBER ( <row marker> )
    let pNestedRowNumberFunction =
        pKeyword "ROW_NUMBER"
        >>. between (token (pstring "(")) (token (pstring ")")) pRowMarker
        |>> NestedRowNumber
        |> withExprPosition

    // 6.11 <value_of expression at row> ::= VALUE_OF ( <value expression> AT <row marker expression>
    //     [ , <value_of default value> ] )
    let pValueOfExpressionAtRow =
        pKeyword "VALUE_OF"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "AT"
                 .>>. pRowMarkerExpression
                 .>>. opt (attempt (token (pstring ",") >>. pExpression)))
        |>> fun ((expr, marker), defaultVal) -> ValueOf(expr, marker, defaultVal)
        |> withExprPosition

    // 6.12 <case abbreviation> ::= NULLIF ( <value expression> , <value expression> )
    let pNullifExpr =
        pKeyword "NULLIF"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> token (pstring ",") .>>. pExpression)
        |>> fun (e1, e2) ->
            Case(
                None,
                [ ({ Kind = BinaryOp(Equal, e1, e2)
                     Pos = e1.Pos },
                   { Kind = Literal Null; Pos = e2.Pos }) ],
                Some e1
            )
        |> withExprPosition

    // 6.12 <case abbreviation> ::= COALESCE ( <value expression> { <comma> <value expression> }... )
    // At least TWO arguments.
    let pCoalesceExpr =
        pKeyword "COALESCE"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>>. many1 (token (pstring ",") >>. pExpression)
                 |>> fun (first, rest) -> first :: rest)
        |>> fun exprs -> Case(None, exprs |> List.map (fun e -> { Kind = IsNull(e, true); Pos = e.Pos }, e), None)
        |> withExprPosition

    // 6.12 <case expression> ::= CASE <case operand> <simple when clause>...
    //     | CASE <searched when clause>... [ ELSE <result> ] END
    // A <when operand> is a <row value predicand> or a predicate part 2 — a TOP-LEVEL
    // boolean-producing expression (comparison, AND/OR/NOT, another predicate) is
    // rejected; a parenthesized form is a <value expression primary> and stays legal.
    let isBooleanTopLevel (e: Expression) =
        match e.Kind with
        | UnaryOp(op, _) when op = UnaryOperator.Not -> true
        | BinaryOp(op, _, _) ->
            op = BinaryOperator.And
            || op = BinaryOperator.Or
            || op = BinaryOperator.Equal
            || op = BinaryOperator.NotEqual
            || op = BinaryOperator.LessThan
            || op = BinaryOperator.LessThanOrEqual
            || op = BinaryOperator.GreaterThan
            || op = BinaryOperator.GreaterThanOrEqual
        | IsNull _
        | IsBoolean _
        | ExpressionKind.Between _
        | InList _
        | InSubquery _
        | Like _
        | SimilarTo _
        | RegexLike _
        | QuantifiedComparison _
        | Exists _
        | ExpressionKind.Unique _
        | IsNormalized _
        | Match _
        | Overlaps _
        | IsDistinctFrom _
        | MemberOf _
        | SubmultisetOf _
        | IsSet _
        | IsOfType _
        | PeriodPredicate _
        | IsJson _ -> true
        | _ -> false

    let pCaseExpression =
        getPosition
        >>= fun pos ->
            let pResultExpr =
                pExpression
                <|> (pKeyword "NULL"
                     >>% { Kind = Literal Null
                           Pos = { Line = pos.Line; Column = pos.Column } })

            let pWhenOperand =
                pExpression
                >>= fun e ->
                    if isBooleanTopLevel e then
                        fail "a <when operand> must be a <row value predicand> (6.12)"
                    else
                        preturn e

            let pSimpleWhenClause =
                pKeyword "WHEN" >>. sepBy1 pWhenOperand (token (pstring ","))
                .>> pKeyword "THEN"
                .>>. pResultExpr

            let pSearchedWhenClause =
                pKeyword "WHEN" >>. pExpression .>> pKeyword "THEN" .>>. pResultExpr

            pKeyword "CASE"
            >>. choice
                    [ attempt (pExpression .>>. many1 pSimpleWhenClause)
                      |>> fun (op, whens) ->
                          let flattened =
                              whens |> List.collect (fun (vals, res) -> vals |> List.map (fun v -> v, res))

                          Case(Some op, flattened, None)
                      many1 pSearchedWhenClause |>> fun whens -> Case(None, whens, None) ]
            .>>. opt (pKeyword "ELSE" >>. pResultExpr)
            .>> pKeyword "END"
            |>> fun (caseBase, els) ->
                match caseBase with
                | Case(op, whens, _) -> Case(op, whens, els)
                | kind -> kind
            |> withExprPosition

    // 6.13 <cast specification> ::= CAST ( <value expression> AS <data type> )
    let pCastSpecification =
        pKeyword "CAST"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        |>> Cast
        |> withExprPosition

    // 6.14 <next value expression> ::= NEXT VALUE FOR <sequence generator name>
    let pNextValueExpression =
        pKeyword "NEXT"
        >>. pKeyword "VALUE"
        >>. pKeyword "FOR"
        >>. pSchemaQualifiedNameExpression
        |>> NextValueFor
        |> withExprPosition

    // 6.16 <subtype treatment> ::= TREAT ( <subtype operand> AS <target subtype> )
    let pSubtypeTreatment =
        pKeyword "TREAT"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        |>> (fun (e, t) -> Treat(e, t))
        |> withExprPosition

    // 10.4 <SQL argument list> (plain — no DISTINCT/ALL; used by <method invocation>,
    // <static method invocation> and <new specification>)
    let pSqlArgumentList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy pExpression (token (pstring ",")))

    let pMethodOrFieldReference =
        // 6.32 <specific type method> ::= <user-defined type value expression> <period> SPECIFICTYPE [ ( ) ]
        // SPECIFICTYPE is a reserved word, so it cannot be reached through pIdentifierExpression below.
        // Local because pMethodOrFieldReference is its only consumer.
        let pSpecificTypeMethod =
            token (pstring ".")
            >>. pKeyword "SPECIFICTYPE"
            >>. opt (between (token (pstring "(")) (token (pstring ")")) (preturn true))
            |>> fun parens ->
                fun r ->
                    { Expression.Kind = SpecificTypeMethod(r, Option.isSome parens)
                      Pos = r.Pos }

        attempt pSpecificTypeMethod
        <|> (token (pstring ".") >>. pIdentifierExpression .>>. opt pSqlArgumentList
             |>> fun (name, args) ->
                 match args with
                 | Some a ->
                     fun r ->
                         { Expression.Kind = MethodInvocation(r, name, a)
                           Pos = r.Pos }
                 | None ->
                     fun r ->
                         { Expression.Kind = FieldReference(r, name)
                           Pos = r.Pos })

    // 6.17 <generalized invocation> ::= ( <value expression primary> AS <data type> )
    //     <period> <method name> [ <SQL argument list> ]
    let pGeneralizedInvocation =
        between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        .>>. (token (pstring ".") >>. pIdentifierExpression .>>. opt pSqlArgumentList)
        |>> fun ((operand, typ), (name, args)) -> GeneralizedInvocation(operand, typ, name, args)
        |> withExprPosition

    // 6.18 <static method invocation> ::= <path-resolved UDT name> :: <method name>
    //     [ <SQL argument list> ]
    let pStaticMethodInvocation =
        pSchemaQualifiedNameExpression
        .>>. (token (pstring "::") >>. pIdentifierExpression .>>. pSqlArgumentList)
        |>> fun (typ, (name, args)) -> StaticMethodInvocation(typ, name, args)
        |> withExprPosition

    // 6.19 <new specification> ::= NEW <path-resolved UDT name> <SQL argument list>
    let pNewSpecification =
        pKeyword "NEW" >>. pSchemaQualifiedNameExpression .>>. pSqlArgumentList
        |>> fun (typ, args) -> NewSpecification(typ, args)
        |> withExprPosition

    // 6.21 <dereference operation> ::= <value expression primary> <dereference operator>
    //     <qualified identifier> [ <SQL argument list> ]
    // <qualified identifier> ::= <identifier> — a SINGLE identifier, not a
    // schema-qualified name.
    let pDereferenceReference =
        token (pstring "->") >>. pIdentifierExpression .>>. opt pSqlArgumentList
        |>> fun (name, args) ->
            fun r ->
                { Expression.Kind = Dereference(r, name, args)
                  Pos = r.Pos }

    // 6.23 <reference resolution> ::= DEREF ( <reference value expression> )
    let pReferenceResolution =
        pKeyword "DEREF"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> Deref
        |> withExprPosition

    // 6.24 <array element reference> — postfix [ <numeric value expression> ]
    let pArrayElementReference =
        between (token pLeftBracket) (token pRightBracket) pNumericValueExpression
        |>> fun idx ->
            fun e ->
                { Expression.Kind = ArrayElement(e, idx)
                  Pos = e.Pos }

    // 6.25 <multiset element reference> ::= ELEMENT ( <multiset value expression> )
    let pMultisetElementReference =
        pKeyword "ELEMENT"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> Element
        |> withExprPosition

    // 6.26 <row pattern navigation operation> ::= <row pattern navigation: logical>
    //     | <row pattern navigation: physical> | <row pattern navigation: compound>
    let pRowPatternNavigation =
        let pFirstOrLast =
            choice [ pKeyword "FIRST" >>% FirstOrLast.First; pKeyword "LAST" >>% FirstOrLast.Last ]

        let pPrevOrNext =
            choice [ pKeyword "PREV" >>% PrevOrNext.Prev; pKeyword "NEXT" >>% PrevOrNext.Next ]

        let pOffset = opt (attempt (token (pstring ",") >>. pSimpleValueSpecification))

        // <row pattern navigation: logical> ::= [ <running or final> ] <first or last>
        //     ( <value expression> [ , <logical offset> ] )
        let pLogical =
            opt pRunningOrFinal
            .>>. pFirstOrLast
            .>>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>>. pOffset)
            |>> fun ((scope, firstOrLast), (e, offset)) -> Logical(scope, firstOrLast, e, offset)

        // <row pattern navigation: physical> ::= <prev or next> ( <value expression>
        //     [ , <physical offset> ] )
        let pPhysical =
            pPrevOrNext
            .>>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>>. pOffset)
            |>> fun (prevOrNext, (e, offset)) -> Physical(prevOrNext, e, offset)

        // <row pattern navigation: compound> ::= <prev or next> ( [ <running or final> ] <first or last>
        //     ( <value expression> [ , <logical offset> ] ) [ , <physical offset> ] )
        let pCompound =
            pPrevOrNext
            .>>. between
                (token (pstring "("))
                (token (pstring ")"))
                (opt pRunningOrFinal
                 .>>. pFirstOrLast
                 .>>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>>. pOffset)
                 .>>. pOffset)
            |>> fun (prevOrNext, (((scope, firstOrLast), (e, logical)), physical)) ->
                Compound(prevOrNext, scope, firstOrLast, e, logical, physical)

        choice [ attempt pCompound; attempt pLogical; attempt pPhysical ]

    // 6.26 <row pattern navigation operation> as a <value expression primary>
    let pRowPatternNavigationOperation =
        pRowPatternNavigation |>> RowPatternNavigation |> withExprPosition

    // 10.12 <JSON representation> ::= JSON [ ENCODING { UTF8 | UTF16 | UTF32 } ]
    let pJsonRepresentation =
        // 10.12 <JSON representation>
        let pJsonEncoding =
            choice
                [ pKeyword "UTF8" >>% Utf8
                  pKeyword "UTF16" >>% Utf16
                  pKeyword "UTF32" >>% Utf32 ]

        pKeyword "JSON" >>. opt (pKeyword "ENCODING" >>. pJsonEncoding) |>> JsonEncoding

    // 10.12 <JSON input clause> ::= FORMAT <JSON representation>
    let pJsonInputClause = pKeyword "FORMAT" >>. pJsonRepresentation

    // 10.13 <JSON output clause> ::= RETURNING <data type> [ FORMAT <JSON representation> ]
    let pJsonOutputClause =
        pKeyword "RETURNING" >>. pDataType
        .>>. opt (pKeyword "FORMAT" >>. pJsonRepresentation)
        |>> fun (ret, fmt) -> { Returning = ret; Format = fmt }

    // 10.14 <JSON passing argument> ::= <JSON value expression> [ <JSON input clause> ] AS <identifier>
    // <JSON value expression> is a value expression — boolean expressions are not allowed.
    let pJsonArgument =
        pNonBooleanValueExpression .>>. opt pJsonInputClause .>> pKeyword "AS"
        .>>. pIdentifierExpression
        |>> fun ((value, inputFormat), name) ->
            { JsonPassingArgument.Value = value
              InputFormat = inputFormat
              Name = name }

    // 10.14 <JSON API common syntax> ::= <JSON context item> , <JSON path specification>
    //     [ AS <JSON table path name> ] [ <JSON passing clause> ]
    // <JSON context item> ::= <JSON value expression> (value expression — no boolean ops,
    // plus an optional FORMAT clause)
    // <JSON path specification> ::= <character string literal> — stored as a plain string.
    let pJsonApiCommonSyntax =
        pNonBooleanValueExpression .>>. opt pJsonInputClause .>> token (pstring ",")
        .>>. pCharacterStringLiteral
        .>>. opt (attempt (pKeyword "AS" >>. pIdentifierExpression))
        .>>. opt (pKeyword "PASSING" >>. sepBy1 pJsonArgument (token (pstring ",")))
        |>> fun ((((context, contextFormat), path), pathName), passing) ->
            { Context = context
              ContextFormat = contextFormat
              Path = path
              PathName = pathName
              Passing = Option.defaultValue [] passing }

    // 6.27 <JSON value function> ::= JSON_VALUE ( <JSON API common syntax>
    //     [ <JSON returning clause> ] [ <JSON value empty behavior> ON EMPTY ]
    //     [ <JSON value error behavior> ON ERROR ] )
    let pJsonValueFunction =
        // 6.27 <JSON value empty/error behavior> ::= ERROR | NULL | DEFAULT <value expression>
        // <value expression> is not boolean, so boolean operators are rejected here.
        let pJsonValueBehavior =
            choice
                [ pKeyword "ERROR" >>% JsonError
                  pKeyword "NULL" >>% JsonNull
                  pKeyword "DEFAULT" >>. pNonBooleanValueExpression |>> JsonDefault ]

        pKeyword "JSON_VALUE"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pJsonApiCommonSyntax
                 .>>. opt (pKeyword "RETURNING" >>. pDataType)
                 .>>. opt (pJsonValueBehavior .>> pKeyword "ON" .>> pKeyword "EMPTY")
                 .>>. opt (pJsonValueBehavior .>> pKeyword "ON" .>> pKeyword "ERROR"))
        |>> fun (((common, returning), onEmpty), onError) -> JsonValue(common, returning, onEmpty, onError)
        |> withExprPosition

    // 6.30 <extract expression> ::= EXTRACT <left paren> <extract field> FROM <extract source> <right paren>
    // <extract field> ::= <primary datetime field> | <time zone field>
    // <primary datetime field> ::= <non-second primary datetime field> | SECOND
    // <time zone field> ::= TIMEZONE_HOUR | TIMEZONE_MINUTE
    let pExtractExpression =
        pKeyword "EXTRACT"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (getPosition
                 .>>. (choice
                     [ attempt (pKeyword "YEAR" >>% "YEAR")
                       attempt (pKeyword "MONTH" >>% "MONTH")
                       attempt (pKeyword "DAY" >>% "DAY")
                       attempt (pKeyword "HOUR" >>% "HOUR")
                       attempt (pKeyword "MINUTE" >>% "MINUTE")
                       attempt (pKeyword "SECOND" >>% "SECOND")
                       attempt (pKeyword "TIMEZONE_HOUR" >>% "TIMEZONE_HOUR")
                       attempt (pKeyword "TIMEZONE_MINUTE" >>% "TIMEZONE_MINUTE") ])
                 .>> pKeyword "FROM"
                 .>>. pExpression)
        |>> fun ((pos, field), src) ->
            Extract(
                { Kind = Identifier field
                  Pos = { Line = pos.Line; Column = pos.Column } },
                src
            )
        |> withExprPosition

    // 6.1 <char length units> ::= CHARACTERS | OCTETS
    // A closed set, so `USING <identifier>` is rejected instead of silently accepted.
    let pCharLengthUnits =
        pKeyword "CHARACTERS" >>% "CHARACTERS" <|> (pKeyword "OCTETS" >>% "OCTETS")

    // The same keyword set in the <position expression> slot, which models the units as an <identifier>.
    let pCharLengthUnitsExpr = pCharLengthUnits |>> Identifier |> withExprPosition

    // 6.30 <position expression> ::= POSITION ( <character value expression> IN
    //     <character value expression> [ USING <char length units> ] )
    // Operands are <character value expression>s (non-boolean). The <binary position
    // expression> form has no USING slot, but the two operand kinds are syntactically
    // indistinguishable, so one parser serves both (see docs/trade-off.md).
    let pPositionExpression =
        pKeyword "POSITION"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pNonBooleanValueExpression .>> pKeyword "IN"
                 .>>. pNonBooleanValueExpression
                 .>>. opt (pKeyword "USING" >>. pCharLengthUnitsExpr))
        |>> (fun ((target, source), unit) -> ExpressionKind.Position(target, source, unit))
        |> withExprPosition

    // 6.30 <length expression> ::= <char length expression> | <octet length expression>
    //   <char length expression> ::= { CHAR_LENGTH | CHARACTER_LENGTH } ( <character value expression>
    //       [ USING <char length units> ] )
    //   <octet length expression> ::= OCTET_LENGTH ( <string value expression> )
    // — <octet length expression> has no USING slot, so it takes a separate body parser.
    let pLengthExpression =
        let pBody =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>>. opt (pKeyword "USING" >>. pCharLengthUnits))

        let pOctetBody = between (token (pstring "(")) (token (pstring ")")) pExpression

        choice
            [ pKeyword "CHAR_LENGTH" >>. pBody
              |>> fun (e, units) -> LengthExpression(LengthFunction.CharLength, e, units)
              pKeyword "CHARACTER_LENGTH" >>. pBody
              |>> fun (e, units) -> LengthExpression(LengthFunction.CharacterLength, e, units)
              pKeyword "OCTET_LENGTH" >>. pOctetBody
              |>> fun e -> LengthExpression(LengthFunction.OctetLength, e, None) ]
        |> withExprPosition

    // 6.30 <numeric value function> — the built-ins of the shape <name> ( <args> )
    let pNumericValueFunction =
        let pUnary name ctor =
            pKeyword name
            >>. between (token (pstring "(")) (token (pstring ")")) pNumericValueExpression
            |>> fun e -> NumericValueFunction(ctor, [ e ])

        let pBinary name ctor =
            pKeyword name
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (pNumericValueExpression .>> token (pstring ",") .>>. pNumericValueExpression)
            |>> fun (a, b) -> NumericValueFunction(ctor, [ a; b ])

        choice
            [ pUnary "CARDINALITY" NumericFunction.Cardinality
              pUnary "ARRAY_MAX_CARDINALITY" NumericFunction.ArrayMaxCardinality
              // 6.30 <absolute value expression> ::= ABS ( <numeric value expression> )
              // 6.38 <interval absolute value function> ::= ABS ( <interval value expression> )
              // (the two are syntactically indistinguishable — one parser, dual citation)
              pUnary "ABS" NumericFunction.AbsoluteValue
              pBinary "MOD" NumericFunction.Modulus
              pUnary "SIN" NumericFunction.Sin
              pUnary "COS" NumericFunction.Cos
              pUnary "TAN" NumericFunction.Tan
              pUnary "SINH" NumericFunction.Sinh
              pUnary "COSH" NumericFunction.Cosh
              pUnary "TANH" NumericFunction.Tanh
              pUnary "ASIN" NumericFunction.Asin
              pUnary "ACOS" NumericFunction.Acos
              pUnary "ATAN" NumericFunction.Atan
              pBinary "LOG" NumericFunction.GeneralLogarithm
              pUnary "LOG10" NumericFunction.CommonLogarithm
              pUnary "LN" NumericFunction.NaturalLogarithm
              pUnary "EXP" NumericFunction.Exponential
              pBinary "POWER" NumericFunction.Power
              pUnary "SQRT" NumericFunction.SquareRoot
              pUnary "FLOOR" NumericFunction.Floor
              pUnary "CEIL" NumericFunction.Ceiling
              pUnary "CEILING" NumericFunction.Ceiling
              // 6.30 <width bucket function> ::= WIDTH_BUCKET ( <operand> , <bound 1> , <bound 2> , <count> )
              pKeyword "WIDTH_BUCKET"
              >>. between
                      (token (pstring "("))
                      (token (pstring ")"))
                      (pNumericValueExpression .>> token (pstring ",") .>>. pNumericValueExpression
                       .>> token (pstring ",")
                       .>>. pNumericValueExpression
                       .>> token (pstring ",")
                       .>>. pNumericValueExpression)
              |>> fun (((a, b), c), d) -> NumericValueFunction(NumericFunction.WidthBucket, [ a; b; c; d ])
              // 6.30 <match number function> ::= MATCH_NUMBER ( )
              pKeyword "MATCH_NUMBER"
              >>. between (token (pstring "(")) (token (pstring ")")) (preturn ())
              |>> fun () -> NumericValueFunction(NumericFunction.MatchNumber, []) ]
        |> withExprPosition

    // 6.30 <regex occurrences function>
    // 6.30 <regex position expression>
    // 6.32 <regex substring function>
    // 6.32 <regex transliteration>
    // — all four share the argument shape
    //   <pattern> [ FLAG <flag> ] IN <subject> [ WITH <replacement> ] [ FROM <start> ]
    //   [ USING <char length units> ] [ OCCURRENCE <occurrence> ] [ GROUP <capture group> ]
    // The operands are <character value expression>s, so the non-boolean expression parser is
    // used — that also keeps `IN` from being read as an 8.4 <in predicate>.
    let pRegexArgument =
        let pOperand = pNonBooleanValueExpression

        let pOccurrence =
            choice
                [ attempt (pKeyword "ALL" >>% RegexOccurrenceAll)
                  pExpression |>> RegexOccurrenceNumber ]

        pOperand .>>. opt (attempt (pKeyword "FLAG" >>. pOperand)) .>> pKeyword "IN"
        .>>. pOperand
        .>>. opt (attempt (pKeyword "WITH" >>. pOperand))
        .>>. opt (attempt (pKeyword "FROM" >>. pOperand))
        .>>. opt (attempt (pKeyword "USING" >>. pCharLengthUnits))
        .>>. opt (attempt (pKeyword "OCCURRENCE" >>. pOccurrence))
        .>>. opt (attempt (pKeyword "GROUP" >>. pOperand))
        |>> fun (((((((pattern, flag), subject), replacement), start), units), occurrence), captureGroup) ->
            { Pattern = pattern
              Flag = flag
              Subject = subject
              Replacement = replacement
              From = start
              Using = units
              Occurrence = occurrence
              CaptureGroup = captureGroup }
            : RegexArgument

    // 6.30 <regex occurrences function> ::= OCCURRENCES_REGEX ( <XQuery pattern> [ FLAG <flag> ]
    //     IN <regex subject string> [ FROM <start position> ] [ USING <char length units> ] )
    let pRegexOccurrencesFunction =
        pKeyword "OCCURRENCES_REGEX"
        >>. between (token (pstring "(")) (token (pstring ")")) pRegexArgument
        |>> RegexOccurrences
        |> withExprPosition

    // 6.30 <regex position expression> ::= POSITION_REGEX ( [ START | AFTER ] <XQuery pattern>
    //     [ FLAG <flag> ] IN <regex subject string> ... )
    let pRegexPositionFunction =
        let pStart =
            opt (
                attempt (
                    choice
                        [ pKeyword "START" >>% RegexStartOfString
                          pKeyword "AFTER" >>% RegexAfterMatch ]
                )
            )

        pKeyword "POSITION_REGEX"
        >>. between (token (pstring "(")) (token (pstring ")")) (pStart .>>. pRegexArgument)
        |>> fun (start, arg) -> RegexPosition(start, arg)
        |> withExprPosition

    // 6.32 <trim function> ::= TRIM ( [ <trim specification> ] [ <trim character> ] FROM <trim source> )
    let pTrimFunction =
        let pSpec =
            opt (
                pKeyword "LEADING" >>% Leading
                <|> (pKeyword "TRAILING" >>% Trailing)
                <|> (pKeyword "BOTH" >>% Both)
            )

        pKeyword "TRIM"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pSpec .>>. opt pExpression .>> pKeyword "FROM" .>>. pExpression)
        |>> (fun ((spec, char), source) -> Trim(spec, char, source))
        |> withExprPosition

    // 6.32 <character substring function> ::= SUBSTRING ( <character value expression> FROM <start position> [ FOR <string length> ] [ USING <char length units> ] )
    // <start position> / <string length> are <numeric value expression>s.
    let pCharacterSubstringFunction =
        pKeyword "SUBSTRING"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pNonBooleanValueExpression .>> pKeyword "FROM"
                 .>>. pNumericValueExpression
                 .>>. opt (pKeyword "FOR" >>. pNumericValueExpression)
                 .>>. opt (pKeyword "USING" >>. pCharLengthUnits))
        |>> fun (((src, start), len), units) -> Substring(src, start, len, units)
        |> withExprPosition

    // 6.32 <character overlay function> ::= OVERLAY ( <character value expression> PLACING <character value expression> FROM <start position> [ FOR <string length> ] )
    let pOverlayFunction =
        pKeyword "OVERLAY"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pNonBooleanValueExpression .>> pKeyword "PLACING"
                 .>>. pNonBooleanValueExpression
                 .>> pKeyword "FROM"
                 .>>. pNumericValueExpression
                 .>>. opt (pKeyword "FOR" >>. pNumericValueExpression))
        |>> fun (((src, placing), start), len) -> Overlay(src, placing, start, len)
        |> withExprPosition

    // 6.32 <regular expression substring function> ::= SUBSTRING ( <character value expression>
    //     SIMILAR <character value expression> ESCAPE <escape character> )
    let pRegularExpressionSubstringFunction =
        pKeyword "SUBSTRING"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pNonBooleanValueExpression .>> pKeyword "SIMILAR"
                 .>>. pNonBooleanValueExpression
                 .>> pKeyword "ESCAPE"
                 .>>. pExpression)
        |>> fun ((src, pattern), escape) -> SubstringSimilar(src, pattern, escape)
        |> withExprPosition

    // 6.32 <fold> ::= { UPPER | LOWER } ( <character value expression> )
    let pFoldFunction =
        choice
            [ pKeyword "UPPER" >>% FoldFunction.FoldUpper
              pKeyword "LOWER" >>% FoldFunction.FoldLower ]
        .>>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> fun (fn, e) -> Fold(fn, e)
        |> withExprPosition

    // 6.32 <transcoding> ::= CONVERT ( <character value expression> USING <transcoding name> )
    let pTranscodingFunction =
        pKeyword "CONVERT"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "USING" .>>. pIdentifierExpression)
        |>> fun (e, name) -> Transcoding(e, name)
        |> withExprPosition

    // 6.32 <character transliteration> ::= TRANSLATE ( <character value expression>
    //     USING <transliteration name> )
    let pCharacterTransliterationFunction =
        pKeyword "TRANSLATE"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "USING" .>>. pIdentifierExpression)
        |>> fun (e, name) -> CharacterTransliteration(e, name)
        |> withExprPosition

    // 6.32 <regex substring function> ::= SUBSTRING_REGEX ( <XQuery pattern> [ FLAG <flag> ]
    //     IN <regex subject string> ... [ OCCURRENCE <regex occurrence> ] [ GROUP <capture group> ] )
    let pRegexSubstringFunction =
        pKeyword "SUBSTRING_REGEX"
        >>. between (token (pstring "(")) (token (pstring ")")) pRegexArgument
        |>> RegexSubstring
        |> withExprPosition

    // 6.32 <regex transliteration> ::= TRANSLATE_REGEX ( <XQuery pattern> [ FLAG <flag> ]
    //     IN <regex subject string> [ WITH <replacement> ] ... )
    let pRegexTransliterateFunction =
        pKeyword "TRANSLATE_REGEX"
        >>. between (token (pstring "(")) (token (pstring ")")) pRegexArgument
        |>> RegexTransliterate
        |> withExprPosition

    // 8.12 <normal form> ::= NFC | NFD | NFKC | NFKD
    // Kept here (not in PredicateParser.fs): the 6.32 <normalize function> consumes it and
    // §6 precedes §8, so define-before-use wins over the module split.
    let pNormalForm =
        choice
            [ pKeyword "NFC" >>% Nfc
              pKeyword "NFD" >>% Nfd
              pKeyword "NFKC" >>% Nfkc
              pKeyword "NFKD" >>% Nfkd ]

    // 6.32 <normalize function> ::= NORMALIZE ( <character value expression>
    //     [ , <normal form> [ , <normalize function result length> ] ] )
    // (<normalize function result length> is parsed as an expression; modelling the
    //  CHARACTER_LENGTH ( n ) / CLOB ( n ) shape separately is not worthwhile —
    //  see docs/trade-off.md.)
    let pNormalizeFunction =
        let pRest =
            opt (
                attempt (
                    token (pstring ",") >>. pNormalForm
                    .>>. opt (attempt (token (pstring ",") >>. pExpression))
                )
            )

        pKeyword "NORMALIZE"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>>. pRest)
        |>> fun (e, rest) ->
            match rest with
            | Some(form, len) -> NormalizeFunction(e, Some form, len)
            | None -> NormalizeFunction(e, None, None)
        |> withExprPosition

    // 6.33 <JSON name and value> ::= [ KEY ] <JSON name> VALUE <JSON value expression>
    //                              | <JSON name> : <JSON value expression>
    // Both sides are <JSON value expression>s — value expressions, not boolean ones.
    let pJsonNameAndValue =
        choice
            [ attempt (
                  opt (pKeyword "KEY") .>>. pNonBooleanValueExpression .>> pKeyword "VALUE"
                  .>>. pNonBooleanValueExpression
                  |>> fun ((key, name), value) ->
                      { Name = name
                        Value = value
                        Key = Option.isSome key }
              )
              pNonBooleanValueExpression .>> token (pstring ":")
              .>>. pNonBooleanValueExpression
              |>> fun (name, value) ->
                  { Name = name
                    Value = value
                    Key = false } ]

    // 6.33 <JSON constructor null clause> ::= NULL ON NULL | ABSENT ON NULL
    let pJsonConstructorNullClause =
        choice
            [ pKeyword "NULL" >>. pKeyword "ON" >>. pKeyword "NULL" >>% JsonNullOnNull
              pKeyword "ABSENT" >>. pKeyword "ON" >>. pKeyword "NULL" >>% JsonAbsentOnNull ]

    // Returns bool (true = WITH UNIQUE, false = WITHOUT UNIQUE); callers wrap in opt.
    // 6.33 <JSON key uniqueness constraint> ::= WITH UNIQUE [ KEYS ] | WITHOUT UNIQUE [ KEYS ]
    let pJsonKeyUniqueness =
        choice
            [ pKeyword "WITH" >>. pKeyword "UNIQUE" >>. opt (pKeyword "KEYS") >>% true
              pKeyword "WITHOUT" >>. pKeyword "UNIQUE" >>. opt (pKeyword "KEYS") >>% false ]

    // 6.33 <JSON object constructor> ::= JSON_OBJECT ( [ <JSON name and value list> ]
    //     [ <JSON constructor null clause> ] [ <JSON key uniqueness constraint> ]
    //     [ <JSON output clause> ] )
    let pJsonObjectFunction =
        pKeyword "JSON_OBJECT"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (sepBy1 pJsonNameAndValue (token (pstring ",")))
                 .>>. opt pJsonConstructorNullClause
                 .>>. opt pJsonKeyUniqueness
                 .>>. opt pJsonOutputClause)
        |>> fun ((((pairs, nullClause), unique), output)) ->
            JsonObject(Option.defaultValue [] pairs, nullClause, unique, output)
        |> withExprPosition

    // 6.33 <JSON array constructor> ::= JSON_ARRAY ( [ <JSON value expression list> ]
    //     [ <JSON constructor null clause> ] [ <JSON output clause> ] )
    let pJsonArrayFunction =
        // A bare NULL/ABSENT followed by ON NULL starts the <JSON constructor null
        // clause>, not an element — reject it as an element so the null clause wins.
        let pJsonArrayElement =
            pExpression .>>? notFollowedBy (attempt (pKeyword "ON" >>. pKeyword "NULL"))

        pKeyword "JSON_ARRAY"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (sepBy pJsonArrayElement (token (pstring ",")))
                 .>>. opt pJsonConstructorNullClause
                 .>>. opt pJsonOutputClause)
        |>> fun ((elements, nullClause), output) -> JsonArray(Option.defaultValue [] elements, nullClause, output)
        |> withExprPosition

    // 6.34 <JSON query wrapper behavior> ::= WITHOUT [ ARRAY ] | WITH [ CONDITIONAL | UNCONDITIONAL ] [ ARRAY ]
    let pJsonQueryWrapper =
        choice
            [ pKeyword "WITHOUT" >>. opt (pKeyword "ARRAY" >>% true)
              |>> fun arr ->
                  { WithWrapper = false
                    Conditional = None
                    Array = Option.defaultValue false arr }
              pKeyword "WITH"
              >>. opt (
                  attempt (
                      pKeyword "CONDITIONAL" >>% Some true
                      <|> (pKeyword "UNCONDITIONAL" >>% Some false)
                  )
              )
              .>>. opt (pKeyword "ARRAY" >>% true)
              |>> fun (cond, arr) ->
                  { WithWrapper = true
                    Conditional = Option.flatten cond
                    Array = Option.defaultValue false arr } ]

    // 6.34 <JSON query quotes behavior> ::= KEEP | OMIT
    let pJsonQueryQuotes = choice [ pKeyword "KEEP" >>% Keep; pKeyword "OMIT" >>% Omit ]

    // 6.34 <JSON query empty/error behavior> ::= ERROR | NULL | EMPTY ARRAY | EMPTY OBJECT
    let pJsonQueryBehavior =
        choice
            [ pKeyword "ERROR" >>% JsonQueryError
              pKeyword "NULL" >>% JsonQueryNull
              pKeyword "EMPTY" >>. pKeyword "ARRAY" >>% JsonQueryEmptyArray
              pKeyword "EMPTY" >>. pKeyword "OBJECT" >>% JsonQueryEmptyObject ]

    // 6.34 <JSON query> ::= JSON_QUERY ( <JSON API common syntax> [ <JSON output clause> ]
    //     [ <JSON query wrapper behavior> WRAPPER ] [ <JSON query quotes behavior> QUOTES
    //     [ ON SCALAR STRING ] ] [ <JSON query empty behavior> ON EMPTY ]
    //     [ <JSON query error behavior> ON ERROR ] )
    let pJsonQueryFunction =
        pKeyword "JSON_QUERY"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pJsonApiCommonSyntax
                 .>>. opt pJsonOutputClause
                 .>>. opt (pJsonQueryWrapper .>> pKeyword "WRAPPER")
                 .>>. opt (
                     pJsonQueryQuotes
                     .>> pKeyword "QUOTES"
                     .>> opt (pKeyword "ON" .>> pKeyword "SCALAR" .>> pKeyword "STRING")
                 )
                 .>>. opt (pJsonQueryBehavior .>> pKeyword "ON" .>> pKeyword "EMPTY")
                 .>>. opt (pJsonQueryBehavior .>> pKeyword "ON" .>> pKeyword "ERROR"))
        |>> fun (((((common, output), wrapper), quotes), onEmpty), onError) ->
            JsonQuery(common, output, wrapper, quotes, onEmpty, onError)
        |> withExprPosition

    // 6.36 <datetime value function> ::= CURRENT_DATE | CURRENT_TIMESTAMP [ <left paren>
    //     <time precision> <right paren> ] | CURRENT_TIME ... | LOCALTIMESTAMP ... | LOCALTIME ...
    let pDateTimeValueFunction =
        let pPrecision =
            opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)

        choice
            [ pKeyword "CURRENT_DATE" >>% CurrentDate
              pKeyword "CURRENT_TIMESTAMP" >>. pPrecision |>> CurrentTimestamp
              pKeyword "CURRENT_TIME" >>. pPrecision |>> CurrentTime
              pKeyword "LOCALTIMESTAMP" >>. pPrecision |>> LocalTimestamp
              pKeyword "LOCALTIME" >>. pPrecision |>> LocalTime ]
        |> withExprPosition

    // 6.37 <interval value expression> ::= ... | ( <datetime value expression> <minus sign> <datetime term> ) <interval qualifier>
    // The difference of two datetimes, qualified as an interval. Tried ahead of the plain
    // parenthesized <value expression> alternative; `attempt` backtracks when no
    // <interval qualifier> follows the closing paren.
    // This covers the 4th alternative only; the full <interval value expression> is
    // defined as pIntervalValueExpression below (after pIntervalTerm).
    let pDatetimeDifference =
        attempt (
            between (token (pstring "(")) (token (pstring ")")) pDatetimeValueExpression
            .>>. pIntervalQualifier
            >>= fun (e, qualifier) ->
                match e.Kind with
                | BinaryOp(Subtract, l, r) -> preturn (DatetimeDifference(l, r, qualifier))
                | _ -> fail "expected <datetime value expression> - <datetime term>"
        )
        |> withExprPosition

    // 6.41 <trim array function> ::= TRIM_ARRAY ( <array value expression> , <numeric value expression> )
    let pTrimArrayFunction =
        pKeyword "TRIM_ARRAY"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> token (pstring ",") .>>. pNumericValueExpression)
        |>> fun (arr, count) -> TrimArray(arr, count)
        |> withExprPosition

    // 6.42 <array value constructor> ::= ARRAY <array value constructor by enumeration>
    //     | ARRAY <array value constructor by query>
    // <array element list> ::= <array element> [ { , <array element> }... ] — at least
    // ONE element; the empty form is only a 6.5 <empty specification>.
    let pArrayValueConstructor =
        pKeyword "ARRAY"
        >>. choice
                [ attempt (
                      between (token pLeftBracket) (token pRightBracket) (sepBy1 pExpression (token (pstring ",")))
                      |>> ArrayConstructor
                  )
                  attempt (between (token (pstring "(")) (token (pstring ")")) pQuery |>> ArrayQuery) ]
        |> withExprPosition

    // 6.44 <multiset set function> ::= SET ( <multiset value expression> )
    let pMultisetSetFunction =
        pKeyword "SET"
        >>. between (token (pstring "(")) (token (pstring ")")) pMultisetValueExpression
        |>> MultisetSetFunction
        |> withExprPosition

    // 6.45 <multiset value constructor> ::= MULTISET <multiset value constructor by enumeration>
    //     | MULTISET <multiset value constructor by query>
    // <multiset element list> requires at least ONE element.
    let pMultisetValueConstructor =
        pKeyword "MULTISET"
        >>. choice
                [ attempt (
                      between (token pLeftBracket) (token pRightBracket) (sepBy1 pExpression (token (pstring ",")))
                      |>> MultisetConstructor
                  )
                  attempt (between (token (pstring "(")) (token (pstring ")")) pQuery |>> MultisetQuery) ]
        |> withExprPosition

    // 6.45 <multiset value constructor> ::= <multiset value constructor by enumeration>
    //     | <multiset value constructor by query> | <table value constructor by query>
    //   <table value constructor by query> ::= TABLE <table subquery>
    let pTableValueConstructorByQuery =
        pKeyword "TABLE" >>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> TableQuery
        |> withExprPosition

    // 7.1 <explicit row value constructor> ::= ( <row value constructor element> <comma>
    //     <row value constructor element list> ) | ROW ( <row value constructor element list> )
    // The parenthesized form requires at least two elements (one element is just a
    // parenthesized <value expression>), so `attempt` backtracks on `(a)` and lets the
    // plain parenthesized branch of pValueExpressionPrimary below handle it.
    let pExplicitRowValueConstructor =
        attempt (
            between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>>. many1 (token (pstring ",") >>. pExpression))
            |>> fun (first, rest) -> RowValueConstructor(first :: rest)
        )
        <|> (pKeyword "ROW"
             >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
             |>> RowValueConstructor)
        |> withExprPosition

    let pRoutineInvocation =
        // 10.9 <aggregate function> ::= COUNT ( <asterisk> ) | ... — the bare `*` is an
        // argument ONLY of COUNT; every other function takes <value expression>s.
        // (The `*` is NOT a <value expression primary>, so it is parsed here directly.)
        let pStarArg =
            pstring "*" .>> ws .>>. getPosition
            |>> fun (_, pos) ->
                [ { Expression.Kind = ExpressionKind.Star
                    Pos = { Line = pos.Line; Column = pos.Column } } ]

        let pArgs =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
                 .>>. (attempt pStarArg <|> sepBy pExpression (token (pstring ","))))

        let pFilter =
            pKeyword "FILTER"
            >>. between (token (pstring "(")) (token (pstring ")")) (pKeyword "WHERE" >>. pExpression)

        let pWithinGroup =
            pKeyword "WITHIN"
            >>. pKeyword "GROUP"
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (pKeyword "ORDER"
                     >>. pKeyword "BY"
                     >>. sepBy1 pSortSpecification (token (pstring ",")))

        let nameExpr =
            getPosition .>>. pRoutineName
            |>> fun (pos, name) ->
                { Expression.Kind = Identifier name
                  Pos = { Line = pos.Line; Column = pos.Column } }

        nameExpr
        .>>. pArgs
        // 10.9 clause order: <args> [ <within group specification> ] [ <filter clause> ]
        // then 6.10 OVER — WITHIN GROUP comes immediately after the argument list,
        // FILTER after it, OVER last.
        .>>. opt pWithinGroup
        .>>. opt pFilter
        .>>. opt pWindowNameOrSpecification
        >>= fun ((((name, (dist, args)), withinGroup), filter), window) ->
            // 6.10 and 10.9 make the suffix mandatory for some reserved function keywords: those
            // names are whitelisted, so without this check `ROW_NUMBER()` or `LISTAGG(x, ',')` would
            // degrade to a plain <routine invocation>.
            let functionName =
                match name.Kind with
                | Identifier n -> n
                | _ -> ""

            if Set.contains functionName windowOnlyFunctionNames && Option.isNone window then
                fail (sprintf "%s requires an OVER clause (6.10 <window function>)." functionName)
            elif
                Set.contains functionName withinGroupOnlyFunctionNames
                && Option.isNone withinGroup
            then
                fail (sprintf "%s requires a WITHIN GROUP clause (10.9)." functionName)
            else
                // 10.9 — WITHIN GROUP is only valid for <ordered set function>s and
                // OVER only for <window function type>s; FILTER only for <set function>s.
                let isRank = Set.contains functionName rankFunctionNames
                let isAggregate = Set.contains functionName aggregateFunctionNames

                let isOrderedSet =
                    isRank
                    || Set.contains functionName inverseDistributionFunctionNames
                    || functionName = "LISTAGG"

                if Option.isSome withinGroup && not isOrderedSet then
                    fail (sprintf "%s does not take a WITHIN GROUP clause (10.9)." functionName)
                elif
                    Option.isSome window
                    && not (isRank || isAggregate || Set.contains functionName windowOnlyFunctionNames)
                then
                    fail (sprintf "%s does not take an OVER clause (6.10 <window function type>)." functionName)
                elif
                    Option.isSome filter
                    && not (isAggregate || isOrderedSet || functionName = "ARRAY_AGG")
                then
                    fail (sprintf "%s does not take a FILTER clause (10.9 <set function>)." functionName)
                else
                    // Arity / argument-shape checks (6.10, 10.9).
                    let argKinds = args |> List.map (fun a -> a.Kind)

                    let failArity what =
                        fail (sprintf "%s expects %s." functionName what)

                    let isSimpleValueSpec k =
                        match k with
                        | Literal _
                        | Parameter _ -> true
                        | _ -> false

                    // 10.9 — the bare `*` argument is only `COUNT ( <asterisk> )`.
                    let hasStarArg = args |> List.exists (fun a -> a.Kind = ExpressionKind.Star)

                    if hasStarArg && functionName <> "COUNT" then
                        failArity "no <asterisk> argument (10.9 <aggregate function>)"
                    elif
                        hasStarArg
                        && match args with
                           | [ _ ] -> false
                           | _ -> true
                    then
                        failArity "exactly one <asterisk> argument (10.9 <aggregate function>)"
                    elif isRank && Option.isSome window && not args.IsEmpty then
                        failArity "no arguments in the OVER form (6.10 <rank function type>)"
                    elif isRank && Option.isSome withinGroup && args.IsEmpty then
                        failArity
                            "at least one argument in the WITHIN GROUP form (10.9 <hypothetical set function value expression list>)"
                    elif functionName = "ROW_NUMBER" && not args.IsEmpty then
                        failArity "no arguments (6.10 <window function type>)"
                    elif
                        functionName = "NTILE"
                        && match args with
                           | [ { Kind = k } ] when isSimpleValueSpec k -> false
                           | _ -> true
                    then
                        failArity "exactly one <simple value specification> (6.10 <ntile function>)"
                    elif
                        (functionName = "LEAD" || functionName = "LAG")
                        && match args with
                           | [ _ ] -> false
                           | [ _; { Kind = Literal(Number _) } ] -> false
                           | [ _; { Kind = Literal(Number _) }; _ ] -> false
                           | _ -> true
                    then
                        failArity
                            "1 to 3 arguments with an <exact numeric literal> offset (6.10 <lead or lag function>)"
                    elif
                        (functionName = "FIRST_VALUE" || functionName = "LAST_VALUE")
                        && match args with
                           | [ _ ] -> false
                           | _ -> true
                    then
                        failArity "exactly one argument (6.10 <first or last value function>)"
                    elif
                        functionName = "NTH_VALUE"
                        && match args with
                           | [ _; { Kind = k } ] when isSimpleValueSpec k -> false
                           | _ -> true
                    then
                        failArity "exactly two arguments (6.10 <nth value function>)"
                    elif
                        isAggregate
                        && functionName <> "ARRAY_AGG"
                        && match args with
                           | [ _ ] -> false
                           | _ -> true
                    then
                        failArity "exactly one argument (10.9 <general set function>)"
                    elif
                        Set.contains functionName binarySetFunctionNames
                        && match args with
                           | [ _; _ ] -> false
                           | _ -> true
                    then
                        failArity "exactly two arguments (10.9 <binary set function>)"
                    elif
                        Set.contains functionName inverseDistributionFunctionNames
                        && match args with
                           | [ _ ] -> false
                           | _ -> true
                    then
                        failArity "exactly one argument (10.9 <inverse distribution function>)"
                    elif
                        functionName = "LISTAGG"
                        && match args with
                           | [ _; { Kind = Literal(String _) } ] -> false
                           | _ -> true
                    then
                        failArity
                            "a <character value expression> and a <character string literal> separator (10.9 <listagg set function>)"
                    elif
                        functionName = "ARRAY_AGG"
                        && match args with
                           | [ _ ] -> false
                           | _ -> true
                    then
                        failArity "exactly one argument (10.9 <array aggregate function>)"
                    else
                        match window with
                        | Some w ->
                            preturn (
                                WindowFunction
                                    { Function = name
                                      Args = args
                                      IsDistinct = Option.defaultValue false dist
                                      Window = w }
                            )
                        | None ->
                            preturn (
                                FunctionCall(name, Option.defaultValue false dist, args, None, filter, withinGroup)
                            )
        |> withExprPosition

    // 10.11 <JSON object aggregate> ::= JSON_OBJECTAGG ( <JSON name and value>
    //     [ <JSON constructor null clause> ] [ <JSON key uniqueness constraint> ]
    //     [ <JSON output clause> ] )
    let pJsonObjectAggFunction =
        pKeyword "JSON_OBJECTAGG"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pJsonNameAndValue
                 .>>. opt pJsonConstructorNullClause
                 .>>. opt pJsonKeyUniqueness
                 .>>. opt pJsonOutputClause)
        |>> fun (((nv, nullClause), unique), output) -> JsonObjectAgg(nv, nullClause, unique, output)
        |> withExprPosition

    // 10.11 <JSON array aggregate> ::= JSON_ARRAYAGG ( <JSON value expression>
    //     [ ORDER BY <sort specification list> ] [ <JSON constructor null clause> ]
    //     [ <JSON output clause> ] )
    let pJsonArrayAggFunction =
        pKeyword "JSON_ARRAYAGG"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression
                 .>>. opt (
                     pKeyword "ORDER"
                     >>. pKeyword "BY"
                     >>. sepBy1 pSortSpecification (token (pstring ","))
                 )
                 .>>. opt pJsonConstructorNullClause
                 .>>. opt pJsonOutputClause)
        |>> fun (((expr, orderBy), nullClause), output) -> JsonArrayAgg(expr, orderBy, nullClause, output)
        |> withExprPosition

    // 6.9 <set function specification> ::= [ <running or final> ] <aggregate function>
    //     | <grouping operation>
    // The RUNNING/FINAL prefix is only accepted in front of an <aggregate function> name.
    let pSetFunctionSpecification =
        getPosition .>>. (pRunningOrFinal .>>. pRoutineInvocation)
        >>= fun (pos, (scope, e)) ->
            match e.Kind with
            | FunctionCall({ Kind = Identifier name }, _, _, _, _, _) when Set.contains name aggregateFunctionNames ->
                preturn
                    { Expression.Kind = SetFunction(Some scope, e)
                      Pos = { Line = pos.Line; Column = pos.Column } }
            | _ -> fail "RUNNING/FINAL requires an <aggregate function>"

    // — the atomic building block of every <value expression>, used as the term parser of the operator-precedence parser below.
    // 6.3 <value expression primary> — the atomic building block of every <value expression>
    // The full form includes one grammar-exceeding approximation: the §8 predicate atoms
    // (pPredicatePrimary, needed so ANY/SOME/ALL (subquery) beats the pRoutineInvocation
    // whitelist and EXISTS/UNIQUE/PERIOD/JSON_EXISTS parse). The 7.16 <asterisk> wildcard
    // is NOT a <value expression primary> and is NOT accepted here — `COUNT ( * )` is its
    // own 10.9 alternative, handled inside pRoutineInvocation's argument list, and the
    // select-list / qualified-asterisk forms are parsed by QueryParser. Contexts that
    // require the grammar's <value expression primary> shape — the 6.35/6.37 datetime &
    // interval chain — use pValueExpressionPrimary, which excludes the predicate
    // atoms (see docs/trade-off.md).
    let pValueExpressionPrimaryImpl (withPredicates: bool) =
        // 6.3 <scalar subquery> ::= ( <subquery> )
        // Local because pValueExpressionPrimaryImpl is its only consumer.
        let pScalarSubquery =
            between (token (pstring "(")) (token (pstring ")")) pQuery
            |>> SubqueryExpression
            |> withExprPosition

        // 6.32 <classifier function> ::= CLASSIFIER ( [ <row pattern variable name> ] )
        let pClassifierFunction =
            pKeyword "CLASSIFIER"
            >>. between (token (pstring "(")) (token (pstring ")")) (opt pExpression)
            |>> Classifier
            |> withExprPosition

        choice
            [ attempt pCastSpecification
              attempt pCaseExpression
              attempt pNullifExpr
              attempt pCoalesceExpr
              attempt pExtractExpression
              attempt pPositionExpression
              attempt pLengthExpression
              attempt pNumericValueFunction
              attempt pRegexOccurrencesFunction
              attempt pRegexPositionFunction
              attempt pTrimFunction
              attempt pRegularExpressionSubstringFunction
              attempt pCharacterSubstringFunction
              attempt pOverlayFunction
              attempt pFoldFunction
              attempt pTranscodingFunction
              attempt pCharacterTransliterationFunction
              attempt pRegexSubstringFunction
              attempt pRegexTransliterateFunction
              attempt pNormalizeFunction
              attempt pClassifierFunction
              attempt pDateTimeValueFunction
              attempt pNextValueExpression
              attempt pSubtypeTreatment
              attempt pReferenceResolution
              attempt pMultisetElementReference
              attempt pArrayValueConstructor
              attempt pMultisetValueConstructor
              attempt pTableValueConstructorByQuery
              attempt pTrimArrayFunction
              attempt pMultisetSetFunction
              attempt pJsonValueFunction
              attempt pJsonQueryFunction
              attempt pJsonObjectFunction
              attempt pJsonArrayFunction
              attempt pJsonObjectAggFunction
              attempt pJsonArrayAggFunction
              if withPredicates then
                  attempt pPredicatePrimary
              attempt pStaticMethodInvocation
              attempt pNewSpecification
              attempt pNestedRowNumberFunction
              attempt pValueOfExpressionAtRow
              attempt pRowPatternNavigationOperation
              attempt pGroupingOperation
              attempt pSetFunctionSpecification
              attempt pRoutineInvocation
              attempt pScalarSubquery
              attempt pLiteralExpression
              attempt pGeneralValueSpecification
              attempt pGeneralizedInvocation
              attempt pDatetimeDifference
              attempt pExplicitRowValueConstructor
              pColumnReferenceExpression
              between (token (pstring "(")) (token (pstring ")")) pExpression ]
        // The postfix loop must be able to leave a '.' behind (e.g. the `.*` of
        // <all fields reference>, 7.16), so both alternatives are backtracking.
        .>>. many (attempt pDereferenceReference <|> attempt pMethodOrFieldReference)
        |>> fun (e, refs) -> List.fold (fun acc f -> f acc) e refs

    let pValueExpressionPrimaryWithPredicates = pValueExpressionPrimaryImpl true

    let pValueExpressionPrimary = pValueExpressionPrimaryImpl false

    // 6.37 <interval primary> ::= <value expression primary> [ <interval qualifier> ]
    //     | <interval value function>
    // A qualifier-less <interval primary> is represented by its <value expression primary>,
    // so wrapping only happens when an <interval qualifier> is actually present.
    // The grammar's primary is used: predicates and the '*' wildcard are not
    // <value expression primary>s and are rejected in interval context.
    let pIntervalPrimary =
        pValueExpressionPrimary .>>. opt (attempt pIntervalQualifier)
        |>> fun (e, qualifier) ->
            match qualifier with
            | Some qual ->
                { Expression.Kind = IntervalPrimary(e, qual)
                  Pos = e.Pos }
            | None -> e

    // 6.35 <time zone> ::= AT <time zone specifier>
    //   <time zone specifier> ::= LOCAL | TIME ZONE <interval primary>
    let pTimeZoneSuffix =
        pKeyword "AT"
        >>. choice
                [ pKeyword "LOCAL" >>% TimeZoneSpecifier.TimeZoneLocal
                  pKeyword "TIME" >>. pKeyword "ZONE" >>. pIntervalPrimary
                  |>> TimeZoneSpecifier.TimeZoneOffset ]
        |>> fun spec ->
            fun e ->
                { Expression.Kind = AtTimeZone(e, spec)
                  Pos = e.Pos }

    // 6.37 <interval factor> ::= [ <sign> ] <interval primary>
    // The '-' alternative must not swallow the start of the '->' dereference operator.
    let pIntervalSign =
        pchar '-' .>> notFollowedBy (pchar '>') .>> ws >>% false
        <|> (pchar '+' .>> ws >>% true)

    let pIntervalSigned sign e =
        match sign with
        | Some true ->
            { Expression.Kind = UnaryOp(UnaryOperator.Plus, e)
              Pos = e.Pos }
        | Some false ->
            { Expression.Kind = UnaryOp(UnaryOperator.Minus, e)
              Pos = e.Pos }
        | None -> e

    // 6.37 <interval term> ::= <interval factor>
    //     | <interval term> <asterisk> <factor>
    //     | <interval term> <solidus> <factor>
    //     | <term> <asterisk> <interval factor>
    // The '*'/'/' right operand is either the grammar's <factor> (6.29: [ <sign> ]
    // <numeric primary>, no qualifier) or an <interval factor> ([ <sign> ]
    // <interval primary>, optional qualifier). <numeric primary> is approximated by
    // <value expression primary> (it already subsumes <numeric value function>, while
    // <interval value function> is syntactically identical to it — see
    // pNumericValueFunction). Since <interval primary> = <value expression primary>
    // [ <interval qualifier> ], one parser (pIntervalFactor) serves both right-hand
    // forms: with a qualifier it is the 4th alternative, without it the 2nd/3rd.
    let pIntervalTerm =
        let pMul =
            attempt (pchar '*' .>> ws)
            >>% fun l r ->
                { Expression.Kind = BinaryOp(Multiply, l, r)
                  Pos = l.Pos }

        let pDiv =
            attempt (pchar '/' .>> ws)
            >>% fun l r ->
                { Expression.Kind = BinaryOp(Divide, l, r)
                  Pos = l.Pos }

        let pIntervalFactor =
            opt (attempt pIntervalSign) .>>. pIntervalPrimary
            |>> fun (sign, e) -> pIntervalSigned sign e

        pIntervalFactor .>>. many (attempt (pMul <|> pDiv .>>. pIntervalFactor))
        |>> fun (first, rest) -> rest |> List.fold (fun acc (op, operand) -> op acc operand) first

    // 6.37 <interval value expression> ::= <interval term>
    //     | <interval value expression> <plus sign> <interval term>
    //     | <interval value expression> <minus sign> <interval term>
    //     | ( <datetime value expression> <minus sign> <datetime term> ) <interval qualifier>
    // The 4th alternative is tried first: pIntervalPrimary would otherwise fold
    // `( <datetime> - <datetime> ) <interval qualifier>` into IntervalPrimary via the plain
    // parenthesized <value expression> + qualifier path. The +/- chain is left-folded so that
    // 'a + b - c' associates to the left; the '-' operator must not swallow the start of the
    // '->' dereference operator. The grammar does not distinguish interval-valued from
    // datetime-valued operands syntactically, so any <value expression primary> that
    // pIntervalTerm accepts goes through here as well (e.g. 1 + 2) — see docs/trade-off.md.
    let pIntervalValueExpression =
        let pOperator =
            attempt (pchar '+' .>> ws) >>% BinaryOperator.Add
            <|> (pchar '-' .>> notFollowedBy (pchar '>') .>> ws >>% BinaryOperator.Subtract)

        choice
            [ attempt pDatetimeDifference
              pIntervalTerm .>>. many (attempt (pOperator .>>. pIntervalTerm))
              |>> fun (first, rest) ->
                  rest
                  |> List.fold
                      (fun acc (op, rhs) ->
                          { Expression.Kind = BinaryOp(op, acc, rhs)
                            Pos = acc.Pos })
                      first ]

    // 6.35 <datetime term> ::= <datetime factor>
    //   <datetime factor> ::= <datetime primary> [ <time zone> ]
    //   <datetime primary> ::= <value expression primary> | <datetime value function>
    // (<datetime value function> is one of the pValueExpressionPrimary alternatives.)
    // The grammar's primary is used: predicates and the '*' wildcard are not
    // <value expression primary>s and are rejected in datetime context.
    let pDatetimeTerm =
        pValueExpressionPrimary .>>. opt (attempt pTimeZoneSuffix)
        |>> fun (e, timeZone) ->
            match timeZone with
            | Some applyTimeZone -> applyTimeZone e
            | None -> e

    // 6.35 <datetime value expression> ::= <datetime term>
    //     | <interval value expression> <plus sign> <datetime term>
    //     | <datetime value expression> <plus sign> <interval term>
    //     | <datetime value expression> <minus sign> <interval term>
    // Left-folded so that 'a + b - c' associates to the left. The right-hand operand may be
    // an <interval term> or a <datetime term> — the two are syntactically indistinguishable.
    pDatetimeValueExpressionRef.Value <-
        (let pOperator =
            attempt (pchar '+' .>> ws) >>% BinaryOperator.Add
            <|> (pchar '-' .>> notFollowedBy (pchar '>') .>> ws >>% BinaryOperator.Subtract)

         pDatetimeTerm
         .>>. many (attempt (pOperator .>>. (attempt pIntervalTerm <|> pDatetimeTerm))))
        |>> fun (first, rest) ->
            rest
            |> List.fold
                (fun acc (op, rhs) ->
                    { Expression.Kind = BinaryOp(op, acc, rhs)
                      Pos = acc.Pos })
                first

    // 6.43 <multiset value expression>
    //   <multiset primary> ::= <multiset value function> | <value expression primary>
    //   <multiset term> ::= <multiset primary>
    //       | <multiset term> MULTISET INTERSECT [ ALL | DISTINCT ] <multiset primary>
    //   <multiset value expression> ::= <multiset term>
    //       | <multiset value expression> MULTISET { UNION | EXCEPT } [ ALL | DISTINCT ] <multiset term>
    // Applied as a left-folded postfix so that it composes with the operator-precedence parser; the
    // left operand is then any <value expression> rather than strictly a <multiset term>
    // (see docs/trade-off.md). The right operand is a <multiset term>, which is what makes
    // MULTISET INTERSECT bind tighter than MULTISET UNION / MULTISET EXCEPT.
    let pMultisetSetOperatorSuffix =
        let pModifier =
            opt (attempt (pKeyword "ALL" >>% true <|> (pKeyword "DISTINCT" >>% false)))

        let pIntersectChain =
            many (
                attempt (
                    pKeyword "MULTISET" >>. pKeyword "INTERSECT" >>. pModifier
                    .>>. pValueExpressionPrimary
                )
            )

        let pTerm =
            pValueExpressionPrimary .>>. pIntersectChain
            |>> fun (first, rest) ->
                rest
                |> List.fold
                    (fun acc (modifier, rhs) ->
                        { Expression.Kind = MultisetSetOperation(MultisetIntersect, modifier, acc, rhs)
                          Pos = acc.Pos })
                    first

        pKeyword "MULTISET"
        >>. choice
                [ pKeyword "UNION" >>% MultisetUnion
                  pKeyword "EXCEPT" >>% MultisetExcept
                  pKeyword "INTERSECT" >>% MultisetIntersect ]
        .>>. pModifier
        .>>. pTerm
        |>> fun ((op, modifier), rhs) ->
            fun lhs ->
                { Expression.Kind = MultisetSetOperation(op, modifier, lhs, rhs)
                  Pos = lhs.Pos }

    // 6.43/6.44 — the self-contained <multiset value expression> used by the 6.44 SET ( ... )
    // The base is a <multiset primary>, so the grammar's primary is used.
    pMultisetValueExpressionRef.Value <-
        pValueExpressionPrimary .>>. many (attempt pMultisetSetOperatorSuffix)
        |>> fun (first, rest) -> rest |> List.fold (fun acc f -> f acc) first

    // 6.28 <value expression> / 6.29 <numeric value expression> / 6.31 <string value expression>
    // Operator-precedence parser for <value expression> (terms, factors, concatenation, comparison)
    let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
    opp.TermParser <- pValueExpressionPrimaryWithPredicates

    let addInfix op precedence assoc mapping =
        opp.AddOperator(InfixOperator(op, ws, precedence, assoc, fun x y -> { Kind = mapping x y; Pos = x.Pos }))

    let addPrefix op precedence mapping =
        opp.AddOperator(PrefixOperator(op, ws, precedence, true, fun x -> { Kind = mapping x; Pos = x.Pos }))

    // 6.29 <factor> ::= [ <sign> ] <numeric primary> — <sign> ::= <plus sign> | <minus sign>
    addPrefix "+" 8 (fun e -> UnaryOp(UnaryOperator.Plus, e))
    addPrefix "-" 8 (fun e -> UnaryOp(UnaryOperator.Minus, e))

    // 6.29 <term> ::= <term> <asterisk> <factor> | <term> <solidus> <factor>
    addInfix "*" 7 Associativity.Left (fun x y -> BinaryOp(Multiply, x, y))
    addInfix "/" 7 Associativity.Left (fun x y -> BinaryOp(Divide, x, y))

    // 6.29 <numeric value expression> ::= <numeric value expression> <plus/minus> <term>
    // 6.31 <concatenation> ::= <character value expression> <concatenation operator> <character factor> — <concatenation operator> ::= ||
    addInfix "+" 6 Associativity.Left (fun x y -> BinaryOp(Add, x, y))
    addInfix "-" 6 Associativity.Left (fun x y -> BinaryOp(Subtract, x, y))
    addInfix "||" 6 Associativity.Left (fun x y -> BinaryOp(Concatenate, x, y))

    // 8.2 <comparison predicate> ::= <row value predicand> <comp op> <row value predicand>
    // 8.9 <quantified comparison predicate> — rewrites <comp op> <quantifier> <table subquery> on the right
    // Kept here (not in PredicateParser.fs): these are operators of `opp`, which §6 owns. The
    // right-hand <quantifier> <table subquery> term is built by PredicateParser.pQuantifiedSubqueryTerm
    // (surfaced through the pPredicatePrimary forward ref used by pValueExpressionPrimaryWithPredicates).
    let comparisonOp op x (y: Expression) =
        match y.Kind with
        | QuantifiedSubquery(quant, q) -> QuantifiedComparison(op, quant, x, q)
        | _ -> BinaryOp(op, x, y)

    // 8.2 <comp op> ::= <equals operator> | <not equals operator> | <less than operator> | <less than or equals operator> | <greater than operator> | <greater than or equals operator>
    addInfix "=" 5 Associativity.Left (comparisonOp Equal)
    addInfix "<>" 5 Associativity.Left (comparisonOp NotEqual)
    addInfix "<" 5 Associativity.Left (comparisonOp LessThan)
    addInfix "<=" 5 Associativity.Left (comparisonOp LessThanOrEqual)
    addInfix ">" 5 Associativity.Left (comparisonOp GreaterThan)
    addInfix ">=" 5 Associativity.Left (comparisonOp GreaterThanOrEqual)

    // 6.28 <value expression> without boolean operators or predicates — used where the
    // grammar requires a non-boolean <value expression> (e.g. <point in time> in
    // <query system time period specification>, 7.6). Stops before AND/OR.
    pValueExpressionNoBooleanRef.Value <- opp.ExpressionParser

    // 6.29 <numeric value expression> — arithmetic-only opp (no comparisons, no predicates).
    // Used where the grammar requires <numeric value expression> (TABLESAMPLE percentage,
    // <repeat argument>, array subscript, SUBSTRING start position / string length,
    // OVERLAY start position / length, TRIM_ARRAY count, POSITION start position,
    // WIDTH_BUCKET bounds/count).
    let oppNumeric = new OperatorPrecedenceParser<Expression, unit, unit>()
    oppNumeric.TermParser <- pValueExpressionPrimary

    let addInfixNum op precedence assoc mapping =
        oppNumeric.AddOperator(InfixOperator(op, ws, precedence, assoc, fun x y -> { Kind = mapping x y; Pos = x.Pos }))

    let addPrefixNum op precedence mapping =
        oppNumeric.AddOperator(PrefixOperator(op, ws, precedence, true, fun x -> { Kind = mapping x; Pos = x.Pos }))

    addPrefixNum "+" 8 (fun e -> UnaryOp(UnaryOperator.Plus, e))
    addPrefixNum "-" 8 (fun e -> UnaryOp(UnaryOperator.Minus, e))
    addInfixNum "*" 7 Associativity.Left (fun x y -> BinaryOp(Multiply, x, y))
    addInfixNum "/" 7 Associativity.Left (fun x y -> BinaryOp(Divide, x, y))
    addInfixNum "+" 6 Associativity.Left (fun x y -> BinaryOp(Add, x, y))
    addInfixNum "-" 6 Associativity.Left (fun x y -> BinaryOp(Subtract, x, y))

    // Wire up the numeric value expression forward ref (breaks the cycle: pValueExpressionPrimaryImpl
    // → pArrayElementReference → pNumericValueExpression → pValueExpressionPrimary → pValueExpressionPrimaryImpl).
    pNumericValueExpressionRef.Value <- oppNumeric.ExpressionParser

    // 6.39 <boolean test> ::= <boolean primary> IS [ NOT ] { TRUE | FALSE | UNKNOWN } — combined here with
    // 8.x <predicate> / 6.24 <array element reference> postfix applied to a <value expression>
    let pBooleanTest =
        opp.ExpressionParser
        .>>. many (
            pPredicate
            <|> pArrayElementReference
            <|> attempt pMultisetSetOperatorSuffix
            <|> attempt pTimeZoneSuffix
        )
        |>> fun (e, suffixes) -> List.fold (fun acc f -> f acc) e suffixes

    // 6.39 <boolean factor> ::= [ NOT ] <boolean test>
    let pBooleanFactor, pNotExprRef = createParserForwardedToRef<Expression, unit> ()

    pNotExprRef.Value <-
        (attempt (pKeyword "NOT" >>. pBooleanFactor)
         |>> fun e ->
             { Expression.Kind = UnaryOp(Not, e)
               Pos = e.Pos })
        <|> pBooleanTest

    // 6.39 <boolean term> ::= <boolean factor> | <boolean term> AND <boolean factor>
    let pBooleanTerm =
        chainl1
            pBooleanFactor
            (pKeyword "AND"
             >>% fun l r ->
                 { Expression.Kind = BinaryOp(And, l, r)
                   Pos = l.Pos })

    // 6.39 <boolean value expression> ::= <boolean term> | <boolean value expression> OR <boolean term>
    let pBooleanValueExpression =
        chainl1
            pBooleanTerm
            (pKeyword "OR"
             >>% fun l r ->
                 { Expression.Kind = BinaryOp(Or, l, r)
                   Pos = l.Pos })

    // ANY/SOME/ALL (subquery) is only valid as the right operand of a comparison operator,
    // where `comparisonOp` rewrites it into QuantifiedComparison. Any QuantifiedSubquery that
    // survives (i.e. was not rewritten) is standalone and must be rejected.
    //
    // The traversal uses an explicit work list so it is tail-recursive: an expression tree can
    // be arbitrarily deep, and a boolean short-circuit (`a || b`) cannot put both recursive
    // calls in tail position. The child collectors sit at module level (rather than inside the
    // search) so they are not rebuilt on every visited node.
    [<TailCall>]
    let rec private containsStandaloneQuantifiedSubqueryIn (work: Expression list) =
        let jsonCommonChildren c =
            [ yield c.Context
              yield! Option.toList c.PathName
              yield! c.Passing |> List.collect (fun (a: JsonPassingArgument) -> [ a.Value; a.Name ]) ]

        let regexArgumentChildren (a: RegexArgument) =
            let occurrenceChildren o =
                match o with
                | RegexOccurrenceNumber e -> [ e ]
                | RegexOccurrenceAll -> []

            [ yield a.Pattern
              yield! Option.toList a.Flag
              yield a.Subject
              yield! Option.toList a.Replacement
              yield! Option.toList a.From
              yield! a.Occurrence |> Option.toList |> List.collect occurrenceChildren
              yield! Option.toList a.CaptureGroup ]

        let expressionChildren (e: Expression) =
            match e.Kind with
            | BinaryOp(_, l, r) -> [ l; r ]
            | UnaryOp(_, x) -> [ x ]
            | FunctionCall(name, _, args, _, filter, withinGroup) ->
                [ yield name
                  yield! args
                  yield! Option.toList filter
                  yield! withinGroup |> Option.defaultValue [] |> List.map (fun (e, _, _) -> e) ]
            | Cast(x, _) -> [ x ]
            | Case(cond, whens, elseExpr) ->
                [ yield! Option.toList cond
                  yield! whens |> List.collect (fun (w, t) -> [ w; t ])
                  yield! Option.toList elseExpr ]
            | WindowFunction wf -> wf.Function :: wf.Args
            | ExpressionKind.Between(x, _, _, lo, hi) -> [ x; lo; hi ]
            | AllFieldsReference(x, cols) -> x :: Option.defaultValue [] cols
            | InList(x, _, items) -> x :: items
            | IsNull(x, _) -> [ x ]
            | IsBoolean(x, _, _) -> [ x ]
            | IsDistinctFrom(x, _, y) -> [ x; y ]
            | Overlaps(x, y) -> [ x; y ]
            | Collate(x, c) -> [ x; c ]
            | Like(x, _, p, esc) -> [ yield x; yield p; yield! Option.toList esc ]
            | SimilarTo(x, _, p, esc) -> [ yield x; yield p; yield! Option.toList esc ]
            | Extract(x, f) -> [ x; f ]
            | Position(x, s, len) -> [ yield x; yield s; yield! Option.toList len ]
            | Trim(_, src, x) -> x :: Option.toList src
            | Substring(x, s, len, _) -> [ yield x; yield s; yield! Option.toList len ]
            | Overlay(x, p, f, len) -> [ yield x; yield p; yield f; yield! Option.toList len ]
            | QuantifiedComparison(_, _, x, _) -> [ x ]
            | CurrentTransformGroupForType x -> [ x ]
            | CollationFor x -> [ x ]
            | ArrayConstructor xs -> xs
            | MultisetConstructor xs -> xs
            | TableQuery _ -> []
            | ArrayElement(x, idx) -> [ x; idx ]
            | Treat(x, _) -> [ x ]
            | Deref x -> [ x ]
            | Element x -> [ x ]
            | IsNormalized(x, _, _) -> [ x ]
            | IsOfType(x, _, types) ->
                x
                :: (types
                    |> List.map (function
                        | Inclusive e
                        | Exclusive e -> e))
            | IsJson(x, _, _, _) -> [ x ]
            | RegexLike(x, _, p, flag) -> [ yield x; yield p; yield! Option.toList flag ]
            | Match(x, _, _, _) -> [ x ]
            | MemberOf(x, _, m) -> [ x; m ]
            | SubmultisetOf(x, _, m) -> [ x; m ]
            | IsSet(x, _) -> [ x ]
            | PeriodPredicate(_, l, r) -> [ l; r ]
            | PeriodValue(s, e) -> [ s; e ]
            | JsonExists(common, _) -> jsonCommonChildren common
            | JsonValue(common, _, onEmpty, onError) ->
                [ yield! jsonCommonChildren common
                  yield!
                      Option.toList onEmpty @ Option.toList onError
                      |> List.collect (function
                          | JsonDefault e -> [ e ]
                          | _ -> []) ]
            | JsonQuery(common, _, _, _, _, _) -> jsonCommonChildren common
            | JsonObject(nvs, _, _, _) -> nvs |> List.collect (fun (nv: JsonNameValue) -> [ nv.Name; nv.Value ])
            | JsonArray(xs, _, _) -> xs
            | JsonObjectAgg(nv, _, _, _) -> [ nv.Name; nv.Value ]
            | JsonArrayAgg(x, orderBy, _, _) ->
                [ yield x
                  yield! orderBy |> Option.defaultValue [] |> List.map (fun (e, _, _) -> e) ]
            | SetFunction(_, x) -> [ x ]
            | Grouping xs -> xs
            | GeneralizedInvocation(r, _, name, args) -> [ yield r; yield name; yield! Option.defaultValue [] args ]
            | Dereference(r, name, args) -> [ yield r; yield name; yield! Option.defaultValue [] args ]
            | RowPatternNavigation(RowPatternNavigation.Logical(_, _, x, offset)) ->
                [ yield x; yield! Option.toList offset ]
            | RowPatternNavigation(RowPatternNavigation.Physical(_, x, offset)) ->
                [ yield x; yield! Option.toList offset ]
            | RowPatternNavigation(RowPatternNavigation.Compound(_, _, _, x, logical, physical)) ->
                [ yield x; yield! Option.toList logical; yield! Option.toList physical ]
            | LengthExpression(_, x, _) -> [ x ]
            | NumericValueFunction(_, args) -> args
            | RegexOccurrences arg -> regexArgumentChildren arg
            | RegexPosition(_, arg) -> regexArgumentChildren arg
            | RegexSubstring arg -> regexArgumentChildren arg
            | RegexTransliterate arg -> regexArgumentChildren arg
            | SubstringSimilar(x, pattern, escape) -> [ x; pattern; escape ]
            | Fold(_, x) -> [ x ]
            | Transcoding(x, name) -> [ x; name ]
            | CharacterTransliteration(x, name) -> [ x; name ]
            | NormalizeFunction(x, _, length) -> [ yield x; yield! Option.toList length ]
            | SpecificTypeMethod(x, _) -> [ x ]
            | Classifier x -> Option.toList x
            | AtTimeZone(x, TimeZoneSpecifier.TimeZoneOffset zone) -> [ x; zone ]
            | AtTimeZone(x, _) -> [ x ]
            | TrimArray(x, count) -> [ x; count ]
            | DatetimeDifference(l, r, _) -> [ l; r ]
            | IntervalPrimary(x, _) -> [ x ]
            | RowValueConstructor items -> items
            | MultisetSetOperation(_, _, l, r) -> [ l; r ]
            | MultisetSetFunction x -> [ x ]
            // QuantifiedSubquery is tested by the search below; unlisted kinds (literals,
            // identifiers, ...) contribute no children.
            | _ -> []

        match work with
        | [] -> false
        | e :: rest ->
            match e.Kind with
            | QuantifiedSubquery _ -> true
            | _ -> containsStandaloneQuantifiedSubqueryIn (expressionChildren e @ rest)

    let containsStandaloneQuantifiedSubquery root =
        containsStandaloneQuantifiedSubqueryIn [ root ]

    pExpressionRef.Value <-
        pBooleanValueExpression
        >>= fun e ->
            if containsStandaloneQuantifiedSubquery e then
                fail "quantified subquery requires a comparison operator"
            else
                preturn e

    // 10.6 <routine type> / 11.51 <partial method specification> — [ INSTANCE | STATIC | CONSTRUCTOR ]
    // Shared by 10.6 (<routine type>), 11.60 (<method specification designator>) and
    // 11.51 (<partial method specification>), all defined in SchemaParser.fs; it lives here
    // because ExpressionParser.fs is compiled before that module. INSTANCE and CONSTRUCTOR are not
    // reserved words, hence the `attempt`s. Placed after the 6.1 family so the file stays in clause order.
    let pMethodKind: Parser<MethodKind, unit> =
        choice
            [ attempt (pKeyword "INSTANCE" >>% MethodKind.Instance)
              attempt (pKeyword "STATIC" >>% MethodKind.Static)
              attempt (pKeyword "CONSTRUCTOR" >>% MethodKind.Constructor) ]
