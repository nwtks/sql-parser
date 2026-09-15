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
    let pValueExpressionNoBoolean, pValueExpressionNoBooleanRef =
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

    // 6.1 <character string type> ::= CHARACTER [ ( <character length> ) ] | CHAR [ ( <length> ) ] | CHARACTER VARYING ( <length> ) | VARCHAR ( <length> ) | <character large object type>
    let pCharacterType =
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
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
        |>> fun (typ, len) -> typ len

    // 6.1 <national character string type> ::= NATIONAL CHARACTER [ ( <character length> ) ] | NCHAR [ ... ] | NATIONAL CHARACTER VARYING ... | <national character large object type>
    let pNationalCharacterType =
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
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
        |>> fun (typ, len) -> typ len

    // 6.1 <binary string type> ::= BINARY [ ( <length> ) ] | BINARY VARYING ( <length> ) | VARBINARY ( <length> ) | <binary large object string type>
    let pBinaryType =
        choice
            [ attempt (pKeyword "BINARY" .>> pKeyword "VARYING") >>% VarBinary
              pKeyword "VARBINARY" >>% VarBinary
              attempt (pKeyword "BINARY" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
              >>% BinaryLargeObject
              pKeyword "BLOB" >>% BinaryLargeObject
              pKeyword "BINARY" >>% Binary ]
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
        |>> fun (typ, len) -> typ len

    // 6.1 <exact numeric type> ::= NUMERIC [ ( <precision> [ , <scale> ] ) ] | DECIMAL [ ... ] | DEC [ ... ] | SMALLINT | INTEGER | INT | BIGINT  —  <decimal floating-point type> ::= DECFLOAT [ ( <precision> ) ]
    let pNumericType =
        let pPrecScale =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (pUnsignedInteger .>>. opt (token (pstring ",") >>. pUnsignedInteger))
            |>> fun (p, s) -> Some(int p), Option.map int s

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
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
              |>> DecFloat
              pKeyword "SMALLINT" >>% SmallInt
              pKeyword "INTEGER" >>% Integer
              pKeyword "INT" >>% Integer
              pKeyword "BIGINT" >>% BigInt ]

    // 6.1 <approximate numeric type> ::= FLOAT [ ( <precision> ) ] | REAL | DOUBLE PRECISION
    let pApproximateNumericType =
        choice
            [ pKeyword "FLOAT"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
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
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
              .>>. pTz
              |>> fun (p, tz) -> TimeType(p, tz)
              pKeyword "TIMESTAMP"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
              .>>. pTz
              |>> fun (p, tz) -> TimestampType(p, tz) ]

    // 6.1 <interval type> ::= INTERVAL <interval qualifier>
    let pIntervalType = pKeyword "INTERVAL" >>. pIntervalQualifier |>> IntervalType

    // 6.7 <column reference> / 5.4 <identifier> — <identifier> | <column reference>
    let pIdentifierExpr = pIdentifier |>> Identifier |> withExprPosition

    // 6.1 <row type> ::= ROW <row type body> — <row type body> ::= ( <field definition> [ { , <field definition> }... ] )
    let pRowType =
        pKeyword "ROW"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 (pIdentifierExpr .>>. pDataType) (token (pstring ",")))
        |>> RowType

    // 5.4 <schema qualified name>
    let pQualifiedNameExpr =
        pSchemaQualifiedName
        |>> function
            | [ s ] -> Identifier s
            | parts -> ColumnReference parts
        |> withExprPosition

    // 6.1 <scope clause> ::= SCOPE <table name>
    // Shared by <reference type> (6.1), <column option list> (11.3) and
    // <add column scope clause> (11.17); it lives here because ExpressionParser.fs is
    // compiled before SchemaParser.fs.
    let pScopeClause: Parser<Expression, unit> = pKeyword "SCOPE" >>. pQualifiedNameExpr

    // 6.1 <data type> — element type parser: all types EXCEPT collection types (to avoid
    // left recursion). The recursive REF / row-field positions use the `pDataType`
    // forward ref, so this parser can be defined before `pCollectionType`.
    let pDataTypeElement: Parser<DataType, unit> =
        choice
            [ attempt pCharacterType
              attempt pNationalCharacterType
              attempt pBinaryType
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

    // 6.1 <collection type> ::= <array type> | <multiset type> — <array type> ::= <data type> ARRAY [ [ <maximum cardinality> ] ] — <multiset type> ::= <data type> MULTISET
    // The suffixes are applied left-to-right and may nest (`INT ARRAY ARRAY` =
    // `ArrayType(ArrayType(Integer, None), None)`), because <data type> on the left of
    // ARRAY/MULTISET may itself be a collection type.
    let pCollectionType =
        let pArraySuffix =
            pKeyword "ARRAY"
            .>>. opt (between (token (pstring "[")) (token (pstring "]")) pUnsignedInteger)
            |>> fun (_, len) -> fun t -> ArrayType(t, Option.map int len)

        let pMultisetSuffix: Parser<DataType -> DataType, unit> =
            pKeyword "MULTISET" >>% (fun t -> MultisetType t)

        pDataTypeElement .>>. many (choice [ pArraySuffix; pMultisetSuffix ])
        |>> fun (t, suffixes) -> List.fold (fun acc f -> f acc) t suffixes

    pDataTypeRef.Value <- choice [ attempt pCollectionType; pDataTypeElement ]

    // 5.1 <left bracket> ::= [ | ??( — and <right bracket> ::= ] | ??)
    let pLeftBracket = pstring "[" <|> pstring "??("
    // 5.1 <right bracket> ::= ] | ??)
    let pRightBracket = pstring "]" <|> pstring "??)"

    // 5.3 <literal> ::= NULL | <character string literal> | <numeric literal>
    //     | <boolean literal> | <datetime literal> | <interval literal> | <hex string literal>
    let pLiteralExpr =
        choice
            [ attempt (pKeyword "NULL" >>% Null |>> Literal)
              attempt (pCharacterStringLiteral |>> String |>> Literal)
              attempt (pNationalCharacterStringLiteral |>> NationalString |>> Literal)
              attempt (pUnicodeCharacterStringLiteral |>> UnicodeString |>> Literal)
              attempt (pUnsignedNumericLiteral |>> Number |>> Literal)
              attempt (pBooleanLiteral |>> Bool |>> Literal)
              attempt (pDateLiteral |>> Date |>> Literal)
              attempt (pTimeLiteral |>> Time |>> Literal)
              attempt (pTimestampLiteral |>> Timestamp |>> Literal)
              attempt (pIntervalLiteral |>> Interval |>> Literal)
              attempt (pBinaryStringLiteral |>> Literal.Binary |>> Literal) ]
        |> withExprPosition

    // 6.4 <dynamic parameter specification> ::= <question mark>
    let pQuestionMark: Parser<char, unit> = pchar '?' .>> ws

    // Literals are handled separately by pLiteralExpr above.
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
              pKeyword "CURRENT_TRANSFORM_GROUP_FOR_TYPE" >>. pQualifiedNameExpr
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
    let pSimpleValueSpecification =
        choice
            [ pLiteralExpr
              pQuestionMark >>% "?" <|> pHostParameter |>> Parameter |> withExprPosition
              pQualifiedNameExpr ]

    // 6.5 <default specification> ::= DEFAULT — only valid in specific contexts (INSERT VALUES, UPDATE SET), not as a general expression. This parser is used by the DML parser for those contexts.
    let pDefaultValue: Parser<Expression, unit> =
        pKeyword "DEFAULT" >>% Default |> withExprPosition

    // A dotted identifier chain stops before '. <identifier> ( ... )' so that a
    // method invocation like a.b.method(x) parses as a column reference (a.b)
    // followed by a method-invocation postfix, while a.b.c (no parens) still
    // parses as a single ColumnReference.
    // 6.7 <column reference> ::= [ <table name> <period> ] <column name>
    let pColumnReferenceExpr =
        pIdentifier
        .>>. many (attempt (token (pstring ".") >>. pIdentifier .>>? notFollowedBy (token (pstring "("))))
        |>> function
            | id, [] -> Identifier id
            | first, rest -> ColumnReference(first :: rest)
        |> withExprPosition

    // 6.9 <grouping operation> ::= GROUPING ( <column reference> [ , <column reference> ]... )
    let pGroupingOperation =
        pKeyword "GROUPING"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
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

    // 7.8 <row pattern measure definition> ::= <row pattern measure expression> AS <measure name>
    let pRowPatternMeasure =
        pExpression .>> pKeyword "AS" .>>. pIdentifierExpr
        |>> fun (expr, name) ->
            { RowPatternMeasure.Expression = expr
              Name = name }

    // 7.8 <row pattern measures> ::= MEASURES <row pattern measure list>
    let pRowPatternMeasures =
        pKeyword "MEASURES" >>. sepBy1 pRowPatternMeasure (token (pstring ","))

    // 7.9 <row pattern> — forward ref (recursive)
    let pRowPattern, pRowPatternRef = createParserForwardedToRef<RowPattern, unit> ()

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
              pIdentifierExpr |>> RowPatternVariable ]

    // 7.9 <row pattern factor> ::= <row pattern primary> [ <row pattern quantifier> ]
    let pRowPatternFactor =
        pRowPatternPrimary .>>. opt (attempt pRowPatternQuantifier)
        |>> fun (primary, quant) ->
            { Primary = primary
              Quantifier = quant }

    // 7.9 <row pattern term> ::= <row pattern factor> | <row pattern term> <row pattern factor>
    let pRowPatternTerm =
        many1 pRowPatternFactor |>> fun factors -> { Factors = factors }

    // 7.9 <row pattern> ::= <row pattern term> | <row pattern alternation>
    pRowPatternRef.Value <- sepBy1 pRowPatternTerm (token pVerticalBar) |>> fun terms -> { Terms = terms }

    // 7.9 <row pattern skip to>
    let pRowPatternSkipTo =
        pKeyword "SKIP"
        >>. pKeyword "TO"
        >>. choice
                [ attempt (pKeyword "NEXT" >>. pKeyword "ROW" >>% SkipToNextRow)
                  attempt (pKeyword "PAST" >>. pKeyword "LAST" >>. pKeyword "ROW" >>% SkipPastLastRow)
                  attempt (pKeyword "FIRST" >>. pIdentifierExpr |>> SkipToFirst)
                  attempt (pKeyword "LAST" >>. pIdentifierExpr |>> SkipToLast)
                  pIdentifierExpr |>> SkipTo ]

    // 7.9 <row pattern subset item> ::= <var> = ( <var> [ , <var> ]... )
    let pRowPatternSubset =
        pIdentifierExpr .>> token (pstring "=")
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ",")))
        |>> fun (name, vars) -> { Name = name; Variables = vars }

    // 7.9 <row pattern definition> ::= <var> AS <search condition>
    let pRowPatternDefinition =
        pIdentifierExpr .>> pKeyword "AS" .>>. pExpression
        |>> fun (name, cond) -> { Name = name; Condition = cond }

    // 7.9 <row pattern common syntax> ::= [ AFTER MATCH <skip to> ] [ INITIAL | SEEK ]
    //     PATTERN ( <row pattern> ) [ <subset clause> ] DEFINE <definition list>
    let pRowPatternCommon =
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
    let pWindowFrame =
        // 7.15 <window frame units> ::= ROWS | RANGE | GROUPS
        let pUnit =
            pKeyword "ROWS" >>% Rows
            <|> (pKeyword "RANGE" >>% Range)
            <|> (pKeyword "GROUPS" >>% Groups)

        // 7.15 <window frame bound> ::= UNBOUNDED PRECEDING | UNBOUNDED FOLLOWING | CURRENT ROW | <value expression> PRECEDING | <value expression> FOLLOWING
        let pBound =
            choice
                [ attempt (pKeyword "UNBOUNDED" >>. pKeyword "PRECEDING" >>% UnboundedPreceding)
                  attempt (pKeyword "UNBOUNDED" >>. pKeyword "FOLLOWING" >>% UnboundedFollowing)
                  attempt (pKeyword "CURRENT" >>. pKeyword "ROW" >>% CurrentRow)
                  attempt (pExpression .>> pKeyword "PRECEDING" |>> Preceding)
                  attempt (pExpression .>> pKeyword "FOLLOWING" |>> Following) ]

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
                  pKeyword "BETWEEN" >>. pBound .>> pKeyword "AND" .>>. pBound
                  |>> fun (s, e) -> s, Some e
              )
              pBound |>> fun s -> s, None ]
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
    let pOrderByItem =
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
            pKeyword "PARTITION"
            >>. pKeyword "BY"
            >>. sepBy1 pExpression (token (pstring ","))

        let pOrderBy =
            pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ","))

        // 7.15 — an <existing window name> cannot be MEASURES when MEASURES starts a
        // <row pattern measures> clause (the optional leading clause of the
        // <window frame clause>). Reject MEASURES followed by <expr> AS <name> so the
        // frame's MEASURES clause is not swallowed as a window name. The whole parser
        // is wrapped in attempt because notFollowedBy marks its failure as fatal, which
        // would otherwise escape the surrounding opt.
        let pExistingWindowName =
            attempt (
                pIdentifierExpr
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
                  .>>. opt pWindowFrame
                  |>> fun (((name, pb), ob), frame) ->
                      { ExistingWindowName = name
                        PartitionBy = Option.defaultValue [] pb
                        OrderBy = Option.defaultValue [] ob
                        Frame = frame })
             <|> (pIdentifierExpr
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
    let pValueOfFunction =
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

    // 6.12 <case abbreviation> ::= COALESCE ( <value expression> [ { , <value expression> }... ] )
    let pCoalesceExpr =
        pKeyword "COALESCE"
        >>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pExpression (token (pstring ",")))
        |>> fun exprs -> Case(None, exprs |> List.map (fun e -> { Kind = IsNull(e, true); Pos = e.Pos }, e), None)
        |> withExprPosition

    // 6.12 <case expression> ::= CASE <case operand> <simple when clause>...
    //     | CASE <searched when clause>... [ ELSE <result> ] END
    let pCaseExpression =
        getPosition
        >>= fun pos ->
            let pResultExpr =
                pExpression
                <|> (pKeyword "NULL"
                     >>% { Kind = Literal Null
                           Pos = { Line = pos.Line; Column = pos.Column } })

            let pSimpleWhenClause =
                pKeyword "WHEN" >>. sepBy1 pExpression (token (pstring ",")) .>> pKeyword "THEN"
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
        pKeyword "NEXT" >>. pKeyword "VALUE" >>. pKeyword "FOR" >>. pQualifiedNameExpr
        |>> NextValueFor
        |> withExprPosition

    // 6.16 <subtype treatment> ::= TREAT ( <subtype operand> AS <target subtype> )
    let pTreatExpression =
        pKeyword "TREAT"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        |>> (fun (e, t) -> Treat(e, t))
        |> withExprPosition

    // 10.4 <SQL argument list> (plain — no DISTINCT/ALL; used by <method invocation>,
    // <static method invocation> and <new specification>)
    let pValueExpressionList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy pExpression (token (pstring ",")))

    let pMethodOrFieldReference =
        // 6.32 <specific type method> ::= <user-defined type value expression> <period> SPECIFICTYPE [ ( ) ]
        // SPECIFICTYPE is a reserved word, so it cannot be reached through pIdentifierExpr below.
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
        <|> (token (pstring ".") >>. pIdentifierExpr .>>. opt pValueExpressionList
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
        .>>. (token (pstring ".") >>. pIdentifierExpr .>>. opt pValueExpressionList)
        |>> fun ((operand, typ), (name, args)) -> GeneralizedInvocation(operand, typ, name, args)
        |> withExprPosition

    // 6.18 <static method invocation> ::= <path-resolved UDT name> :: <method name>
    //     [ <SQL argument list> ]
    let pStaticMethodInvocation =
        pQualifiedNameExpr
        .>>. (token (pstring "::") >>. pIdentifierExpr .>>. pValueExpressionList)
        |>> fun (typ, (name, args)) -> StaticMethodInvocation(typ, name, args)
        |> withExprPosition

    // 6.19 <new specification> ::= NEW <path-resolved UDT name> <SQL argument list>
    let pNewSpecification =
        pKeyword "NEW" >>. pQualifiedNameExpr .>>. pValueExpressionList
        |>> fun (typ, args) -> NewSpecification(typ, args)
        |> withExprPosition

    // 6.20 <attribute or method reference> / 6.21 <dereference operation> / 6.22 <method reference>
    //   <value expression primary> <dereference operator> <qualified identifier> [ <SQL argument list> ]
    // The dereference operator is the right arrow. It is matched here (inside the term parser) so
    // that the '-' operator of the precedence parser never sees it.
    let pDereferenceReference =
        token (pstring "->") >>. pQualifiedNameExpr .>>. opt pValueExpressionList
        |>> fun (name, args) ->
            fun r ->
                { Expression.Kind = Dereference(r, name, args)
                  Pos = r.Pos }

    // 6.23 <reference resolution> ::= DEREF ( <reference value expression> )
    let pDerefExpression =
        pKeyword "DEREF"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> Deref
        |> withExprPosition

    // 6.24 <array element reference> — postfix [ <numeric value expression> ]
    let pArrayElementReference =
        between (token pLeftBracket) (token pRightBracket) pExpression
        |>> fun idx ->
            fun e ->
                { Expression.Kind = ArrayElement(e, idx)
                  Pos = e.Pos }

    // 6.25 <multiset element reference> ::= ELEMENT ( <multiset value expression> )
    let pElementExpression =
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
        pValueExpressionNoBoolean .>>. opt pJsonInputClause .>> pKeyword "AS"
        .>>. pIdentifierExpr
        |>> fun ((value, inputFormat), name) ->
            { JsonPassingArgument.Value = value
              InputFormat = inputFormat
              Name = name }

    // 10.14 <JSON API common syntax> ::= <JSON context item> , <JSON path specification>
    //     [ AS <JSON table path name> ] [ <JSON passing clause> ]
    // <JSON context item> ::= <JSON value expression> (value expression — no boolean ops,
    // plus an optional FORMAT clause)
    // <JSON path specification> ::= <character string literal> — stored as a plain string.
    let pJsonApiCommon =
        pValueExpressionNoBoolean .>>. opt pJsonInputClause .>> token (pstring ",")
        .>>. pCharacterStringLiteral
        .>>. opt (attempt (pKeyword "AS" >>. pIdentifierExpr))
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
                  pKeyword "DEFAULT" >>. pValueExpressionNoBoolean |>> JsonDefault ]

        pKeyword "JSON_VALUE"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pJsonApiCommon
                 .>>. opt (pKeyword "RETURNING" >>. pDataType)
                 .>>. opt (pJsonValueBehavior .>> pKeyword "ON" .>> pKeyword "EMPTY")
                 .>>. opt (pJsonValueBehavior .>> pKeyword "ON" .>> pKeyword "ERROR"))
        |>> fun (((common, returning), onEmpty), onError) -> JsonValue(common, returning, onEmpty, onError)
        |> withExprPosition

    // 6.30 <extract expression> ::= EXTRACT <left paren> <extract field> FROM <extract source> <right paren>
    let pExtractExpression =
        pKeyword "EXTRACT"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (getPosition
                 .>>. (choice
                     [ pKeyword "YEAR" >>% "YEAR"
                       pKeyword "MONTH" >>% "MONTH"
                       pKeyword "DAY" >>% "DAY"
                       pKeyword "HOUR" >>% "HOUR"
                       pKeyword "MINUTE" >>% "MINUTE"
                       pKeyword "SECOND" >>% "SECOND"
                       pIdentifierRaw ])
                 .>> pKeyword "FROM"
                 .>>. pExpression)
        |>> fun ((pos, field), src) ->
            Extract(
                { Kind = Identifier field
                  Pos = { Line = pos.Line; Column = pos.Column } },
                src
            )
        |> withExprPosition

    // 6.30 <position expression> ::= POSITION ( <character value expression> IN
    //     <character value expression> [ USING <char length units> ] )
    let pPositionExpression =
        pKeyword "POSITION"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "IN"
                 .>>. pExpression
                 .>>. opt (pKeyword "USING" >>. pIdentifierExpr))
        |>> (fun ((target, source), unit) -> ExpressionKind.Position(target, source, unit))
        |> withExprPosition

    // 6.30 <length expression> ::= <char length expression> | <octet length expression>
    //   <char length expression> ::= { CHAR_LENGTH | CHARACTER_LENGTH } ( <character value expression>
    //       [ USING <char length units> ] )
    //   <octet length expression> ::= OCTET_LENGTH ( <string value expression> )
    let pLengthExpression =
        let pBody =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>>. opt (pKeyword "USING" >>. pIdentifierRaw))

        choice
            [ pKeyword "CHAR_LENGTH" >>. pBody
              |>> fun (e, units) -> LengthExpression(LengthFunction.CharLength, e, units)
              pKeyword "CHARACTER_LENGTH" >>. pBody
              |>> fun (e, units) -> LengthExpression(LengthFunction.CharacterLength, e, units)
              pKeyword "OCTET_LENGTH" >>. pBody
              |>> fun (e, units) -> LengthExpression(LengthFunction.OctetLength, e, units) ]
        |> withExprPosition

    // 6.30 <numeric value function> — the built-ins of the shape <name> ( <args> )
    let pNumericValueFunction =
        let pUnary name ctor =
            pKeyword name
            >>. between (token (pstring "(")) (token (pstring ")")) pExpression
            |>> fun e -> NumericValueFunction(ctor, [ e ])

        let pBinary name ctor =
            pKeyword name
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (pExpression .>> token (pstring ",") .>>. pExpression)
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
                      (pExpression .>> token (pstring ",") .>>. pExpression .>> token (pstring ",")
                       .>>. pExpression
                       .>> token (pstring ",")
                       .>>. pExpression)
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
        let pOperand = pValueExpressionNoBoolean

        let pOccurrence =
            choice
                [ attempt (pKeyword "ALL" >>% RegexOccurrenceAll)
                  pExpression |>> RegexOccurrenceNumber ]

        pOperand .>>. opt (attempt (pKeyword "FLAG" >>. pOperand)) .>> pKeyword "IN"
        .>>. pOperand
        .>>. opt (attempt (pKeyword "WITH" >>. pOperand))
        .>>. opt (attempt (pKeyword "FROM" >>. pOperand))
        .>>. opt (attempt (pKeyword "USING" >>. pIdentifierRaw))
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
    let pCharacterSubstringFunction =
        pKeyword "SUBSTRING"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "FROM"
                 .>>. pExpression
                 .>>. opt (pKeyword "FOR" >>. pExpression)
                 .>>. opt (pKeyword "USING" >>. pIdentifierRaw))
        |>> fun (((src, start), len), units) -> Substring(src, start, len, units)
        |> withExprPosition

    // 6.32 <character overlay function> ::= OVERLAY ( <character value expression> PLACING <character value expression> FROM <start position> [ FOR <string length> ] )
    let pOverlayFunction =
        pKeyword "OVERLAY"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "PLACING" .>>. pExpression .>> pKeyword "FROM"
                 .>>. pExpression
                 .>>. opt (pKeyword "FOR" >>. pExpression))
        |>> fun (((src, placing), start), len) -> Overlay(src, placing, start, len)
        |> withExprPosition

    // 6.32 <regular expression substring function> ::= SUBSTRING ( <character value expression>
    //     SIMILAR <character value expression> ESCAPE <escape character> )
    let pSubstringSimilarFunction =
        pKeyword "SUBSTRING"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pValueExpressionNoBoolean .>> pKeyword "SIMILAR" .>>. pValueExpressionNoBoolean
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
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "USING" .>>. pIdentifierExpr)
        |>> fun (e, name) -> Transcoding(e, name)
        |> withExprPosition

    // 6.32 <character transliteration> ::= TRANSLATE ( <character value expression>
    //     USING <transliteration name> )
    let pCharacterTransliterationFunction =
        pKeyword "TRANSLATE"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "USING" .>>. pIdentifierExpr)
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
                  opt (pKeyword "KEY") .>>. pValueExpressionNoBoolean .>> pKeyword "VALUE"
                  .>>. pValueExpressionNoBoolean
                  |>> fun ((key, name), value) ->
                      { Name = name
                        Value = value
                        Key = Option.isSome key }
              )
              pValueExpressionNoBoolean .>> token (pstring ":") .>>. pValueExpressionNoBoolean
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
                (pJsonApiCommon
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
            opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)

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
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> token (pstring ",") .>>. pExpression)
        |>> fun (arr, count) -> TrimArray(arr, count)
        |> withExprPosition

    // 6.42 <array value constructor> ::= ARRAY <array value constructor by enumeration>
    //     | ARRAY <array value constructor by query>
    let pArrayValueConstructor =
        pKeyword "ARRAY"
        >>. choice
                [ attempt (
                      between (token pLeftBracket) (token pRightBracket) (sepBy pExpression (token (pstring ",")))
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
    let pMultisetValueConstructor =
        pKeyword "MULTISET"
        >>. choice
                [ attempt (
                      between (token pLeftBracket) (token pRightBracket) (sepBy pExpression (token (pstring ",")))
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

    // plus optional OVER (window), FILTER (WHERE), WITHIN GROUP (ORDER BY) clauses.
    // 10.4 <routine invocation> ::= <routine name> <SQL argument list>
    // 10.9 <aggregate function> — names used by <aggregate function>, <binary set function> and
    // <hypothetical set function>. Shared by the 10.4 <routine invocation> reserved-name whitelist
    // and by the 6.9 <set function specification> RUNNING/FINAL prefix check.
    let aggregateFunctionKeywords =
        [ "AVG"
          "MAX"
          "MIN"
          "SUM"
          "EVERY"
          "ANY"
          "SOME"
          "COUNT"
          "STDDEV_POP"
          "STDDEV_SAMP"
          "VAR_SAMP"
          "VAR_POP"
          "COLLECT"
          "FUSION"
          "INTERSECTION"
          "COVAR_POP"
          "COVAR_SAMP"
          "CORR"
          "REGR_SLOPE"
          "REGR_INTERCEPT"
          "REGR_COUNT"
          "REGR_R2"
          "REGR_AVGX"
          "REGR_AVGY"
          "REGR_SXX"
          "REGR_SYY"
          "REGR_SXY"
          "RANK"
          "DENSE_RANK"
          "PERCENT_RANK"
          "CUME_DIST"
          "LISTAGG"
          "ARRAY_AGG" ]

    let aggregateFunctionNames = Set.ofList aggregateFunctionKeywords

    let pRoutineInvocation =
        let pArgs =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
                 .>>. sepBy pExpression (token (pstring ",")))

        let pFilter =
            pKeyword "FILTER"
            >>. between (token (pstring "(")) (token (pstring ")")) (pKeyword "WHERE" >>. pExpression)

        let pWithinGroup =
            pKeyword "WITHIN"
            >>. pKeyword "GROUP"
            >>. between
                    (token (pstring "("))
                    (token (pstring ")"))
                    (pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ",")))

        // 10.4 <routine name> ::= [ <schema name> <period> ] <qualified identifier>
        // — <qualified identifier> is a <nonreserved qualifier>, so a reserved word
        // cannot normally name a routine. However the standard also spells a large
        // family of built-in functions using *reserved* keywords (<aggregate function>,
        // <window function type>, <inverse distribution function type>,
        // <numeric value function>, <string value function>, <array value function>,
        // <multiset value function>, <grouping operation>). Those keywords are
        // whitelisted here so they still parse as routine invocations; every other
        // reserved word (EXISTS, UNIQUE, PERIOD, VALUE_OF, SELECT, ...) is rejected,
        // and the dedicated parsers for the special forms are tried before this one.
        let functionKeywords =
            [ // <aggregate function>
              // <binary set function>
              // <hypothetical set function>
              yield! aggregateFunctionKeywords
              // <inverse distribution function type>
              "PERCENTILE_CONT"
              "PERCENTILE_DISC"
              // <window function type>
              "ROW_NUMBER"
              "NTILE"
              "LEAD"
              "LAG"
              "FIRST_VALUE"
              "LAST_VALUE"
              "NTH_VALUE" ]

        let pReservedFunctionName: Parser<string, unit> =
            functionKeywords
            |> List.map pKeyword
            |> choice
            >>= fun kw ->
                if reservedWords.Contains kw then
                    preturn kw
                else
                    fail "not a reserved function keyword."

        // 5.4 <identifier> — regular (non-reserved), delimited, or reserved function keyword
        let pRoutineName: Parser<string, unit> =
            choice
                [ attempt pReservedFunctionName
                  attempt pUnicodeDelimitedIdentifier
                  pRegularIdentifier
                  pDelimitedIdentifier ]

        let nameExpr =
            getPosition .>>. pRoutineName
            |>> fun (pos, name) ->
                { Expression.Kind = Identifier name
                  Pos = { Line = pos.Line; Column = pos.Column } }

        nameExpr
        .>>. pArgs
        .>>. opt pWindowNameOrSpecification
        .>>. opt pFilter
        .>>. opt pWithinGroup
        |>> fun ((((name, (dist, args)), window), filter), withinGroup) ->
            match window with
            | Some w ->
                WindowFunction
                    { Function = name
                      Args = args
                      IsDistinct = Option.defaultValue false dist
                      Window = w }
            | None -> FunctionCall(name, Option.defaultValue false dist, args, None, filter, withinGroup)
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
                 .>>. opt (pKeyword "ORDER" >>. pKeyword "BY" >>. sepBy1 pOrderByItem (token (pstring ",")))
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

    // 7.17 <with list element> ::= <query name> [ ( <with column list> ) ] AS <table subquery> [ <search or cycle clause> ]
    let pCte =
        // 7.18 <search clause> ::= SEARCH { DEPTH FIRST | BREADTH FIRST } BY <cols> SET <col>
        // Local because pCte is the only consumer of the 7.18 clauses.
        let pSearchClause =
            pKeyword "SEARCH"
            >>. (attempt (pKeyword "DEPTH" >>. pKeyword "FIRST" >>% true)
                 <|> (pKeyword "BREADTH" >>. pKeyword "FIRST" >>% false))
            .>> pKeyword "BY"
            .>>. sepBy1 pIdentifierExpr (token (pstring ","))
            .>> pKeyword "SET"
            .>>. pIdentifierExpr
            |>> fun ((isDepthFirst, orderBy), setCol) ->
                { IsDepthFirst = isDepthFirst
                  OrderBy = orderBy
                  SetColumn = setCol }

        // 7.18 <cycle clause> ::= CYCLE <cols> SET <col> TO <mark> DEFAULT <default> USING <path>
        let pCycleClause =
            pKeyword "CYCLE" >>. sepBy1 pIdentifierExpr (token (pstring ","))
            .>> pKeyword "SET"
            .>>. pIdentifierExpr
            .>> pKeyword "TO"
            .>>. pExpression
            .>> pKeyword "DEFAULT"
            .>>. pExpression
            .>> pKeyword "USING"
            .>>. pIdentifierExpr
            |>> fun ((((cols, setCol), mark), defaultVal), path) ->
                { CycleColumns = cols
                  SetColumn = setCol
                  MarkValue = mark
                  DefaultValue = defaultVal
                  PathColumn = path }

        pIdentifierExpr
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) (sepBy1 pIdentifierExpr (token (pstring ","))))
        .>> pKeyword "AS"
        .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
        .>>. (opt (attempt pSearchClause) .>>. opt (attempt pCycleClause))
        |>> fun (((name, cols), q), (search, cycle)) ->
            { Cte.Name = name
              Columns = cols
              Query = q
              SearchClause = search
              CycleClause = cycle }

    // 7.17 <with clause> ::= WITH [ RECURSIVE ] <with list>
    let pWithClause =
        pKeyword "WITH" >>. opt (pKeyword "RECURSIVE" >>% true)
        .>>. sepBy1 pCte (token (pstring ","))
        |>> fun (recu, ctes) -> Option.defaultValue false recu, ctes

    // — the atomic building block of every <value expression>, used as the term parser of the operator-precedence parser below.
    // 6.3 <value expression primary> — the atomic building block of every <value expression>
    // The full form includes two grammar-exceeding approximations: the §8 predicate atoms
    // (pPredicatePrimary, needed so ANY/SOME/ALL (subquery) beats the pRoutineInvocation
    // whitelist and EXISTS/UNIQUE/PERIOD/JSON_EXISTS parse) and the 7.16 <asterisk>
    // wildcard (needed by SELECT item lists). Contexts that require the grammar's
    // <value expression primary> shape — the 6.35/6.37 datetime & interval chain — use
    // pValueExpressionPrimaryStrict, which excludes both (see docs/trade-off.md).
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

        // 7.16 <asterisk> ::= * — also used as <value expression primary> wildcard
        let pStarExpr = pstring "*" .>> ws >>% ExpressionKind.Star |> withExprPosition

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
              attempt pSubstringSimilarFunction
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
              attempt pTreatExpression
              attempt pDerefExpression
              attempt pElementExpression
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
              attempt pValueOfFunction
              attempt pRowPatternNavigationOperation
              attempt pGroupingOperation
              attempt pSetFunctionSpecification
              attempt pRoutineInvocation
              attempt pScalarSubquery
              attempt pLiteralExpr
              attempt pGeneralValueSpecification
              if withPredicates then
                  attempt pStarExpr
              attempt pGeneralizedInvocation
              attempt pDatetimeDifference
              attempt pExplicitRowValueConstructor
              pColumnReferenceExpr
              between (token (pstring "(")) (token (pstring ")")) pExpression ]
        // The postfix loop must be able to leave a '.' behind (e.g. the `.*` of
        // <all fields reference>, 7.16), so both alternatives are backtracking.
        .>>. many (attempt pDereferenceReference <|> attempt pMethodOrFieldReference)
        |>> fun (e, refs) -> List.fold (fun acc f -> f acc) e refs

    let pValueExpressionPrimary = pValueExpressionPrimaryImpl true

    let pValueExpressionPrimaryStrict = pValueExpressionPrimaryImpl false

    // 6.37 <interval primary> ::= <value expression primary> [ <interval qualifier> ]
    //     | <interval value function>
    // A qualifier-less <interval primary> is represented by its <value expression primary>,
    // so wrapping only happens when an <interval qualifier> is actually present.
    // The strict primary is used: predicates and the '*' wildcard are not
    // <value expression primary>s and are rejected in interval context.
    let pIntervalPrimary =
        pValueExpressionPrimaryStrict .>>. opt (attempt pIntervalQualifier)
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
    // The '*'/'/' right operand is the grammar's <factor> (6.29), i.e. [ <sign> ]
    // <numeric primary>. <numeric primary> is approximated by <value expression primary>
    // (it already subsumes <numeric value function>, while <interval value function> is
    // syntactically identical to it — see pNumericValueFunction). Unlike <interval factor>,
    // <factor> has no [ <interval qualifier> ] suffix, so `INTERVAL '1' DAY * ? DAY` is
    // rejected. The 4th alternative is still covered: <value expression primary> accepts an
    // interval literal on the right of `*`.
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

        let pNumericFactor =
            opt (attempt pIntervalSign) .>>. pValueExpressionPrimaryStrict
            |>> fun (sign, e) -> pIntervalSigned sign e

        pIntervalFactor .>>. many (attempt (pMul <|> pDiv .>>. pNumericFactor))
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
    // The strict primary is used: predicates and the '*' wildcard are not
    // <value expression primary>s and are rejected in datetime context.
    let pDatetimeTerm =
        pValueExpressionPrimaryStrict .>>. opt (attempt pTimeZoneSuffix)
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
                    .>>. pValueExpressionPrimaryStrict
                )
            )

        let pTerm =
            pValueExpressionPrimaryStrict .>>. pIntersectChain
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
    // The base is a <multiset primary>, so the strict primary is used.
    pMultisetValueExpressionRef.Value <-
        pValueExpressionPrimaryStrict .>>. many (attempt pMultisetSetOperatorSuffix)
        |>> fun (first, rest) -> rest |> List.fold (fun acc f -> f acc) first

    // 6.28 <value expression> / 6.29 <numeric value expression> / 6.31 <string value expression>
    // Operator-precedence parser for <value expression> (terms, factors, concatenation, comparison)
    let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
    opp.TermParser <- pValueExpressionPrimary

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
    // (surfaced through the pPredicatePrimary forward ref used by pValueExpressionPrimary).
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
    let pNotExpr, pNotExprRef = createParserForwardedToRef<Expression, unit> ()

    pNotExprRef.Value <-
        (attempt (pKeyword "NOT" >>. pNotExpr)
         |>> fun e ->
             { Expression.Kind = UnaryOp(Not, e)
               Pos = e.Pos })
        <|> pBooleanTest

    // 6.39 <boolean term> ::= <boolean factor> | <boolean term> AND <boolean factor>
    let pAndExpr =
        chainl1
            pNotExpr
            (pKeyword "AND"
             >>% fun l r ->
                 { Expression.Kind = BinaryOp(And, l, r)
                   Pos = l.Pos })

    // 6.39 <boolean value expression> ::= <boolean term> | <boolean value expression> OR <boolean term>
    let pOrExpr =
        chainl1
            pAndExpr
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
        pOrExpr
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
