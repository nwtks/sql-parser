namespace SqlParser

open FParsec
open SqlParser.Lexer

module ExpressionParser =
    // 6.3 <value expression primary> helper — attaches source position to an ExpressionKind
    let withExprPosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Expression.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 6.10 <window function type> — these keywords are only valid with an OVER clause.
    let private windowOnlyFunctionNames =
        Set.ofList
            [ "ROW_NUMBER"
              "NTILE"
              "LEAD"
              "LAG"
              "FIRST_VALUE"
              "LAST_VALUE"
              "NTH_VALUE" ]

    // 6.10 <rank function type> — RANK | DENSE_RANK | PERCENT_RANK | CUME_DIST.
    // Used both as a <window function type> (empty parens + OVER) and as a
    // <hypothetical set function> (>= 1 arguments + WITHIN GROUP).
    let private rankFunctionNames =
        Set.ofList [ "RANK"; "DENSE_RANK"; "PERCENT_RANK"; "CUME_DIST" ]

    // 10.9 <inverse distribution function type> — WITHIN GROUP is required.
    let private inverseDistributionFunctionNames =
        Set.ofList [ "PERCENTILE_CONT"; "PERCENTILE_DISC" ]

    // 10.9 <binary set function type> — exactly two arguments.
    let private binarySetFunctionNames =
        Set.ofList
            [ "COVAR_POP"
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
              "REGR_SXY" ]

    // 10.9 <listagg set function> — WITHIN GROUP is required.
    let private withinGroupOnlyFunctionNames = Set.ofList [ "LISTAGG" ]

    // 5.3 <literal> ::= <signed numeric literal> | <general literal> — NULL is NOT a
    //     <literal> (it is the 6.5 <null specification>, wired per context below).
    let pLiteralExpression = pLiteral |> withExprPosition

    // 5.4 <schema qualified name>
    let pSchemaQualifiedNameExpression =
        pSchemaQualifiedName
        |>> function
            | [ s ] -> Identifier s
            | parts -> ColumnReference parts
        |> withExprPosition

    // 6.1 <char length units> ::= CHARACTERS | OCTETS
    // A closed set, so `USING <identifier>` is rejected instead of silently accepted.
    let private pCharLengthUnit =
        pKeyword "CHARACTERS" >>% Characters <|> (pKeyword "OCTETS" >>% Octets)

    // The same closed set in the `string` shape the 6.30 position/length slots store.
    let private pCharLengthUnits =
        pCharLengthUnit
        |>> function
            | Characters -> "CHARACTERS"
            | Octets -> "OCTETS"

    // 6.1 <length> ::= <unsigned integer> — the trailing <separator> is consumed so that a
    // following <multiplier> or <char length units> keyword can match.
    let private pLengthValue = pUnsignedIntegerAsInt .>> ws

    // 6.1 <character length> ::= <length> [ <char length units> ]
    let private pCharacterLength =
        pLengthValue .>>. opt (attempt pCharLengthUnit)
        |>> fun (value, unit) -> { Value = value; Unit = unit }

    // 6.1 <large object length> ::= <unsigned integer> [ <multiplier> ] | <large object length token>
    let private pLargeObjectLength =
        // 6.1 <multiplier> ::= K | M | G | T | P
        let pLengthMultiplier =
            choice
                [ pKeyword "K" >>% Kilo
                  pKeyword "M" >>% Mega
                  pKeyword "G" >>% Giga
                  pKeyword "T" >>% Tera
                  pKeyword "P" >>% Peta ]

        pLengthValue .>>. opt (attempt pLengthMultiplier)
        |>> fun (value, multiplier) ->
            { Value = value
              Multiplier = multiplier
              Unit = None }

    // 6.1 <character large object length> ::= <large object length> [ <char length units> ]
    let private pCharacterLargeObjectLength =
        pLargeObjectLength .>>. opt (attempt pCharLengthUnit)
        |>> fun (length, unit) -> { length with Unit = unit }

    // 6.1 <predefined type> — attach the `[ CHARACTER SET <character set specification> ]`
    // `[ <collate clause> ]` modifiers to the character string type they modify.
    let private withCharacterTypeModifiers modifiers ty =
        match modifiers with
        | Some m -> CharacterTypeWithModifiers(ty, m)
        | None -> ty

    // 6.1 <character string type> ::= CHARACTER [ ( <character length> ) ] | CHAR [ ( <character length> ) ]
    //     | CHARACTER VARYING ( <character length> ) | CHAR VARYING ( <character length> )
    //     | VARCHAR ( <character length> ) | <character large object type>
    // <character large object type> ::= CHARACTER LARGE OBJECT [ ( <character large object length> ) ]
    //     | CHAR LARGE OBJECT [ ( <character large object length> ) ] | CLOB [ ( <character large object length> ) ]
    // The varying forms REQUIRE their length. The <predefined type> alternatives attach
    // `[ CHARACTER SET <character set specification> ] [ <collate clause> ]`.
    let private pCharacterStringType =
        let pOptionalLength =
            opt (attempt (between (token (pstring "(")) (token (pstring ")")) pCharacterLength))

        let pRequiredLength =
            between (token (pstring "(")) (token (pstring ")")) pCharacterLength

        let pOptionalLargeObjectLength =
            opt (attempt (between (token (pstring "(")) (token (pstring ")")) pCharacterLargeObjectLength))

        // 6.1 <predefined type> — the type-level modifiers of a character string type.
        let pModifiers =
            opt (attempt (pKeyword "CHARACTER" >>. pKeyword "SET" >>. pSchemaQualifiedNameExpression))
            .>>. opt (attempt (pKeyword "COLLATE" >>. pSchemaQualifiedNameExpression))
            |>> fun (charSet, collation) ->
                match charSet, collation with
                | None, None -> None
                | _ ->
                    Some
                        { CharacterSet = charSet
                          Collation = collation }

        let pBase =
            choice
                [ attempt (pKeyword "CHARACTER" .>> pKeyword "VARYING") >>. pRequiredLength
                  |>> Varchar
                  attempt (pKeyword "CHAR" .>> pKeyword "VARYING") >>. pRequiredLength |>> Varchar
                  pKeyword "VARCHAR" >>. pRequiredLength |>> Varchar
                  attempt (pKeyword "CHARACTER" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
                  >>. pOptionalLargeObjectLength
                  |>> CharacterLargeObject
                  attempt (pKeyword "CHAR" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
                  >>. pOptionalLargeObjectLength
                  |>> CharacterLargeObject
                  pKeyword "CLOB" >>. pOptionalLargeObjectLength |>> CharacterLargeObject
                  pKeyword "CHARACTER" >>. pOptionalLength |>> Character
                  pKeyword "CHAR" >>. pOptionalLength |>> Character ]

        pBase .>>. pModifiers
        |>> fun (ty, modifiers) -> withCharacterTypeModifiers modifiers ty

    // 6.1 <national character string type> ::= NATIONAL CHARACTER [ ( <character length> ) ]
    //     | NATIONAL CHAR [ ( <character length> ) ] | NCHAR [ ( <character length> ) ]
    //     | NATIONAL CHARACTER VARYING ( <character length> )
    //     | NATIONAL CHAR VARYING ( <character length> ) | NCHAR VARYING ( <character length> )
    //     | <national character large object type>
    // Only the `<collate clause>` modifier is admissible (there is no CHARACTER SET slot).
    // The varying forms REQUIRE their length.
    let private pNationalCharacterStringType =
        let pOptionalLength =
            opt (attempt (between (token (pstring "(")) (token (pstring ")")) pCharacterLength))

        let pRequiredLength =
            between (token (pstring "(")) (token (pstring ")")) pCharacterLength

        let pOptionalLargeObjectLength =
            opt (attempt (between (token (pstring "(")) (token (pstring ")")) pCharacterLargeObjectLength))

        let pModifiers =
            opt (attempt (pKeyword "COLLATE" >>. pSchemaQualifiedNameExpression))
            |>> fun collation ->
                match collation with
                | None -> None
                | Some _ ->
                    Some
                        { CharacterSet = None
                          Collation = collation }

        let pBase =
            choice
                [ attempt (pKeyword "NATIONAL" .>> pKeyword "CHARACTER" .>> pKeyword "VARYING")
                  >>. pRequiredLength
                  |>> NationalVarchar
                  attempt (pKeyword "NATIONAL" .>> pKeyword "CHAR" .>> pKeyword "VARYING")
                  >>. pRequiredLength
                  |>> NationalVarchar
                  attempt (pKeyword "NCHAR" .>> pKeyword "VARYING") >>. pRequiredLength
                  |>> NationalVarchar
                  attempt (
                      pKeyword "NATIONAL"
                      .>> pKeyword "CHARACTER"
                      .>> pKeyword "LARGE"
                      .>> pKeyword "OBJECT"
                  )
                  >>. pOptionalLargeObjectLength
                  |>> NationalCharacterLargeObject
                  attempt (pKeyword "NCHAR" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
                  >>. pOptionalLargeObjectLength
                  |>> NationalCharacterLargeObject
                  pKeyword "NCLOB" >>. pOptionalLargeObjectLength |>> NationalCharacterLargeObject
                  attempt (pKeyword "NATIONAL" .>> pKeyword "CHARACTER") >>. pOptionalLength
                  |>> NationalCharacter
                  attempt (pKeyword "NATIONAL" .>> pKeyword "CHAR") >>. pOptionalLength
                  |>> NationalCharacter
                  pKeyword "NCHAR" >>. pOptionalLength |>> NationalCharacter ]

        pBase .>>. pModifiers
        |>> fun (ty, modifiers) -> withCharacterTypeModifiers modifiers ty

    // 6.1 <binary string type> ::= BINARY [ ( <length> ) ] | BINARY VARYING ( <length> )
    //     | VARBINARY ( <length> ) | <binary large object string type>
    // <binary large object string type> ::= BINARY LARGE OBJECT [ ( <large object length> ) ]
    //     | BLOB [ ( <large object length> ) ]
    // <length> is a plain <unsigned integer> (no units, no multiplier) and the varying forms
    // REQUIRE it; a <binary string type> takes no CHARACTER SET / COLLATE modifier.
    let private pBinaryStringType =
        let pOptionalLength =
            opt (attempt (between (token (pstring "(")) (token (pstring ")")) pLengthValue))

        let pRequiredLength =
            between (token (pstring "(")) (token (pstring ")")) pLengthValue

        let pOptionalLargeObjectLength =
            opt (attempt (between (token (pstring "(")) (token (pstring ")")) pLargeObjectLength))

        choice
            [ attempt (pKeyword "BINARY" .>> pKeyword "VARYING") >>. pRequiredLength
              |>> VarBinary
              pKeyword "VARBINARY" >>. pRequiredLength |>> VarBinary
              attempt (pKeyword "BINARY" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
              >>. pOptionalLargeObjectLength
              |>> BinaryLargeObject
              pKeyword "BLOB" >>. pOptionalLargeObjectLength |>> BinaryLargeObject
              pKeyword "BINARY" >>. pOptionalLength |>> Binary ]

    // 6.1 <exact numeric type> ::= NUMERIC [ ( <precision> [ , <scale> ] ) ] | DECIMAL [ ... ] | DEC [ ... ] | SMALLINT | INTEGER | INT | BIGINT  —  <decimal floating-point type> ::= DECFLOAT [ ( <precision> ) ]
    let private pNumericType =
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
    let private pApproximateNumericType =
        choice
            [ pKeyword "FLOAT"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt)
              |>> Float
              pKeyword "REAL" >>% Real
              attempt (pKeyword "DOUBLE" .>> pKeyword "PRECISION") >>% DoublePrecision ]

    // 6.1 <datetime type> ::= DATE | TIME [ ( <time precision> ) ] [ <with or without time zone> ] | TIMESTAMP [ ( <timestamp precision> ) ] [ <with or without time zone> ]
    let private pDateTimeType =
        // 6.1 <with or without time zone> ::= WITH TIME ZONE | WITHOUT TIME ZONE
        // Map each alternative with `>>%`: `pstringCI` returns the keyword as written in
        // the input, so comparing the parsed string (`Some "WITH" -> true`) silently
        // misread a lower-case `with` as WITHOUT TIME ZONE.
        let pTz =
            opt (
                pKeyword "WITH" >>% true <|> (pKeyword "WITHOUT" >>% false)
                .>> pKeyword "TIME"
                .>> pKeyword "ZONE"
            )
            |>> Option.defaultValue false

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
    let private pIntervalType =
        pKeyword "INTERVAL" >>. pIntervalQualifier |>> IntervalType

    // 6.1 <data type> — forward ref (wired below in this module; used by CAST / JSON returning)
    let pDataType, private pDataTypeRef = createParserForwardedToRef<DataType, unit> ()

    // 6.7 <column reference> / 5.4 <identifier> — <identifier> | <column reference>
    let pIdentifierExpression = pIdentifier |>> Identifier |> withExprPosition

    // 6.1 <row type> ::= ROW <row type body> — <row type body> ::= ( <field definition> [ { , <field definition> }... ] )
    let private pRowType =
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
    let private pDataTypeElement =
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
    let private pArraySuffix =
        pKeyword "ARRAY"
        .>>. opt (between (token pLeftBracket) (token pRightBracket) (pUnsignedIntegerAsInt .>> pSeparator))
        |>> fun (_, len) -> fun t -> ArrayType(t, len)

    let private pMultisetSuffix = pKeyword "MULTISET" >>% fun t -> MultisetType t

    let private pCollectionType =
        let pCollectionSuffixes =
            many (choice [ pArraySuffix; pMultisetSuffix ])
            |>> fun suffixes t -> List.fold (fun acc f -> f acc) t suffixes

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

    // 6.39 <boolean value expression> / 6.28 <value expression> — forward ref (central expression parser)
    let pExpression, private pExpressionRef =
        createParserForwardedToRef<Expression, unit> ()

    // Literals are handled separately by pLiteralExpression above.
    // 6.4 <general value specification> — parameter forms (`?` dynamic parameter, `:name` host parameter) plus the keyword forms (CURRENT_USER, SESSION_USER, ...).
    let private pGeneralValueSpecification =
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

    // 6.5 <null specification> ::= NULL — the implicitly-typed half of a 6.5
    //     <contextually typed value specification>. NOT a 5.3 <literal>: it is accepted
    //     only in the contextually-typed slots that OR it in (CAST operand, SQL arguments,
    //     INSERT/MERGE VALUES, SET clauses, <default option>, <parameter default>) and in
    //     the 16.2 <return value> / 6.12 <result> alternatives.
    let pNullSpecification: Parser<Expression, unit> =
        pKeyword "NULL" >>% Literal Null |> withExprPosition

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

    // 6.9 <grouping operation> ::= GROUPING ( <column reference> [ , <column reference> ]... )
    // Plain column references — no <collate clause> slot.
    let private pGroupingOperation =
        pKeyword "GROUPING"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 pColumnReferenceExpression (token (pstring ",")))
        |>> Grouping
        |> withExprPosition

    // 6.9 / 6.26 <running or final> ::= RUNNING | FINAL
    let private pRunningOrFinal =
        choice
            [ pKeyword "RUNNING" >>% RunningOrFinal.Running
              pKeyword "FINAL" >>% RunningOrFinal.Final ]

    // 6.11 <row marker delta> / 7.9 <row pattern quantifier> — unsigned integer as <value expression> (row marker offsets / quantifier bounds)
    let pUnsignedIntegerExpr: Parser<Expression, unit> =
        getPosition .>>. token pUnsignedInteger
        |>> fun (pos, n) ->
            { Kind = Literal(Number(decimal n))
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 6.11 <row marker> ::= BEGIN_PARTITION | BEGIN_FRAME | CURRENT_ROW | FRAME_ROW | END_FRAME | END_PARTITION
    let private pRowMarker =
        choice
            [ pKeyword "BEGIN_PARTITION" >>% RowMarker.BeginPartition
              pKeyword "BEGIN_FRAME" >>% RowMarker.BeginFrame
              pKeyword "CURRENT_ROW" >>% RowMarker.CurrentRow
              pKeyword "FRAME_ROW" >>% RowMarker.FrameRow
              pKeyword "END_FRAME" >>% RowMarker.EndFrame
              pKeyword "END_PARTITION" >>% RowMarker.EndPartition ]

    // 6.11 <nested row number function> ::= ROW_NUMBER ( <row marker> )
    let pNestedRowNumberFunction =
        pKeyword "ROW_NUMBER"
        >>. between (token (pstring "(")) (token (pstring ")")) pRowMarker
        |>> NestedRowNumber
        |> withExprPosition

    // 6.11 <value_of expression at row> ::= VALUE_OF ( <value expression> AT <row marker expression>
    //     [ , <value_of default value> ] )
    let pValueOfExpressionAtRow =
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
    let private pNullifExpr =
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
    let private pCoalesceExpr =
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
    // rejected; a PARENTHESIZED expression is a 6.39 <boolean predicand> and stays legal
    // (the Parenthesized node keeps the parens, so the content is not top-level).
    let isBooleanTopLevel (e: Expression) =
        match e.Kind with
        // 6.39 <boolean predicand> ::= <parenthesized boolean value expression>
        //     | <nonparenthesized value expression primary> — a parenthesized expression
        // is a predicand even when its content is boolean (the AST keeps the parens).
        | Parenthesized _ -> false
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
        | IsJson _
        | JsonExists _ -> true
        | _ -> false

    // 6.12 <when operand> — the 8.2/8.3/8.4/8.5/8.6/8.7/8.8/8.9/8.12/8.13/8.14 predicate
    // part-2 forms as a function applied to the <case operand> (defined in
    // PredicateParser.fs, wired in SqlParser.fs).
    let private pWhenOperandPart2, pWhenOperandPart2Ref =
        createParserForwardedToRef<Expression -> Expression, unit> ()

    // 6.12 <case expression> ::= CASE <case operand> <simple when clause>... [ <else clause> ] END
    //     | CASE <searched when clause>... [ <else clause> ] END
    // A <case operand> / <when operand> is a <row value predicand>: a TOP-LEVEL
    // boolean-producing expression is rejected, a PARENTHESIZED one is a 6.39
    // <boolean predicand> and stays legal. A <when operand> may also be a predicate
    // part 2 (8.2/8.3/8.4/8.5/8.6/8.7/8.8/8.9/8.12/8.13/8.14) — the <case operand> then
    // supplies the missing part 1 (6.12 General Rules), so such a case is represented as a
    // searched case: each simple when clause becomes the OR of its operands' predicates
    // (`WHEN 1, = 2` ≡ `WHEN x = 1 OR x = 2`).
    let private pCaseExpression =
        getPosition
        >>= fun pos ->
            let pResultExpr =
                pExpression
                <|> (pKeyword "NULL"
                     >>% { Kind = Literal Null
                           Pos = { Line = pos.Line; Column = pos.Column } })

            let pWhenOperandExpr =
                pExpression
                >>= fun e ->
                    if isBooleanTopLevel e then
                        fail "a <when operand> must be a <row value predicand> (6.12)"
                    else
                        preturn e

            let pWhenOperand caseOp =
                attempt (pWhenOperandPart2 |>> fun applyToCaseOp -> Choice2Of2(applyToCaseOp caseOp))
                <|> (pWhenOperandExpr |>> Choice1Of2)

            let pSimpleWhenClause caseOp =
                pKeyword "WHEN" >>. sepBy1 (pWhenOperand caseOp) (token (pstring ","))
                .>> pKeyword "THEN"
                .>>. pResultExpr

            let pSimpleCase =
                attempt (
                    pExpression
                    >>= fun caseOp ->
                        if isBooleanTopLevel caseOp then
                            fail "a <case operand> must be a <row value predicand> (6.12)"
                        else
                            many1 (pSimpleWhenClause caseOp)
                            |>> fun clauses ->
                                let hasPart2 =
                                    clauses
                                    |> List.exists (fun (operands, _) ->
                                        operands
                                        |> List.exists (function
                                            | Choice2Of2 _ -> true
                                            | Choice1Of2 _ -> false))

                                if hasPart2 then
                                    let searched =
                                        clauses
                                        |> List.map (fun (operands, result) ->
                                            let conditions =
                                                operands
                                                |> List.map (function
                                                    | Choice1Of2 w ->
                                                        { Expression.Kind = BinaryOp(Equal, caseOp, w)
                                                          Pos = caseOp.Pos }
                                                    | Choice2Of2 predicate -> predicate)

                                            let condition =
                                                conditions
                                                |> List.reduce (fun l r ->
                                                    { Expression.Kind = BinaryOp(Or, l, r)
                                                      Pos = l.Pos })

                                            condition, result)

                                    Case(None, searched, None)
                                else
                                    let flattened =
                                        clauses
                                        |> List.collect (fun (operands, result) ->
                                            operands
                                            |> List.map (fun operand ->
                                                match operand with
                                                | Choice1Of2 w -> w, result
                                                | Choice2Of2 predicate -> predicate, result))

                                    Case(Some caseOp, flattened, None)
                )

            let pSearchedWhenClause =
                pKeyword "WHEN" >>. pExpression .>> pKeyword "THEN" .>>. pResultExpr

            pKeyword "CASE"
            >>. choice
                    [ pSimpleCase
                      many1 pSearchedWhenClause |>> fun whens -> Case(None, whens, None) ]
            .>>. opt (pKeyword "ELSE" >>. pResultExpr)
            .>> pKeyword "END"
            |>> fun (caseBase, els) ->
                match caseBase with
                | Case(op, whens, _) -> Case(op, whens, els)
                | kind -> kind
            |> withExprPosition

    // 6.13 <cast specification> ::= CAST ( <cast operand> AS <cast target> [ FORMAT <cast template> ] )
    // <cast operand> ::= <value expression> | <implicitly typed value specification> (6.5)
    // <cast template> ::= <character string literal>
    // A <cast target> is a <domain name> or a <data type> — DESCRIPTOR is neither (the
    // CAST ( NULL AS DESCRIPTOR ) form is the 10.4 <descriptor argument>, parsed in
    // pDescriptorArgument below).
    let private pCastSpecification =
        pKeyword "CAST"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression <|> pNullSpecification .>> pKeyword "AS"
                 .>>. pDataType
                 .>>. opt (attempt (pKeyword "FORMAT" >>. pCharacterStringLiteral))
                 >>= fun ((operand, target), template) ->
                     match target with
                     | UserDefinedType { Kind = Identifier "DESCRIPTOR" } ->
                         fail
                             "6.13 <cast target> cannot be DESCRIPTOR (use CAST ( NULL AS DESCRIPTOR ), 10.4 <descriptor argument>)"
                     | _ -> preturn (Cast(operand, target, template)))
        |> withExprPosition

    // 6.14 <next value expression> ::= NEXT VALUE FOR <sequence generator name>
    let private pNextValueExpression =
        pKeyword "NEXT"
        >>. pKeyword "VALUE"
        >>. pKeyword "FOR"
        >>. pSchemaQualifiedNameExpression
        |>> NextValueFor
        |> withExprPosition

    // 10.4 <SQL argument list> — forward refs: the definitions live in ControlParser.fs
    // (compiled after this module), which wires them. Consumed by the method / static
    // method / new / dereference postfixes and by the routine-invocation body below.
    let private pSqlArgumentList, pSqlArgumentListRef =
        createParserForwardedToRef<SqlArgumentList, unit> ()

    // 6.15 <field reference> ::= <value expression primary> <period> <field name>
    // 6.17 <direct invocation> ::= <value expression primary> <period> <method name>
    //     [ <SQL argument list> ]
    // 6.32 <specific type method> ::= <user-defined type value expression> <period> SPECIFICTYPE [ ( ) ]
    // One postfix parser serves all three: `. name` is a 6.15 field reference, `. name ( … )`
    // a 6.17 direct method invocation, and `. SPECIFICTYPE [ ( ) ]` the 6.32 specific type
    // method (SPECIFICTYPE is a reserved word, so it cannot be reached through the
    // pIdentifierExpression branch below).
    let private pMethodOrFieldReference =
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

    // 6.16 <subtype treatment> ::= TREAT ( <subtype operand> AS <target subtype> )
    let private pSubtypeTreatment =
        pKeyword "TREAT"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        |>> (fun (e, t) -> Treat(e, t))
        |> withExprPosition

    // 6.17 <generalized invocation> ::= ( <value expression primary> AS <data type> )
    //     <period> <method name> [ <SQL argument list> ]
    let private pGeneralizedInvocation =
        between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        .>>. (token (pstring ".") >>. pIdentifierExpression .>>. opt pSqlArgumentList)
        |>> fun ((operand, typ), (name, args)) -> GeneralizedInvocation(operand, typ, name, args)
        |> withExprPosition

    // 6.18 <static method invocation> ::= <path-resolved UDT name> :: <method name>
    //     [ <SQL argument list> ]
    let private pStaticMethodInvocation =
        pSchemaQualifiedNameExpression
        .>>. (token (pstring "::") >>. pIdentifierExpression .>>. pSqlArgumentList)
        |>> fun (typ, (name, args)) -> StaticMethodInvocation(typ, name, args)
        |> withExprPosition

    // 6.19 <new specification> ::= NEW <path-resolved UDT name> <SQL argument list>
    let private pNewSpecification =
        pKeyword "NEW" >>. pSchemaQualifiedNameExpression .>>. pSqlArgumentList
        |>> fun (typ, args) -> NewSpecification(typ, args)
        |> withExprPosition

    // 6.21 <dereference operation> ::= <value expression primary> <dereference operator>
    //     <qualified identifier> [ <SQL argument list> ]
    // <qualified identifier> ::= <identifier> — a SINGLE identifier, not a
    // schema-qualified name.
    let private pDereferenceReference =
        token (pstring "->") >>. pIdentifierExpression .>>. opt pSqlArgumentList
        |>> fun (name, args) ->
            fun r ->
                { Expression.Kind = Dereference(r, name, args)
                  Pos = r.Pos }

    // 6.23 <reference resolution> ::= DEREF ( <reference value expression> )
    let private pReferenceResolution =
        pKeyword "DEREF"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> Deref
        |> withExprPosition

    // 6.29 <numeric value expression> — forward ref (wired after pValueExpressionPrimary is defined
    // to break the cycle: pValueExpressionPrimaryImpl → pArrayElementReference → pNumericValueExpression
    // → pValueExpressionPrimary → pValueExpressionPrimaryImpl).
    let pNumericValueExpression, private pNumericValueExpressionRef =
        createParserForwardedToRef<Expression, unit> ()

    // 6.24 <array element reference> — postfix [ <numeric value expression> ]
    let private pArrayElementReference =
        between (token pLeftBracket) (token pRightBracket) pNumericValueExpression
        |>> fun idx ->
            fun e ->
                { Expression.Kind = ArrayElement(e, idx)
                  Pos = e.Pos }

    // 6.25 <multiset element reference> ::= ELEMENT ( <multiset value expression> )
    let private pMultisetElementReference =
        pKeyword "ELEMENT"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> Element
        |> withExprPosition

    // 6.26 <row pattern navigation operation> as a <value expression primary>
    let private pRowPatternNavigationOperation =
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

        pRowPatternNavigation |>> RowPatternNavigation |> withExprPosition

    // 6.28 <value expression> without boolean operators — forward ref (wired to
    // opp.ExpressionParser after the operator-precedence parser is built below).
    // Used where the grammar requires a non-boolean <value expression> (JSON slots,
    // <point in time>, etc.).
    let private pNonBooleanValueExpression, private pNonBooleanValueExpressionRef =
        createParserForwardedToRef<Expression, unit> ()

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
    let private pJsonOutputClause =
        pKeyword "RETURNING" >>. pDataType
        .>>. opt (pKeyword "FORMAT" >>. pJsonRepresentation)
        |>> fun (ret, fmt) -> { Returning = ret; Format = fmt }

    // 10.14 <JSON API common syntax> ::= <JSON context item> , <JSON path specification>
    //     [ AS <JSON table path name> ] [ <JSON passing clause> ]
    // <JSON context item> ::= <JSON value expression> (value expression — no boolean ops,
    // plus an optional FORMAT clause)
    // <JSON path specification> ::= <character string literal> — stored as a plain string.
    let pJsonApiCommonSyntax =
        // 10.14 <JSON argument> ::= <JSON value expression> [ <JSON input clause> ] AS <identifier>
        // <JSON value expression> is a value expression — boolean expressions are not allowed.
        let pJsonArgument =
            pNonBooleanValueExpression .>>. opt pJsonInputClause .>> pKeyword "AS"
            .>>. pIdentifierExpression
            |>> fun ((value, inputFormat), name) ->
                { JsonPassingArgument.Value = value
                  InputFormat = inputFormat
                  Name = name }

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
    let private pJsonValueFunction =
        // 6.27 <JSON value empty behavior> ::= ERROR | NULL | DEFAULT <value expression>
        // 6.27 <JSON value error behavior> ::= ERROR | NULL
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
    let private pExtractExpression =
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


    // 6.30 <position expression> ::= POSITION ( <character value expression> IN
    //     <character value expression> [ USING <char length units> ] )
    // Operands are <character value expression>s (non-boolean). The <binary position
    // expression> form has no USING slot, but the two operand kinds are syntactically
    // indistinguishable, so one parser serves both (see docs/trade-off.md).
    let private pPositionExpression =
        // The same keyword set in the <position expression> slot, which models the units as an <identifier>.
        let pCharLengthUnitsExpr = pCharLengthUnits |>> Identifier |> withExprPosition

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
    // Both argument slots are value (non-boolean) expressions.
    let private pLengthExpression =
        let pBody =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (pNonBooleanValueExpression .>>. opt (pKeyword "USING" >>. pCharLengthUnits))

        let pOctetBody =
            between (token (pstring "(")) (token (pstring ")")) pNonBooleanValueExpression

        choice
            [ pKeyword "CHAR_LENGTH" >>. pBody
              |>> fun (e, units) -> LengthExpression(LengthFunction.CharLength, e, units)
              pKeyword "CHARACTER_LENGTH" >>. pBody
              |>> fun (e, units) -> LengthExpression(LengthFunction.CharacterLength, e, units)
              pKeyword "OCTET_LENGTH" >>. pOctetBody
              |>> fun e -> LengthExpression(LengthFunction.OctetLength, e, None) ]
        |> withExprPosition

    // 6.30 <numeric value function> — the built-ins of the shape <name> ( <args> )
    let private pNumericValueFunction =
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
    // — the four share the head <pattern> [ FLAG <flag> ] IN <subject> but each production
    // admits a different set of trailing clauses (docs/trade-off.md). The operands are
    // <character value expression>s, so the non-boolean expression parser is used — that
    // also keeps `IN` from being read as an 8.4 <in predicate>.
    //   allowWith / allowOccurrence / allowAll / allowGroup select the production:
    //   occurrences: false false false false
    //   position & substring: false true false true
    //   transliteration: true true true false
    let private pRegexArgument allowWith allowOccurrence allowAll allowGroup =
        let pOperand = pNonBooleanValueExpression

        let pOccurrence =
            if allowOccurrence then
                let pOcc =
                    if allowAll then
                        choice
                            [ attempt (pKeyword "ALL" >>% RegexOccurrenceAll)
                              pExpression |>> RegexOccurrenceNumber ]
                    else
                        pExpression |>> RegexOccurrenceNumber

                opt (attempt (pKeyword "OCCURRENCE" >>. pOcc))
            else
                preturn None

        let pWith =
            if allowWith then
                opt (attempt (pKeyword "WITH" >>. pOperand))
            else
                preturn None

        let pGrp =
            if allowGroup then
                opt (attempt (pKeyword "GROUP" >>. pOperand))
            else
                preturn None

        pOperand .>>. opt (attempt (pKeyword "FLAG" >>. pOperand)) .>> pKeyword "IN"
        .>>. pOperand
        .>>. pWith
        .>>. opt (attempt (pKeyword "FROM" >>. pOperand))
        .>>. opt (attempt (pKeyword "USING" >>. pCharLengthUnits))
        .>>. pOccurrence
        .>>. pGrp
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
    let private pRegexOccurrencesFunction =
        pKeyword "OCCURRENCES_REGEX"
        >>. between (token (pstring "(")) (token (pstring ")")) (pRegexArgument false false false false)
        |>> RegexOccurrences
        |> withExprPosition

    // 6.30 <regex position expression> ::= POSITION_REGEX ( [ START | AFTER ] <XQuery pattern>
    //     [ FLAG <flag> ] IN <regex subject string> [ FROM ] [ USING ] [ OCCURRENCE n ] [ GROUP ] )
    let private pRegexPositionFunction =
        let pStart =
            opt (
                attempt (
                    choice
                        [ pKeyword "START" >>% RegexStartOfString
                          pKeyword "AFTER" >>% RegexAfterMatch ]
                )
            )

        pKeyword "POSITION_REGEX"
        >>. between (token (pstring "(")) (token (pstring ")")) (pStart .>>. pRegexArgument false true false true)
        |>> fun (start, arg) -> RegexPosition(start, arg)
        |> withExprPosition

    // 6.32 <trim function> ::= TRIM ( [ <trim specification> ] [ <trim character> ] FROM <trim source> )
    //     | TRIM ( <trim source> )
    // The shorthand is a separate production of 6.32: neither a specification nor a character.
    let private pTrimFunction =
        let pSpec =
            opt (
                pKeyword "LEADING" >>% Leading
                <|> (pKeyword "TRAILING" >>% Trailing)
                <|> (pKeyword "BOTH" >>% Both)
            )

        // `[ <trim specification> ] [ <trim character> ] FROM <trim source>`
        // — both slots are <character value expression>s (non-boolean).
        let pExplicitForm =
            pSpec .>>. opt pNonBooleanValueExpression .>> pKeyword "FROM"
            .>>. pNonBooleanValueExpression
            |>> fun ((spec, character), source) -> spec, character, source

        // `TRIM ( <trim source> )` — the shorthand carries neither part.
        let pShorthandForm = pNonBooleanValueExpression |>> fun source -> None, None, source

        pKeyword "TRIM"
        >>. between (token (pstring "(")) (token (pstring ")")) (attempt pExplicitForm <|> pShorthandForm)
        |>> (fun (spec, character, source) -> Trim(spec, character, source))
        |> withExprPosition

    // 6.32 <character substring function> ::= SUBSTRING ( <character value expression> FROM <start position> [ FOR <string length> ] [ USING <char length units> ] )
    // <start position> / <string length> are <numeric value expression>s.
    let private pCharacterSubstringFunction =
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
    let private pOverlayFunction =
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
    let private pRegularExpressionSubstringFunction =
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
    let private pFoldFunction =
        choice
            [ pKeyword "UPPER" >>% FoldFunction.FoldUpper
              pKeyword "LOWER" >>% FoldFunction.FoldLower ]
        .>>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> fun (fn, e) -> Fold(fn, e)
        |> withExprPosition

    // 6.32 <transcoding> ::= CONVERT ( <character value expression> USING <transcoding name> )
    let private pTranscodingFunction =
        pKeyword "CONVERT"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "USING" .>>. pIdentifierExpression)
        |>> fun (e, name) -> Transcoding(e, name)
        |> withExprPosition

    // 6.32 <character transliteration> ::= TRANSLATE ( <character value expression>
    //     USING <transliteration name> )
    let private pCharacterTransliterationFunction =
        pKeyword "TRANSLATE"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> pKeyword "USING" .>>. pIdentifierExpression)
        |>> fun (e, name) -> CharacterTransliteration(e, name)
        |> withExprPosition

    // 6.32 <regex substring function> ::= SUBSTRING_REGEX ( <XQuery pattern> [ FLAG <flag> ]
    //     IN <regex subject string> [ FROM ] [ USING ] [ OCCURRENCE <regex occurrence> ] [ GROUP ] )
    let private pRegexSubstringFunction =
        pKeyword "SUBSTRING_REGEX"
        >>. between (token (pstring "(")) (token (pstring ")")) (pRegexArgument false true false true)
        |>> RegexSubstring
        |> withExprPosition

    // 6.32 <regex transliteration> ::= TRANSLATE_REGEX ( <XQuery pattern> [ FLAG <flag> ]
    //     IN <regex subject string> [ WITH <replacement> ] [ FROM ] [ USING ]
    //     [ OCCURRENCE <regex transliteration occurrence> ] )  — no GROUP.
    let private pRegexTransliterateFunction =
        pKeyword "TRANSLATE_REGEX"
        >>. between (token (pstring "(")) (token (pstring ")")) (pRegexArgument true true true false)
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
    let private pNormalizeFunction =
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
    let private pJsonNameAndValue =
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
    let private pJsonConstructorNullClause =
        choice
            [ pKeyword "NULL" >>. pKeyword "ON" >>. pKeyword "NULL" >>% JsonNullOnNull
              pKeyword "ABSENT" >>. pKeyword "ON" >>. pKeyword "NULL" >>% JsonAbsentOnNull ]

    // Returns bool (true = WITH UNIQUE, false = WITHOUT UNIQUE); callers wrap in opt.
    // 6.33 <JSON key uniqueness constraint> ::= WITH UNIQUE [ KEYS ] | WITHOUT UNIQUE [ KEYS ]
    let private pJsonKeyUniqueness =
        choice
            [ pKeyword "WITH" >>. pKeyword "UNIQUE" >>. opt (pKeyword "KEYS") >>% true
              pKeyword "WITHOUT" >>. pKeyword "UNIQUE" >>. opt (pKeyword "KEYS") >>% false ]

    // 6.33 <JSON object constructor> ::= JSON_OBJECT ( [ <JSON name and value list> ]
    //     [ <JSON constructor null clause> ] [ <JSON key uniqueness constraint> ]
    //     [ <JSON output clause> ] )
    let private pJsonObjectFunction =
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
    let private pJsonArrayFunction =
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

    // 6.34 <JSON query> ::= JSON_QUERY ( <JSON API common syntax> [ <JSON output clause> ]
    //     [ <JSON query wrapper behavior> WRAPPER ] [ <JSON query quotes behavior> QUOTES
    //     [ ON SCALAR STRING ] ] [ <JSON query empty behavior> ON EMPTY ]
    //     [ <JSON query error behavior> ON ERROR ] )
    let private pJsonQueryFunction =
        // 6.34 <JSON query empty behavior> ::= ERROR | NULL | EMPTY ARRAY | EMPTY OBJECT
        // 6.34 <JSON query error behavior> ::= ERROR | NULL | EMPTY ARRAY | EMPTY OBJECT
        let pJsonQueryBehavior =
            choice
                [ pKeyword "ERROR" >>% JsonQueryError
                  pKeyword "NULL" >>% JsonQueryNull
                  pKeyword "EMPTY" >>. pKeyword "ARRAY" >>% JsonQueryEmptyArray
                  pKeyword "EMPTY" >>. pKeyword "OBJECT" >>% JsonQueryEmptyObject ]

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

    // 6.35 <datetime value expression> — forward ref. Defined after pTimeZoneSuffix,
    // but needed by the 6.37 <interval value expression> 4th alternative.
    let pDatetimeValueExpression, private pDatetimeValueExpressionRef =
        createParserForwardedToRef<Expression, unit> ()

    // 6.37 <interval value expression> ::= ... | ( <datetime value expression> <minus sign> <datetime term> ) <interval qualifier>
    // The difference of two datetimes, qualified as an interval. Tried ahead of the plain
    // parenthesized <value expression> alternative; `attempt` backtracks when no
    // <interval qualifier> follows the closing paren.
    // This covers the 4th alternative only; the full <interval value expression> is
    // defined as pIntervalValueExpression below (after pIntervalTerm).
    let private pDatetimeDifference =
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
    let private pTrimArrayFunction =
        pKeyword "TRIM_ARRAY"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pExpression .>> token (pstring ",") .>>. pNumericValueExpression)
        |>> fun (arr, count) -> TrimArray(arr, count)
        |> withExprPosition

    // 7.17 <query expression> — forward ref (defined in QueryParser; used by scalar/quantified subqueries)
    let pQuery, pQueryRef = createParserForwardedToRef<Query, unit> ()

    // 6.42 <array value constructor> ::= ARRAY <array value constructor by enumeration>
    //     | ARRAY <array value constructor by query>
    // <array element list> ::= <array element> [ { , <array element> }... ] — at least
    // ONE element; the empty form is only a 6.5 <empty specification>.
    let private pArrayValueConstructor =
        pKeyword "ARRAY"
        >>. choice
                [ attempt (
                      between (token pLeftBracket) (token pRightBracket) (sepBy1 pExpression (token (pstring ",")))
                      |>> ArrayConstructor
                  )
                  attempt (between (token (pstring "(")) (token (pstring ")")) pQuery |>> ArrayQuery) ]
        |> withExprPosition

    // 6.43 <multiset value expression> / 6.44 <multiset set function> — forward ref.
    // Defined after pValueExpressionPrimary, but needed by the 6.44 SET (...) parser
    // (which is itself a <value expression primary>), hence the indirection.
    let private pMultisetValueExpression, private pMultisetValueExpressionRef =
        createParserForwardedToRef<Expression, unit> ()

    // 6.44 <multiset set function> ::= SET ( <multiset value expression> )
    let private pMultisetSetFunction =
        pKeyword "SET"
        >>. between (token (pstring "(")) (token (pstring ")")) pMultisetValueExpression
        |>> MultisetSetFunction
        |> withExprPosition

    // 6.45 <multiset value constructor> ::= MULTISET <multiset value constructor by enumeration>
    //     | MULTISET <multiset value constructor by query>
    // <multiset element list> requires at least ONE element.
    let private pMultisetValueConstructor =
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
    let private pTableValueConstructorByQuery =
        pKeyword "TABLE" >>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> TableQuery
        |> withExprPosition

    // 7.1 <explicit row value constructor> ::= ( <row value constructor element> <comma>
    //     <row value constructor element list> ) | ROW ( <row value constructor element list> )
    // The parenthesized form requires at least two elements (one element is just a
    // parenthesized <value expression>), so `attempt` backtracks on `(a)` and lets the
    // plain parenthesized branch of pValueExpressionPrimary below handle it.
    let private pExplicitRowValueConstructor =
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

    // 6.10 <window name or specification> — forward ref: the parser is defined in
    // QueryParser.fs, which is compiled after this module and wires the ref there. The
    // OVER clause body of pRoutineInvocation below consumes it.
    let private pWindowNameOrSpecification, pWindowNameOrSpecificationRef =
        createParserForwardedToRef<WindowDefinition, unit> ()

    let private pSqlArgumentListBody, pSqlArgumentListBodyRef =
        createParserForwardedToRef<SqlArgumentList, unit> ()

    // 10.10 <sort specification> — forward ref: the parser is defined in QueryParser.fs,
    // which is compiled after this module and wires the ref there. Consumed by the 10.4
    // table argument ordering list, the 10.9 WITHIN GROUP body and the 10.11 JSON_ARRAYAGG
    // body below.
    let private pSortSpecification, pSortSpecificationRef =
        createParserForwardedToRef<Expression * bool * NullsOrder option, unit> ()

    // 10.4 <routine invocation> ::= <routine name> <SQL argument list>
    // No forward ref: the only earlier consumer (the 10.4 table-argument parser) lives in
    // ControlParser.fs, and this module's own uses all follow this definition. The mutual
    // recursion is broken by the pSqlArgumentListBody / pExpression forwarding parsers.
    let pRoutineInvocation =
        // 10.9 <aggregate function> ::= COUNT ( <asterisk> ) | ... — the bare `*` is an
        // argument ONLY of COUNT; every other function takes <value expression>s.
        // (The `*` is NOT a <value expression primary>, so it is parsed here directly.)
        let pStarArg =
            pstring "*" .>> ws .>>. getPosition
            |>> fun (_, pos) ->
                { Arguments =
                    [ SqlArgumentValue
                          { Expression.Kind = ExpressionKind.Star
                            Pos = { Line = pos.Line; Column = pos.Column } } ]
                  Copartition = None }

        let pArgs =
            between
                (token (pstring "("))
                (token (pstring ")"))
                (opt (pKeyword "DISTINCT" >>% true <|> (pKeyword "ALL" >>% false))
                 .>>. (attempt pStarArg <|> pSqlArgumentListBody))

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
        >>= fun ((((name, (dist, argumentList)), withinGroup), filter), window) ->
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
                    // 10.4 — the arity/shape rules below speak about <value expression>
                    // arguments. A <table argument> / <named argument> / <descriptor argument>
                    // cannot satisfy any of them, so a reserved built-in rejects one up front;
                    // a general routine name (a PTF) may take them.
                    let valueArguments =
                        argumentList.Arguments
                        |> List.choose (function
                            | SqlArgumentValue e -> Some e
                            | _ -> None)

                    let hasNonValueArgument = argumentList.Arguments.Length > valueArguments.Length
                    let args = valueArguments

                    // Arity / argument-shape checks (6.10, 10.9).
                    let failArity what =
                        fail (sprintf "%s expects %s." functionName what)

                    let isSimpleValueSpec k =
                        match k with
                        | Literal _
                        | Parameter _ -> true
                        | _ -> false

                    // 10.9 — the bare `*` argument is only `COUNT ( <asterisk> )`.
                    let hasStarArg = args |> List.exists (fun a -> a.Kind = ExpressionKind.Star)

                    if hasNonValueArgument && List.contains functionName functionKeywords then
                        failArity "only <value expression> arguments (10.4)"
                    elif hasStarArg && functionName <> "COUNT" then
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
                                FunctionCall(
                                    name,
                                    Option.defaultValue false dist,
                                    argumentList,
                                    None,
                                    filter,
                                    withinGroup
                                )
                            )
        |> withExprPosition

    // 10.4 — the expressions an <SQL argument> carries, used by the post-parse checks in
    // `findExpressionViolationIn` below. A <table argument>'s `TABLE ( <query> )` stays
    // opaque, like `TableQuery` (6.45). The chain of <named argument specification>s
    // (`a => b => 1`) is walked with an explicit work list so the collector is
    // tail-recursive, like the search itself: a plain `name :: children value` recursion
    // would not be.
    let private tableArgumentChildren (t: TableArgument) =
        [ yield!
              match t.Table with
              | TableArgumentName e -> [ e ]
              | TableArgumentTableQuery _ -> []
              | TableArgumentInvocation e -> [ e ]
          yield!
              t.Correlation
              |> Option.toList
              |> List.collect (fun (correlation, columns) -> correlation :: Option.defaultValue [] columns)
          yield! Option.defaultValue [] t.PartitionBy
          yield! t.OrderBy |> Option.defaultValue [] |> List.map (fun (e, _, _) -> e) ]

    [<TailCall>]
    let rec private collectSqlArgumentChildren (work: SqlArgument list) (acc: Expression list) =
        match work with
        | [] -> List.rev acc
        | arg :: rest ->
            match arg with
            | SqlArgumentValue e -> collectSqlArgumentChildren rest (e :: acc)
            | SqlArgumentGeneralized(e, _) -> collectSqlArgumentChildren rest (e :: acc)
            | SqlArgumentDescriptor e -> collectSqlArgumentChildren rest (e :: acc)
            | SqlArgumentNamed(name, value) -> collectSqlArgumentChildren (value :: rest) (name :: acc)
            | SqlArgumentTable t -> collectSqlArgumentChildren rest (List.rev (tableArgumentChildren t) @ acc)

    // 10.11 <JSON object aggregate constructor> ::= JSON_OBJECTAGG ( <JSON name and value>
    //     [ <JSON constructor null clause> ] [ <JSON key uniqueness constraint> ]
    //     [ <JSON output clause> ] )
    let private pJsonObjectAggFunction =
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

    // 10.11 <JSON array aggregate constructor> ::= JSON_ARRAYAGG ( <JSON value expression>
    //     [ ORDER BY <sort specification list> ] [ <JSON constructor null clause> ]
    //     [ <JSON output clause> ] )
    let private pJsonArrayAggFunction =
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
    let private pSetFunctionSpecification =
        getPosition .>>. (pRunningOrFinal .>>. pRoutineInvocation)
        >>= fun (pos, (scope, e)) ->
            match e.Kind with
            | FunctionCall({ Kind = Identifier name }, _, _, _, _, _) when Set.contains name aggregateFunctionNames ->
                preturn
                    { Expression.Kind = SetFunction(Some scope, e)
                      Pos = { Line = pos.Line; Column = pos.Column } }
            | _ -> fail "RUNNING/FINAL requires an <aggregate function>"

    // 8.9 ANY/SOME/ALL (subquery) / 8.10 EXISTS / 8.11 UNIQUE / 8.20 PERIOD () / 8.23
    // JSON_EXISTS — the §8 parsers that are also <value expression primary> alternatives,
    // bundled into one parser.
    let private pPredicatePrimary, pPredicatePrimaryRef =
        createParserForwardedToRef<Expression, unit> ()

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
    let private pValueExpressionPrimaryImpl withPredicates =
        // 6.3 <scalar subquery> ::= ( <subquery> )
        // Local because pValueExpressionPrimaryImpl is its only consumer.
        let pScalarSubquery =
            between (token (pstring "(")) (token (pstring ")")) pQuery
            |>> SubqueryExpression
            |> withExprPosition

        // 6.32 <classifier function> ::= CLASSIFIER ( [ <row pattern variable name> ] )
        // <row pattern variable name> is a simple identifier, not an arbitrary expression.
        let pClassifierFunction =
            pKeyword "CLASSIFIER"
            >>. between (token (pstring "(")) (token (pstring ")")) (opt pIdentifierExpression)
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
              // 6.3 <parenthesized value expression> — kept as a Parenthesized node so a
              // parenthesized boolean expression is a 6.39 <boolean predicand>, not an operator.
              between (token (pstring "(")) (token (pstring ")")) pExpression
              |>> Parenthesized
              |> withExprPosition ]
        // The postfix loop must be able to leave a '.' behind (e.g. the `.*` of
        // <all fields reference>, 7.16), so both alternatives are backtracking.
        .>>. many (attempt pDereferenceReference <|> attempt pMethodOrFieldReference)
        |>> fun (e, refs) -> List.fold (fun acc f -> f acc) e refs

    let private pValueExpressionPrimaryWithPredicates = pValueExpressionPrimaryImpl true

    let pValueExpressionPrimary = pValueExpressionPrimaryImpl false

    // 6.37 <interval primary> ::= <value expression primary> [ <interval qualifier> ]
    //     | <interval value function>
    // A qualifier-less <interval primary> is represented by its <value expression primary>,
    // so wrapping only happens when an <interval qualifier> is actually present.
    // The grammar's primary is used: predicates and the '*' wildcard are not
    // <value expression primary>s and are rejected in interval context.
    let private pIntervalPrimary =
        pValueExpressionPrimary .>>. opt (attempt pIntervalQualifier)
        |>> fun (e, qualifier) ->
            match qualifier with
            | Some qual ->
                { Expression.Kind = IntervalPrimary(e, qual)
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
    let private pIntervalTerm =
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

    // 6.35 <time zone> ::= AT <time zone specifier>
    //   <time zone specifier> ::= LOCAL | TIME ZONE <interval primary>
    let private pTimeZoneSuffix =
        pKeyword "AT"
        >>. choice
                [ pKeyword "LOCAL" >>% TimeZoneSpecifier.TimeZoneLocal
                  pKeyword "TIME" >>. pKeyword "ZONE" >>. pIntervalPrimary
                  |>> TimeZoneSpecifier.TimeZoneOffset ]
        |>> fun spec ->
            fun e ->
                { Expression.Kind = AtTimeZone(e, spec)
                  Pos = e.Pos }

    // 6.35 <datetime term> ::= <datetime factor>
    //   <datetime factor> ::= <datetime primary> [ <time zone> ]
    //   <datetime primary> ::= <value expression primary> | <datetime value function>
    // (<datetime value function> is one of the pValueExpressionPrimary alternatives.)
    // The grammar's primary is used: predicates and the '*' wildcard are not
    // <value expression primary>s and are rejected in datetime context.
    let private pDatetimeTerm =
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
    let private pMultisetSetOperatorSuffix =
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

    let private addPrefix op precedence mapping =
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

    // 6.28 <value expression> — `opp` plus the 8.2/8.9 comparison-operand check.
    let private pValueExpressionChecked =
        // 8.2/8.9 — both operands of a comparison are <row value predicand>s, so a TOP-LEVEL
        // boolean operand is rejected: left-associative `opp` would otherwise chain
        // (`a = b = c` ≡ `(a = b) = c`) and a predicate could stand on either side
        // (`x = EXISTS (...)`). The check reads the parser result's top node — a chain always
        // surfaces there, while a parenthesized boolean (`(a = b) = c`) stays legal; the `=`
        // DESUGARED by 6.12 NULLIF is nested inside its Case and must not be flagged.
        let isComparisonOperator =
            function
            | BinaryOperator.Equal
            | BinaryOperator.NotEqual
            | BinaryOperator.LessThan
            | BinaryOperator.LessThanOrEqual
            | BinaryOperator.GreaterThan
            | BinaryOperator.GreaterThanOrEqual -> true
            | _ -> false

        let invalidComparisonOperands (e: Expression) =
            match e.Kind with
            | BinaryOp(op, l, r) when isComparisonOperator op -> isBooleanTopLevel l || isBooleanTopLevel r
            | QuantifiedComparison(_, _, x, _) -> isBooleanTopLevel x
            | _ -> false

        opp.ExpressionParser
        >>= fun e ->
            if invalidComparisonOperands e then
                fail "the operands of a comparison must be <row value predicand>s (8.2)"
            else
                preturn e

    // 6.28 <value expression> without boolean operators or predicates — used where the
    // grammar requires a non-boolean <value expression> (e.g. <point in time> in
    // <query system time period specification>, 7.6). Stops before AND/OR.
    pNonBooleanValueExpressionRef.Value <- pValueExpressionChecked

    // 6.29 <numeric value expression> — arithmetic-only opp (no comparisons, no predicates).
    // Used where the grammar requires <numeric value expression> (TABLESAMPLE percentage,
    // <repeat argument>, array subscript, SUBSTRING start position / string length,
    // OVERLAY start position / length, TRIM_ARRAY count, POSITION start position,
    // WIDTH_BUCKET bounds/count).
    let private oppNumeric = new OperatorPrecedenceParser<Expression, unit, unit>()
    oppNumeric.TermParser <- pValueExpressionPrimary

    let private addInfixNum op precedence assoc mapping =
        oppNumeric.AddOperator(InfixOperator(op, ws, precedence, assoc, fun x y -> { Kind = mapping x y; Pos = x.Pos }))

    let private addPrefixNum op precedence mapping =
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

    // 6.39 <boolean test> ::= <boolean primary> IS [ NOT ] { TRUE | FALSE | UNKNOWN }
    // The only predicate suffix that may follow a boolean primary, so ExpressionParser's
    // pBooleanTestSuffixes offers it alone once the accumulated expression is a top-level
    // boolean. `IS [ NOT ] NULL` (8.8) is a separate alternative (pNullPart2 below).
    let pBooleanTestPart2 =
        attempt (
            pKeyword "IS" >>. opt (pKeyword "NOT")
            .>>. (pKeyword "TRUE" >>% Some true
                  <|> (pKeyword "FALSE" >>% Some false)
                  <|> (pKeyword "UNKNOWN" >>% None))
            |>> fun (isNot, b) ->
                fun e ->
                    { Expression.Kind = IsBoolean(e, Option.isSome isNot, b)
                      Pos = e.Pos }
        )

    // 8 Predicates — forward refs (defined in PredicateParser.fs, compiled after QueryParser.fs;
    // wired in SqlParser.fs). §6.3 <value expression primary> and §6.39 <boolean test> consume
    // them, so they must be referenced before PredicateParser is compiled.
    // 8.1 <predicate> — the postfix predicate suffix (Expression -> Expression)
    let private pPredicate, pPredicateRef =
        createParserForwardedToRef<Expression -> Expression, unit> ()

    // 8.1 <predicate> without the 6.39 <boolean test> alternative — used when the accumulated
    // expression may take every §8 predicate suffix but not a boolean test, because a boolean
    // test's left operand is a <boolean primary> (see pBooleanTestSuffixes below).
    let private pPredicateNoBooleanTest, pPredicateNoBooleanTestRef =
        createParserForwardedToRef<Expression -> Expression, unit> ()

    // 6.39 <boolean test> / 8.x <predicate> / 6.24 <array element reference> / 6.35 AT TIME ZONE /
    // 6.43 multiset set operators — postfix suffixes applied to a <value expression>.
    //
    // The predicate alternative is conditioned on the accumulated expression, because
    // 6.39 <boolean primary> ::= <predicate> | <boolean predicand> and
    //   <boolean predicand> ::= <parenthesized boolean value expression>
    //                         | <nonparenthesized value expression primary>:
    //   * a top-level boolean (a comparison or a predicate) may take the boolean test ALONE —
    //     every 8.x predicate takes a <row value predicand>, which a predicate is not;
    //   * a boolean test is not a <boolean primary>, so no predicate suffix may follow it;
    //   * a term (`1 + 1`) takes every 8.x predicate — it is a <common value expression>,
    //     hence a <row value predicand> — but NOT the boolean test;
    //   * every other shape (a primary, a parenthesized expression, `EXISTS (...)`) may take
    //     any suffix, pPredicate offering the boolean test itself.
    // NOTE: no [<TailCall>] here — the loop is monadic (`suffix >>= …`), so F# reports it as
    // non-tail-recursive (FS3569). Its depth is bounded by the number of postfix suffixes in
    // the input, each of which consumes at least one token.
    let rec private pBooleanTestSuffixes e =
        let pNonPredicateSuffix =
            (pArrayElementReference |>> fun applySuffix -> applySuffix e)
            <|> attempt (pMultisetSetOperatorSuffix |>> fun applySuffix -> applySuffix e)
            <|> attempt (pTimeZoneSuffix |>> fun applySuffix -> applySuffix e)

        let predicateSuffix =
            match e.Kind with
            | IsBoolean _ -> None
            | _ when isBooleanTopLevel e -> Some(pBooleanTestPart2 |>> fun applySuffix -> applySuffix e)
            | BinaryOp _
            | UnaryOp _
            | RowValueConstructor _ -> Some(pPredicateNoBooleanTest |>> fun applySuffix -> applySuffix e)
            | _ -> Some(pPredicate |>> fun applySuffix -> applySuffix e)

        let suffix =
            match predicateSuffix with
            | Some predicate -> predicate <|> pNonPredicateSuffix
            | None -> pNonPredicateSuffix

        suffix >>= pBooleanTestSuffixes <|> preturn e

    let private pBooleanTest = pValueExpressionChecked >>= pBooleanTestSuffixes

    // 6.39 <boolean factor> ::= [ NOT ] <boolean test>
    let private pBooleanFactor, private pNotExprRef =
        createParserForwardedToRef<Expression, unit> ()

    pNotExprRef.Value <-
        (attempt (pKeyword "NOT" >>. pBooleanFactor)
         |>> fun e ->
             { Expression.Kind = UnaryOp(Not, e)
               Pos = e.Pos })
        <|> pBooleanTest

    // 6.39 <boolean value expression> ::= <boolean term> | <boolean value expression> OR <boolean term>
    let private pBooleanValueExpression =
        // 6.39 <boolean term> ::= <boolean factor> | <boolean term> AND <boolean factor>
        let pBooleanTerm =
            chainl1
                pBooleanFactor
                (pKeyword "AND"
                 >>% fun l r ->
                     { Expression.Kind = BinaryOp(And, l, r)
                       Pos = l.Pos })

        chainl1
            pBooleanTerm
            (pKeyword "OR"
             >>% fun l r ->
                 { Expression.Kind = BinaryOp(Or, l, r)
                   Pos = l.Pos })

    // Post-parse violations: a standalone 8.9 ANY/SOME/ALL (subquery) term (`comparisonOp`
    // rewrites it into QuantifiedComparison when a comparison operator precedes it; one that
    // survives is standalone) and a 8.20 <period predicate> whose left operand is not a
    // <period predicand>. Every other predicate left operand is enforced while parsing —
    // pBooleanTestSuffixes only offers a predicand-taking suffix when the accumulated
    // expression is not a boolean, because a desugared COALESCE builds IsNull nodes that
    // must not be re-checked here.
    //
    // The traversal uses an explicit work list so it is tail-recursive: an expression tree can
    // be arbitrarily deep, and a boolean short-circuit (`a || b`) cannot put both recursive
    // calls in tail position. The child collectors sit at module level (rather than inside the
    // search) so they are not rebuilt on every visited node.
    [<TailCall>]
    let rec private findExpressionViolationIn (work: Expression list) =
        // 8.20 <period reference> ::= <basic identifier chain> — the only left operand a
        // <period predicate> admits besides PERIOD ( <start>, <end> ).
        let isPeriodReference (e: Expression) =
            match e.Kind with
            | Identifier _
            | ColumnReference _
            | PeriodValue _ -> true
            | _ -> false

        let sqlArgumentChildren (arguments: SqlArgument list) = collectSqlArgumentChildren arguments []

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
            // A Parenthesized node TRANSLUCENTLY forwards its child: the standalone-ANY
            // guard below must see through it (docs/gotchas.md — a new Expression case
            // holding an Expression silently escapes the catch-all arm if unlisted).
            | Parenthesized inner -> [ inner ]
            | FunctionCall(name, _, arguments, _, filter, withinGroup) ->
                [ yield name
                  yield! sqlArgumentChildren arguments.Arguments
                  yield! Option.toList filter
                  yield! withinGroup |> Option.defaultValue [] |> List.map (fun (e, _, _) -> e) ]
            | Cast(x, _, _) -> [ x ]
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
            | IsJson(x, _, _, _, _) -> [ x ]
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
            | GeneralizedInvocation(r, _, name, arguments) ->
                [ yield r
                  yield name
                  yield! sqlArgumentChildren (arguments |> Option.toList |> List.collect (fun a -> a.Arguments)) ]
            | Dereference(r, name, arguments) ->
                [ yield r
                  yield name
                  yield! sqlArgumentChildren (arguments |> Option.toList |> List.collect (fun a -> a.Arguments)) ]
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
        | [] -> None
        | e :: rest ->
            match e.Kind with
            | QuantifiedSubquery _ -> Some "quantified subquery requires a comparison operator"
            | PeriodPredicate(_, left, _) when not (isPeriodReference left) ->
                Some "the left operand of a <period predicate> must be a <period predicand> (8.20)"
            | _ -> findExpressionViolationIn (expressionChildren e @ rest)

    pExpressionRef.Value <-
        pBooleanValueExpression
        >>= fun e ->
            match findExpressionViolationIn [ e ] with
            | Some message -> fail message
            | None -> preturn e
