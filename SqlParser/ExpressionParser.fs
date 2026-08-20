namespace SqlParser

open FParsec
open SqlParser.Lexer

module ExpressionParser =
    // 6.39 <boolean value expression> / 6.3 <value expression> — forward ref (central expression parser)
    let pExpression, pExpressionRef = createParserForwardedToRef<Expression, unit> ()
    // 6.1 <data type> — forward ref (defined in TypeParser; used by CAST / JSON returning)
    let pDataType, pDataTypeRef = createParserForwardedToRef<DataType, unit> ()
    // 7.17 <query expression> — forward ref (defined in QueryParser; used by scalar/quantified subqueries)
    let pQuery, pQueryRef = createParserForwardedToRef<Query, unit> ()

    // Forward references to the 7.8/7.9 row pattern parsers (defined later in this
    // module, after pExpression). Used by the 7.15 <window frame clause> and by
    // QueryParser's MATCH_RECOGNIZE.
    // 7.8 <row pattern measures> — forward ref
    let pRowPatternMeasures, pRowPatternMeasuresRef =
        createParserForwardedToRef<RowPatternMeasure list, unit> ()

    // 7.9 <row pattern common syntax> — forward ref
    let pRowPatternCommon, pRowPatternCommonRef =
        createParserForwardedToRef<RowPatternCommon, unit> ()

    // 6.3 <value expression primary> helper — attaches source position to an ExpressionKind
    let withExprPosition p =
        getPosition .>>. p
        |>> fun (pos, kind) ->
            { Expression.Kind = kind
              Pos = { Line = pos.Line; Column = pos.Column } }

    // 6.3 <literal> ::= NULL | <character string literal> | <numeric literal>
    //     | <boolean literal> | <datetime literal> | <interval literal> | <hex string literal>
    let pLiteralExpr =
        choice
            [ attempt (pKeyword "NULL" >>% Null |>> Literal)
              attempt (pCharacterStringLiteral |>> String |>> Literal)
              attempt (pNationalCharacterStringLiteral |>> NationalString |>> Literal)
              attempt (pUnicodeCharacterStringLiteral |>> UnicodeString |>> Literal)
              attempt (pNumericLiteral |>> Number |>> Literal)
              attempt (pBooleanLiteral |>> Bool |>> Literal)
              attempt (pDateLiteral |>> Date |>> Literal)
              attempt (pTimeLiteral |>> Time |>> Literal)
              attempt (pTimestampLiteral |>> Timestamp |>> Literal)
              attempt (pIntervalLiteral |>> Interval |>> Literal)
              attempt (pHexStringLiteral |>> Literal.Binary |>> Literal) ]
        |> withExprPosition

    // 6.3 <column reference> / 5.4 <identifier> — <identifier> | <column reference>
    let pIdentifierExpr = pIdentifier |>> Identifier |> withExprPosition

    // 7.16 <asterisk> ::= * — also used as <value expression primary> wildcard
    let pStarExpr = pstring "*" .>> ws >>% ExpressionKind.Star |> withExprPosition

    // A dotted identifier chain stops before '. <identifier> ( ... )' so that a
    // method invocation like a.b.method(x) parses as a column reference (a.b)
    // followed by a method-invocation postfix, while a.b.c (no parens) still
    // parses as a single ColumnReference.
    // 6.3 <column reference> ::= [ <table name> <period> ] <column name>
    let pColumnReferenceExpr =
        pIdentifier
        .>>. many (attempt (token (pstring ".") >>. pIdentifier .>>? notFollowedBy (token (pstring "("))))
        |>> function
            | (id, []) -> Identifier id
            | (first, rest) -> ColumnReference(first :: rest)
        |> withExprPosition

    // 5.4 <schema qualified name> / <identifier chain> — <identifier> [ { <period> <identifier> }... ]
    let pQualifiedName =
        pIdentifier .>>. many (token (pstring ".") >>. pIdentifier)
        |>> fun (first, rest) ->
            let parts = first :: rest

            match parts with
            | [ s ] -> Identifier s
            | _ -> ColumnReference parts
        |> withExprPosition

    // 6.13 <cast specification> ::= CAST ( <value expression> AS <data type> )
    let pCastSpecification =
        pKeyword "CAST"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        |>> Cast
        |> withExprPosition

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

    // 6.13 <case abbreviation> ::= NULLIF ( <value expression> , <value expression> )
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

    // 6.13 <case abbreviation> ::= COALESCE ( <value expression> [ { , <value expression> }... ] )
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

    // 7.17 <sort specification> ::= <sort key> [ <ordering specification> ] [ <null ordering> ]
    // 10.4 <ordering specification> ::= ASC | DESC — 7.17 <null ordering> ::= NULLS FIRST | NULLS LAST
    let pOrderByItem =
        let pNullsOrder =
            pKeyword "NULLS"
            >>. (pKeyword "FIRST" >>% NullsFirst <|> (pKeyword "LAST" >>% NullsLast))

        pExpression
        .>>. opt (attempt (pKeyword "ASC" >>% true) <|> attempt (pKeyword "DESC" >>% false))
        .>>. opt (attempt pNullsOrder)
        |>> fun ((expr, asc), nulls) -> expr, Option.defaultValue true asc, nulls

    // — the OVER (...) clause attached to a window function (also parsed at the query level for the WINDOW clause).
    // 7.15 <window name or specification> ::= <window name> | <window specification>
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
        // frame's MEASURES clause is not swallowed as a window name.
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

    // Literals are handled separately by pLiteralExpr above.
    // 6.4 <general value specification> — parameter forms (`?` dynamic parameter, `:name` host parameter) plus the keyword forms (CURRENT_USER, SESSION_USER, ...).
    let pGeneralValueSpecification =
        choice
            [ pQuestionMark >>% "?" <|> pHostParameter |>> Parameter |> withExprPosition
              pKeyword "CURRENT_CATALOG" >>% CurrentCatalog |> withExprPosition
              pKeyword "CURRENT_DEFAULT_TRANSFORM_GROUP" >>% CurrentDefaultTransformGroup
              |> withExprPosition
              pKeyword "CURRENT_PATH" >>% CurrentPath |> withExprPosition
              pKeyword "CURRENT_ROLE" >>% CurrentRole |> withExprPosition
              pKeyword "CURRENT_SCHEMA" >>% CurrentSchema |> withExprPosition
              pKeyword "CURRENT_USER" >>% CurrentUser |> withExprPosition
              pKeyword "SESSION_USER" >>% SessionUser |> withExprPosition
              pKeyword "SYSTEM_USER" >>% SystemUser |> withExprPosition
              pKeyword "USER" >>% User |> withExprPosition
              pKeyword "VALUE" >>% Value |> withExprPosition
              pKeyword "CURRENT_TRANSFORM_GROUP_FOR_TYPE" >>. pQualifiedName
              |>> CurrentTransformGroupForType
              |> withExprPosition
              // <current collation specification> ::= COLLATION FOR ( <string value expression> )
              pKeyword "COLLATION"
              >>. pKeyword "FOR"
              >>. between (token (pstring "(")) (token (pstring ")")) pExpression
              |>> CollationFor
              |> withExprPosition ]

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

    // 6.21 <datetime value function> ::= CURRENT_DATE | CURRENT_TIMESTAMP [ <left paren>
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

    // 6.14 <next value expression> ::= NEXT VALUE FOR <sequence generator name>
    let pNextValueExpression =
        pKeyword "NEXT" >>. pKeyword "VALUE" >>. pKeyword "FOR" >>. pQualifiedName
        |>> NextValueFor
        |> withExprPosition

    // 6.24 <left bracket> ::= [ | ??(  and  <right bracket> ::= ] | ??)
    let pLeftBracket = pstring "[" <|> pstring "??("
    // 6.24 <right bracket> ::= ] | ??)
    let pRightBracket = pstring "]" <|> pstring "??)"

    // 6.16 <subtype treatment> ::= TREAT ( <subtype operand> AS <target subtype> )
    let pTreatExpression =
        pKeyword "TREAT"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> pKeyword "AS" .>>. pDataType)
        |>> (fun (e, t) -> Treat(e, t))
        |> withExprPosition

    // 6.23 <reference resolution> ::= DEREF ( <reference value expression> )
    let pDerefExpression =
        pKeyword "DEREF"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> Deref
        |> withExprPosition

    // 6.25 <multiset element reference> ::= ELEMENT ( <multiset value expression> )
    let pElementExpression =
        pKeyword "ELEMENT"
        >>. between (token (pstring "(")) (token (pstring ")")) pExpression
        |>> Element
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

    // 6.24 <array element reference> — postfix [ <numeric value expression> ]
    let pArrayElementReference =
        between (token pLeftBracket) (token pRightBracket) pExpression
        |>> fun idx ->
            fun e ->
                { Expression.Kind = ArrayElement(e, idx)
                  Pos = e.Pos }

    // 8.20 <period predicate> operators (OVERLAPS is covered by the existing Overlaps case)
    let pPeriodPredicateKind =
        choice
            [ pKeyword "EQUALS" >>% PeriodEquals
              pKeyword "CONTAINS" >>% PeriodContains
              pKeyword "PRECEDES" >>% PeriodPrecedes
              pKeyword "SUCCEEDS" >>% PeriodSucceeds
              attempt (pKeyword "IMMEDIATELY" >>. pKeyword "PRECEDES" >>% PeriodImmediatelyPrecedes)
              attempt (pKeyword "IMMEDIATELY" >>. pKeyword "SUCCEEDS" >>% PeriodImmediatelySucceeds) ]

    // 8.20 <period predicand> ::= <period reference> | PERIOD ( <start> , <end> )
    let pPeriodValue =
        pKeyword "PERIOD"
        >>. between (token (pstring "(")) (token (pstring ")")) (pExpression .>> token (pstring ",") .>>. pExpression)
        |>> (fun (start, finish) -> PeriodValue(start, finish))
        |> withExprPosition

    // 10.12 <JSON representation> ::= JSON [ ENCODING { UTF8 | UTF16 | UTF32 } ]
    let pJsonEncoding =
        choice
            [ pKeyword "UTF8" >>% Utf8
              pKeyword "UTF16" >>% Utf16
              pKeyword "UTF32" >>% Utf32 ]

    // 10.12 <JSON representation> ::= JSON [ ENCODING { UTF8 | UTF16 | UTF32 } ]
    // 10.12 <JSON representation> ::= JSON [ ENCODING { UTF8 | UTF16 | UTF32 } ]
    let pJsonRepresentation =
        pKeyword "JSON" >>. opt (pKeyword "ENCODING" >>. pJsonEncoding) |>> JsonEncoding

    // 10.13 <JSON input clause> ::= FORMAT <JSON representation>
    let pJsonInputClause = pKeyword "FORMAT" >>. pJsonRepresentation

    // 10.13 <JSON output clause> ::= RETURNING <data type> [ FORMAT <JSON representation> ]
    let pJsonOutputClause =
        pKeyword "RETURNING" >>. pDataType
        .>>. opt (pKeyword "FORMAT" >>. pJsonRepresentation)
        |>> fun (ret, fmt) -> { Returning = ret; Format = fmt }

    // 6.27 <JSON value empty/error behavior> ::= ERROR | NULL | DEFAULT <value expression>
    let pJsonValueBehavior =
        choice
            [ pKeyword "ERROR" >>% JsonError
              pKeyword "NULL" >>% JsonNull
              pKeyword "DEFAULT" >>. pExpression |>> JsonDefault ]

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

    // 6.33 <JSON name and value> ::= [ KEY ] <JSON name> VALUE <JSON value expression>
    //                              | <JSON name> : <JSON value expression>
    let pJsonNameAndValue =
        choice
            [ attempt (
                  opt (pKeyword "KEY") .>>. pExpression .>> pKeyword "VALUE" .>>. pExpression
                  |>> fun ((key, name), value) ->
                      { Name = name
                        Value = value
                        Key = Option.isSome key }
              )
              pExpression .>> token (pstring ":") .>>. pExpression
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

    // 8.23 <JSON exists error behavior> ::= TRUE | FALSE | UNKNOWN | ERROR
    let pJsonExistsErrorBehavior =
        choice
            [ pKeyword "TRUE" >>% JsonExistsTrue
              pKeyword "FALSE" >>% JsonExistsFalse
              pKeyword "UNKNOWN" >>% JsonExistsUnknown
              pKeyword "ERROR" >>% JsonExistsError ]

    // 10.14 <JSON passing argument> ::= <JSON value expression> [ <JSON input clause> ] AS <identifier>
    let pJsonArgument =
        pExpression .>>. opt pJsonInputClause .>> pKeyword "AS" .>>. pIdentifierExpr
        |>> fun ((expr, _), name) -> (expr, name)

    // 10.14 <JSON API common syntax> ::= <JSON context item> , <JSON path specification>
    //     [ AS <JSON table path name> ] [ <JSON passing clause> ]
    let pJsonApiCommon =
        pExpression .>>. opt pJsonInputClause .>> token (pstring ",")
        .>>. (pCharacterStringLiteral |>> String |>> Literal |> withExprPosition)
        .>>. opt (attempt (pKeyword "AS" >>. pIdentifierExpr))
        .>>. opt (pKeyword "PASSING" >>. sepBy1 pJsonArgument (token (pstring ",")))
        |>> fun ((((context, _), path), pathName), passing) ->
            { Context = context
              Path = path
              PathName = pathName
              Passing = Option.defaultValue [] passing }

    // 7.9 <row pattern> — forward ref (recursive)
    let pRowPattern, pRowPatternRef = createParserForwardedToRef<RowPattern, unit> ()

    // 7.9 <row pattern quantifier> / 6.11 <row marker delta> — unsigned integer as <value expression> (quantifier bounds / row marker offsets)
    let pUnsignedIntegerExpr: Parser<Expression, unit> =
        getPosition .>>. token pUnsignedInteger
        |>> fun (pos, n) ->
            { Kind = Literal(Number(decimal n))
              Pos = { Line = pos.Line; Column = pos.Column } }

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
                      (token (pstring "{"))
                      (token (pstring "}"))
                      (opt pUnsignedIntegerExpr .>> token (pstring ",") .>>. opt pUnsignedIntegerExpr)
                  .>>. opt (token (pstring "?"))
                  |>> fun ((lo, hi), q) -> RowPatternQuantifier.Brace(lo, hi, Option.isSome q)
              )
              attempt (
                  between (token (pstring "{")) (token (pstring "}")) pUnsignedIntegerExpr
                  |>> RowPatternQuantifier.BraceExact
              ) ]

    // 7.9 <row pattern primary>
    let pRowPatternPrimary =
        choice
            [ attempt (
                  token (pstring "{-") >>. pRowPattern .>> token (pstring "-}")
                  |>> RowPatternExclude
              )
              attempt (token (pstring "^") >>% RowPatternAnchorStart)
              attempt (token (pstring "$") >>% RowPatternAnchorEnd)
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
    pRowPatternRef.Value <- sepBy1 pRowPatternTerm (token (pstring "|")) |>> fun terms -> { Terms = terms }

    // 7.8 <row pattern measure definition> ::= <row pattern measure expression> AS <measure name>
    let pRowPatternMeasure =
        pExpression .>> pKeyword "AS" .>>. pIdentifierExpr
        |>> fun (expr, name) ->
            { RowPatternMeasure.Expression = expr
              Name = name }

    // 7.8 <row pattern measures> ::= MEASURES <row pattern measure list>
    pRowPatternMeasuresRef.Value <- pKeyword "MEASURES" >>. sepBy1 pRowPatternMeasure (token (pstring ","))

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
    pRowPatternCommonRef.Value <-
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

    // 6.27 <JSON value function> ::= JSON_VALUE ( <JSON API common syntax>
    //     [ <JSON returning clause> ] [ <JSON value empty behavior> ON EMPTY ]
    //     [ <JSON value error behavior> ON ERROR ] )
    let pJsonValueFunction =
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

    // 6.36 <JSON object aggregate> ::= JSON_OBJECTAGG ( <JSON name and value>
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

    // 6.36 <JSON array aggregate> ::= JSON_ARRAYAGG ( <JSON value expression>
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

    // plus optional OVER (window), FILTER (WHERE), WITHIN GROUP (ORDER BY) clauses.
    // 10.9 <routine invocation> ::= <routine name> <SQL argument list>
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

        pIdentifierRaw |>> Identifier |> withExprPosition
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

    // 6.3 <scalar subquery> ::= ( <subquery> )
    let pScalarSubquery =
        between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> SubqueryExpression
        |> withExprPosition

    // 6.3 <SQL argument list> (plain — no DISTINCT/ALL; used by <method invocation>,
    // <static method invocation> and <new specification>)
    let pValueExpressionList =
        between (token (pstring "(")) (token (pstring ")")) (sepBy pExpression (token (pstring ",")))

    // 6.18 <static method invocation> ::= <path-resolved UDT name> :: <method name>
    //     [ <SQL argument list> ]
    let pStaticMethodInvocation =
        pQualifiedName
        .>>. (token (pstring "::") >>. pIdentifierExpr .>>. pValueExpressionList)
        |>> fun (typ, (name, args)) -> StaticMethodInvocation(typ, name, args)
        |> withExprPosition

    // 6.19 <new specification> ::= NEW <path-resolved UDT name> <SQL argument list>
    let pNewSpecification =
        pKeyword "NEW" >>. pQualifiedName .>>. pValueExpressionList
        |>> fun (typ, args) -> NewSpecification(typ, args)
        |> withExprPosition

    // — postfix '. <method name> [ ( <args> ) ]' applied to any <value expression primary> (with args → MethodInvocation; without → FieldReference)
    // 6.17 <method invocation> (direct form) / 6.15 <field reference> ::= <value expression primary> <period> <method name> [ <SQL argument list> ] | <value expression primary> <period> <field name>
    let pMethodOrFieldReference =
        token (pstring ".") >>. pIdentifierExpr .>>. opt pValueExpressionList
        |>> fun (name, args) ->
            match args with
            | Some a ->
                fun r ->
                    { Expression.Kind = MethodInvocation(r, name, a)
                      Pos = r.Pos }
            | None ->
                fun r ->
                    { Expression.Kind = FieldReference(r, name)
                      Pos = r.Pos }

    // 7.18 <search clause> ::= SEARCH { DEPTH FIRST | BREADTH FIRST } BY <cols> SET <col>
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

    // 7.17 <with list element> ::= <query name> [ ( <with column list> ) ] AS <table subquery> [ <search or cycle clause> ]
    let pCte =
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

    // 8.9 <quantifier> ::= ALL | SOME | ANY
    let pQuantifier =
        choice
            [ pKeyword "ANY" >>% Quantifier.Any
              pKeyword "SOME" >>% Quantifier.SomeQuantifier
              pKeyword "ALL" >>% Quantifier.All ]

    // Only valid as the right operand of a comparison operator (see comparisonOp).
    // 8.9 <quantified comparison predicate> — the ANY | SOME | ALL subquery term
    let pQuantifiedSubqueryTerm =
        pQuantifier .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> fun (quant, q) -> QuantifiedSubquery(quant, q)
        |> withExprPosition

    // 8.19 <user-defined type specification> ::= <user-defined type name> | ONLY <user-defined type name>
    let pTypeSpec =
        choice
            [ pKeyword "ONLY" >>. pQualifiedName |>> Exclusive
              pQualifiedName |>> Inclusive ]

    // 8.1 <predicate> — a postfix predicate applied to a <value expression primary>:
    //   <between predicate>, <in predicate>, <null predicate>, <distinct predicate>,
    //   <overlaps predicate>, <like predicate>, <similar predicate>, plus <collate clause>.
    let pPredicate pExpr =
        choice
            [ // 8.3 <between predicate> ::= <row value predicand> [ NOT ] BETWEEN [ ASYMMETRIC | SYMMETRIC ] <row value predicand> AND <row value predicand>
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "BETWEEN"
                  .>>. opt (pKeyword "ASYMMETRIC" <|> pKeyword "SYMMETRIC")
                  .>>. pExpr
                  .>> pKeyword "AND"
                  .>>. pExpr
                  |>> fun (((isNot, sym), start), endBound) ->
                      fun e ->
                          { Expression.Kind =
                              ExpressionKind.Between(e, Option.isSome isNot, Option.isSome sym, start, endBound)
                            Pos = e.Pos }
              )
              // 8.4 <in predicate> ::= <row value predicand> [ NOT ] IN { <table subquery> | <in predicate value list> }
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "IN"
                  .>>. between
                      (token (pstring "("))
                      (token (pstring ")"))
                      (attempt pQuery |>> Choice1Of2
                       <|> (sepBy1 pExpr (token (pstring ",")) |>> Choice2Of2))
                  |>> fun (isNot, res) ->
                      fun e ->
                          let kind =
                              match res with
                              | Choice1Of2 q -> InSubquery(e, Option.isSome isNot, q)
                              | Choice2Of2 l -> InList(e, Option.isSome isNot, l)

                          { Expression.Kind = kind; Pos = e.Pos }
              )
              // 8.8 <null predicate> ::= <row value predicand> IS [ NOT ] NULL — 8.21 <boolean test> ::= <boolean primary> IS [ NOT ] { TRUE | FALSE | UNKNOWN }
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT")
                  .>>. (pKeyword "NULL" >>% Choice1Of2()
                        <|> (pKeyword "TRUE" >>% Choice2Of2(Some true))
                        <|> (pKeyword "FALSE" >>% Choice2Of2(Some false))
                        <|> (pKeyword "UNKNOWN" >>% Choice2Of2 None))
                  |>> fun (isNot, res) ->
                      fun e ->
                          let kind =
                              match res with
                              | Choice1Of2() -> IsNull(e, Option.isSome isNot)
                              | Choice2Of2 b -> IsBoolean(e, Option.isSome isNot, b)

                          { Expression.Kind = kind; Pos = e.Pos }
              )
              // 8.15 <distinct predicate> ::= <row value predicand> IS [ NOT ] DISTINCT FROM <row value predicand>
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT")
                  .>> pKeyword "DISTINCT"
                  .>> pKeyword "FROM"
                  .>>. pExpr
                  |>> fun (isNot, r) ->
                      fun l ->
                          { Expression.Kind = IsDistinctFrom(l, Option.isSome isNot, r)
                            Pos = l.Pos }
              )
              // 8.14 <overlaps predicate> ::= <row value predicand 1> OVERLAPS <row value predicand 2>
              attempt (
                  pKeyword "OVERLAPS" >>. pExpr
                  |>> fun r ->
                      fun l ->
                          { Expression.Kind = Overlaps(l, r)
                            Pos = l.Pos }
              )
              // 8.5 <like predicate> ::= <character string value expression> [ NOT ] LIKE <character string pattern> [ ESCAPE <escape character> ]
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "LIKE"
                  .>>. pExpr
                  .>>. opt (pKeyword "ESCAPE" >>. pExpr)
                  |>> fun ((isNot, pattern), escape) ->
                      fun l ->
                          { Expression.Kind = Like(l, Option.isSome isNot, pattern, escape)
                            Pos = l.Pos }
              )
              // 8.6 <similar predicate> ::= <character string value expression> [ NOT ] SIMILAR TO <character string pattern> [ ESCAPE <escape character> ]
              attempt (
                  opt (pKeyword "NOT") .>> attempt (pKeyword "SIMILAR" .>> pKeyword "TO")
                  .>>. pExpr
                  .>>. opt (pKeyword "ESCAPE" >>. pExpr)
                  |>> fun ((isNot, pattern), escape) ->
                      fun l ->
                          { Expression.Kind = SimilarTo(l, Option.isSome isNot, pattern, escape)
                            Pos = l.Pos }
              )
              // 6.31 <collate clause> ::= COLLATE <collation name>
              attempt (
                  pKeyword "COLLATE" >>. pIdentifierExpr
                  |>> fun collation ->
                      fun e ->
                          { Expression.Kind = Collate(e, collation)
                            Pos = e.Pos }
              )
              // 8.12 <normalized predicate> ::= IS [ NOT ] [ <normal form> ] NORMALIZED
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT")
                  .>>. opt (
                      pKeyword "NFC" >>% Nfc
                      <|> (pKeyword "NFD" >>% Nfd)
                      <|> (pKeyword "NFKC" >>% Nfkc)
                      <|> (pKeyword "NFKD" >>% Nfkd)
                  )
                  .>> pKeyword "NORMALIZED"
                  |>> fun (isNot, form) ->
                      fun e ->
                          { Expression.Kind = IsNormalized(e, Option.isSome isNot, form)
                            Pos = e.Pos }
              )
              // 8.19 <type predicate> ::= IS [ NOT ] OF ( <type list> )
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "OF"
                  .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy1 pTypeSpec (token (pstring ",")))
                  |>> fun (isNot, types) ->
                      fun e ->
                          { Expression.Kind = IsOfType(e, Option.isSome isNot, types)
                            Pos = e.Pos }
              )
              // 8.22 <JSON predicate> ::= IS [ NOT ] JSON [ VALUE | ARRAY | OBJECT | SCALAR ]
              //     [ WITH | WITHOUT UNIQUE [ KEYS ] ]
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "JSON"
                  .>>. opt (
                      pKeyword "VALUE" >>% JsonTypeValue
                      <|> (pKeyword "ARRAY" >>% JsonTypeArray)
                      <|> (pKeyword "OBJECT" >>% JsonTypeObject)
                      <|> (pKeyword "SCALAR" >>% JsonTypeScalar)
                  )
                  .>>. opt (
                      attempt (
                          pKeyword "WITH" >>% Some true <|> (pKeyword "WITHOUT" >>% Some false)
                          .>> pKeyword "UNIQUE"
                          .>> opt (pKeyword "KEYS")
                      )
                  )
                  |>> fun ((isNot, typeConstraint), unique) ->
                      fun e ->
                          { Expression.Kind = IsJson(e, Option.isSome isNot, typeConstraint, Option.flatten unique)
                            Pos = e.Pos }
              )
              // 8.7 <regex like predicate> ::= [ NOT ] LIKE_REGEX <XQuery pattern>
              //     [ FLAG <XQuery option flag> ]
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "LIKE_REGEX"
                  .>>. pExpr
                  .>>. opt (pKeyword "FLAG" >>. pExpr)
                  |>> fun ((isNot, pattern), flag) ->
                      fun e ->
                          { Expression.Kind = RegexLike(e, Option.isSome isNot, pattern, flag)
                            Pos = e.Pos }
              )
              // 8.13 <match predicate> ::= MATCH [ UNIQUE ] [ SIMPLE | PARTIAL | FULL ]
              //     <table subquery>
              attempt (
                  pKeyword "MATCH" >>. opt (pKeyword "UNIQUE" >>% true)
                  .>>. opt (
                      pKeyword "SIMPLE" >>% Simple
                      <|> (pKeyword "PARTIAL" >>% Partial)
                      <|> (pKeyword "FULL" >>% Full)
                  )
                  .>>. between (token (pstring "(")) (token (pstring ")")) pQuery
                  |>> fun ((isUnique, matchOption), q) ->
                      fun e ->
                          { Expression.Kind = Match(e, Option.isSome isUnique, matchOption, q)
                            Pos = e.Pos }
              )
              // 8.16 <member predicate> ::= [ NOT ] MEMBER [ OF ] <multiset value expression>
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "MEMBER" .>> opt (pKeyword "OF") .>>. pExpr
                  |>> fun (isNot, multiset) ->
                      fun e ->
                          { Expression.Kind = MemberOf(e, Option.isSome isNot, multiset)
                            Pos = e.Pos }
              )
              // 8.17 <submultiset predicate> ::= [ NOT ] SUBMULTISET [ OF ] <multiset value expression>
              attempt (
                  opt (pKeyword "NOT") .>> pKeyword "SUBMULTISET" .>> opt (pKeyword "OF")
                  .>>. pExpr
                  |>> fun (isNot, multiset) ->
                      fun e ->
                          { Expression.Kind = SubmultisetOf(e, Option.isSome isNot, multiset)
                            Pos = e.Pos }
              )
              // 8.18 <set predicate> ::= IS [ NOT ] A SET
              attempt (
                  pKeyword "IS" >>. opt (pKeyword "NOT") .>> pKeyword "A" .>> pKeyword "SET"
                  |>> fun isNot ->
                      fun e ->
                          { Expression.Kind = IsSet(e, Option.isSome isNot)
                            Pos = e.Pos }
              )
              // 8.20 <period predicate> ::= <period predicate operator> <period predicand>
              attempt (
                  pPeriodPredicateKind .>>. (attempt pPeriodValue <|> pExpr)
                  |>> fun (kind, right) ->
                      fun left ->
                          { Expression.Kind = PeriodPredicate(kind, left, right)
                            Pos = left.Pos }
              ) ]

    // 8.10 <exists predicate> ::= EXISTS ( <subquery> )
    let pExistsPredicate =
        pKeyword "EXISTS" >>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> Exists
        |> withExprPosition

    // 8.11 <unique predicate> ::= UNIQUE ( <subquery> )
    let pUniquePredicate =
        pKeyword "UNIQUE" >>. between (token (pstring "(")) (token (pstring ")")) pQuery
        |>> ExpressionKind.Unique
        |> withExprPosition

    // 8.23 <JSON exists predicate> ::= JSON_EXISTS ( <JSON API common syntax>
    //     [ <JSON exists error behavior> ON ERROR ] )
    let pJsonExistsPredicate =
        pKeyword "JSON_EXISTS"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (pJsonApiCommon
                 .>>. opt (pJsonExistsErrorBehavior .>> pKeyword "ON" .>> pKeyword "ERROR"))
        |>> fun (common, onError) -> JsonExists(common, onError)
        |> withExprPosition

    // — the atomic building block of every <value expression>, used as the term parser of the operator-precedence parser below.
    // 6.3 <value expression primary> — the atomic building block of every <value expression>
    let pValueExpressionPrimary =
        choice
            [ attempt pCastSpecification
              attempt pCaseExpression
              attempt pNullifExpr
              attempt pCoalesceExpr
              attempt pExtractExpression
              attempt pPositionExpression
              attempt pTrimFunction
              attempt pCharacterSubstringFunction
              attempt pOverlayFunction
              attempt pDateTimeValueFunction
              attempt pNextValueExpression
              attempt pTreatExpression
              attempt pDerefExpression
              attempt pElementExpression
              attempt pArrayValueConstructor
              attempt pMultisetValueConstructor
              attempt pJsonValueFunction
              attempt pJsonQueryFunction
              attempt pJsonObjectFunction
              attempt pJsonArrayFunction
              attempt pJsonObjectAggFunction
              attempt pJsonArrayAggFunction
              attempt pExistsPredicate
              attempt pUniquePredicate
              attempt pJsonExistsPredicate
              attempt pPeriodValue
              attempt pStaticMethodInvocation
              attempt pNewSpecification
              attempt pNestedRowNumberFunction
              attempt pValueOfFunction
              attempt pRoutineInvocation
              attempt pScalarSubquery
              attempt pLiteralExpr
              attempt pGeneralValueSpecification
              attempt pStarExpr
              attempt pQuantifiedSubqueryTerm
              pColumnReferenceExpr
              between (token (pstring "(")) (token (pstring ")")) pExpression ]
        .>>. many pMethodOrFieldReference
        |>> fun (e, refs) -> List.fold (fun acc f -> f acc) e refs

    // 6.4 <default specification> ::= DEFAULT — only valid in specific contexts (INSERT VALUES, UPDATE SET), not as a general expression. This parser is used by the DML parser for those contexts.
    let pDefaultValue: Parser<Expression, unit> =
        pKeyword "DEFAULT" >>% Default |> withExprPosition

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
    let comparisonOp (op: BinaryOperator) (x: Expression) (y: Expression) : ExpressionKind =
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

    // 6.29 <value expression> without boolean operators or predicates — used where the
    // grammar requires a non-boolean <value expression> (e.g. <point in time> in
    // <query system time period specification>, 7.6). Stops before AND/OR.
    let pValueExpressionNoBoolean = opp.ExpressionParser

    // 8.21 <boolean test> ::= <boolean primary> IS [ NOT ] { TRUE | FALSE | UNKNOWN } — combined here with
    // 8.x <predicate> / 6.24 <array element reference> postfix applied to a <value expression>
    let pBooleanTest =
        opp.ExpressionParser
        .>>. many (pPredicate opp.ExpressionParser <|> pArrayElementReference)
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
    let rec containsStandaloneQuantifiedSubquery (e: Expression) =
        let containsJsonCommon (c: JsonApiCommon) =
            containsStandaloneQuantifiedSubquery c.Context
            || containsStandaloneQuantifiedSubquery c.Path
            || Option.exists containsStandaloneQuantifiedSubquery c.PathName
            || List.exists
                (fun (v, n) -> containsStandaloneQuantifiedSubquery v || containsStandaloneQuantifiedSubquery n)
                c.Passing

        let containsJsonValueBehavior (b: JsonValueBehavior) =
            match b with
            | JsonDefault e -> containsStandaloneQuantifiedSubquery e
            | _ -> false

        let containsJsonQueryBehavior (_: JsonQueryBehavior) = false

        let containsJsonNameValue (nv: JsonNameValue) =
            containsStandaloneQuantifiedSubquery nv.Name
            || containsStandaloneQuantifiedSubquery nv.Value

        match e.Kind with
        | QuantifiedSubquery _ -> true
        | BinaryOp(_, l, r) -> containsStandaloneQuantifiedSubquery l || containsStandaloneQuantifiedSubquery r
        | UnaryOp(_, x) -> containsStandaloneQuantifiedSubquery x
        | FunctionCall(name, _, args, _, filter, withinGroup) ->
            containsStandaloneQuantifiedSubquery name
            || List.exists containsStandaloneQuantifiedSubquery args
            || Option.exists containsStandaloneQuantifiedSubquery filter
            || Option.exists (List.exists (fun (e, _, _) -> containsStandaloneQuantifiedSubquery e)) withinGroup
        | Cast(x, _) -> containsStandaloneQuantifiedSubquery x
        | Case(cond, whens, elseExpr) ->
            Option.exists containsStandaloneQuantifiedSubquery cond
            || List.exists
                (fun (w, t) -> containsStandaloneQuantifiedSubquery w || containsStandaloneQuantifiedSubquery t)
                whens
            || Option.exists containsStandaloneQuantifiedSubquery elseExpr
        | WindowFunction wf ->
            containsStandaloneQuantifiedSubquery wf.Function
            || List.exists containsStandaloneQuantifiedSubquery wf.Args
        | ExpressionKind.Between(x, _, _, lo, hi) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery lo
            || containsStandaloneQuantifiedSubquery hi
        | AllFieldsReference(x, cols) ->
            containsStandaloneQuantifiedSubquery x
            || Option.exists (List.exists containsStandaloneQuantifiedSubquery) cols
        | InList(x, _, items) ->
            containsStandaloneQuantifiedSubquery x
            || List.exists containsStandaloneQuantifiedSubquery items
        | IsNull(x, _) -> containsStandaloneQuantifiedSubquery x
        | IsBoolean(x, _, _) -> containsStandaloneQuantifiedSubquery x
        | IsDistinctFrom(x, _, y) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery y
        | Overlaps(x, y) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery y
        | Collate(x, c) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery c
        | Like(x, _, p, esc) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery p
            || Option.exists containsStandaloneQuantifiedSubquery esc
        | SimilarTo(x, _, p, esc) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery p
            || Option.exists containsStandaloneQuantifiedSubquery esc
        | Extract(x, f) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery f
        | Position(x, s, len) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery s
            || Option.exists containsStandaloneQuantifiedSubquery len
        | Trim(_, src, x) ->
            Option.exists containsStandaloneQuantifiedSubquery src
            || containsStandaloneQuantifiedSubquery x
        | Substring(x, s, len, _) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery s
            || Option.exists containsStandaloneQuantifiedSubquery len
        | Overlay(x, p, f, len) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery p
            || containsStandaloneQuantifiedSubquery f
            || Option.exists containsStandaloneQuantifiedSubquery len
        | QuantifiedComparison(_, _, x, _) -> containsStandaloneQuantifiedSubquery x
        | CurrentTransformGroupForType x -> containsStandaloneQuantifiedSubquery x
        | CollationFor x -> containsStandaloneQuantifiedSubquery x
        | ArrayConstructor xs -> List.exists containsStandaloneQuantifiedSubquery xs
        | MultisetConstructor xs -> List.exists containsStandaloneQuantifiedSubquery xs
        | ArrayElement(x, idx) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery idx
        | Treat(x, _) -> containsStandaloneQuantifiedSubquery x
        | Deref x -> containsStandaloneQuantifiedSubquery x
        | Element x -> containsStandaloneQuantifiedSubquery x
        | IsNormalized(x, _, _) -> containsStandaloneQuantifiedSubquery x
        | IsOfType(x, _, types) ->
            containsStandaloneQuantifiedSubquery x
            || List.exists
                (fun t ->
                    match t with
                    | Inclusive e -> containsStandaloneQuantifiedSubquery e
                    | Exclusive e -> containsStandaloneQuantifiedSubquery e)
                types
        | IsJson(x, _, _, _) -> containsStandaloneQuantifiedSubquery x
        | RegexLike(x, _, p, flag) ->
            containsStandaloneQuantifiedSubquery x
            || containsStandaloneQuantifiedSubquery p
            || Option.exists containsStandaloneQuantifiedSubquery flag
        | Match(x, _, _, _) -> containsStandaloneQuantifiedSubquery x
        | MemberOf(x, _, m) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery m
        | SubmultisetOf(x, _, m) -> containsStandaloneQuantifiedSubquery x || containsStandaloneQuantifiedSubquery m
        | IsSet(x, _) -> containsStandaloneQuantifiedSubquery x
        | PeriodPredicate(_, l, r) -> containsStandaloneQuantifiedSubquery l || containsStandaloneQuantifiedSubquery r
        | PeriodValue(s, e) -> containsStandaloneQuantifiedSubquery s || containsStandaloneQuantifiedSubquery e
        | JsonExists(common, _) -> containsJsonCommon common
        | JsonValue(common, _, onEmpty, onError) ->
            containsJsonCommon common
            || Option.exists containsJsonValueBehavior onEmpty
            || Option.exists containsJsonValueBehavior onError
        | JsonQuery(common, _, _, _, onEmpty, onError) ->
            containsJsonCommon common
            || Option.exists containsJsonQueryBehavior onEmpty
            || Option.exists containsJsonQueryBehavior onError
        | JsonObject(nvs, _, _, _) -> List.exists containsJsonNameValue nvs
        | JsonArray(xs, _, _) -> List.exists containsStandaloneQuantifiedSubquery xs
        | JsonObjectAgg(nv, _, _, _) -> containsJsonNameValue nv
        | JsonArrayAgg(x, orderBy, _, _) ->
            containsStandaloneQuantifiedSubquery x
            || Option.exists (List.exists (fun (e, _, _) -> containsStandaloneQuantifiedSubquery e)) orderBy
        | _ -> false

    pExpressionRef.Value <-
        pOrExpr
        >>= fun e ->
            if containsStandaloneQuantifiedSubquery e then
                fail "quantified subquery requires a comparison operator"
            else
                preturn e
