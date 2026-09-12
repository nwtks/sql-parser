namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module Types =
    // 6.1 <data type> — re-export (defined in ExpressionParser/TypeParser)
    let pDataType = ExpressionParser.pDataType

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
    let pIntervalType =
        pKeyword "INTERVAL" >>. many1Chars (noneOf "();,") .>> ws |>> IntervalType

    // 6.1 <row type> ::= ROW <row type body> — <row type body> ::= ( <field definition> [ { , <field definition> }... ] )
    let pRowType =
        pKeyword "ROW"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 (pIdentifierExpr .>>. pDataType) (token (pstring ",")))
        |>> RowType

    // 6.1 <data type> — element type parser: all types EXCEPT collection types (to avoid left recursion)
    let pDataTypeElement, pDataTypeElementRef =
        createParserForwardedToRef<DataType, unit> ()

    // 10.6 <routine type> / 11.51 <partial method specification> — [ INSTANCE | STATIC | CONSTRUCTOR ]
    // Shared by 10.6 (<routine type>, DdlParser.fs), 11.60 (<method specification designator>,
    // RoutineParser.fs) and 11.51 (<partial method specification>, TypeParser.fs); it lives here
    // because Types.fs is compiled before all three. INSTANCE and CONSTRUCTOR are not reserved
    // words, hence the `attempt`s.
    let pMethodKind: Parser<MethodKind, unit> =
        choice
            [ attempt (pKeyword "INSTANCE" >>% MethodKind.Instance)
              attempt (pKeyword "STATIC" >>% MethodKind.Static)
              attempt (pKeyword "CONSTRUCTOR" >>% MethodKind.Constructor) ]

    // 6.1 <scope clause> ::= SCOPE <table name>
    // Shared by <reference type> (6.1), <column option list> (11.3) and
    // <add column scope clause> (11.17); it lives here because Types.fs is
    // compiled before DdlParser.fs.
    let pScopeClause: Parser<Expression, unit> = pKeyword "SCOPE" >>. pQualifiedNameExpr

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

    pDataTypeElementRef.Value <-
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

    pDataTypeRef.Value <- choice [ attempt pCollectionType; pDataTypeElement ]
