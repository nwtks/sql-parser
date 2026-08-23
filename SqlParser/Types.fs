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

    // 6.1 <collection type> ::= <array type> | <multiset type> — <array type> ::= <data type> ARRAY [ [ <maximum cardinality> ] ] — <multiset type> ::= <data type> MULTISET
    let pCollectionType =
        pDataTypeElement
        .>>. choice
            [ pKeyword "ARRAY"
              .>>. opt (between (token (pstring "[")) (token (pstring "]")) pUnsignedInteger)
              |>> fun (_, len) -> fun t -> ArrayType(t, Option.map int len)
              pKeyword "MULTISET" >>% MultisetType ]
        |>> fun (t, f) -> f t

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
                  .>>. opt (pKeyword "SCOPE" >>. pQualifiedNameExpr)
                  |>> fun (t, scope) -> ReferenceType(t, scope)
              )
              pIdentifierExpr |>> UserDefinedType ]

    pDataTypeRef.Value <- choice [ attempt pCollectionType; pDataTypeElement ]
