namespace SqlParser

open FParsec
open SqlParser.Ast
open SqlParser.Lexer

module Types =
    let pUnsignedInteger = pUnsignedInteger

    let pDataType, pDataTypeRef = createParserForwardedToRef<DataType, unit> ()

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

    let pNationalCharacterType =
        choice
            [ attempt (pKeyword "NATIONAL" .>> pKeyword "CHARACTER" .>> pKeyword "VARYING")
              >>% NationalVarchar
              attempt (pKeyword "NATIONAL" .>> pKeyword "CHAR" .>> pKeyword "VARYING")
              >>% NationalVarchar
              pKeyword "NCHAR" .>> pKeyword "VARYING" >>% NationalVarchar
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
              pKeyword "NATIONAL" .>> pKeyword "CHARACTER" >>% NationalCharacter
              pKeyword "NATIONAL" .>> pKeyword "CHAR" >>% NationalCharacter
              pKeyword "NCHAR" >>% NationalCharacter ]
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
        |>> fun (typ, len) -> typ len

    let pBinaryType =
        choice
            [ attempt (pKeyword "BINARY" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
              >>% BinaryLargeObject
              pKeyword "BLOB" >>% BinaryLargeObject ]
        .>>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
        |>> fun (typ, len) -> typ len

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
              pKeyword "SMALLINT" >>% SmallInt
              pKeyword "INTEGER" >>% Integer
              pKeyword "INT" >>% Integer
              pKeyword "BIGINT" >>% BigInt ]

    let pApproximateNumericType =
        choice
            [ pKeyword "FLOAT"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
              |>> Float
              pKeyword "REAL" >>% Real
              attempt (pKeyword "DOUBLE" .>> pKeyword "PRECISION") >>% DoublePrecision ]

    let pDateTimeType =
        choice
            [ pKeyword "DATE" >>% DateType
              pKeyword "TIME"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
              .>>. (opt (pKeyword "WITH" >>. pKeyword "TIME" >>. pKeyword "ZONE") |>> Option.isSome)
              |>> fun (p, tz) -> TimeType(p, tz)
              pKeyword "TIMESTAMP"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
              .>>. (opt (pKeyword "WITH" >>. pKeyword "TIME" >>. pKeyword "ZONE") |>> Option.isSome)
              |>> fun (p, tz) -> TimestampType(p, tz) ]

    let pIntervalType =
        pKeyword "INTERVAL" >>. many1Chars (noneOf "();,") .>> ws |>> IntervalType

    let pRowType =
        pKeyword "ROW"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 (pIdentifier .>>. pDataType) (token (pstring ",")))
        |>> RowType

    pDataTypeRef.Value <-
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
              pIdentifier |>> UserDefinedType ]
