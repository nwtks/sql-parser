namespace SqlParser

open FParsec
open SqlParser.Lexer
open SqlParser.ExpressionParser

module Types =
    let pDataType = ExpressionParser.pDataType

    let pUnsignedInteger = pUnsignedInteger

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
            [ attempt (pKeyword "BINARY" .>> pKeyword "VARYING") >>% VarBinary
              pKeyword "VARBINARY" >>% VarBinary
              attempt (pKeyword "BINARY" .>> pKeyword "LARGE" .>> pKeyword "OBJECT")
              >>% BinaryLargeObject
              pKeyword "BLOB" >>% BinaryLargeObject
              pKeyword "BINARY" >>% Binary ]
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
              pKeyword "DECFLOAT"
              >>. opt (between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int)
              |>> DecFloat
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

    let pIntervalType =
        pKeyword "INTERVAL" >>. many1Chars (noneOf "();,") .>> ws |>> IntervalType

    let pRowType =
        pKeyword "ROW"
        >>. between
                (token (pstring "("))
                (token (pstring ")"))
                (sepBy1 (pIdentifierExpr .>>. pDataType) (token (pstring ",")))
        |>> RowType

    let pCollectionType =
        pDataType
        .>>. choice
            [ pKeyword "ARRAY"
              .>>. opt (between (token (pstring "[")) (token (pstring "]")) pUnsignedInteger)
              |>> fun (_, len) -> fun t -> ArrayType(t, Option.map int len)
              pKeyword "MULTISET" >>% MultisetType ]
        |>> fun (t, f) -> f t

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
              attempt pCollectionType
              pIdentifierExpr |>> UserDefinedType ]
