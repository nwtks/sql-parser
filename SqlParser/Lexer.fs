namespace SqlParser

open FParsec

module Lexer =
    let reservedWords =
        Set.ofList
            [ "ABS"
              "ACOS"
              "ALL"
              "ALLOCATE"
              "ALTER"
              "AND"
              "ANY"
              "ARE"
              "ARRAY"
              "ARRAY_AGG"
              "ARRAY_MAX_CARDINALITY"
              "AS"
              "ASENSITIVE"
              "ASIN"
              "ASYMMETRIC"
              "AT"
              "ATAN"
              "ATOMIC"
              "AUTHORIZATION"
              "AVG"
              "BEGIN"
              "BEGIN_FRAME"
              "BEGIN_PARTITION"
              "BETWEEN"
              "BIGINT"
              "BINARY"
              "BLOB"
              "BOOLEAN"
              "BOTH"
              "BY"
              "CALL"
              "CALLED"
              "CARDINALITY"
              "CASCADED"
              "CASE"
              "CAST"
              "CEIL"
              "CEILING"
              "CHAR"
              "CHARACTER"
              "CHARACTER_LENGTH"
              "CHAR_LENGTH"
              "CHECK"
              "CLASSIFIER"
              "CLOB"
              "CLOSE"
              "COALESCE"
              "COLLATE"
              "COLLECT"
              "COLUMN"
              "COMMIT"
              "CONDITION"
              "CONNECT"
              "CONSTRAINT"
              "CONTAINS"
              "CONVERT"
              "COPY"
              "CORR"
              "CORRESPONDING"
              "COS"
              "COSH"
              "COUNT"
              "COVAR_POP"
              "COVAR_SAMP"
              "CREATE"
              "CROSS"
              "CUBE"
              "CUME_DIST"
              "CURRENT"
              "CURRENT_CATALOG"
              "CURRENT_DATE"
              "CURRENT_DEFAULT_TRANSFORM_GROUP"
              "CURRENT_PATH"
              "CURRENT_ROLE"
              "CURRENT_ROW"
              "CURRENT_SCHEMA"
              "CURRENT_TIME"
              "CURRENT_TIMESTAMP"
              "CURRENT_TRANSFORM_GROUP_FOR_TYPE"
              "CURRENT_USER"
              "CURSOR"
              "CYCLE"
              "DATE"
              "DAY"
              "DEALLOCATE"
              "DEC"
              "DECFLOAT"
              "DECIMAL"
              "DECLARE"
              "DEFAULT"
              "DEFINE"
              "DELETE"
              "DENSE_RANK"
              "DEREF"
              "DESCRIBE"
              "DETERMINISTIC"
              "DISCONNECT"
              "DISTINCT"
              "DOUBLE"
              "DROP"
              "DYNAMIC"
              "EACH"
              "ELEMENT"
              "ELSE"
              "EMPTY"
              "END"
              "END_FRAME"
              "END_PARTITION"
              "EQUALS"
              "ESCAPE"
              "EVERY"
              "EXCEPT"
              "EXEC"
              "EXECUTE"
              "EXISTS"
              "EXP"
              "EXTERNAL"
              "EXTRACT"
              "FALSE"
              "FETCH"
              "FILTER"
              "FIRST_VALUE"
              "FLOAT"
              "FLOOR"
              "FOR"
              "FOREIGN"
              "FRAME_ROW"
              "FREE"
              "FROM"
              "FULL"
              "FUNCTION"
              "FUSION"
              "GET"
              "GLOBAL"
              "GRANT"
              "GROUP"
              "GROUPING"
              "GROUPS"
              "HAVING"
              "HOLD"
              "HOUR"
              "IDENTITY"
              "IN"
              "INDICATOR"
              "INITIAL"
              "INNER"
              "INOUT"
              "INSENSITIVE"
              "INSERT"
              "INT"
              "INTEGER"
              "INTERSECT"
              "INTERSECTION"
              "INTERVAL"
              "INTO"
              "IS"
              "JOIN"
              "JSON_ARRAY"
              "JSON_ARRAYAGG"
              "JSON_EXISTS"
              "JSON_OBJECT"
              "JSON_OBJECTAGG"
              "JSON_QUERY"
              "JSON_TABLE"
              "JSON_TABLE_PRIMITIVE"
              "JSON_VALUE"
              "LAG"
              "LANGUAGE"
              "LARGE"
              "LAST_VALUE"
              "LATERAL"
              "LEAD"
              "LEADING"
              "LEFT"
              "LIKE"
              "LIKE_REGEX"
              "LISTAGG"
              "LN"
              "LOCAL"
              "LOCALTIME"
              "LOCALTIMESTAMP"
              "LOG"
              "LOG10"
              "LOWER"
              "MATCH"
              "MATCHES"
              "MATCH_NUMBER"
              "MATCH_RECOGNIZE"
              "MAX"
              "MEMBER"
              "MERGE"
              "METHOD"
              "MIN"
              "MINUTE"
              "MOD"
              "MODIFIES"
              "MODULE"
              "MONTH"
              "MULTISET"
              "NATIONAL"
              "NATURAL"
              "NCHAR"
              "NCLOB"
              "NEW"
              "NO"
              "NONE"
              "NORMALIZE"
              "NOT"
              "NTH_VALUE"
              "NTILE"
              "NULL"
              "NULLIF"
              "NUMERIC"
              "OCCURRENCES_REGEX"
              "OCTET_LENGTH"
              "OF"
              "OFFSET"
              "OLD"
              "OMIT"
              "ON"
              "ONE"
              "ONLY"
              "OPEN"
              "OR"
              "ORDER"
              "OUT"
              "OUTER"
              "OVER"
              "OVERLAPS"
              "OVERLAY"
              "PARAMETER"
              "PARTITION"
              "PATTERN"
              "PER"
              "PERCENT"
              "PERCENTILE_CONT"
              "PERCENTILE_DISC"
              "PERCENT_RANK"
              "PERIOD"
              "PORTION"
              "POSITION"
              "POSITION_REGEX"
              "POWER"
              "PRECEDES"
              "PRECISION"
              "PREPARE"
              "PRIMARY"
              "PROCEDURE"
              "PTF"
              "RANGE"
              "RANK"
              "READS"
              "REAL"
              "RECURSIVE"
              "REF"
              "REFERENCES"
              "REFERENCING"
              "REGR_AVGX"
              "REGR_AVGY"
              "REGR_COUNT"
              "REGR_INTERCEPT"
              "REGR_R2"
              "REGR_SLOPE"
              "REGR_SXX"
              "REGR_SXY"
              "REGR_SYY"
              "RELEASE"
              "RESULT"
              "RETURN"
              "RETURNS"
              "REVOKE"
              "RIGHT"
              "ROLLBACK"
              "ROLLUP"
              "ROW"
              "ROWS"
              "ROW_NUMBER"
              "RUNNING"
              "SAVEPOINT"
              "SCOPE"
              "SCROLL"
              "SEARCH"
              "SECOND"
              "SEEK"
              "SELECT"
              "SENSITIVE"
              "SESSION_USER"
              "SET"
              "SHOW"
              "SIMILAR"
              "SIN"
              "SINH"
              "SKIP"
              "SMALLINT"
              "SOME"
              "SPECIFIC"
              "SPECIFICTYPE"
              "SQL"
              "SQLEXCEPTION"
              "SQLSTATE"
              "SQLWARNING"
              "SQRT"
              "START"
              "STATIC"
              "STDDEV_POP"
              "STDDEV_SAMP"
              "SUBMULTISET"
              "SUBSET"
              "SUBSTRING"
              "SUBSTRING_REGEX"
              "SUCCEEDS"
              "SUM"
              "SYMMETRIC"
              "SYSTEM"
              "SYSTEM_TIME"
              "SYSTEM_USER"
              "TABLE"
              "TABLESAMPLE"
              "TAN"
              "TANH"
              "THEN"
              "TIME"
              "TIMESTAMP"
              "TIMEZONE_HOUR"
              "TIMEZONE_MINUTE"
              "TO"
              "TRAILING"
              "TRANSLATE"
              "TRANSLATE_REGEX"
              "TRANSLATION"
              "TREAT"
              "TRIGGER"
              "TRIM"
              "TRIM_ARRAY"
              "TRUE"
              "TRUNCATE"
              "UESCAPE"
              "UNION"
              "UNIQUE"
              "UNKNOWN"
              "UNNEST"
              "UPDATE"
              "UPPER"
              "USER"
              "USING"
              "VALUE"
              "VALUES"
              "VALUE_OF"
              "VARBINARY"
              "VARCHAR"
              "VARYING"
              "VAR_POP"
              "VAR_SAMP"
              "VERSIONING"
              "WHEN"
              "WHENEVER"
              "WHERE"
              "WIDTH_BUCKET"
              "WINDOW"
              "WITH"
              "WITHIN"
              "WITHOUT"
              "YEAR" ]

    let ws = spaces

    let token p = p .>> ws

    let pKeyword s =
        attempt (pstringCI s .>> notFollowedBy (asciiLetter <|> digit <|> pchar '_'))
        .>> ws

    let pQuote = pchar '\''

    let pHexit = hex <|> digit

    let pAnyRune =
        anyChar
        >>= fun c1 ->
            if System.Char.IsHighSurrogate c1 then
                anyChar
                >>= fun c2 ->
                    if System.Char.IsLowSurrogate c2 then
                        System.Text.Rune(c1, c2) |> string |> preturn
                    else
                        fail "invalid surrogate pair."
            elif System.Char.IsLowSurrogate c1 then
                fail "unexpected low surrogate."
            else
                System.Text.Rune c1 |> string |> preturn

    let pCharacterEscape esc = pstring esc >>. pstring esc

    [<TailCall>]
    let rec loopParseHead p n acc =
        if n = 0 then
            acc |> List.rev |> preturn
        else
            p >>= fun x -> loopParseHead p (n - 1) (x :: acc)

    let parseHead p n = loopParseHead p n []

    let hexToInt32 chars =
        System.Convert.ToInt32(chars |> List.toArray |> string, 16)

    let pUnicode4DigitEscape esc =
        pstring esc >>. parseHead pHexit 4
        |>> hexToInt32
        |>> System.Convert.ToChar
        |>> string

    let pUnicode6DigitEscape esc =
        pstring esc >>. pchar '+' >>. parseHead pHexit 6
        |>> hexToInt32
        |>> System.Char.ConvertFromUtf32

    let pUnicodeEscapeSpecifier =
        opt (pKeyword "UESCAPE" >>. pQuote >>. pAnyRune .>> pQuote)
        |>> Option.defaultValue "\\"

    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    let pIdentifierRaw =
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        |>> (fun s -> s.ToUpperInvariant())
        .>> ws

    let pRegularIdentifier =
        attempt (
            pIdentifierRaw
            >>= fun s ->
                if reservedWords.Contains s then
                    fail "reserved word."
                else
                    preturn s
        )

    let pDelimitedIdentifier =
        between (pchar '\"') (pchar '\"') (manyChars (attempt (pstring "\"\"") >>% '\"' <|> noneOf "\""))

    let pUnicodeDelimitedIdentifier =
        pchar 'U' >>. pchar '&' >>. pDelimitedIdentifier .>>. pUnicodeEscapeSpecifier
        |>> fun (id, esc) -> id

    let pIdentifier =
        choice
            [ attempt pUnicodeDelimitedIdentifier
              pRegularIdentifier
              pDelimitedIdentifier ]
        .>> ws

    let pCharacterRepresentation = attempt (pstring "''") >>% '\'' <|> noneOf "'"

    let pSeparator =
        skipMany (
            skipMany1 (pchar ' ' <|> pchar '\t' <|> pchar '\n' <|> pchar '\r')
            <|> (attempt (pstring "--") >>. skipMany (noneOf "\n") >>. skipChar '\n')
            <|> (attempt (pstring "/*") >>. skipCharsTillString "*/" true 10000 >>% ())
        )

    let pCharacterStringLiteral: Parser<string, unit> =
        let pSegment = between pQuote pQuote (manyChars pCharacterRepresentation)

        pSegment .>>. many (attempt (pSeparator >>. pSegment))
        |>> fun (first, rest) -> String.concat "" (first :: rest)
        .>> ws

    let pNationalCharacterStringLiteral: Parser<string, unit> =
        pchar 'N' >>. pCharacterStringLiteral

    let pHexStringLiteral: Parser<byte[], unit> =
        let pSegment =
            between
                pQuote
                pQuote
                (many (
                    pHexit .>>. pHexit
                    |>> fun (h1, h2) -> System.Convert.ToByte(sprintf "%c%c" h1 h2, 16)
                ))

        pchar 'X' >>. pSegment .>>. many (attempt (pSeparator >>. pSegment))
        |>> fun (first, rest) -> first :: rest |> List.concat |> List.toArray
        .>> ws

    let pUnicodeCharacterStringLiteral: Parser<string, unit> =
        pchar 'U' >>. pchar '&' >>. pCharacterStringLiteral .>>. pUnicodeEscapeSpecifier
        |>> fun (s, esc) -> s

    let pUnsignedInteger: Parser<uint64, unit> = many1Chars digit |>> uint64

    let pExactNumericLiteral =
        attempt (
            pipe2 (many1Chars digit) (opt (pchar '.' >>. manyChars digit)) (fun p f ->
                match f with
                | Some fStr -> decimal (p + "." + fStr)
                | None -> decimal p)
        )
        <|> (pchar '.' >>. many1Chars digit |>> fun f -> decimal ("0." + f))
        .>> ws

    let pApproximateNumericLiteral =
        pipe3
            pExactNumericLiteral
            (pchar 'E' <|> pchar 'e')
            (pipe2 (opt (pchar '+' <|> pchar '-')) (many1Chars digit) (fun s d ->
                (Option.defaultValue '+' s |> string) + d))
            (fun m _ e -> m * decimal (10.0 ** float e))
        .>> ws

    let pNumericLiteral: Parser<decimal, unit> =
        attempt pApproximateNumericLiteral <|> pExactNumericLiteral

    let pDateString = between pQuote pQuote (many1Chars (noneOf "'"))
    let pDateLiteral: Parser<string, unit> = pKeyword "DATE" >>. pDateString .>> ws

    let pTimeString = between pQuote pQuote (many1Chars (noneOf "'"))
    let pTimeLiteral: Parser<string, unit> = pKeyword "TIME" >>. pTimeString .>> ws

    let pTimestampString = between pQuote pQuote (many1Chars (noneOf "'"))

    let pTimestampLiteral: Parser<string, unit> =
        pKeyword "TIMESTAMP" >>. pTimestampString .>> ws

    let pBooleanLiteral: Parser<bool option, unit> =
        pKeyword "TRUE" >>% Some true
        <|> (pKeyword "FALSE" >>% Some false)
        <|> (pKeyword "UNKNOWN" >>% None)

    let pQuestionMark: Parser<char, unit> = pchar '?' .>> ws

    let pHostParameter: Parser<string, unit> =
        pchar ':' >>. pIdentifier |>> (fun name -> ":" + name) .>> ws
