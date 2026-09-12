namespace SqlParser

open FParsec

module Lexer =
    // 5.2 <reserved word> — keywords that cannot be used as <regular identifier>
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

    // 5.1 <quote> ::= '
    let pQuote = pchar '\''

    // 5.2 <separator> ::= { <comment> | <white space> }...
    // 5.2 <comment> ::= <simple comment> | <bracketed comment>
    let pSeparator =
        skipMany (
            spaces1
            <|> (attempt (pstring "--") >>. skipMany (noneOf "\n") >>. skipChar '\n')
            <|> (attempt (pstring "/*") >>. skipCharsTillString "*/" true 10000 >>% ())
        )

    // 5.2 <token> helper — <token> [ <separator> ]
    let token p = p .>> pSeparator

    // 5.2 <key word> helper
    // Matches a specific keyword (case-insensitive) followed by a non-identifier character.
    let pKeyword s =
        attempt (pstringCI s .>> notFollowedBy (asciiLetter <|> digit <|> pchar '_'))
        .>> ws

    // 5.2 <Unicode escape value> helper — reads one Unicode scalar value (handles surrogate pairs)
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

    // 5.2 <Unicode escape value> — <Unicode escape character> <Unicode escape character>
    let pUnicodeEscape esc = pstring esc >>. pstring esc

    [<TailCall>]
    let rec loopParseHead p n acc =
        if n = 0 then
            acc |> List.rev |> preturn
        else
            p >>= fun x -> loopParseHead p (n - 1) (x :: acc)

    let parseHead p n = loopParseHead p n []

    let hexToInt32 (chars: char list) =
        System.Convert.ToInt32(System.String.Concat chars, 16)

    // 5.3 <hexit> ::= <digit> | A | B | C | D | E | F | a | b | c | d | e | f
    let pHexit = hex <|> digit

    // 5.2 <Unicode 4 digit escape value> ::= <Unicode escape character> <hexit> <hexit> <hexit> <hexit>
    let pUnicode4DigitEscape esc =
        pstring esc >>. parseHead pHexit 4
        |>> hexToInt32
        |>> System.Convert.ToChar
        |>> string

    // 5.2 <Unicode 6 digit escape value> ::= <Unicode escape character> <plus sign> <hexit> <hexit> <hexit> <hexit> <hexit> <hexit>
    let pUnicode6DigitEscape esc =
        pstring esc >>. pchar '+' >>. parseHead pHexit 6
        |>> hexToInt32
        |>> System.Char.ConvertFromUtf32

    // 5.2 <Unicode escape specifier> ::= [ UESCAPE <quote> <Unicode escape character> <quote> ]
    let pUnicodeEscapeSpecifier =
        opt (pKeyword "UESCAPE" >>. pQuote >>. pAnyRune .>> pQuote)
        |>> Option.defaultValue "\\"

    // 5.3 <unsigned integer> ::= <digit>...
    let pUnsignedInteger = many1Chars digit |>> uint64

    // 5.3 <exact numeric literal> ::= <unsigned integer> [ <period> [ <unsigned integer> ] ] | <period> <unsigned integer>
    let pExactNumericLiteral =
        attempt (
            pipe2 (many1Chars digit) (opt (pchar '.' >>. manyChars digit)) (fun p f ->
                match f with
                | Some fStr -> decimal (p + "." + fStr)
                | None -> decimal p)
        )
        <|> (pchar '.' >>. many1Chars digit |>> fun f -> decimal ("0." + f))
        .>> ws

    // 5.3 <approximate numeric literal> ::= <mantissa> E <exponent>
    let pApproximateNumericLiteral =
        pipe3
            pExactNumericLiteral
            (pchar 'E' <|> pchar 'e')
            (pipe2 (opt (pchar '+' <|> pchar '-')) (many1Chars digit) (fun s d ->
                (Option.defaultValue '+' s |> string) + d))
            (fun m _ e ->
                let exp = int e

                if exp > 28 then
                    // Decimal max exponent is ~28; clamp to avoid OverflowException
                    m * decimal (10.0 ** float (min exp 28))
                else
                    m * decimal (10.0 ** float exp))
        .>> ws

    // 5.3 <unsigned numeric literal> ::= <exact numeric literal> | <approximate numeric literal>
    let pUnsignedNumericLiteral: Parser<decimal, unit> =
        attempt pApproximateNumericLiteral <|> pExactNumericLiteral

    // 5.3 <signed numeric literal> ::= [ <sign> ] <unsigned numeric literal>
    let pSignedNumericLiteral =
        opt (pchar '-' <|> pchar '+') .>>. pUnsignedNumericLiteral
        |>> fun (sign, n) -> if sign = Some '-' then -n else n

    // 5.3 <character representation> ::= <nonquote character> | <quote symbol>
    // <quote symbol> ::= <quote> <quote>
    let pCharacterRepresentation = attempt (pstring "''") >>% '\'' <|> noneOf "'"

    // 5.3 <introducer> ::= <underscore>
    let pIntroducer = pchar '_' .>> ws

    // 5.2 <SQL language identifier> ::= <SQL language identifier start> [ <SQL language identifier part>... ]
    // <SQL language identifier start> ::= <simple Latin letter>
    // <SQL language identifier part> ::= <simple Latin letter> | <digit> | <underscore>
    let pSqlLanguageIdentifier =
        many1Satisfy2L isAsciiLetter (fun c -> isAsciiLetter c || isDigit c || c = '_') "SQL language identifier"
        |>> (fun s -> s.ToUpperInvariant())
        .>> ws

    // 10.5 <character set specification> ::= <character set name>
    // <character set name> ::= [ <schema name> <period> ] <SQL language identifier>
    let pCharacterSetSpecification =
        opt (pSqlLanguageIdentifier .>> token (pstring ".")) .>>. pSqlLanguageIdentifier
        |>> fun (schema, name) ->
            match schema with
            | Some s -> s + "." + name
            | None -> name

    // 5.3 <character string literal> ::= [ <introducer> <character set specification> ] <quote> [ <character representation>... ] <quote> [ { <separator> <quote> [ <character representation>... ] <quote> }... ]
    let pCharacterStringLiteral =
        let pSegment = between pQuote pQuote (manyChars pCharacterRepresentation)

        opt (pIntroducer >>. pCharacterSetSpecification)
        .>>. (pSegment .>>. many (attempt (pSeparator >>. pSegment)))
        |>> fun (_, (first, rest)) -> String.concat "" (first :: rest)
        .>> ws

    // 5.3 <national character string literal> ::= N <quote> [ <character representation>... ] <quote> [ { <separator> <quote> [ <character representation>... ] <quote> }... ]
    let pNationalCharacterStringLiteral = pchar 'N' >>. pCharacterStringLiteral

    // 5.3 <Unicode representation> ::= <character representation> | <Unicode escape value>
    let pUnicodeRepresentation esc =
        many (
            choice
                [ attempt (pUnicode6DigitEscape esc)
                  attempt (pUnicodeEscape esc)
                  attempt (pUnicode4DigitEscape esc)
                  attempt (pUnicodeEscape "'")
                  attempt (many1Chars (noneOf (esc + "'"))) ]
        )
        |>> String.concat ""

    // 5.3 <Unicode character string literal> ::= [ <introducer> <character set specification> ] U <ampersand> <quote> [ <Unicode representation>... ] <quote> [ { <separator> <quote> [ <Unicode representation>... ] <quote> }... ] <Unicode escape specifier>
    let pUnicodeCharacterStringLiteral =
        opt (pIntroducer >>. pCharacterSetSpecification)
        .>>. (pchar 'U'
              >>. pchar '&'
              >>. lookAhead (pCharacterStringLiteral .>>. pUnicodeEscapeSpecifier |>> snd)
              >>= fun esc ->
                  let pSegment = between pQuote pQuote (pUnicodeRepresentation esc)

                  pSegment .>>. many (attempt (pSeparator >>. pSegment))
                  |>> fun (f, rest) -> String.concat "" (f :: rest)
                  .>> ws
                  .>> pUnicodeEscapeSpecifier)
        |>> snd

    // 5.3 <binary string literal> ::= X <quote> [ <space>... ] [ { <hexit> [ <space>... ] <hexit> [ <space>... ] }... ] <quote> [ { <separator> <quote> ... ] <quote> }... ]
    let pBinaryStringLiteral =
        let pSegment =
            between
                pQuote
                pQuote
                (many (
                    attempt (
                        spaces >>. pHexit .>>. pHexit
                        |>> fun (h1, h2) -> System.Convert.ToByte(sprintf "%c%c" h1 h2, 16)
                    )
                 )
                 .>> spaces)

        pchar 'X' >>. pSegment .>>. many (attempt (pSeparator >>. pSegment))
        |>> fun (first, rest) -> first :: rest |> List.concat |> List.toArray
        .>> ws

    // 5.3 <date value> ::= <years value> <minus sign> <months value> <minus sign> <days value>
    let pDateValue =
        pUnsignedInteger .>> pchar '-' .>>. pUnsignedInteger .>> pchar '-'
        .>>. pUnsignedInteger
        >>= fun ((y, m), d) ->
            let yi, mi, di = int y, int m, int d

            if mi < 1 || mi > 12 || di < 1 || di > 31 then
                fail "invalid date"
            else
                preturn { Year = yi; Month = mi; Day = di }

    // 5.3 <date literal> ::= DATE <date string>
    let pDateLiteral = pKeyword "DATE" >>. between pQuote pQuote pDateValue .>> ws

    // 5.3 <time zone interval> ::= <sign> <hours value> <colon> <minutes value>
    let pTimeZoneInterval =
        pchar '+' >>% 1 <|> (pchar '-' >>% -1) .>>. (pint32 .>> pchar ':' .>>. pint32)
        |>> fun (sign, (h, m)) -> { Sign = sign; Hours = h; Minutes = m }

    // 5.3 <unquoted time string> ::= <time value>  [ <time zone interval>  ]
    // <time value> ::= <hours value> <colon> <minutes value> <colon> <seconds value>
    let pUnquotedTimeString =
        pipe4
            (pint32 .>> pchar ':')
            (pint32 .>> pchar ':')
            (pipe2 (many1Chars digit) (opt (pchar '.' >>. manyChars digit)) (fun s f ->
                match f with
                | Some frac -> decimal (s + "." + frac)
                | None -> decimal s))
            (opt (spaces >>. pTimeZoneInterval))
            (fun h m s tz ->
                { Hour = h
                  Minute = m
                  Second = s
                  TzOffset = tz })

    // 5.3 <time literal> ::= TIME <time string>
    let pTimeLiteral =
        pKeyword "TIME" >>. between pQuote pQuote pUnquotedTimeString .>> ws

    // 5.3 <unquoted timestamp string> ::= <unquoted date string> <space> <unquoted time string>
    let pUnquotedTimestampString =
        pDateValue .>> spaces1 .>>. pUnquotedTimeString
        |>> fun (d, t) -> { Date = d; Time = t }

    // 5.3 <timestamp literal> ::= TIMESTAMP <timestamp string>
    let pTimestampLiteral =
        pKeyword "TIMESTAMP" >>. between pQuote pQuote pUnquotedTimestampString .>> ws

    // 10.1 <non-second primary datetime field> ::= YEAR | MONTH | DAY | HOUR | MINUTE
    let pNonSecondPrimaryDatetimeField =
        choice
            [ attempt (pKeyword "YEAR") >>% Year
              attempt (pKeyword "MONTH") >>% Month
              attempt (pKeyword "DAY") >>% Day
              attempt (pKeyword "HOUR") >>% Hour
              attempt (pKeyword "MINUTE") >>% Minute ]

    // 10.1 <interval leading field precision> ::= <unsigned integer>
    let pIntervalLeadingFieldPrecision =
        between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int

    // 10.1 <interval fractional seconds precision> ::= <unsigned integer>
    let pIntervalFractionalSecondsPrecision =
        between (token (pstring "(")) (token (pstring ")")) pUnsignedInteger |>> int

    // 10.1 <start field> ::= <non-second primary datetime field> [ ( <interval leading field precision> ) ]
    let pStartField =
        pNonSecondPrimaryDatetimeField .>>. opt pIntervalLeadingFieldPrecision
        |>> fun (field, leading) ->
            field,
            leading
            |> Option.map (fun l ->
                { IntervalPrecision.Leading = Some l
                  FractionalSeconds = None })

    // 10.1 <end field> ::= <non-second primary datetime field> | SECOND [ ( <interval fractional seconds precision> ) ]
    let pEndField =
        (pNonSecondPrimaryDatetimeField |>> fun f -> f, None)
        <|> (pKeyword "SECOND" >>. opt pIntervalFractionalSecondsPrecision
             |>> fun frac ->
                 Second,
                 frac
                 |> Option.map (fun fs ->
                     { IntervalPrecision.Leading = None
                       FractionalSeconds = Some fs }))

    // 10.1 <single datetime field> ::= <non-second primary datetime field> [ ( <interval leading field precision> ) ]
    //   | SECOND [ ( <interval leading field precision> [ , <interval fractional seconds precision> ] ) ]
    let pSingleDatetimeField =
        (pNonSecondPrimaryDatetimeField .>>. opt pIntervalLeadingFieldPrecision
         |>> fun (field, leading) ->
             field,
             leading
             |> Option.map (fun l ->
                 { IntervalPrecision.Leading = Some l
                   FractionalSeconds = None }))
        <|> (pKeyword "SECOND"
             >>. opt (
                 between
                     (token (pstring "("))
                     (token (pstring ")"))
                     (pUnsignedInteger .>>. opt (token (pstring ",") >>. pUnsignedInteger))
                 |>> fun (leading, frac) -> int leading, Option.map int frac
             )
             |>> fun prec ->
                 Second,
                 prec
                 |> Option.map (fun (leading, frac) ->
                     { IntervalPrecision.Leading = Some leading
                       FractionalSeconds = frac }))

    // 10.1 <interval qualifier> ::= <start field> TO <end field> | <single datetime field>
    let pIntervalQualifier =
        let pRange =
            pStartField .>> pKeyword "TO" .>>. pEndField
            |>> fun ((startF, startPrec), (endF, endPrec)) ->
                let prec =
                    match startPrec, endPrec with
                    | None, None -> None
                    | _ ->
                        Some
                            { IntervalPrecision.Leading = startPrec |> Option.bind (fun p -> p.Leading)
                              FractionalSeconds = endPrec |> Option.bind (fun p -> p.FractionalSeconds) }

                IntervalQualifier.Range(startF, endF, prec)

        let pSingle =
            pSingleDatetimeField
            |>> fun (field, prec) -> IntervalQualifier.SingleField(field, prec)

        attempt pRange <|> pSingle

    // 5.3 <unquoted interval string> — validates <year-month literal> | <day-time literal> against <interval qualifier>
    let isValidIntervalValue (q: IntervalQualifier) (s: string) =
        let d = @"\d+"
        let sec = @"\d+(\.\d+)?"

        let pattern =
            match q with
            | IntervalQualifier.SingleField(Year, _)
            | IntervalQualifier.SingleField(Month, _)
            | IntervalQualifier.SingleField(Day, _)
            | IntervalQualifier.SingleField(Hour, _)
            | IntervalQualifier.SingleField(Minute, _) -> "^" + d + "$"
            | IntervalQualifier.SingleField(Second, _) -> "^" + sec + "$"
            | IntervalQualifier.Range(Year, Month, _) -> "^" + d + "-" + d + "$"
            | IntervalQualifier.Range(Day, Hour, _) -> "^" + d + @"\s+" + d + ":" + d + "$"
            | IntervalQualifier.Range(Day, Minute, _) -> "^" + d + @"\s+" + d + ":" + d + ":" + d + "$"
            | IntervalQualifier.Range(Day, Second, _) -> "^" + d + @"\s+" + d + ":" + d + ":" + sec + "$"
            | IntervalQualifier.Range(Hour, Minute, _) -> "^" + d + ":" + d + "$"
            | IntervalQualifier.Range(Hour, Second, _) -> "^" + d + ":" + d + ":" + sec + "$"
            | IntervalQualifier.Range(Minute, Second, _) -> "^" + d + ":" + sec + "$"
            | _ -> "^$"

        System.Text.RegularExpressions.Regex.IsMatch(s, pattern)

    // 5.3 <interval literal> ::= INTERVAL [ <sign> ] <interval string> <interval qualifier>
    let pIntervalLiteral =
        pKeyword "INTERVAL" >>. ws .>>. between pQuote pQuote (manyChars (noneOf "'"))
        .>> ws
        .>>. pIntervalQualifier
        .>> ws
        >>= fun ((_, v), q) ->
            // The sign is part of the quoted value per the grammar
            let isNeg, valueStr =
                match v with
                | s when s.StartsWith "-" -> true, s.Substring 1
                | s when s.StartsWith "+" -> false, s.Substring 1
                | s -> false, s

            if isValidIntervalValue q valueStr then
                preturn
                    { IsNegative = isNeg
                      ValueString = valueStr
                      Qualifier = q }
            else
                fail "invalid interval value"

    // 5.3 <boolean literal> ::= TRUE | FALSE | UNKNOWN
    let pBooleanLiteral: Parser<bool option, unit> =
        pKeyword "TRUE" >>% Some true
        <|> (pKeyword "FALSE" >>% Some false)
        <|> (pKeyword "UNKNOWN" >>% None)

    // 5.2 <identifier body> — <identifier start> [ <identifier part>... ]
    let isIdentifierStartChar c = isLetter c || c = '_'
    let isIdentifierPartChar c = isLetter c || isDigit c || c = '_'

    // 5.2 <regular identifier> ::= <identifier body>
    let pIdentifierRaw =
        many1Satisfy2L isIdentifierStartChar isIdentifierPartChar "identifier"
        |>> (fun s -> s.ToUpperInvariant())
        .>> ws

    // 5.2 <regular identifier> — fails on <reserved word>
    let pRegularIdentifier =
        attempt (
            pIdentifierRaw
            >>= fun s ->
                if reservedWords.Contains s then
                    fail "reserved word."
                else
                    preturn s
        )

    // 5.2 <delimited identifier> ::= <double quote> <delimited identifier body> <double quote>
    // <delimited identifier body> ::= <delimited identifier part> ...
    // <delimited identifier part> ::= <nondoublequote character> | <doublequote symbol>
    let pDelimitedIdentifier =
        between (pchar '\"') (pchar '\"') (manyChars (attempt (pstring "\"\"") >>% '\"' <|> noneOf "\""))

    // 5.2 <Unicode delimiter body> ::= <Unicode identifier part>...
    // <Unicode identifier part> ::= <delimited identifier part> | <Unicode escape value>
    let pUnicodeDelimiterBody esc =
        many (
            choice
                [ attempt (pUnicode6DigitEscape esc)
                  attempt (pUnicodeEscape esc)
                  attempt (pUnicode4DigitEscape esc)
                  attempt (pUnicodeEscape "\"")
                  attempt (many1Chars (noneOf (esc + "\""))) ]
        )
        |>> String.concat ""

    // 5.2 <Unicode delimited identifier> ::= U <ampersand> <double quote> <Unicode delimiter body> <double quote> <Unicode escape specifier>
    let pUnicodeDelimitedIdentifier =
        pchar 'U'
        >>. pchar '&'
        >>. lookAhead (pDelimitedIdentifier .>>. pUnicodeEscapeSpecifier |>> snd)
        >>= fun esc ->
            between (pchar '"') (pchar '"') (pUnicodeDelimiterBody esc)
            .>> pUnicodeEscapeSpecifier

    // 5.4 <identifier> ::= <actual identifier> — <regular identifier> | <delimited identifier> | <Unicode delimited identifier>
    let pIdentifier =
        choice
            [ attempt pUnicodeDelimitedIdentifier
              pRegularIdentifier
              pDelimitedIdentifier ]
        .>> ws

    // 5.4 <schema qualified name> ::= [ <schema name> <period> ] <qualified identifier>
    // <schema name> ::= [ <catalog name> <period> ] <unqualified schema name>
    // <qualified identifier> ::= <identifier>
    // Returns the parts in order: [ <catalog name>; <schema name>; <qualified identifier> ]
    let pSchemaQualifiedName =
        pIdentifier .>>. many (token (pstring ".") >>. pIdentifier)
        |>> fun (first, rest) -> first :: rest

    // 6.4 <dynamic parameter specification> ::= <question mark>
    let pQuestionMark: Parser<char, unit> = pchar '?' .>> ws

    // 5.4 <host parameter name> ::= <colon> <identifier>
    let pHostParameter: Parser<string, unit> =
        pchar ':' >>. pIdentifier |>> (fun name -> ":" + name) .>> ws
