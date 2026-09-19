namespace SqlParser

open FParsec

module Lexer =
    let ws = spaces

    // 5.1 <quote> ::= '
    let pQuote: Parser<char, unit> = pchar '\''

    // ---- 5.1 <SQL special character> -----------------------------------------
    // Terminal characters that have no other home. <simple Latin letter>,
    // <digit>, <space>, <underscore>, <double quote>, <ampersand>, <apostrophe>,
    // and the operator/punctuation terminals consumed by <value expression> /
    // <predicate> / statement parsers are already modelled by their own parsers.
    // <percent> and <reverse solidus> are deliberately absent: they occur only
    // inside the embedded XQuery-regex (8.6) and SQL/JSON-path (9.38/9.39)
    // languages, whose text is kept opaque — see docs/trade-off.md.
    // 5.1 <left brace> ::= {
    let pLeftBrace: Parser<char, unit> = pchar '{'
    // 5.1 <right brace> ::= }
    let pRightBrace: Parser<char, unit> = pchar '}'
    // 5.1 <circumflex> ::= ^
    let pCircumflex: Parser<char, unit> = pchar '^'
    // 5.1 <vertical bar> ::= |
    let pVerticalBar: Parser<char, unit> = pchar '|'
    // 5.1 <dollar sign> ::= $
    let pDollarSign: Parser<char, unit> = pchar '$'
    // 5.1 <left bracket> ::= [ | ??(
    let pLeftBracket: Parser<string, unit> = pstring "[" <|> pstring "??("
    // 5.1 <right bracket> ::= ] | ??)
    let pRightBracket: Parser<string, unit> = pstring "]" <|> pstring "??)"

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

    // 5.2 <left brace minus> ::= {-
    let pLeftBraceMinus: Parser<string, unit> = pstring "{-"
    // 5.2 <right minus brace> ::= -}
    let pRightMinusBrace: Parser<string, unit> = pstring "-}"

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
    // The six hexits are a <Unicode scalar value>, so an out-of-range code point or a surrogate
    // is rejected instead of throwing out of `ConvertFromUtf32`.
    let pUnicode6DigitEscape esc =
        pstring esc >>. pchar '+' >>. parseHead pHexit 6
        >>= fun chars ->
            let code = hexToInt32 chars

            if code > 0x10FFFF || (code >= 0xD800 && code <= 0xDFFF) then
                fail "invalid Unicode escape value."
            else
                preturn (System.Char.ConvertFromUtf32 code)

    // 5.2 <Unicode escape character> — shall not be a <hexit>, plus sign, quote, double quote or space.
    let pUnicodeEscapeCharacter =
        pAnyRune
        >>= fun c ->
            let forbidden ch =
                System.Char.IsDigit ch
                || (ch >= 'a' && ch <= 'f')
                || (ch >= 'A' && ch <= 'F')
                || ch = '+'
                || ch = '\''
                || ch = '"'
                || ch = ' '

            if c.Length <> 1 || not (forbidden c.[0]) then
                preturn c
            else
                fail "invalid Unicode escape character."

    // 5.2 <Unicode escape specifier> ::= [ UESCAPE <quote> <Unicode escape character> <quote> ]
    let pUnicodeEscapeSpecifier =
        opt (pKeyword "UESCAPE" >>. pQuote >>. pUnicodeEscapeCharacter .>> pQuote)
        |>> Option.defaultValue "\\"

    // 5.2 <SQL language identifier> ::= <SQL language identifier start> [ <SQL language identifier part>... ]
    // <SQL language identifier start> ::= <simple Latin letter>
    // <SQL language identifier part> ::= <simple Latin letter> | <digit> | <underscore>
    let pSqlLanguageIdentifier =
        many1Satisfy2L isAsciiLetter (fun c -> isAsciiLetter c || isDigit c || c = '_') "SQL language identifier"
        |>> (fun s -> s.ToUpperInvariant())
        .>> ws

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

    // A surrogate pair must be complete: a lone surrogate is not a Unicode scalar value. The check
    // runs on the assembled escape output, so a legitimate pair (two escapes) is still accepted.
    [<TailCall>]
    let rec private loopUnicodeScalars (text: string) i =
        if i >= text.Length then
            true
        elif System.Char.IsHighSurrogate text.[i] then
            if i + 1 < text.Length && System.Char.IsLowSurrogate text.[i + 1] then
                loopUnicodeScalars text (i + 2)
            else
                false
        elif System.Char.IsLowSurrogate text.[i] then
            false
        else
            loopUnicodeScalars text (i + 1)

    let private validateUnicodeScalars text =
        if loopUnicodeScalars text 0 then
            preturn text
        else
            fail "invalid surrogate pair."

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
        >>= fun parts -> validateUnicodeScalars (String.concat "" parts)

    // 5.2 <Unicode delimited identifier> ::= U <ampersand> <double quote> <Unicode delimiter body> <double quote> <Unicode escape specifier>
    let pUnicodeDelimitedIdentifier =
        pchar 'U'
        >>. pchar '&'
        >>. lookAhead (pDelimitedIdentifier .>>. pUnicodeEscapeSpecifier |>> snd)
        >>= fun esc ->
            between (pchar '"') (pchar '"') (pUnicodeDelimiterBody esc)
            .>> pUnicodeEscapeSpecifier

    // 5.3 <unsigned integer> ::= <digit>...
    // Checked conversions: an out-of-range digit run must fail the parse, never throw an
    // exception out of `SqlParser.parse` (see docs/gotchas.md). Numeric text is read with the
    // invariant culture so the host locale cannot reinterpret '.' (de-DE reads "1.5" as 15).
    let private invariantCulture = System.Globalization.CultureInfo.InvariantCulture

    let private toUnsignedInteger (s: string) =
        match System.UInt64.TryParse(s, System.Globalization.NumberStyles.None, invariantCulture) with
        | true, v -> preturn v
        | _ -> fail "unsigned integer is out of range."

    let private toDecimal (s: string) =
        match System.Decimal.TryParse(s, System.Globalization.NumberStyles.AllowDecimalPoint, invariantCulture) with
        | true, v -> preturn v
        | _ -> fail "numeric literal is out of range."

    let pUnsignedInteger = many1Chars digit >>= toUnsignedInteger

    // The same digit run narrowed to `int` (<length>, <precision>, <interval leading field precision>).
    let pUnsignedIntegerAsInt =
        pUnsignedInteger
        >>= fun v ->
            if v > uint64 System.Int32.MaxValue then
                fail "unsigned integer is out of range."
            else
                preturn (int v)

    // 5.3 <exact numeric literal> ::= <unsigned integer> [ <period> [ <unsigned integer> ] ] | <period> <unsigned integer>
    // No trailing <separator>: pApproximateNumericLiteral reuses this as its <mantissa>, and a
    // <mantissa> must not be separated from its E <exponent> by white space (5.3 tokenisation).
    let private pExactNumericLiteralRaw =
        attempt (
            pipe2 (many1Chars digit) (opt (pchar '.' >>. manyChars digit)) (fun p f ->
                match f with
                | Some fStr when fStr <> "" -> p + "." + fStr
                | _ -> p)
        )
        <|> (pchar '.' >>. many1Chars digit |>> fun f -> "0." + f)
        >>= toDecimal

    // 5.3 <exact numeric literal> — the tokenised form (consumes the trailing <separator>).
    let pExactNumericLiteral = pExactNumericLiteralRaw .>> ws

    // 5.3 <exponent> ::= <signed integer> — the magnitude is bounded so `int` cannot overflow.
    let private pExponentMagnitude =
        many1Chars digit
        >>= fun d ->
            match System.Int32.TryParse(d, System.Globalization.NumberStyles.None, invariantCulture) with
            | true, v -> preturn v
            | _ -> fail "exponent is out of range."

    // 10^n for 0 <= n <= 28; the exponent is bounded by the callers, so this cannot overflow.
    [<TailCall>]
    let rec private loopPow10 acc n =
        if n = 0 then acc else loopPow10 (acc * 10m) (n - 1)

    // 5.3 <approximate numeric literal> ::= <mantissa> E <exponent>
    // An exponent whose value does not fit `decimal` is rejected instead of being silently clamped
    // to a different number: <Number> holds a decimal, so an unrepresentable literal cannot round-trip.
    let private pApproximateNumericLiteralRaw =
        pipe3
            pExactNumericLiteralRaw
            (pchar 'E' <|> pchar 'e')
            (opt (pchar '+' <|> pchar '-') .>>. pExponentMagnitude)
            (fun m _ (sign, mag) -> m, (if sign = Some '-' then -mag else mag))
        >>= fun (m, exponent) ->
            if exponent > 28 || exponent < -28 then
                fail "approximate numeric literal is out of range."
            else
                let scaled =
                    try
                        if exponent >= 0 then
                            m * loopPow10 1m exponent
                        else
                            m / loopPow10 1m (-exponent)
                    with :? System.OverflowException ->
                        System.Decimal.Zero

                if scaled = 0m && m <> 0m then
                    fail "approximate numeric literal is out of range."
                else
                    preturn scaled

    // 5.3 <approximate numeric literal> — the tokenised form (consumes the trailing <separator>).
    let pApproximateNumericLiteral = pApproximateNumericLiteralRaw .>> ws

    // 5.3 <unsigned numeric literal> ::= <exact numeric literal> | <approximate numeric literal>
    // Maximal munch: a numeric token must not be immediately followed by an identifier character,
    // so `1E`, `1E5x` and `0x10` are rejected instead of being re-read as a number plus an alias.
    let pUnsignedNumericLiteral =
        attempt (
            attempt pApproximateNumericLiteralRaw <|> pExactNumericLiteralRaw
            .>>? notFollowedBy (asciiLetter <|> digit <|> pchar '_')
        )
        .>> ws

    // 5.3 <signed numeric literal> ::= [ <sign> ] <unsigned numeric literal>
    let pSignedNumericLiteral =
        opt (pchar '-' <|> pchar '+') .>>. pUnsignedNumericLiteral
        |>> fun (sign, n) -> if sign = Some '-' then -n else n

    // 5.3 <character representation> ::= <nonquote character> | <quote symbol>
    // <quote symbol> ::= <quote> <quote>
    let pCharacterRepresentation = attempt (pstring "''") >>% '\'' <|> noneOf "'"

    // 5.3 <introducer> ::= <underscore>
    let pIntroducer = pchar '_' .>> ws

    // 10.5 <character set specification> ::= <character set name>
    // <character set name> ::= [ <schema name> <period> ] <SQL language identifier>
    let pCharacterSetSpecification =
        // `attempt` is required: pSqlLanguageIdentifier consumes the name before the
        // optional <period> fails, which would otherwise reject an unqualified
        // <character set name> such as `_UTF8'abc'`.
        opt (attempt (pSqlLanguageIdentifier .>> token (pstring ".")))
        .>>. pSqlLanguageIdentifier
        |>> fun (schema, name) ->
            match schema with
            | Some s -> s + "." + name
            | None -> name

    // 5.3 <character string literal> — the body is shared with the <national character string
    // literal>, whose production has no <introducer> <character set specification> slot.
    let pCharacterStringLiteralBody =
        let pSegment = between pQuote pQuote (manyChars pCharacterRepresentation)

        pSegment .>>. many (attempt (pSeparator >>. pSegment))
        |>> fun (first, rest) -> String.concat "" (first :: rest)

    // 5.3 <character string literal> ::= [ <introducer> <character set specification> ] <quote> [ <character representation>... ] <quote> [ { <separator> <quote> [ <character representation>... ] <quote> }... ]
    let pCharacterStringLiteral =
        opt (pIntroducer >>. pCharacterSetSpecification)
        .>>. pCharacterStringLiteralBody
        |>> snd
        .>> ws

    // 5.3 <national character string literal> ::= N <quote> [ <character representation>... ] <quote> [ { <separator> <quote> [ <character representation>... ] <quote> }... ]
    let pNationalCharacterStringLiteral =
        pchar 'N' >>. pCharacterStringLiteralBody .>> ws

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
        >>= fun parts -> validateUnicodeScalars (String.concat "" parts)

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

    // 5.3 <boolean literal> ::= TRUE | FALSE | UNKNOWN
    let pBooleanLiteral =
        pKeyword "TRUE" >>% Some true
        <|> (pKeyword "FALSE" >>% Some false)
        <|> (pKeyword "UNKNOWN" >>% None)

    // 5.3 <date value> ::= <years value> <minus sign> <months value> <minus sign> <days value>
    let pDateValue =
        pUnsignedIntegerAsInt .>> pchar '-' .>>. pUnsignedIntegerAsInt .>> pchar '-'
        .>>. pUnsignedIntegerAsInt
        >>= fun ((yi, mi), di) ->
            if yi < 1 || yi > 9999 || mi < 1 || mi > 12 || di < 1 || di > 31 then
                fail "invalid date"
            else
                preturn { Year = yi; Month = mi; Day = di }

    // 5.3 <date literal> ::= DATE <date string>
    let pDateLiteral = pKeyword "DATE" >>. between pQuote pQuote pDateValue .>> ws

    // 5.3 <time zone interval> ::= <sign> <hours value> <colon> <minutes value>
    // The displacement is bounded by +-14:00.
    let pTimeZoneInterval =
        pchar '+' >>% 1 <|> (pchar '-' >>% -1)
        .>>. (pUnsignedIntegerAsInt .>> pchar ':' .>>. pUnsignedIntegerAsInt)
        >>= fun (sign, (h, m)) ->
            if h > 14 || m > 59 || h = 14 && m > 0 then
                fail "invalid time zone interval"
            else
                preturn { Sign = sign; Hours = h; Minutes = m }

    // 5.3 <unquoted time string> ::= <time value>  [ <time zone interval>  ]
    // <time value> ::= <hours value> <colon> <minutes value> <colon> <seconds value>
    // The fields are unsigned, so a negative time of day cannot be written; the ranges are
    // hours 0-23, minutes 0-59 and seconds 0-60 (a leap second).
    let pUnquotedTimeString =
        pipe4
            (pUnsignedIntegerAsInt .>> pchar ':')
            (pUnsignedIntegerAsInt .>> pchar ':')
            (pipe2 (many1Chars digit) (opt (pchar '.' >>. manyChars digit)) (fun s f ->
                match f with
                | Some frac when frac <> "" -> s + "." + frac
                | _ -> s)
             >>= toDecimal)
            (opt (spaces >>. pTimeZoneInterval))
            (fun h m s tz -> h, m, s, tz)
        >>= fun (h, m, s, tz) ->
            if h > 23 || m > 59 || s > 60m then
                fail "invalid time"
            else
                preturn
                    { Hour = h
                      Minute = m
                      Second = s
                      TzOffset = tz }

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
        between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt

    // 10.1 <interval fractional seconds precision> ::= <unsigned integer>
    let pIntervalFractionalSecondsPrecision =
        between (token (pstring "(")) (token (pstring ")")) pUnsignedIntegerAsInt

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
                     (pUnsignedIntegerAsInt .>>. opt (token (pstring ",") >>. pUnsignedIntegerAsInt))
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
            >>= fun ((startF, startPrec), (endF, endPrec)) ->
                // 10.1 — only the restricted start/end combinations are valid, so `YEAR TO DAY`
                // and friends are rejected at the qualifier rather than by the value pattern.
                let validPair =
                    match startF, endF with
                    | Year, Month -> true
                    | Day, (Hour | Minute | Second) -> true
                    | Hour, (Minute | Second) -> true
                    | Minute, Second -> true
                    | _ -> false

                if not validPair then
                    fail "invalid interval qualifier"
                else
                    let prec =
                        match startPrec, endPrec with
                        | None, None -> None
                        | _ ->
                            Some
                                { IntervalPrecision.Leading = startPrec |> Option.bind (fun p -> p.Leading)
                                  FractionalSeconds = endPrec |> Option.bind (fun p -> p.FractionalSeconds) }

                    preturn (IntervalQualifier.Range(startF, endF, prec))

        let pSingle =
            pSingleDatetimeField
            |>> fun (field, prec) -> IntervalQualifier.SingleField(field, prec)

        attempt pRange <|> pSingle

    // 5.3 <unquoted interval string> — validates <year-month literal> | <day-time literal> against the
    // <interval qualifier>, and cross-checks the value's digits against the qualifier's
    // <interval leading field precision> and <interval fractional seconds precision>.
    let isValidIntervalValue q s =
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
            | IntervalQualifier.Range(Day, Hour, _) -> "^" + d + @"\s+" + d + "$"
            | IntervalQualifier.Range(Day, Minute, _) -> "^" + d + @"\s+" + d + ":" + d + "$"
            | IntervalQualifier.Range(Day, Second, _) -> "^" + d + @"\s+" + d + ":" + d + ":" + sec + "$"
            | IntervalQualifier.Range(Hour, Minute, _) -> "^" + d + ":" + d + "$"
            | IntervalQualifier.Range(Hour, Second, _) -> "^" + d + ":" + d + ":" + sec + "$"
            | IntervalQualifier.Range(Minute, Second, _) -> "^" + d + ":" + sec + "$"
            | _ ->
                // Unreachable: pIntervalQualifier only builds the combinations listed above.
                "$^"

        let leading, fractional =
            match q with
            | IntervalQualifier.SingleField(_, p)
            | IntervalQualifier.Range(_, _, p) ->
                match p with
                | Some p -> p.Leading, p.FractionalSeconds
                | None -> None, None

        // The first digit run is the leading field in every accepted shape.
        let leadingOk =
            match leading with
            | Some bound ->
                let m = System.Text.RegularExpressions.Regex.Match(s, d)
                not m.Success || m.Value.Length <= bound
            | None -> true

        let fractionalOk =
            match fractional with
            | Some bound ->
                let m = System.Text.RegularExpressions.Regex.Match(s, @"\.(\d+)")
                not m.Success || m.Groups.[1].Value.Length <= bound
            | None -> true

        System.Text.RegularExpressions.Regex.IsMatch(s, pattern)
        && leadingOk
        && fractionalOk

    // 5.3 <interval literal> ::= INTERVAL [ <sign> ] <interval string> <interval qualifier>
    // Both <sign> slots are optional and may co-occur; the literal is negative when exactly
    // one of them is '-'. The quoted sign stays part of <unquoted interval string> and is
    // stripped here so ValueString holds only the <year-month|day-time literal>.
    let pIntervalLiteral =
        pKeyword "INTERVAL" >>. opt (pchar '-' <|> pchar '+') .>> ws
        .>>. between pQuote pQuote (manyChars (noneOf "'"))
        .>> ws
        .>>. pIntervalQualifier
        .>> ws
        >>= fun ((outerSign, v), q) ->
            let outerNeg = outerSign = Some '-'

            let isNeg, valueStr =
                match v with
                | s when s.StartsWith "-" -> true, s.Substring 1
                | s when s.StartsWith "+" -> false, s.Substring 1
                | s -> false, s

            if isValidIntervalValue q valueStr then
                preturn
                    { IsNegative = outerNeg <> isNeg
                      ValueString = valueStr
                      Qualifier = q }
            else
                fail "invalid interval value"

    // 5.3 <literal> ::= <signed numeric literal> | <general literal>
    //     (character string | numeric | boolean | datetime | interval | hex string)
    // NULL is NOT a <literal>: it is the 6.5 <null specification>, parsed only in the
    // contextually-typed slots wired via ExpressionParser.pNullSpecification.
    let pLiteral =
        choice
            [ attempt (pCharacterStringLiteral |>> String |>> Literal)
              attempt (pNationalCharacterStringLiteral |>> NationalString |>> Literal)
              attempt (pUnicodeCharacterStringLiteral |>> UnicodeString |>> Literal)
              attempt (pUnsignedNumericLiteral |>> Number |>> Literal)
              attempt (pBooleanLiteral |>> Bool |>> Literal)
              attempt (pDateLiteral |>> Date |>> Literal)
              attempt (pTimeLiteral |>> Time |>> Literal)
              attempt (pTimestampLiteral |>> Timestamp |>> Literal)
              attempt (pIntervalLiteral |>> Interval |>> Literal)
              attempt (pBinaryStringLiteral |>> Literal.Binary |>> Literal) ]

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
          "REGR_SXY" ]

    let aggregateFunctionNames = Set.ofList aggregateFunctionKeywords

    // 6.10 <window function type> — these keywords are only valid with an OVER clause.
    let windowOnlyFunctionNames =
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
    let rankFunctionNames =
        Set.ofList [ "RANK"; "DENSE_RANK"; "PERCENT_RANK"; "CUME_DIST" ]

    // 10.9 <inverse distribution function type> — WITHIN GROUP is required.
    let inverseDistributionFunctionNames =
        Set.ofList [ "PERCENTILE_CONT"; "PERCENTILE_DISC" ]

    // 10.9 <binary set function type> — exactly two arguments.
    let binarySetFunctionNames =
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
    let withinGroupOnlyFunctionNames = Set.ofList [ "LISTAGG" ]

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
          // <hypothetical set function> rank names (not <general set function> types)
          "RANK"
          "DENSE_RANK"
          "PERCENT_RANK"
          "CUME_DIST"
          // <listagg set function>
          "LISTAGG"
          // <array aggregate function>
          "ARRAY_AGG"
          // <window function type>
          "ROW_NUMBER"
          "NTILE"
          "LEAD"
          "LAG"
          "FIRST_VALUE"
          "LAST_VALUE"
          "NTH_VALUE" ]

    let pReservedFunctionName =
        functionKeywords
        |> List.map pKeyword
        |> choice
        >>= fun kw ->
            if reservedWords.Contains kw then
                preturn kw
            else
                fail "not a reserved function keyword."

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
    // At most THREE parts (catalog.schema.identifier) — a 4th dot-part is rejected.
    let pSchemaQualifiedName =
        pIdentifier .>>. many (token (pstring ".") >>. pIdentifier)
        >>= fun (first, rest) ->
            if List.length rest > 2 then
                fail "<schema qualified name> allows at most three parts"
            else
                preturn (first :: rest)

    // 5.4 <host parameter name> ::= <colon> <identifier>
    let pHostParameter = pchar ':' >>. pIdentifier |>> (fun name -> ":" + name) .>> ws

    // 5.4 <scope option> ::= GLOBAL | LOCAL
    let pScopeOption: Parser<ScopeOption, unit> =
        pKeyword "GLOBAL" >>% ScopeOption.ScopeGlobal
        <|> (pKeyword "LOCAL" >>% ScopeOption.ScopeLocal)

    // 5.4 <identifier> — regular (non-reserved), delimited, or reserved function keyword
    let pRoutineName =
        choice
            [ attempt pReservedFunctionName
              attempt pUnicodeDelimitedIdentifier
              pRegularIdentifier
              pDelimitedIdentifier ]
