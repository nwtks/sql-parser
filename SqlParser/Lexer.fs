namespace SqlParser

open FParsec

module Lexer =
    let ws = spaces
    let token p = p .>> ws

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

    let nonReservedWords =
        Set.ofList
            [ "A"
              "ABSOLUTE"
              "ACTION"
              "ADA"
              "ADD"
              "ADMIN"
              "AFTER"
              "ALWAYS"
              "ASC"
              "ASSERTION"
              "ASSIGNMENT"
              "ATTRIBUTE"
              "ATTRIBUTES"
              "BEFORE"
              "BERNOULLI"
              "BREADTH"
              "C"
              "CASCADE"
              "CATALOG"
              "CATALOG_NAME"
              "CHAIN"
              "CHAINING"
              "CHARACTER_SET_CATALOG"
              "CHARACTER_SET_NAME"
              "CHARACTER_SET_SCHEMA"
              "CHARACTERISTICS"
              "CHARACTERS"
              "CLASS_ORIGIN"
              "COBOL"
              "COLLATION"
              "COLLATION_CATALOG"
              "COLLATION_NAME"
              "COLLATION_SCHEMA"
              "COLUMNS"
              "COLUMN_NAME"
              "COMMAND_FUNCTION"
              "COMMAND_FUNCTION_CODE"
              "COMMITTED"
              "CONDITIONAL"
              "CONDITION_NUMBER"
              "CONNECTION"
              "CONNECTION_NAME"
              "CONSTRAINT_CATALOG"
              "CONSTRAINT_NAME"
              "CONSTRAINT_SCHEMA"
              "CONSTRAINTS"
              "CONSTRUCTOR"
              "CONTINUE"
              "CURSOR_NAME"
              "DATA"
              "DATETIME_INTERVAL_CODE"
              "DATETIME_INTERVAL_PRECISION"
              "DEFAULTS"
              "DEFERRABLE"
              "DEFERRED"
              "DEFINED"
              "DEFINER"
              "DEGREE"
              "DEPTH"
              "DERIVED"
              "DESC"
              "DESCRIBE_CATALOG"
              "DESCRIBE_NAME"
              "DESCRIBE_PROCEDURE_SPECIFIC_CATALOG"
              "DESCRIBE_PROCEDURE_SPECIFIC_NAME"
              "DESCRIBE_PROCEDURE_SPECIFIC_SCHEMA"
              "DESCRIBE_SCHEMA"
              "DESCRIPTOR"
              "DIAGNOSTICS"
              "DISPATCH"
              "DOMAIN"
              "DYNAMIC_FUNCTION"
              "DYNAMIC_FUNCTION_CODE"
              "ENCODING"
              "ENFORCED"
              "ERROR"
              "EXCLUDE"
              "EXCLUDING"
              "EXPRESSION"
              "FINAL"
              "FINISH"
              "FINISH_CATALOG"
              "FINISH_NAME"
              "FINISH_PROCEDURE_SPECIFIC_CATALOG"
              "FINISH_PROCEDURE_SPECIFIC_NAME"
              "FINISH_PROCEDURE_SPECIFIC_SCHEMA"
              "FINISH_SCHEMA"
              "FIRST"
              "FLAG"
              "FOLLOWING"
              "FORMAT"
              "FORTRAN"
              "FOUND"
              "FULFILL"
              "FULFILL_CATALOG"
              "FULFILL_NAME"
              "FULFILL_PROCEDURE_SPECIFIC_CATALOG"
              "FULFILL_PROCEDURE_SPECIFIC_NAME"
              "FULFILL_PROCEDURE_SPECIFIC_SCHEMA"
              "FULFILL_SCHEMA"
              "G"
              "GENERAL"
              "GENERATED"
              "GO"
              "GOTO"
              "GRANTED"
              "HAS_PASS_THROUGH_COLUMNS"
              "HAS_PASS_THRU_COLS"
              "HIERARCHY"
              "IGNORE"
              "IMMEDIATE"
              "IMMEDIATELY"
              "IMPLEMENTATION"
              "INCLUDING"
              "INCREMENT"
              "INITIALLY"
              "INPUT"
              "INSTANCE"
              "INSTANTIABLE"
              "INSTEAD"
              "INVOKER"
              "ISOLATION"
              "IS_PRUNABLE"
              "JSON"
              "K"
              "KEEP"
              "KEY"
              "KEYS"
              "KEY_MEMBER"
              "KEY_TYPE"
              "LAST"
              "LENGTH"
              "LEVEL"
              "LOCATOR"
              "M"
              "MAP"
              "MATCHED"
              "MAXVALUE"
              "MESSAGE_LENGTH"
              "MESSAGE_OCTET_LENGTH"
              "MESSAGE_TEXT"
              "MINVALUE"
              "MORE"
              "MUMPS"
              "NAME"
              "NAMES"
              "NESTED"
              "NESTING"
              "NEXT"
              "NFC"
              "NFD"
              "NFKC"
              "NFKD"
              "NORMALIZED"
              "NULLABLE"
              "NULLS"
              "NUMBER"
              "OBJECT"
              "OCTETS"
              "OPTION"
              "OPTIONS"
              "ORDERING"
              "ORDINALITY"
              "OTHERS"
              "OUTPUT"
              "OVERFLOW"
              "OVERRIDING"
              "P"
              "PAD"
              "PARAMETER_MODE"
              "PARAMETER_NAME"
              "PARAMETER_ORDINAL_POSITION"
              "PARAMETER_SPECIFIC_CATALOG"
              "PARAMETER_SPECIFIC_NAME"
              "PARAMETER_SPECIFIC_SCHEMA"
              "PARTIAL"
              "PASCAL"
              "PASS"
              "PASSING"
              "PAST"
              "PATH"
              "PLACING"
              "PLAN"
              "PLI"
              "PRECEDING"
              "PRESERVE"
              "PRIOR"
              "PRIVATE"
              "PRIVATE_PARAMETERS"
              "PRIVATE_PARAMS_S"
              "PRIVILEGES"
              "PRUNE"
              "PUBLIC"
              "QUOTES"
              "READ"
              "RELATIVE"
              "REPEATABLE"
              "RESPECT"
              "RESTART"
              "RESTRICT"
              "RETURNED_CARDINALITY"
              "RETURNED_LENGTH"
              "RETURNED_OCTET_LENGTH"
              "RETURNED_SQLSTATE"
              "RETURNING"
              "RETURNS_ONLY_PASS_THROUGH"
              "RET_ONLY_PASS_THRU"
              "ROLE"
              "ROUTINE"
              "ROUTINE_CATALOG"
              "ROUTINE_NAME"
              "ROUTINE_SCHEMA"
              "ROW_COUNT"
              "SCALAR"
              "SCALE"
              "SCHEMA"
              "SCHEMA_NAME"
              "SCOPE_CATALOG"
              "SCOPE_NAME"
              "SCOPE_SCHEMA"
              "SECTION"
              "SECURITY"
              "SELF"
              "SEQUENCE"
              "SERIALIZABLE"
              "SERVER_NAME"
              "SESSION"
              "SETS"
              "SIMPLE"
              "SIZE"
              "SOURCE"
              "SPACE"
              "SPECIFIC_NAME"
              "START_CATALOG"
              "START_NAME"
              "START_PROCEDURE_SPECIFIC_CATALOG"
              "START_PROCEDURE_SPECIFIC_NAME"
              "START_PROCEDURE_SPECIFIC_SCHEMA"
              "START_SCHEMA"
              "STATE"
              "STATEMENT"
              "STRING"
              "STRUCTURE"
              "STYLE"
              "SUBCLASS_ORIGIN"
              "T"
              "TABLE_NAME"
              "TABLE_SEMANTICS"
              "TEMPORARY"
              "THROUGH"
              "TIES"
              "TOP_LEVEL_COUNT"
              "TRANSACTION"
              "TRANSACTIONS_COMMITTED"
              "TRANSACTIONS_ROLLED_BACK"
              "TRANSACTION_ACTIVE"
              "TRANSFORM"
              "TRANSFORMS"
              "TRIGGER_CATALOG"
              "TRIGGER_NAME"
              "TRIGGER_SCHEMA"
              "TYPE"
              "UNBOUNDED"
              "UNCOMMITTED"
              "UNCONDITIONAL"
              "UNDER"
              "UNNAMED"
              "USAGE"
              "USER_DEFINED_TYPE_CATALOG"
              "USER_DEFINED_TYPE_CODE"
              "USER_DEFINED_TYPE_NAME"
              "USER_DEFINED_TYPE_SCHEMA"
              "UTF16"
              "UTF32"
              "UTF8"
              "VIEW"
              "WORK"
              "WRAPPER"
              "WRITE"
              "ZONE" ]

    let pReservedWord: Parser<string, unit> =
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        let p = many1Satisfy2L isLetter isIdentifierChar "reserved word"

        p
        >>= fun s ->
            let upper = s.ToUpperInvariant()

            if reservedWords.Contains upper then
                preturn upper
            else
                fail "not a reserved word"

    let pKeyword s =
        attempt (pstringCI s .>> notFollowedBy (asciiLetter <|> digit <|> pchar '_'))
        .>> ws

    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    let pIdentifierRaw =
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        |>> (fun s -> s.ToUpperInvariant())
        .>> ws

    let pIdentifier =
        let pRegularIdentifier =
            attempt (
                pIdentifierRaw
                >>= fun s ->
                    if reservedWords.Contains s then
                        fail "reserved word"
                    else
                        preturn s
            )

        let pDelimitedIdentifier =
            between (pchar '\"') (pchar '\"') (manyChars (attempt (pstring "\"\"") >>% '\"' <|> noneOf "\""))

        let pUnicodeDelimitedIdentifier =
            pchar 'U' >>. pchar '&' >>. pDelimitedIdentifier
            .>>. opt (pKeyword "UESCAPE" >>. pchar '\'' >>. anyChar .>> pchar '\'')
            |>> fun (id, esc) -> id

        choice
            [ attempt pUnicodeDelimitedIdentifier
              pRegularIdentifier
              pDelimitedIdentifier ]
        .>> ws

    let pQuote = pchar '\''
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

    let pHexit = hex <|> digit

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
        |>> fun (first, rest) -> List.toArray (List.concat (first :: rest))
        .>> ws

    let pUnicodeEscapeSpecifier =
        opt (pKeyword "UESCAPE" >>. pQuote >>. anyChar .>> pQuote)

    let pampersand = pchar '&'

    [<TailCall>]
    let rec loopCount p n acc =
        if n = 0 then
            preturn (List.rev acc)
        else
            p >>= fun x -> loopCount p (n - 1) (x :: acc)

    let pCount n p = loopCount p n []

    let pUnicode4DigitEscape =
        pchar '\\' >>. pCount 4 pHexit
        |>> fun chars -> System.Convert.ToChar(System.Convert.ToInt32(System.String(List.toArray chars), 16))

    let pUnicode6DigitEscape =
        pstring "\\+" >>. pCount 6 pHexit
        |>> fun chars -> System.Char.ConvertFromUtf32(System.Convert.ToInt32(System.String(List.toArray chars), 16))

    let pUnicodeCharacterStringLiteral: Parser<string, unit> =
        pchar 'U' >>. pampersand >>. pCharacterStringLiteral
        .>>. pUnicodeEscapeSpecifier
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
        attempt (
            pipe3
                pExactNumericLiteral
                (pchar 'E' <|> pchar 'e')
                (pipe2 (opt (pchar '+' <|> pchar '-')) (many1Chars digit) (fun s d ->
                    (Option.defaultValue '+' s |> string) + d))
                (fun m _ e -> m * decimal (10.0 ** float e))
        )
        .>> ws

    let pNumericLiteral: Parser<decimal, unit> =
        attempt pApproximateNumericLiteral <|> pExactNumericLiteral

    let pDateString = between pQuote pQuote (many1Chars (noneOf "'"))
    let pTimeString = between pQuote pQuote (many1Chars (noneOf "'"))
    let pTimestampString = between pQuote pQuote (many1Chars (noneOf "'"))

    let pDateLiteral: Parser<string, unit> = pKeyword "DATE" >>. pDateString .>> ws
    let pTimeLiteral: Parser<string, unit> = pKeyword "TIME" >>. pTimeString .>> ws

    let pTimestampLiteral: Parser<string, unit> =
        pKeyword "TIMESTAMP" >>. pTimestampString .>> ws

    let pBooleanLiteral: Parser<bool, unit> =
        pKeyword "TRUE" >>% true
        <|> (pKeyword "FALSE" >>% false)
        <|> (pKeyword "UNKNOWN" >>% false)

    let pQuestionMark: Parser<char, unit> = pchar '?' .>> ws

    let pHostParameter: Parser<string, unit> =
        pchar ':' >>. pIdentifier |>> (fun name -> ":" + name) .>> ws
