namespace SqlParser

open FParsec
open SqlParser.Ast

module ExpressionParser =
    let keywords =
        Set.ofList
            [ "ABSOLUTE"
              "ACTION"
              "ADD"
              "ALL"
              "ALLOCATE"
              "ALTER"
              "AND"
              "ANY"
              "ARE"
              "AS"
              "ASC"
              "ASSERTION"
              "AT"
              "AUTHORIZATION"
              "AVG"
              "BEGIN"
              "BETWEEN"
              "BIT"
              "BIT_LENGTH"
              "BOTH"
              "BY"
              "CASCADE"
              "CASCADED"
              "CASE"
              "CAST"
              "CATALOG"
              "CHAR"
              "CHARACTER"
              "CHARACTER_LENGTH"
              "CHAR_LENGTH"
              "CHECK"
              "CLOSE"
              "COALESCE"
              "COLLATE"
              "COLLATION"
              "COLUMN"
              "COMMIT"
              "CONNECT"
              "CONNECTION"
              "CONSTRAINT"
              "CONSTRAINTS"
              "CONTINUE"
              "CONVERT"
              "CORRESPONDING"
              "COUNT"
              "CREATE"
              "CROSS"
              "CURRENT"
              "CURRENT_DATE"
              "CURRENT_TIME"
              "CURRENT_TIMESTAMP"
              "CURRENT_USER"
              "CURSOR"
              "DATE"
              "DAY"
              "DEALLOCATE"
              "DEC"
              "DECIMAL"
              "DECLARE"
              "DEFAULT"
              "DEFERRABLE"
              "DEFERRED"
              "DELETE"
              "DESC"
              "DESCRIBE"
              "DESCRIPTOR"
              "DIAGNOSTICS"
              "DISCONNECT"
              "DISTINCT"
              "DOMAIN"
              "DOUBLE"
              "DROP"
              "ELSE"
              "END"
              "END-EXEC"
              "ESCAPE"
              "EXCEPT"
              "EXCEPTION"
              "EXEC"
              "EXECUTE"
              "EXISTS"
              "EXTERNAL"
              "EXTRACT"
              "FALSE"
              "FETCH"
              "FIRST"
              "FLOAT"
              "FOR"
              "FOREIGN"
              "FOUND"
              "FROM"
              "FULL"
              "GET"
              "GLOBAL"
              "GO"
              "GOTO"
              "GRANT"
              "GROUP"
              "HAVING"
              "HOUR"
              "IDENTITY"
              "IMMEDIATE"
              "IN"
              "INDICATOR"
              "INITIALLY"
              "INNER"
              "INPUT"
              "INSENSITIVE"
              "INSERT"
              "INT"
              "INTEGER"
              "INTERSECT"
              "INTERVAL"
              "INTO"
              "IS"
              "ISOLATION"
              "JOIN"
              "KEY"
              "LANGUAGE"
              "LAST"
              "LEADING"
              "LEFT"
              "LEVEL"
              "LIKE"
              "LIMIT"
              "LOCAL"
              "LOWER"
              "MATCH"
              "MATCHED"
              "MERGE"
              "MAX"
              "MIN"
              "MINUTE"
              "MODULE"
              "MONTH"
              "NAMES"
              "NATIONAL"
              "NATURAL"
              "NCHAR"
              "NEXT"
              "NO"
              "NOT"
              "NULL"
              "NULLIF"
              "NUMERIC"
              "OCTET_LENGTH"
              "OF"
              "ON"
              "ONLY"
              "OPEN"
              "OPTION"
              "OR"
              "ORDER"
              "OUTER"
              "OUTPUT"
              "OVERLAPS"
              "PAD"
              "PARTIAL"
              "POSITION"
              "PRECISION"
              "PREPARE"
              "PRESERVE"
              "PRIMARY"
              "PRIOR"
              "PRIVILEGES"
              "PROCEDURE"
              "PUBLIC"
              "READ"
              "REAL"
              "RECURSIVE"
              "REFERENCES"
              "RELATIVE"
              "RESTRICT"
              "REVOKE"
              "RIGHT"
              "ROLLBACK"
              "ROWS"
              "SCHEMA"
              "SCROLL"
              "SECOND"
              "SECTION"
              "SELECT"
              "SESSION"
              "SESSION_USER"
              "SET"
              "SIZE"
              "SMALLINT"
              "SOME"
              "SPACE"
              "SQL"
              "SQLCODE"
              "SQLERROR"
              "SQLSTATE"
              "SUBSTRING"
              "SUM"
              "SYSTEM_USER"
              "TABLE"
              "TEMPORARY"
              "THEN"
              "TIME"
              "TIMESTAMP"
              "TIMEZONE_HOUR"
              "TIMEZONE_MINUTE"
              "TO"
              "TRAILING"
              "TRANSACTION"
              "TRANSLATE"
              "TRANSLATION"
              "TRIM"
              "TRUE"
              "UNION"
              "UNIQUE"
              "UNKNOWN"
              "UPDATE"
              "UPPER"
              "USAGE"
              "USER"
              "USING"
              "VALUE"
              "VALUES"
              "VARCHAR"
              "VARYING"
              "VIEW"
              "WHEN"
              "WHENEVER"
              "WHERE"
              "WITH"
              "WORK"
              "WRITE"
              "YEAR"
              "ZONE" ]

    let ws = spaces
    let token p = p .>> ws

    let withExprPosition (p: Parser<ExpressionKind, unit>) : Parser<Expression, unit> =
        getPosition .>>. p
        |>> fun (pos, kind) -> Expression(kind, { Line = pos.Line; Column = pos.Column })

    let pIdentifierPart: Parser<string, unit> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'

        attempt (
            many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
            >>= fun s ->
                let upper = s.ToUpperInvariant()

                if keywords.Contains upper then
                    fail "keyword"
                else
                    preturn upper
        )

    let pIdentifier: Parser<string, unit> =
        sepBy1 pIdentifierPart (pstring ".") |>> String.concat "."

    let pStringContent = between (pstring "'") (pstring "'") (manyChars (noneOf "'"))

    let pStringLiteral: Parser<Literal, unit> = pStringContent |>> String

    let pNumberLiteral: Parser<Literal, unit> =
        numberLiteral NumberLiteralOptions.DefaultFloat "number"
        |>> fun nl -> Number(System.Decimal.Parse(nl.String, System.Globalization.CultureInfo.InvariantCulture))

    let pDateLiteral: Parser<Literal, unit> =
        pstringCI "DATE" >>. ws >>. pStringContent
        >>= fun d ->
            match System.DateTime.TryParseExact(d, "yyyy-MM-dd", null, System.Globalization.DateTimeStyles.None) with
            | true, _ -> preturn (Date d)
            | _ -> fail "Invalid date format, expected YYYY-MM-DD"

    let pTimeLiteral: Parser<Literal, unit> =
        pstringCI "TIME" >>. ws >>. pStringContent
        >>= fun t ->
            match System.TimeSpan.TryParse t with
            | true, _ -> preturn (Time t)
            | _ -> fail "Invalid time format"

    let pTimestampLiteral: Parser<Literal, unit> =
        pstringCI "TIMESTAMP" >>. ws >>. pStringContent
        >>= fun ts ->
            match System.DateTime.TryParse ts with
            | true, _ -> preturn (Timestamp ts)
            | _ -> fail "Invalid timestamp format"

    let pIntervalLiteral: Parser<Literal, unit> =
        pstringCI "INTERVAL" >>. ws >>. pStringContent .>> ws .>>. manyChars asciiLetter
        |>> fun (v, unit) ->
            let u = unit.ToUpperInvariant()
            Interval(if System.String.IsNullOrEmpty u then v else v + " " + u)

    let pBinaryLiteral: Parser<Literal, unit> =
        pstringCI "X" >>. pStringContent
        >>= fun hex ->
            if hex.Length % 2 <> 0 then
                fail "Invalid binary literal length"
            else
                try
                    let bytes = Array.zeroCreate (hex.Length / 2)

                    for i = 0 to bytes.Length - 1 do
                        bytes.[i] <- System.Convert.ToByte(hex.Substring(i * 2, 2), 16)

                    preturn (Binary bytes)
                with _ ->
                    fail "Invalid hex character in binary literal"

    let pLiteral: Parser<Literal, unit> =
        choice
            [ attempt pDateLiteral
              attempt pTimeLiteral
              attempt pTimestampLiteral
              attempt pIntervalLiteral
              attempt pBinaryLiteral
              pStringLiteral
              pNumberLiteral
              attempt (pstringCI "TRUE") >>% Bool true
              attempt (pstringCI "FALSE") >>% Bool false
              attempt (pstringCI "NULL") >>% Null ]

    let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
    let pExpression = opp.ExpressionParser

    let pFunctionCall =
        pIdentifier
        .>>. between (token (pstring "(")) (token (pstring ")")) (sepBy pExpression (token (pstring ",")))
        |>> FunctionCall

    let pTerm =
        choice
            [ withExprPosition (token pLiteral |>> Literal)
              withExprPosition (attempt pFunctionCall)
              withExprPosition (token pIdentifier |>> Identifier)
              between (token (pstring "(")) (token (pstring ")")) pExpression ]

    opp.TermParser <- pTerm

    let addInfix op precedence assoc mapping =
        opp.AddOperator(InfixOperator(op, ws, precedence, assoc, fun (_, pos as x) y -> Expression(mapping x y, pos)))

    let addInfixWord op precedence assoc mapping =
        let wordBoundary = notFollowedBy (asciiLetter <|> digit <|> pchar '_')

        opp.AddOperator(
            InfixOperator(
                op,
                wordBoundary .>> ws,
                precedence,
                assoc,
                fun (_, pos as x) y -> Expression(mapping x y, pos)
            )
        )

    let addPrefixWord op precedence mapping =
        let wordBoundary = notFollowedBy (asciiLetter <|> digit <|> pchar '_')

        opp.AddOperator(
            PrefixOperator(op, wordBoundary .>> ws, precedence, true, fun (_, pos as x) -> Expression(mapping x, pos))
        )

    addInfixWord "OR" 1 Associativity.Left (fun x y -> BinaryOp(Or, x, y))
    addInfixWord "or" 1 Associativity.Left (fun x y -> BinaryOp(Or, x, y))
    addInfixWord "AND" 2 Associativity.Left (fun x y -> BinaryOp(And, x, y))
    addInfixWord "and" 2 Associativity.Left (fun x y -> BinaryOp(And, x, y))
    addPrefixWord "NOT" 3 (fun x -> UnaryOp("NOT", x))
    addPrefixWord "not" 3 (fun x -> UnaryOp("NOT", x))
    addInfix "=" 3 Associativity.Left (fun x y -> BinaryOp(Equal, x, y))
    addInfix "<>" 3 Associativity.Left (fun x y -> BinaryOp(NotEqual, x, y))
    addInfix "!=" 3 Associativity.Left (fun x y -> BinaryOp(NotEqual, x, y))
    addInfix "<" 3 Associativity.Left (fun x y -> BinaryOp(LessThan, x, y))
    addInfix "<=" 3 Associativity.Left (fun x y -> BinaryOp(LessThanOrEqual, x, y))
    addInfix ">" 3 Associativity.Left (fun x y -> BinaryOp(GreaterThan, x, y))
    addInfix ">=" 3 Associativity.Left (fun x y -> BinaryOp(GreaterThanOrEqual, x, y))
    addInfix "+" 4 Associativity.Left (fun x y -> BinaryOp(Add, x, y))
    addInfix "-" 4 Associativity.Left (fun x y -> BinaryOp(Subtract, x, y))
    addInfix "*" 5 Associativity.Left (fun x y -> BinaryOp(Multiply, x, y))
    addInfix "/" 5 Associativity.Left (fun x y -> BinaryOp(Divide, x, y))
