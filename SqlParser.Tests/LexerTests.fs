module SqlParser.Tests.LexerTests

open Xunit
open FParsec
open SqlParser
open SqlParser.Lexer

let test p s =
    match run (p .>> eof) s with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let testFails p s =
    match run (p .>> eof) s with
    | Success _ -> failwith (sprintf "Expected parse failure for %s" s)
    | Failure _ -> ()

[<Fact>]
let ``SQL terminal characters are parsed correctly`` () =
    Assert.Equal('{', test pLeftBrace "{")
    Assert.Equal('}', test pRightBrace "}")
    Assert.Equal('^', test pCircumflex "^")
    Assert.Equal('|', test pVerticalBar "|")
    Assert.Equal('$', test pDollarSign "$")
    Assert.Equal("{-", test pLeftBraceMinus "{-")
    Assert.Equal("-}", test pRightMinusBrace "-}")

[<Fact>]
let ``Separators are white space and comments (5.2)`` () =
    // <white space> alone
    test pSeparator "   " |> ignore
    // <simple comment>: LF, CRLF and CR newlines all terminate it
    test pSeparator "-- comment\n" |> ignore
    test pSeparator "-- comment\r\n" |> ignore
    test pSeparator "-- comment\r" |> ignore
    // <bracketed comment>, including one longer than the old 10000-character search limit
    test pSeparator "/* comment */" |> ignore
    test pSeparator ("/* " + String.replicate 12000 "x" + " */") |> ignore
    // the end of the input also terminates a <simple comment>
    test pSeparator "-- trailing" |> ignore
    // any number of separators may follow one another
    test pSeparator "  -- a\n/* b */   /* c */" |> ignore

[<Fact>]
let ``An unterminated bracketed comment is rejected (5.2)`` () =
    testFails pSeparator "/* comment"
    test (pSeparator .>> pIdentifier) "/* comment */ x" |> ignore
    testFails (pSeparator .>> pIdentifier) "/* comment x"

[<Fact>]
let ``A comment is a separator between tokens (5.2)`` () =
    test (pKeyword "select") "SELECT/*c*/" |> ignore
    Assert.Equal("X", test pIdentifier "x/*c*/")
    Assert.Equal(Literal(Number 1m), test pLiteral "1/*c*/")
    Assert.Equal(Literal(Number 1m), test pLiteral "1 -- c\n")

[<Fact>]
let ``Regular identifiers are parsed correctly`` () =
    Assert.Equal("ID", test pIdentifier "id")
    Assert.Equal("MY_TABLE", test pIdentifier "my_table")
    Assert.Equal("COL123", test pIdentifier "col123")
    Assert.Equal("_X", test pIdentifier "_x")

[<Fact>]
let ``Reserved words as identifiers are rejected`` () =
    Assert.Throws<System.Exception>(fun () -> test pIdentifier "SELECT" |> ignore)
    |> ignore

[<Fact>]
let ``Delimited identifiers are parsed correctly`` () =
    Assert.Equal("SELECT", test pIdentifier "\"SELECT\"")
    Assert.Equal("My Table", test pIdentifier "\"My Table\"")
    Assert.Equal("Quoted \" quote", test pIdentifier "\"Quoted \"\" quote\"")
    Assert.Equal("data", test pIdentifier "U&\"d\\0061t\\0061\"")

[<Fact>]
let ``Unsigned integer and int conversions are checked`` () =
    Assert.Equal(123UL, test pUnsignedInteger "123")
    Assert.Equal(123, test pUnsignedIntegerAsInt "123")
    testFails pUnsignedInteger "99999999999999999999999999"
    testFails pUnsignedIntegerAsInt "99999999999"

[<Fact>]
let ``Numeric literal edge forms are parsed correctly (5.3)`` () =
    Assert.Equal(Literal(Number 0.5m), test pLiteral ".5")
    Assert.Equal(Literal(ApproximateNumber 100000m), test pLiteral "1E+5")
    Assert.Equal(Literal(ApproximateNumber 0.00001m), test pLiteral "1E-5")
    Assert.Equal(Literal(ApproximateNumber 0.0000000000000000000000000001m), test pLiteral "1E-28")

[<Theory>]
[<InlineData("1E29")>]
[<InlineData("1E99999999999999999999")>]
[<InlineData("9E28")>]
[<InlineData("0.1E-28")>]
[<InlineData("999999999999999999999999999999")>]
let ``Out-of-range numeric literals are rejected (5.3)`` (sql: string) = testFails pLiteral sql

[<Fact>]
let ``Character set specification is parsed correctly`` () =
    Assert.Equal("UTF8", test pCharacterSetSpecification "utf8")
    Assert.Equal("APP.UTF8", test pCharacterSetSpecification "app.utf8")
    Assert.Equal("abc", test pCharacterStringLiteral "_UTF8'abc'")
    Assert.Equal("abc", test pCharacterStringLiteral "_APP.UTF8'abc'")

[<Fact>]
let ``String literals are parsed correctly`` () =
    Assert.Equal("hello", test pCharacterStringLiteral "'hello'")
    Assert.Equal("It's a trap", test pCharacterStringLiteral "'It''s a trap'")
    Assert.Equal("Multiline", test pCharacterStringLiteral "'Multi' 'line'")

[<Fact>]
let ``Unicode character string literals are parsed correctly (5.3)`` () =
    // <Unicode 4 digit escape value> with the default <Unicode escape character> (backslash)
    Assert.Equal(Literal(UnicodeString "data"), test pLiteral "U&'d\\0061t\\0061'")
    // <Unicode 6 digit escape value>
    Assert.Equal(Literal(UnicodeString "\U0001F600"), test pLiteral "U&'\\+01F600'")
    // a surrogate pair written as two 4 digit escapes is one Unicode scalar value
    Assert.Equal(Literal(UnicodeString "\U0001F600"), test pLiteral "U&'\\D83D\\DE00'")
    // adjacent <character string literal> segments are concatenated
    Assert.Equal(Literal(UnicodeString "ab"), test pLiteral "U&'a' 'b'")
    // an explicit UESCAPE chooses the escape character
    Assert.Equal(Literal(UnicodeString "dat"), test pLiteral "U&'d!0061t' UESCAPE '!'")
    // the escaped quote and the escaped escape character
    Assert.Equal(Literal(UnicodeString "'"), test pLiteral "U&'\\0027'")
    Assert.Equal(Literal(UnicodeString "\\"), test pLiteral "U&'\\\\'")
    // a non-BMP <Unicode escape character> (a surrogate pair in the source text)
    Assert.Equal(Literal(UnicodeString "dat"), test pLiteral "U&'d\U0001F6000061t' UESCAPE '\U0001F600'")

[<Fact>]
let ``A surrogate UESCAPE character must be a complete pair (5.2)`` () =
    // pAnyRune — a lone surrogate is not a Unicode scalar value.
    testFails pLiteral ("U&'x' UESCAPE '" + string (char 0xD83D) + "a'")
    testFails pLiteral ("U&'x' UESCAPE '" + string (char 0xDE00) + "'")

[<Theory>]
[<InlineData("U&'\\D83D'")>]
[<InlineData("U&'\\DE00'")>]
[<InlineData("U&'\\+110000'")>]
[<InlineData("U&'\\+00D800'")>]
[<InlineData("U&'x' UESCAPE '0'")>]
let ``Invalid Unicode character string literals are rejected (5.3)`` (sql: string) = testFails pLiteral sql

[<Fact>]
let ``Date literals are parsed correctly (5.3)`` () =
    Assert.Equal(Literal(Date { Year = 2020; Month = 2; Day = 29 }), test pLiteral "DATE '2020-02-29'")

[<Theory>]
[<InlineData("DATE '2020-13-01'")>]
[<InlineData("DATE '2020-00-10'")>]
[<InlineData("DATE '2020-01-32'")>]
let ``Invalid date literals are rejected (5.3)`` (sql: string) = testFails pLiteral sql

[<Fact>]
let ``Time literals are parsed correctly (5.3)`` () =
    Assert.Equal(
        Literal(
            Time
                { Hour = 12
                  Minute = 30
                  Second = 45.5m
                  TzOffset = Some { Sign = 1; Hours = 5; Minutes = 30 } }
        ),
        test pLiteral "TIME '12:30:45.5+05:30'"
    )
    // a leap second is accepted
    Assert.Equal(
        Literal(
            Time
                { Hour = 23
                  Minute = 59
                  Second = 60m
                  TzOffset = None }
        ),
        test pLiteral "TIME '23:59:60'"
    )

[<Theory>]
[<InlineData("TIME '24:00:00'")>]
[<InlineData("TIME '12:60:00'")>]
[<InlineData("TIME '12:00:61'")>]
[<InlineData("TIME '12:00:00+15:00'")>]
[<InlineData("TIME '12:00:00+05:60'")>]
let ``Invalid time literals are rejected (5.3)`` (sql: string) = testFails pLiteral sql

[<Fact>]
let ``Timestamp literals are parsed correctly (5.3)`` () =
    Assert.Equal(
        Literal(
            Timestamp
                { Date = { Year = 2020; Month = 1; Day = 1 }
                  Time =
                    { Hour = 12
                      Minute = 0
                      Second = 0m
                      TzOffset = None } }
        ),
        test pLiteral "TIMESTAMP '2020-01-01 12:00:00'"
    )

[<Fact>]
let ``Interval qualifiers are parsed correctly (10.1)`` () =
    Assert.Equal(IntervalQualifier.SingleField(Second, None), test pIntervalQualifier "SECOND")

    Assert.Equal(
        IntervalQualifier.SingleField(
            Day,
            Some
                { Leading = Some 3
                  FractionalSeconds = None }
        ),
        test pIntervalQualifier "DAY(3)"
    )

    Assert.Equal(
        IntervalQualifier.SingleField(
            Second,
            Some
                { Leading = Some 2
                  FractionalSeconds = Some 3 }
        ),
        test pIntervalQualifier "SECOND(2,3)"
    )

    Assert.Equal(IntervalQualifier.Range(Year, Month, None), test pIntervalQualifier "YEAR TO MONTH")

    Assert.Equal(
        IntervalQualifier.Range(
            Year,
            Month,
            Some
                { Leading = Some 2
                  FractionalSeconds = None }
        ),
        test pIntervalQualifier "YEAR(2) TO MONTH"
    )

    Assert.Equal(IntervalQualifier.Range(Day, Hour, None), test pIntervalQualifier "DAY TO HOUR")
    Assert.Equal(IntervalQualifier.Range(Day, Minute, None), test pIntervalQualifier "DAY TO MINUTE")
    Assert.Equal(IntervalQualifier.Range(Day, Second, None), test pIntervalQualifier "DAY TO SECOND")
    Assert.Equal(IntervalQualifier.Range(Hour, Minute, None), test pIntervalQualifier "HOUR TO MINUTE")
    Assert.Equal(IntervalQualifier.Range(Hour, Second, None), test pIntervalQualifier "HOUR TO SECOND")
    Assert.Equal(IntervalQualifier.Range(Minute, Second, None), test pIntervalQualifier "MINUTE TO SECOND")

[<Theory>]
[<InlineData("YEAR TO DAY")>]
[<InlineData("HOUR TO YEAR")>]
[<InlineData("MONTH TO SECOND")>]
let ``Invalid interval qualifiers are rejected (10.1)`` (sql: string) = testFails pIntervalQualifier sql

[<Fact>]
let ``Interval literals are parsed correctly (5.3)`` () =
    Assert.Equal(
        Literal(
            Interval
                { IsNegative = false
                  ValueString = "1-2"
                  Qualifier = IntervalQualifier.Range(Year, Month, None) }
        ),
        test pLiteral "INTERVAL '1-2' YEAR TO MONTH"
    )

    Assert.Equal(
        Literal(
            Interval
                { IsNegative = false
                  ValueString = "5 12:30"
                  Qualifier = IntervalQualifier.Range(Day, Minute, None) }
        ),
        test pLiteral "INTERVAL '5 12:30' DAY TO MINUTE"
    )

    Assert.Equal(
        Literal(
            Interval
                { IsNegative = false
                  ValueString = "1 12:30:45.5"
                  Qualifier = IntervalQualifier.Range(Day, Second, None) }
        ),
        test pLiteral "INTERVAL '1 12:30:45.5' DAY TO SECOND"
    )

    Assert.Equal(
        Literal(
            Interval
                { IsNegative = false
                  ValueString = "1:30:45"
                  Qualifier = IntervalQualifier.Range(Hour, Second, None) }
        ),
        test pLiteral "INTERVAL '1:30:45' HOUR TO SECOND"
    )

    Assert.Equal(
        Literal(
            Interval
                { IsNegative = false
                  ValueString = "1.5"
                  Qualifier = IntervalQualifier.SingleField(Second, None) }
        ),
        test pLiteral "INTERVAL '1.5' SECOND"
    )

    Assert.Equal(
        Literal(
            Interval
                { IsNegative = false
                  ValueString = "12"
                  Qualifier =
                    IntervalQualifier.SingleField(
                        Day,
                        Some
                            { Leading = Some 2
                              FractionalSeconds = None }
                    ) }
        ),
        test pLiteral "INTERVAL '12' DAY(2)"
    )

    Assert.Equal(
        Literal(
            Interval
                { IsNegative = false
                  ValueString = "1.23"
                  Qualifier =
                    IntervalQualifier.SingleField(
                        Second,
                        Some
                            { Leading = Some 2
                              FractionalSeconds = Some 2 }
                    ) }
        ),
        test pLiteral "INTERVAL '1.23' SECOND(2,2)"
    )

    // the sign may be written inside the quotes ...
    Assert.Equal(
        Literal(
            Interval
                { IsNegative = true
                  ValueString = "1-2"
                  Qualifier = IntervalQualifier.Range(Year, Month, None) }
        ),
        test pLiteral "INTERVAL '-1-2' YEAR TO MONTH"
    )

    // ... or outside them
    Assert.Equal(
        Literal(
            Interval
                { IsNegative = true
                  ValueString = "1"
                  Qualifier = IntervalQualifier.SingleField(Day, None) }
        ),
        test pLiteral "INTERVAL -'1' DAY"
    )

    Assert.Equal(
        Literal(
            Interval
                { IsNegative = false
                  ValueString = "1"
                  Qualifier = IntervalQualifier.SingleField(Day, None) }
        ),
        test pLiteral "INTERVAL +'1' DAY"
    )

[<Theory>]
[<InlineData("INTERVAL '1-2' DAY")>]
[<InlineData("INTERVAL '1.5' DAY")>]
[<InlineData("INTERVAL '123' DAY(2)")>]
[<InlineData("INTERVAL '1.234' SECOND(2,2)")>]
let ``Invalid interval literals are rejected (5.3)`` (sql: string) = testFails pLiteral sql

[<Fact>]
let ``Schema qualified names are parsed correctly`` () =
    Assert.Equal<string list>([ "APP" ], test pSchemaQualifiedName "app")
    Assert.Equal<string list>([ "APP"; "USERS" ], test pSchemaQualifiedName "app.users")
    Assert.Equal<string list>([ "CAT"; "APP"; "USERS" ], test pSchemaQualifiedName "cat.app.users")
    Assert.Equal<string list>([ "APP"; "USERS" ], test pSchemaQualifiedName "app . users")
    Assert.Equal<string list>([ "APP"; "USERS" ], test pSchemaQualifiedName "app.\"USERS\"")

[<Fact>]
let ``Schema qualified names with reserved words are rejected`` () =
    testFails pSchemaQualifiedName "app.SELECT"
    testFails pSchemaQualifiedName "SELECT.app"
