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
let ``Regular identifiers are parsed correctly`` () =
    Assert.Equal("ID", test pIdentifier "id")
    Assert.Equal("MY_TABLE", test pIdentifier "my_table")
    Assert.Equal("COL123", test pIdentifier "col123")

[<Fact>]
let ``Reserved words as identifiers are rejected`` () =
    Assert.Throws<System.Exception>(fun () -> test pIdentifier "SELECT" |> ignore)
    |> ignore

[<Fact>]
let ``Delimited identifiers are parsed correctly`` () =
    Assert.Equal("SELECT", test pIdentifier "\"SELECT\"")
    Assert.Equal("My Table", test pIdentifier "\"My Table\"")
    Assert.Equal("Quoted \" quote", test pIdentifier "\"Quoted \"\" quote\"")

[<Fact>]
let ``Unicode escape value decodes a pair and rejects lone surrogates`` () =
    let high = string (char 0xD83D)
    let low = string (char 0xDE00)
    Assert.Equal(high + low, test pAnyRune (high + low))
    testFails pAnyRune low
    testFails pAnyRune high
    testFails pAnyRune (high + "A")

[<Fact>]
let ``Unicode escape sequences are decoded correctly`` () =
    Assert.Equal("A", test pUnicodeCharacterStringLiteral "U&'\\0041'")
    Assert.Equal("AB", test pUnicodeCharacterStringLiteral "U&'\\0041\\0042'")
    Assert.Equal("A", test pUnicodeDelimitedIdentifier "U&\"\\0041\"")

[<Fact>]
let ``Unicode six digit escape is decoded correctly`` () =
    Assert.Equal("\uD83D\uDE00", test pUnicodeCharacterStringLiteral "U&'\\+01F600'")
    Assert.Equal("A", test pUnicodeCharacterStringLiteral "U&'\\+000041'")

[<Fact>]
let ``Unicode escape specifier accepts a surrogate pair rune`` () =
    Assert.Equal("\uD83D\uDE00", test pUnicodeEscapeSpecifier "UESCAPE '\uD83D\uDE00'")

[<Fact>]
let ``Numeric literals are parsed correctly`` () =
    Assert.Equal(123m, test pUnsignedNumericLiteral "123")
    Assert.Equal(123.45m, test pUnsignedNumericLiteral "123.45")
    Assert.Equal(0.45m, test pUnsignedNumericLiteral ".45")
    Assert.Equal(12300m, test pUnsignedNumericLiteral "1.23E4")
    Assert.Equal(0.0123m, test pUnsignedNumericLiteral "1.23E-2")

[<Fact>]
let ``Large exponent literals do not overflow`` () =
    let result = test pUnsignedNumericLiteral "1E400"
    Assert.True(result > 0m)

[<Fact>]
let ``Approximate numeric literals with an explicit exponent sign are parsed`` () =
    Assert.Equal(12300m, test pUnsignedNumericLiteral "1.23E+4")
    Assert.True(test pUnsignedNumericLiteral "1E+400" > 0m)

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
let ``Binary literals are parsed correctly`` () =
    Assert.Equal<byte array>([| 0x01uy; 0xAFuy |], test pBinaryStringLiteral "X'01AF'")

[<Fact>]
let ``Hex literal with spaces between hexit pairs is parsed`` () =
    Assert.Equal<byte array>([| 0x01uy; 0xAFuy; 0x02uy |], test pBinaryStringLiteral "X'01 AF 02'")

[<Fact>]
let ``Boolean literals are parsed correctly`` () =
    Assert.Equal(Some true, test pBooleanLiteral "TRUE")
    Assert.Equal(Some false, test pBooleanLiteral "FALSE")
    Assert.Equal(None, test pBooleanLiteral "UNKNOWN")

[<Fact>]
let ``Date, Time, Timestamp literals are parsed correctly`` () =
    Assert.Equal({ Year = 2023; Month = 1; Day = 1 }, test pDateLiteral "DATE '2023-01-01'")

    Assert.Equal(
        { Hour = 12
          Minute = 0
          Second = 0m
          TzOffset = None },
        test pTimeLiteral "TIME '12:00:00'"
    )

    Assert.Equal(
        { Date = { Year = 2023; Month = 1; Day = 1 }
          Time =
            { Hour = 12
              Minute = 0
              Second = 0m
              TzOffset = None } },
        test pTimestampLiteral "TIMESTAMP '2023-01-01 12:00:00'"
    )

[<Fact>]
let ``Invalid date values are rejected`` () =
    testFails pDateLiteral "DATE '2023-13-01'"
    testFails pDateLiteral "DATE '2023-01-40'"
    testFails pDateLiteral "DATE '2023--1-01'"
    testFails pDateLiteral "DATE '2023-00-01'"
    testFails pDateLiteral "DATE '2023-01-00'"

[<Fact>]
let ``Invalid time seconds fail cleanly`` () =
    match run (pTimeLiteral .>> eof) "TIME '12:00:00.5.5'" with
    | Failure _ -> ()
    | Success _ -> Assert.Fail("Expected TIME literal with invalid seconds to fail")

[<Fact>]
let ``Interval literals are parsed correctly`` () =
    Assert.Equal(
        { IsNegative = false
          ValueString = "1-2"
          Qualifier = IntervalQualifier.Range(Year, Month, None) },
        test pIntervalLiteral "INTERVAL '1-2' YEAR TO MONTH"
    )

[<Fact>]
let ``Interval sign inside quotes is parsed`` () =
    Assert.Equal(
        { IsNegative = true
          ValueString = "1-2"
          Qualifier = IntervalQualifier.Range(Year, Month, None) },
        test pIntervalLiteral "INTERVAL '-1-2' YEAR TO MONTH"
    )

[<Fact>]
let ``Interval single field with leading precision is parsed`` () =
    Assert.Equal(
        { IsNegative = false
          ValueString = "1"
          Qualifier =
            IntervalQualifier.SingleField(
                Year,
                Some
                    { IntervalPrecision.Leading = Some 4
                      FractionalSeconds = None }
            ) },
        test pIntervalLiteral "INTERVAL '1' YEAR(4)"
    )

[<Fact>]
let ``Interval single SECOND with leading and fractional precision is parsed`` () =
    Assert.Equal(
        { IsNegative = false
          ValueString = "1.5"
          Qualifier =
            IntervalQualifier.SingleField(
                Second,
                Some
                    { IntervalPrecision.Leading = Some 2
                      FractionalSeconds = Some 3 }
            ) },
        test pIntervalLiteral "INTERVAL '1.5' SECOND(2,3)"
    )

[<Fact>]
let ``Interval range with leading precision on start field is parsed`` () =
    Assert.Equal(
        { IsNegative = false
          ValueString = "1-2"
          Qualifier =
            IntervalQualifier.Range(
                Year,
                Month,
                Some
                    { IntervalPrecision.Leading = Some 4
                      FractionalSeconds = None }
            ) },
        test pIntervalLiteral "INTERVAL '1-2' YEAR(4) TO MONTH"
    )

[<Fact>]
let ``Interval range with fractional seconds precision on SECOND end is parsed`` () =
    Assert.Equal(
        { IsNegative = false
          ValueString = "1:30:05.5"
          Qualifier =
            IntervalQualifier.Range(
                Hour,
                Second,
                Some
                    { IntervalPrecision.Leading = None
                      FractionalSeconds = Some 3 }
            ) },
        test pIntervalLiteral "INTERVAL '1:30:05.5' HOUR TO SECOND(3)"
    )

[<Fact>]
let ``Interval single DAY HOUR and MINUTE fields are parsed`` () =
    Assert.Equal(IntervalQualifier.SingleField(Day, None), (test pIntervalLiteral "INTERVAL '5' DAY").Qualifier)

    Assert.Equal(IntervalQualifier.SingleField(Hour, None), (test pIntervalLiteral "INTERVAL '5' HOUR").Qualifier)

    Assert.Equal(IntervalQualifier.SingleField(Minute, None), (test pIntervalLiteral "INTERVAL '5' MINUTE").Qualifier)

    Assert.Equal(
        IntervalQualifier.SingleField(
            Day,
            Some
                { IntervalPrecision.Leading = Some 2
                  FractionalSeconds = None }
        ),
        (test pIntervalLiteral "INTERVAL '5' DAY(2)").Qualifier
    )

    Assert.Equal(
        IntervalQualifier.SingleField(
            Second,
            Some
                { IntervalPrecision.Leading = Some 2
                  FractionalSeconds = None }
        ),
        (test pIntervalLiteral "INTERVAL '5' SECOND(2)").Qualifier
    )

[<Fact>]
let ``Interval ranges across DAY HOUR MINUTE and SECOND are parsed`` () =
    Assert.Equal(
        IntervalQualifier.Range(Day, Hour, None),
        (test pIntervalLiteral "INTERVAL '5 12' DAY TO HOUR").Qualifier
    )

    Assert.Equal(
        IntervalQualifier.Range(Day, Minute, None),
        (test pIntervalLiteral "INTERVAL '5 12:30' DAY TO MINUTE").Qualifier
    )

    Assert.Equal(
        IntervalQualifier.Range(Day, Second, None),
        (test pIntervalLiteral "INTERVAL '5 12:30:05.5' DAY TO SECOND").Qualifier
    )

    Assert.Equal(
        IntervalQualifier.Range(Minute, Second, None),
        (test pIntervalLiteral "INTERVAL '12:30.5' MINUTE TO SECOND").Qualifier
    )

[<Fact>]
let ``Interval qualifier with an invalid field combination is rejected`` () =
    testFails pIntervalLiteral "INTERVAL '1' YEAR TO DAY"

[<Fact>]
let ``Invalid interval values are rejected`` () =
    testFails pIntervalLiteral "INTERVAL 'abc' YEAR"
    testFails pIntervalLiteral "INTERVAL '1-2' YEAR"

    Assert.Equal(
        { IsNegative = false
          ValueString = "1"
          Qualifier = IntervalQualifier.SingleField(Year, None) },
        test pIntervalLiteral "INTERVAL '1' YEAR"
    )

    Assert.Equal(
        { IsNegative = false
          ValueString = "1:30"
          Qualifier = IntervalQualifier.Range(Hour, Minute, None) },
        test pIntervalLiteral "INTERVAL '1:30' HOUR TO MINUTE"
    )

[<Fact>]
let ``Interval precision with invalid value shape is rejected`` () =
    // The precision clause is parsed, but the value string must still match the qualifier shape.
    testFails pIntervalLiteral "INTERVAL '1-2' YEAR(4)"
    testFails pIntervalLiteral "INTERVAL 'abc' SECOND(2,3)"
    testFails pIntervalLiteral "INTERVAL '1:30' HOUR TO SECOND(3)"

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
