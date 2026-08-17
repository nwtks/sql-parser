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
let ``Numeric literals are parsed correctly`` () =
    Assert.Equal(123m, test pNumericLiteral "123")
    Assert.Equal(123.45m, test pNumericLiteral "123.45")
    Assert.Equal(0.45m, test pNumericLiteral ".45")
    Assert.Equal(12300m, test pNumericLiteral "1.23E4")
    Assert.Equal(0.0123m, test pNumericLiteral "1.23E-2")

[<Fact>]
let ``String literals are parsed correctly`` () =
    Assert.Equal("hello", test pCharacterStringLiteral "'hello'")
    Assert.Equal("It's a trap", test pCharacterStringLiteral "'It''s a trap'")
    Assert.Equal("Multiline", test pCharacterStringLiteral "'Multi' 'line'")

[<Fact>]
let ``Binary literals are parsed correctly`` () =
    Assert.Equal<byte array>([| 0x01uy; 0xAFuy |], test pHexStringLiteral "X'01AF'")

[<Fact>]
let ``Hex literal with spaces between hexit pairs is parsed`` () =
    Assert.Equal<byte array>([| 0x01uy; 0xAFuy; 0x02uy |], test pHexStringLiteral "X'01 AF 02'")

[<Fact>]
let ``Invalid date values are rejected`` () =
    testFails pDateLiteral "DATE '2023-13-01'"
    testFails pDateLiteral "DATE '2023-01-40'"
    testFails pDateLiteral "DATE '2023--1-01'"

[<Fact>]
let ``Invalid interval values are rejected`` () =
    testFails pIntervalLiteral "INTERVAL 'abc' YEAR"
    testFails pIntervalLiteral "INTERVAL '1-2' YEAR"

    Assert.Equal(
        { IsNegative = false
          ValueString = "1"
          Qualifier = IntervalQualifier.SingleField Year },
        test pIntervalLiteral "INTERVAL '1' YEAR"
    )

    Assert.Equal(
        { IsNegative = false
          ValueString = "1:30"
          Qualifier = IntervalQualifier.Range(Hour, Minute) },
        test pIntervalLiteral "INTERVAL '1:30' HOUR TO MINUTE"
    )

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
let ``Interval literals are parsed correctly`` () =
    Assert.Equal(
        { IsNegative = false
          ValueString = "1-2"
          Qualifier = IntervalQualifier.Range(Year, Month) },
        test pIntervalLiteral "INTERVAL '1-2' YEAR TO MONTH"
    )

[<Fact>]
let ``Interval sign inside quotes is parsed`` () =
    Assert.Equal(
        { IsNegative = true
          ValueString = "1-2"
          Qualifier = IntervalQualifier.Range(Year, Month) },
        test pIntervalLiteral "INTERVAL '-1-2' YEAR TO MONTH"
    )

[<Fact>]
let ``Unicode escape sequences are decoded correctly`` () =
    Assert.Equal("A", test pUnicodeCharacterStringLiteral "U&'\\0041'")
    Assert.Equal("AB", test pUnicodeCharacterStringLiteral "U&'\\0041\\0042'")
    Assert.Equal("A", test pUnicodeDelimitedIdentifier "U&\"\\0041\"")

[<Fact>]
let ``Large exponent literals do not overflow`` () =
    let result = test pNumericLiteral "1E400"
    Assert.True(result > 0m)

[<Fact>]
let ``Invalid time seconds fail cleanly`` () =
    match run (pTimeLiteral .>> eof) "TIME '12:00:00.5.5'" with
    | Failure _ -> ()
    | Success _ -> Assert.Fail("Expected TIME literal with invalid seconds to fail")

[<Fact>]
let ``Boolean literals are parsed correctly`` () =
    Assert.Equal(Some true, test pBooleanLiteral "TRUE")
    Assert.Equal(Some false, test pBooleanLiteral "FALSE")
    Assert.Equal(None, test pBooleanLiteral "UNKNOWN")
