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
let ``Unsigned integer and int conversions are checked`` () =
    Assert.Equal(123UL, test pUnsignedInteger "123")
    Assert.Equal(123, test pUnsignedIntegerAsInt "123")
    testFails pUnsignedInteger "99999999999999999999999999"
    testFails pUnsignedIntegerAsInt "99999999999"

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
