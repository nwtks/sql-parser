module SqlParser.Tests.SqlParserTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
let parse (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok { Kind = res } -> res
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``Full Statement parsing verification`` () =
    match parse "WITH cte AS (SELECT * FROM users) SELECT * FROM cte" with
    | WithStatement(false, [ { Name = { Kind = Identifier "CTE" } } ], Select _) -> ()
    | res -> Assert.Fail(sprintf "Expected WithStatement, got %A" res)

[<Fact>]
let ``Multiple statements parsing verification`` () =
    match parse "SELECT * FROM users" with
    | Select _ -> ()
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Direct SQL statement verification`` () =
    match SqlParser.parse "SELECT 1 FROM t;" with
    | Ok { Kind = Select _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Direct SQL statement with a schema statement verification`` () =
    match SqlParser.parse "CREATE TABLE t (a INT);" with
    | Ok { Kind = CreateTable _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable, got %A" res)

[<Fact>]
let ``Direct SQL statement with trailing whitespace verification`` () =
    match SqlParser.parse "SELECT 1 FROM t;  " with
    | Ok { Kind = Select _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Select, got %A" res)

[<Fact>]
let ``Comments are separators anywhere a separator is allowed (5.2)`` () =
    // A <comment> is a <separator>, so it may separate any two tokens — including a keyword
    // from a literal, a literal from a keyword, an identifier from punctuation and a comment
    // from the statement's semicolon (docs/trade-off.md).
    for sql in
        [ "SELECT/*c*/1 FROM t;"
          "SELECT 1/*c*/FROM t;"
          "SELECT 1 FROM/*c*/t;"
          "SELECT 1 FROM t/*c*/;"
          "SELECT/**/1/**/FROM/**/t;"
          "SELECT -- c\n1 FROM t;"
          "SELECT 1 -- c\nFROM t;"
          "-- leading\nSELECT 1 FROM t;"
          "/* leading */ SELECT 1 FROM t;"
          "SELECT/* multi\nline */1 FROM t;"
          "SELECT 1 FROM t; -- trailing comment" ] do
        match SqlParser.parse sql with
        | Ok { Kind = Select _ } -> ()
        | res -> Assert.Fail(sprintf "Expected Select for %A, got %A" sql res)

[<Fact>]
let ``An unterminated bracketed comment is rejected (5.2)`` () =
    match SqlParser.parse "SELECT 1 /* unterminated FROM t;" with
    | Ok _ -> Assert.Fail "Expected the unterminated comment to be rejected"
    | Error _ -> ()

[<Fact>]
let ``Direct SQL statement accepts the direct SQL data families`` () =
    match SqlParser.parse "INSERT INTO t VALUES (1);" with
    | Ok { Kind = Insert _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Insert, got %A" res)

    match SqlParser.parse "UPDATE t SET a = 1;" with
    | Ok { Kind = Update _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Update, got %A" res)

    match SqlParser.parse "DELETE FROM t WHERE a = 1;" with
    | Ok { Kind = Delete _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Delete, got %A" res)

    match SqlParser.parse "MERGE INTO t USING s ON t.a = s.a WHEN MATCHED THEN DELETE;" with
    | Ok { Kind = Merge _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Merge, got %A" res)

    match SqlParser.parse "TRUNCATE TABLE t CONTINUE IDENTITY;" with
    | Ok { Kind = Truncate _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Truncate, got %A" res)

    // 14.16 <temporary table declaration> is part of <direct SQL data statement>.
    match SqlParser.parse "DECLARE LOCAL TEMPORARY TABLE tt (a INT);" with
    | Ok { Kind = DeclareTemporaryTable _ } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareTemporaryTable, got %A" res)

    match SqlParser.parse "WITH cte AS (SELECT 1 FROM t) SELECT * FROM cte;" with
    | Ok { Kind = WithStatement _ } -> ()
    | res -> Assert.Fail(sprintf "Expected WithStatement, got %A" res)

[<Fact>]
let ``Direct SQL statement without semicolon is rejected`` () =
    match SqlParser.parse "SELECT 1" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a missing semicolon to be rejected, got %A" res)

[<Fact>]
let ``Direct SQL statement with two semicolons is rejected`` () =
    match SqlParser.parse "SELECT 1;;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a second semicolon to be rejected, got %A" res)

[<Fact>]
let ``Direct SQL statement with only a semicolon is rejected`` () =
    match SqlParser.parse ";" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a bare semicolon to be rejected, got %A" res)

[<Fact>]
let ``Direct SQL statement rejects a positioned UPDATE / DELETE (22.1)`` () =
    match SqlParser.parse "UPDATE t SET a = 1 WHERE CURRENT OF c;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a positioned UPDATE to be rejected, got %A" res)

    match SqlParser.parse "DELETE FROM t WHERE CURRENT OF c;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a positioned DELETE to be rejected, got %A" res)

[<Fact>]
let ``Direct SQL statement rejects the SQL procedure families (22.1)`` () =
    match SqlParser.parse "DECLARE c CURSOR FOR SELECT a FROM t;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected DECLARE CURSOR to be rejected, got %A" res)

    match SqlParser.parse "OPEN c;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected OPEN to be rejected, got %A" res)

    match SqlParser.parse "FETCH c INTO x;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected FETCH to be rejected, got %A" res)

    match SqlParser.parse "CLOSE c;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected CLOSE to be rejected, got %A" res)

    match SqlParser.parse "CALL p();" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected CALL to be rejected, got %A" res)

    match SqlParser.parse "GET DIAGNOSTICS x = NUMBER;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected GET DIAGNOSTICS to be rejected, got %A" res)

    match SqlParser.parse "PREPARE s FROM 'SELECT 1';" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected PREPARE to be rejected, got %A" res)

    match SqlParser.parse "FREE LOCATOR l;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected FREE LOCATOR to be rejected, got %A" res)

[<Fact>]
let ``A WITH body that is not a query is rejected (7.17)`` () =
    match SqlParser.parse "WITH cte AS (SELECT 1) UPDATE t SET a = 1;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected WITH + UPDATE to be rejected, got %A" res)

    // <with clause> is a prefix of <query expression>, so this is invalid on every
    // entry point — not merely a 22.1 exclusion.
    match SqlParser.parseStatement "WITH cte AS (SELECT 1) INSERT INTO t VALUES (1);" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected WITH + INSERT to be rejected, got %A" res)

    match SqlParser.parseStatement "WITH cte AS (SELECT 1) DELETE FROM t;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected WITH + DELETE to be rejected, got %A" res)

[<Fact>]
let ``General statement entry point rejects the direct SQL query forms (13.4)`` () =
    // A multi-row SELECT (22.2) and a WITH-prefixed query are not <SQL procedure statement>s;
    // 13.4 admits only <select statement: single row> (SELECT ... INTO).
    match SqlParser.parseStatement "SELECT a FROM t;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a multi-row SELECT to be rejected, got %A" res)

    match SqlParser.parseStatement "WITH cte AS (SELECT 1 FROM t) SELECT * FROM cte;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a WITH query to be rejected, got %A" res)

[<Fact>]
let ``General statement entry point rejects the non-13.4 statement families`` () =
    // 14.1 <declare cursor> (SQL-client module) and 14.16 <temporary table declaration>
    // (22.1 only) are not <SQL procedure statement>s.
    match SqlParser.parseStatement "DECLARE c CURSOR FOR SELECT a FROM t;" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected DECLARE CURSOR to be rejected, got %A" res)

    match SqlParser.parseStatement "DECLARE LOCAL TEMPORARY TABLE tt (a INT);" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a temporary table declaration to be rejected, got %A" res)

[<Fact>]
let ``General statement entry point accepts the SQL procedure families (13.4)`` () =
    match SqlParser.parseStatement "OPEN c;" with
    | Ok { Kind = Open _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Open, got %A" res)

    match SqlParser.parseStatement "CALL p();" with
    | Ok { Kind = Call _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Call, got %A" res)

    match SqlParser.parseStatement "GET DIAGNOSTICS x = NUMBER;" with
    | Ok { Kind = GetDiagnostics _ } -> ()
    | res -> Assert.Fail(sprintf "Expected GetDiagnostics, got %A" res)

    match SqlParser.parseStatement "PREPARE s FROM 'SELECT 1';" with
    | Ok { Kind = Prepare _ } -> ()
    | res -> Assert.Fail(sprintf "Expected Prepare, got %A" res)

    match SqlParser.parseStatement "UPDATE t SET a = 1 WHERE CURRENT OF c;" with
    | Ok { Kind = Update { Cursor = Some _ } } -> ()
    | res -> Assert.Fail(sprintf "Expected a positioned Update, got %A" res)

[<Fact>]
let ``General statement entry point requires the semicolon`` () =
    match SqlParser.parseStatement "OPEN c" with
    | Error _ -> ()
    | Ok res -> Assert.Fail(sprintf "Expected a missing semicolon to be rejected, got %A" res)
