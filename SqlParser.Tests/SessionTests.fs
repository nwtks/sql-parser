module SqlParser.Tests.SessionTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
let parse (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseFails (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

[<Fact>]
let ``SET SESSION CHARACTERISTICS verification`` () =
    match parse "SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL SERIALIZABLE, READ ONLY" with
    | SetSessionCharacteristics [ Isolation Serializable; AccessMode ReadOnly ] -> ()
    | res -> Assert.Fail(sprintf "Expected SetSessionCharacteristics, got %A" res)

[<Fact>]
let ``SET SESSION AUTHORIZATION verification`` () =
    match parse "SET SESSION AUTHORIZATION 'alice'" with
    | SetSessionAuthorization { Kind = Literal(String "alice") } -> ()
    | res -> Assert.Fail(sprintf "Expected SetSessionAuthorization, got %A" res)

[<Fact>]
let ``SET ROLE verification`` () =
    match parse "SET ROLE 'admin'" with
    | SetRole(Some { Kind = Literal(String "admin") }) -> ()
    | res -> Assert.Fail(sprintf "Expected SetRole, got %A" res)

    match parse "SET ROLE NONE" with
    | SetRole None -> ()
    | res -> Assert.Fail(sprintf "Expected SetRole NONE, got %A" res)

[<Fact>]
let ``SET ROLE without role is rejected`` () = parseFails "SET ROLE"

[<Fact>]
let ``SET TIME ZONE LOCAL verification`` () =
    match parse "SET TIME ZONE LOCAL" with
    | SetTimeZone None -> ()
    | res -> Assert.Fail(sprintf "Expected SetTimeZone LOCAL, got %A" res)

[<Fact>]
let ``SET TIME ZONE interval verification`` () =
    match parse "SET TIME ZONE INTERVAL '1' HOUR" with
    | SetTimeZone(Some _) -> ()
    | res -> Assert.Fail(sprintf "Expected SetTimeZone interval, got %A" res)

[<Fact>]
let ``SET TIME ZONE datetime difference interval verification`` () =
    // 6.37 4th alternative: ( <datetime value expression> - <datetime term> ) <interval qualifier>
    match parse "SET TIME ZONE (ts1 - ts2) DAY" with
    | SetTimeZone(Some { Kind = DatetimeDifference(_, _, _) }) -> ()
    | res -> Assert.Fail(sprintf "Expected DatetimeDifference, got %A" res)

[<Fact>]
let ``SET TIME ZONE interval chain verification`` () =
    // 6.37 2nd/3rd alternatives, left-folded
    match parse "SET TIME ZONE INTERVAL '1' DAY + INTERVAL '2' HOUR - INTERVAL '3' MINUTE" with
    | SetTimeZone(Some { Kind = BinaryOp(Subtract, { Kind = BinaryOp(Add, _, _) }, { Kind = Literal(Interval _) }) }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected a left-folded interval chain, got %A" res)

[<Fact>]
let ``SET TIME ZONE interval term with numeric factor verification`` () =
    match parse "SET TIME ZONE INTERVAL '1' DAY * 2" with
    | SetTimeZone(Some { Kind = BinaryOp(Multiply, { Kind = Literal(Interval _) }, { Kind = Literal(Number 2m) }) }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected an interval term with a numeric factor, got %A" res)

[<Fact>]
let ``SET TIME ZONE rejects comparison and boolean expressions 19.4 verification`` () =
    // The dedicated 6.37 parser stops before comparison/boolean operators, so the
    // remaining input cannot be consumed by the statement and the parse fails.
    parseFails "SET TIME ZONE x = y"
    parseFails "SET TIME ZONE a OR b"

[<Fact>]
let ``SET TIME ZONE rejects predicates and star 6.37 verification`` () =
    // pIntervalPrimary uses pValueExpressionPrimary: §8 predicate atoms and
    // the 7.16 '*' wildcard are not <value expression primary>s.
    parseFails "SET TIME ZONE EXISTS (SELECT 1)"
    parseFails "SET TIME ZONE *"

[<Fact>]
let ``SET CATALOG/SCHEMA/NAMES/PATH reject expressions`` () =
    parseFails "SET CATALOG 1 + 2"
    parseFails "SET SCHEMA a || b"
    parseFails "SET NAMES x = y"
    parseFails "SET PATH a AND b"
    parseFails "SET CATALOG EXISTS (SELECT 1)"

[<Fact>]
let ``SET TRANSFORM GROUP rejects expression for group value`` () =
    parseFails "SET DEFAULT TRANSFORM GROUP 1 + 2"
    parseFails "SET TRANSFORM GROUP FOR TYPE t a || b"

[<Fact>]
let ``SET COLLATION rejects expression for collation`` () =
    parseFails "SET COLLATION a + b"
    parseFails "SET COLLATION x = y"

[<Fact>]
let ``SET CATALOG verification`` () =
    match parse "SET CATALOG 'c1'" with
    | SetCatalog { Kind = Literal(String "c1") } -> ()
    | res -> Assert.Fail(sprintf "Expected SetCatalog, got %A" res)

[<Fact>]
let ``SET SCHEMA verification`` () =
    match parse "SET SCHEMA 's1'" with
    | SetSchema { Kind = Literal(String "s1") } -> ()
    | res -> Assert.Fail(sprintf "Expected SetSchema, got %A" res)

[<Fact>]
let ``SET NAMES verification`` () =
    match parse "SET NAMES 'UTF8'" with
    | SetNames { Kind = Literal(String "UTF8") } -> ()
    | res -> Assert.Fail(sprintf "Expected SetNames, got %A" res)

[<Fact>]
let ``SET PATH verification`` () =
    match parse "SET PATH 'p1'" with
    | SetPath { Kind = Literal(String "p1") } -> ()
    | res -> Assert.Fail(sprintf "Expected SetPath, got %A" res)

[<Fact>]
let ``SET DEFAULT TRANSFORM GROUP verification`` () =
    match parse "SET DEFAULT TRANSFORM GROUP 'g1'" with
    | SetTransformGroup({ Kind = Literal(String "g1") }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected SetTransformGroup, got %A" res)

[<Fact>]
let ``SET TRANSFORM GROUP FOR TYPE verification`` () =
    match parse "SET TRANSFORM GROUP FOR TYPE t 'g1'" with
    | SetTransformGroup({ Kind = Literal(String "g1") }, Some { Kind = Identifier "T" }) -> ()
    | res -> Assert.Fail(sprintf "Expected SetTransformGroup FOR TYPE, got %A" res)

[<Fact>]
let ``SET COLLATION verification`` () =
    match parse "SET COLLATION 'c1'" with
    | SetSessionCollation(Some { Kind = Literal(String "c1") }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected SetSessionCollation, got %A" res)

[<Fact>]
let ``SET NO COLLATION verification`` () =
    match parse "SET NO COLLATION" with
    | SetSessionCollation(None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected SetSessionCollation NO, got %A" res)

[<Fact>]
let ``SET COLLATION FOR verification`` () =
    match parse "SET COLLATION 'c1' FOR cs1" with
    | SetSessionCollation(Some { Kind = Literal(String "c1") }, Some [ "CS1" ]) -> ()
    | res -> Assert.Fail(sprintf "Expected SetSessionCollation FOR, got %A" res)

    // 19.10 <character set specification list> — a comma-separated list of
    // <character set specification>s (10.5), each optionally schema-qualified.
    match parse "SET NO COLLATION FOR cs1, s2.cs2" with
    | SetSessionCollation(None, Some [ "CS1"; "S2.CS2" ]) -> ()
    | res -> Assert.Fail(sprintf "Expected charset list, got %A" res)

    // A delimited identifier is not an <SQL language identifier>, so it is rejected here.
    parseFails "SET COLLATION 'c1' FOR \"cs1\""
