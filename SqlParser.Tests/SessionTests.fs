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
let ``SET ROLE verification`` () =
    match parse "SET ROLE admin" with
    | SetRole(Some { Kind = Identifier "ADMIN" }) -> ()
    | res -> Assert.Fail(sprintf "Expected SetRole, got %A" res)

    match parse "SET ROLE NONE" with
    | SetRole None -> ()
    | res -> Assert.Fail(sprintf "Expected SetRole NONE, got %A" res)

[<Fact>]
let ``SET ROLE without role is rejected`` () = parseFails "SET ROLE"

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
    match parse "SET COLLATION 'c1' FOR 'cs1'" with
    | SetSessionCollation(Some { Kind = Literal(String "c1") }, Some [ { Kind = Literal(String "cs1") } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected SetSessionCollation FOR, got %A" res)
