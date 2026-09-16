module SqlParser.Tests.ConnectionTests

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
let ``CONNECT TO verification`` () =
    match parse "CONNECT TO server1" with
    | Connect { Server = Some { Kind = Identifier "SERVER1" }
                ConnectionName = None
                User = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Connect, got %A" res)

[<Fact>]
let ``CONNECT TO DEFAULT verification`` () =
    match parse "CONNECT TO DEFAULT" with
    | Connect { Server = None
                ConnectionName = None
                User = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Connect DEFAULT, got %A" res)

[<Fact>]
let ``CONNECT TO with AS and USER verification`` () =
    match parse "CONNECT TO server1 AS c1 USER u1" with
    | Connect { Server = Some { Kind = Identifier "SERVER1" }
                ConnectionName = Some { Kind = Identifier "C1" }
                User = Some { Kind = Identifier "U1" } } -> ()
    | res -> Assert.Fail(sprintf "Expected Connect with AS/USER, got %A" res)

[<Fact>]
let ``CONNECT TO without target is rejected`` () = parseFails "CONNECT TO"

[<Fact>]
let ``SET CONNECTION verification`` () =
    match parse "SET CONNECTION c1" with
    | SetConnection(Some { Kind = Identifier "C1" }) -> ()
    | res -> Assert.Fail(sprintf "Expected SetConnection, got %A" res)

[<Fact>]
let ``SET CONNECTION DEFAULT verification`` () =
    match parse "SET CONNECTION DEFAULT" with
    | SetConnection None -> ()
    | res -> Assert.Fail(sprintf "Expected SetConnection DEFAULT, got %A" res)

[<Fact>]
let ``DISCONNECT verification`` () =
    match parse "DISCONNECT c1" with
    | Disconnect(DisconnectName { Kind = Identifier "C1" }) -> ()
    | res -> Assert.Fail(sprintf "Expected Disconnect, got %A" res)

[<Fact>]
let ``DISCONNECT ALL verification`` () =
    match parse "DISCONNECT ALL" with
    | Disconnect DisconnectAll -> ()
    | res -> Assert.Fail(sprintf "Expected Disconnect ALL, got %A" res)

[<Fact>]
let ``DISCONNECT CURRENT verification`` () =
    match parse "DISCONNECT CURRENT" with
    | Disconnect DisconnectCurrent -> ()
    | res -> Assert.Fail(sprintf "Expected Disconnect CURRENT, got %A" res)

[<Fact>]
let ``DISCONNECT DEFAULT verification`` () =
    match parse "DISCONNECT DEFAULT" with
    | Disconnect DisconnectDefault -> ()
    | res -> Assert.Fail(sprintf "Expected Disconnect DEFAULT, got %A" res)

[<Fact>]
let ``DISCONNECT without object is rejected`` () = parseFails "DISCONNECT"

[<Fact>]
let ``CONNECT TO rejects expressions for server/name/user`` () =
    parseFails "CONNECT TO 'a' || 'b'"
    parseFails "CONNECT TO server AS a + b"
    parseFails "CONNECT TO server USER a = b"

[<Fact>]
let ``SET CONNECTION rejects expression`` () =
    parseFails "SET CONNECTION a + b"
    parseFails "SET CONNECTION x = y"

[<Fact>]
let ``DISCONNECT rejects expression`` () =
    parseFails "DISCONNECT a + b"
    parseFails "DISCONNECT x = y"
