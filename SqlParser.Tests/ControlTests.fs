module SqlParser.Tests.ControlTests

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
let ``CALL statement verification`` () =
    match parse "CALL cleanup_logs()" with
    | Call({ Kind = Identifier "CLEANUP_LOGS" }, []) -> ()
    | res -> Assert.Fail(sprintf "Expected Call, got %A" res)

    match parse "CALL app.prune(30, 'days')" with
    | Call({ Kind = ColumnReference [ "APP"; "PRUNE" ] },
           [ { Kind = Literal(Number 30m) }; { Kind = Literal(String "days") } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Call with args, got %A" res)

[<Fact>]
let ``CALL without routine is rejected`` () = parseFails "CALL"

[<Fact>]
let ``RETURN statement verification`` () =
    match parse "RETURN 42" with
    | Return { Kind = Literal(Number 42m) } -> ()
    | res -> Assert.Fail(sprintf "Expected Return, got %A" res)

    match parse "RETURN NULL" with
    | Return { Kind = Literal Null } -> ()
    | res -> Assert.Fail(sprintf "Expected Return NULL, got %A" res)

    match parse "RETURN 'hello'" with
    | Return { Kind = Literal(String "hello") } -> ()
    | res -> Assert.Fail(sprintf "Expected Return string, got %A" res)

[<Fact>]
let ``RETURN without value is rejected`` () = parseFails "RETURN"
