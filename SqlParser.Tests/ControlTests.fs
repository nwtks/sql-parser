module SqlParser.Tests.ControlTests

open Xunit
open SqlParser

// CALL / RETURN are <SQL procedure statement>s (13.4), not directly executable (22.1),
// so these use the general entry point.
let parseStatement (sql: string) =
    match SqlParser.parseStatement (sql.TrimEnd() + ";") with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseStatementFails (sql: string) =
    match SqlParser.parseStatement (sql.TrimEnd() + ";") with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

[<Fact>]
let ``CALL statement verification`` () =
    match parseStatement "CALL cleanup_logs()" with
    | Call({ Kind = Identifier "CLEANUP_LOGS" }, []) -> ()
    | res -> Assert.Fail(sprintf "Expected Call, got %A" res)

    match parseStatement "CALL app.prune(30, 'days')" with
    | Call({ Kind = ColumnReference [ "APP"; "PRUNE" ] },
           [ { Kind = Literal(Number 30m) }; { Kind = Literal(String "days") } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Call with args, got %A" res)

    // 6.5 <contextually typed value specification> — NULL is a legal <SQL argument>.
    match parseStatement "CALL write_log(NULL)" with
    | Call({ Kind = Identifier "WRITE_LOG" }, [ { Kind = Literal Null } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Call with NULL argument, got %A" res)

[<Fact>]
let ``CALL without routine is rejected`` () = parseStatementFails "CALL"

[<Fact>]
let ``RETURN statement verification`` () =
    match parseStatement "RETURN 42" with
    | Return { Kind = Literal(Number 42m) } -> ()
    | res -> Assert.Fail(sprintf "Expected Return, got %A" res)

    match parseStatement "RETURN NULL" with
    | Return { Kind = Literal Null } -> ()
    | res -> Assert.Fail(sprintf "Expected Return NULL, got %A" res)

    match parseStatement "RETURN 'hello'" with
    | Return { Kind = Literal(String "hello") } -> ()
    | res -> Assert.Fail(sprintf "Expected Return string, got %A" res)

[<Fact>]
let ``RETURN without value is rejected`` () = parseStatementFails "RETURN"
