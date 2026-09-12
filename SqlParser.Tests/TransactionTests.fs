module SqlParser.Tests.TransactionTests

open Xunit
open SqlParser

// 22.1 <direct SQL statement> requires a trailing <semicolon>.
let parse (sql: string) =
    match SqlParser.parse (sql.TrimEnd() + ";") with
    | Ok { Kind = res } -> res
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``START TRANSACTION verification`` () =
    match parse "START TRANSACTION" with
    | StartTransaction [] -> ()
    | res -> Assert.Fail(sprintf "Expected StartTransaction, got %A" res)

    match parse "START TRANSACTION ISOLATION LEVEL SERIALIZABLE, READ ONLY" with
    | StartTransaction [ Isolation Serializable; AccessMode ReadOnly ] -> ()
    | res -> Assert.Fail(sprintf "Expected StartTransaction with modes, got %A" res)

[<Fact>]
let ``SET TRANSACTION verification`` () =
    match parse "SET TRANSACTION READ WRITE" with
    | SetTransaction(false, [ AccessMode ReadWrite ]) -> ()
    | res -> Assert.Fail(sprintf "Expected SetTransaction, got %A" res)

    match parse "SET LOCAL TRANSACTION ISOLATION LEVEL READ COMMITTED" with
    | SetTransaction(true, [ Isolation ReadCommitted ]) -> ()
    | res -> Assert.Fail(sprintf "Expected SetTransaction LOCAL, got %A" res)

[<Fact>]
let ``DIAGNOSTICS SIZE transaction mode verification`` () =
    match parse "START TRANSACTION DIAGNOSTICS SIZE 5" with
    | StartTransaction [ DiagnosticsSize { Kind = Literal(Number 5m) } ] -> ()
    | res -> Assert.Fail(sprintf "Expected StartTransaction DIAGNOSTICS SIZE, got %A" res)

    match parse "SET TRANSACTION DIAGNOSTICS SIZE 10" with
    | SetTransaction(false, [ DiagnosticsSize { Kind = Literal(Number 10m) } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected SetTransaction DIAGNOSTICS SIZE, got %A" res)

    match parse "START TRANSACTION ISOLATION LEVEL READ COMMITTED, DIAGNOSTICS SIZE 5" with
    | StartTransaction [ Isolation ReadCommitted; DiagnosticsSize { Kind = Literal(Number 5m) } ] -> ()
    | res -> Assert.Fail(sprintf "Expected StartTransaction with DIAGNOSTICS SIZE combined, got %A" res)

    match parse "SET SESSION CHARACTERISTICS AS TRANSACTION DIAGNOSTICS SIZE 5" with
    | SetSessionCharacteristics [ DiagnosticsSize { Kind = Literal(Number 5m) } ] -> ()
    | res -> Assert.Fail(sprintf "Expected SetSessionCharacteristics DIAGNOSTICS SIZE, got %A" res)

[<Fact>]
let ``SET CONSTRAINTS verification`` () =
    match parse "SET CONSTRAINTS ALL DEFERRED" with
    | SetConstraints(None, true) -> ()
    | res -> Assert.Fail(sprintf "Expected SetConstraints ALL DEFERRED, got %A" res)

    match parse "SET CONSTRAINTS fk1, fk2 IMMEDIATE" with
    | SetConstraints(Some([ { Kind = Identifier "FK1" }; { Kind = Identifier "FK2" } ]), false) -> ()
    | res -> Assert.Fail(sprintf "Expected SetConstraints names, got %A" res)

[<Fact>]
let ``SAVEPOINT verification`` () =
    match parse "SAVEPOINT sp1" with
    | Savepoint { Kind = Identifier "SP1" } -> ()
    | res -> Assert.Fail(sprintf "Expected Savepoint, got %A" res)

[<Fact>]
let ``RELEASE SAVEPOINT verification`` () =
    match parse "RELEASE SAVEPOINT sp1" with
    | ReleaseSavepoint { Kind = Identifier "SP1" } -> ()
    | res -> Assert.Fail(sprintf "Expected ReleaseSavepoint, got %A" res)

[<Fact>]
let ``COMMIT verification`` () =
    match parse "COMMIT" with
    | Commit None -> ()
    | res -> Assert.Fail(sprintf "Expected Commit, got %A" res)

    match parse "COMMIT WORK AND CHAIN" with
    | Commit(Some true) -> ()
    | res -> Assert.Fail(sprintf "Expected Commit AND CHAIN, got %A" res)

    match parse "COMMIT AND NO CHAIN" with
    | Commit(Some false) -> ()
    | res -> Assert.Fail(sprintf "Expected Commit AND NO CHAIN, got %A" res)

[<Fact>]
let ``ROLLBACK verification`` () =
    match parse "ROLLBACK" with
    | Rollback(None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Rollback, got %A" res)

    match parse "ROLLBACK WORK TO SAVEPOINT sp1" with
    | Rollback(None, Some({ Kind = Identifier "SP1" })) -> ()
    | res -> Assert.Fail(sprintf "Expected Rollback to savepoint, got %A" res)
