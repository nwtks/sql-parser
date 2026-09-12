module SqlParser.Tests.DiagnosticsTests

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
let ``GET DIAGNOSTICS statement information verification`` () =
    match parse "GET DIAGNOSTICS x = NUMBER, y = ROW_COUNT" with
    | GetDiagnostics(StatementInfo [ ({ Kind = Identifier "X" }, "NUMBER"); ({ Kind = Identifier "Y" }, "ROW_COUNT") ]) ->
        ()
    | res -> Assert.Fail(sprintf "Expected GetDiagnostics statement info, got %A" res)

[<Fact>]
let ``GET DIAGNOSTICS condition information verification`` () =
    match parse "GET DIAGNOSTICS CONDITION 1 x = MESSAGE_TEXT" with
    | GetDiagnostics(ConditionInfo({ Kind = Literal(Number 1m) }, [ ({ Kind = Identifier "X" }, "MESSAGE_TEXT") ])) ->
        ()
    | res -> Assert.Fail(sprintf "Expected GetDiagnostics condition info, got %A" res)

[<Fact>]
let ``GET DIAGNOSTICS all information verification`` () =
    match parse "GET DIAGNOSTICS x = ALL" with
    | GetDiagnostics(AllInfo({ Kind = Identifier "X" }, None)) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDiagnostics all info, got %A" res)

[<Fact>]
let ``GET DIAGNOSTICS all condition verification`` () =
    match parse "GET DIAGNOSTICS x = ALL CONDITION 1" with
    | GetDiagnostics(AllInfo({ Kind = Identifier "X" }, Some(AllCondition(Some { Kind = Literal(Number 1m) })))) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDiagnostics all condition, got %A" res)

[<Fact>]
let ``GET DIAGNOSTICS all statement verification`` () =
    match parse "GET DIAGNOSTICS x = ALL STATEMENT" with
    | GetDiagnostics(AllInfo({ Kind = Identifier "X" }, Some AllStatement)) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDiagnostics all statement, got %A" res)

[<Fact>]
let ``GET DIAGNOSTICS without information is rejected`` () = parseFails "GET DIAGNOSTICS"
