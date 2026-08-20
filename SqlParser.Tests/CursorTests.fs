module SqlParser.Tests.CursorTests

open Xunit
open SqlParser

let parse sql =
    match SqlParser.parse sql with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseFails sql =
    match SqlParser.parse sql with
    | Ok _ -> failwithf "Expected parse failure for %s" sql
    | Error _ -> ()

[<Fact>]
let ``OPEN verification`` () =
    match parse "OPEN cur" with
    | Open { Kind = Identifier "CUR" } -> ()
    | res -> Assert.Fail(sprintf "Expected Open, got %A" res)

[<Fact>]
let ``OPEN without cursor name is rejected`` () = parseFails "OPEN"

[<Fact>]
let ``FETCH verification`` () =
    match parse "FETCH cur INTO a, b" with
    | Fetch(None, { Kind = Identifier "CUR" }, [ { Kind = Identifier "A" }; { Kind = Identifier "B" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Fetch, got %A" res)

[<Fact>]
let ``FETCH NEXT FROM verification`` () =
    match parse "FETCH NEXT FROM cur INTO a" with
    | Fetch(Some Next, { Kind = Identifier "CUR" }, [ { Kind = Identifier "A" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Fetch NEXT FROM, got %A" res)

[<Fact>]
let ``FETCH FROM verification`` () =
    match parse "FETCH FROM cur INTO a" with
    | Fetch(None, { Kind = Identifier "CUR" }, [ { Kind = Identifier "A" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Fetch FROM, got %A" res)

[<Fact>]
let ``FETCH ABSOLUTE verification`` () =
    match parse "FETCH ABSOLUTE 5 FROM cur INTO a" with
    | Fetch(Some(Absolute { Kind = Literal(Number 5m) }), { Kind = Identifier "CUR" }, [ { Kind = Identifier "A" } ]) ->
        ()
    | res -> Assert.Fail(sprintf "Expected Fetch ABSOLUTE, got %A" res)

[<Fact>]
let ``FETCH RELATIVE verification`` () =
    match parse "FETCH RELATIVE -1 FROM cur INTO a" with
    | Fetch(Some(Relative { Kind = UnaryOp(Minus, { Kind = Literal(Number 1m) }) }),
            { Kind = Identifier "CUR" },
            [ { Kind = Identifier "A" } ]) -> ()
    | res -> Assert.Fail(sprintf "Expected Fetch RELATIVE, got %A" res)

[<Fact>]
let ``FETCH without INTO is rejected`` () = parseFails "FETCH cur"

[<Fact>]
let ``CLOSE verification`` () =
    match parse "CLOSE cur" with
    | Close { Kind = Identifier "CUR" } -> ()
    | res -> Assert.Fail(sprintf "Expected Close, got %A" res)

[<Fact>]
let ``CLOSE without cursor name is rejected`` () = parseFails "CLOSE"

[<Fact>]
let ``SELECT INTO verification`` () =
    match parse "SELECT a, b INTO x, y FROM t WHERE id = 1" with
    | SelectInto { IsDistinct = false
                   Columns = [ Column({ Kind = Identifier "A" }, None); Column({ Kind = Identifier "B" }, None) ]
                   Into = [ { Kind = Identifier "X" }; { Kind = Identifier "Y" } ]
                   From = [ { Kind = Table({ Kind = Identifier "T" }, None) } ]
                   Where = Some _ } -> ()
    | res -> Assert.Fail(sprintf "Expected SelectInto, got %A" res)

[<Fact>]
let ``SELECT DISTINCT INTO verification`` () =
    match parse "SELECT DISTINCT a INTO x FROM t" with
    | SelectInto { IsDistinct = true
                   Columns = [ Column({ Kind = Identifier "A" }, None) ]
                   Into = [ { Kind = Identifier "X" } ]
                   From = [ { Kind = Table({ Kind = Identifier "T" }, None) } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected SelectInto DISTINCT, got %A" res)

[<Fact>]
let ``SELECT INTO without select list is rejected`` () = parseFails "SELECT INTO x"
