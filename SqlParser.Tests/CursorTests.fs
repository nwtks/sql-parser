module SqlParser.Tests.CursorTests

open Xunit
open FParsec
open SqlParser

let parse sql =
    match SqlParser.parse sql with
    | Result.Ok res -> res.Kind
    | Result.Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseFails sql =
    match SqlParser.parse sql with
    | Result.Ok _ -> failwithf "Expected parse failure for %s" sql
    | Result.Error _ -> ()

[<Theory>]
[<InlineData("SENSITIVE", "Sensitive")>]
[<InlineData("INSENSITIVE", "Insensitive")>]
[<InlineData("ASENSITIVE", "Asensitive")>]
let ``DECLARE CURSOR sensitivity verification`` (keyword: string) (expected: string) =
    match parse (sprintf "DECLARE cur %s CURSOR FOR SELECT a FROM t" keyword) with
    | DeclareCursor { Properties = { Sensitivity = Some actual } } -> Assert.Equal(expected, sprintf "%A" actual)
    | res -> Assert.Fail(sprintf "Expected %s sensitivity, got %A" keyword res)

[<Theory>]
[<InlineData("SCROLL", "Scroll")>]
[<InlineData("NO SCROLL", "NoScroll")>]
let ``DECLARE CURSOR scrollability verification`` (keyword: string) (expected: string) =
    match parse (sprintf "DECLARE cur %s CURSOR FOR SELECT a FROM t" keyword) with
    | DeclareCursor { Properties = { Scrollability = Some actual } } -> Assert.Equal(expected, sprintf "%A" actual)
    | res -> Assert.Fail(sprintf "Expected %s scrollability, got %A" keyword res)

[<Theory>]
[<InlineData("WITH HOLD", "WithHold")>]
[<InlineData("WITHOUT HOLD", "WithoutHold")>]
let ``DECLARE CURSOR holdability verification`` (keyword: string) (expected: string) =
    match parse (sprintf "DECLARE cur CURSOR %s FOR SELECT a FROM t" keyword) with
    | DeclareCursor { Properties = { Holdability = Some actual } } -> Assert.Equal(expected, sprintf "%A" actual)
    | res -> Assert.Fail(sprintf "Expected %s holdability, got %A" keyword res)

[<Theory>]
[<InlineData("WITH RETURN", "WithReturn")>]
[<InlineData("WITHOUT RETURN", "WithoutReturn")>]
let ``DECLARE CURSOR returnability verification`` (keyword: string) (expected: string) =
    match parse (sprintf "DECLARE cur CURSOR %s FOR SELECT a FROM t" keyword) with
    | DeclareCursor { Properties = { Returnability = Some actual } } -> Assert.Equal(expected, sprintf "%A" actual)
    | res -> Assert.Fail(sprintf "Expected %s returnability, got %A" keyword res)

[<Fact>]
let ``CURSOR ATTRIBUTES verification`` () =
    match run (CursorParser.pCursorAttributes .>> eof) "SENSITIVE NO SCROLL WITH HOLD WITHOUT RETURN" with
    | Success(attrs, _, _) ->
        Assert.Equal<CursorAttribute list>(
            [ CursorAttribute.SensitivityAttribute Sensitive
              CursorAttribute.ScrollabilityAttribute NoScroll
              CursorAttribute.HoldabilityAttribute WithHold
              CursorAttribute.ReturnabilityAttribute WithoutReturn ],
            attrs
        )
    | Failure(msg, _, _) -> Assert.Fail(msg)

[<Fact>]
let ``CURSOR ATTRIBUTES rejects a non-attribute`` () =
    match run (CursorParser.pCursorAttributes .>> eof) "SENSITIVE UPDATE" with
    | Success _ -> Assert.Fail("Expected CURSOR ATTRIBUTES to reject UPDATE")
    | Failure _ -> ()

[<Fact>]
let ``DECLARE CURSOR verification`` () =
    match parse "DECLARE cur CURSOR FOR SELECT a FROM t" with
    | DeclareCursor { Name = { Kind = Identifier "CUR" }
                      Properties = { Sensitivity = None
                                     Scrollability = None
                                     Holdability = None
                                     Returnability = None }
                      Specification = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareCursor, got %A" res)

[<Fact>]
let ``DECLARE CURSOR with all cursor properties verification`` () =
    match parse "DECLARE cur INSENSITIVE NO SCROLL CURSOR WITH HOLD WITH RETURN FOR SELECT a FROM t" with
    | DeclareCursor { Properties = { Sensitivity = Some Insensitive
                                     Scrollability = Some NoScroll
                                     Holdability = Some WithHold
                                     Returnability = Some WithReturn } } -> ()
    | res -> Assert.Fail(sprintf "Expected all cursor properties, got %A" res)

[<Fact>]
let ``DECLARE CURSOR with updatability clause verification`` () =
    match parse "DECLARE cur CURSOR FOR SELECT a FROM t FOR UPDATE OF a" with
    | DeclareCursor { Specification = SelectQuery s } ->
        match s.Locking with
        | Some(ForUpdate(Some [ { Kind = Identifier "A" } ])) -> ()
        | other -> Assert.Fail(sprintf "Expected FOR UPDATE OF a, got %A" other)
    | res -> Assert.Fail(sprintf "Expected DeclareCursor, got %A" res)

[<Fact>]
let ``DECLARE CURSOR without CURSOR keyword is rejected`` () =
    parseFails "DECLARE cur FOR SELECT a FROM t"

[<Fact>]
let ``DECLARE CURSOR without FOR is rejected`` () = parseFails "DECLARE cur CURSOR"

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
let ``SELECT INTO GROUP BY verification`` () =
    match parse "SELECT a INTO x FROM t GROUP BY a" with
    | SelectInto { Columns = [ _ ]
                   GroupBy = [ GroupingSet [ { Kind = Identifier "A" } ] ]
                   GroupByDistinct = false } -> ()
    | res -> Assert.Fail(sprintf "Expected SelectInto GROUP BY, got %A" res)

[<Fact>]
let ``SELECT INTO GROUP BY DISTINCT verification`` () =
    match parse "SELECT a INTO x FROM t GROUP BY DISTINCT a" with
    | SelectInto { GroupBy = [ GroupingSet [ _ ] ]
                   GroupByDistinct = true } -> ()
    | res -> Assert.Fail(sprintf "Expected SelectInto GROUP BY DISTINCT, got %A" res)

[<Fact>]
let ``SELECT INTO without select list is rejected`` () = parseFails "SELECT INTO x"

[<Fact>]
let ``Temporary table declaration verification`` () =
    match parse "DECLARE LOCAL TEMPORARY TABLE t (a INT, b VARCHAR(10)) ON COMMIT PRESERVE ROWS" with
    | DeclareTemporaryTable { Name = { Kind = Identifier "T" }
                              Columns = [ { Name = { Kind = Identifier "A" } }; { Name = { Kind = Identifier "B" } } ]
                              Constraints = []
                              OnCommit = Some PreserveOnCommit } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareTemporaryTable, got %A" res)

[<Fact>]
let ``Temporary table declaration without ON COMMIT verification`` () =
    match parse "DECLARE LOCAL TEMPORARY TABLE t (a INT)" with
    | DeclareTemporaryTable { Columns = [ _ ]
                              Constraints = []
                              OnCommit = None } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareTemporaryTable, got %A" res)

[<Fact>]
let ``Temporary table declaration with table constraint verification`` () =
    match parse "DECLARE LOCAL TEMPORARY TABLE t (a INT, PRIMARY KEY (a)) ON COMMIT DELETE ROWS" with
    | DeclareTemporaryTable { Columns = [ _ ]
                              Constraints = [ PrimaryKey(None, [ { Kind = Identifier "A" } ]) ]
                              OnCommit = Some DeleteOnCommit } -> ()
    | res -> Assert.Fail(sprintf "Expected DeclareTemporaryTable, got %A" res)

[<Fact>]
let ``Temporary table declaration without table element list is rejected`` () =
    parseFails "DECLARE LOCAL TEMPORARY TABLE t"

[<Fact>]
let ``FREE LOCATOR verification`` () =
    match parse "FREE LOCATOR :loc" with
    | FreeLocator [ { Kind = Parameter ":LOC" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected FreeLocator, got %A" res)

[<Fact>]
let ``FREE LOCATOR multiple references verification`` () =
    match parse "FREE LOCATOR :a, ?" with
    | FreeLocator [ { Kind = Parameter ":A" }; { Kind = Parameter "?" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected FreeLocator with two references, got %A" res)

[<Fact>]
let ``FREE LOCATOR without locator reference is rejected`` () = parseFails "FREE LOCATOR"

[<Fact>]
let ``HOLD LOCATOR verification`` () =
    match parse "HOLD LOCATOR :loc" with
    | HoldLocator [ { Kind = Parameter ":LOC" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected HoldLocator, got %A" res)

[<Fact>]
let ``HOLD LOCATOR multiple references verification`` () =
    match parse "HOLD LOCATOR ?, :b" with
    | HoldLocator [ { Kind = Parameter "?" }; { Kind = Parameter ":B" } ] -> ()
    | res -> Assert.Fail(sprintf "Expected HoldLocator with two references, got %A" res)

[<Fact>]
let ``HOLD LOCATOR without locator reference is rejected`` () = parseFails "HOLD LOCATOR"
