module SqlParser.Tests.DynamicTests

open Xunit
open SqlParser

// Dynamic-SQL statements are <SQL procedure statement>s (13.4), not directly executable
// (22.1), so these use the general entry point.
let parseStatement (sql: string) =
    match SqlParser.parseStatement (sql.TrimEnd() + ";") with
    | Result.Ok res -> res.Kind
    | Result.Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

let parseStatementFails (sql: string) =
    match SqlParser.parseStatement (sql.TrimEnd() + ";") with
    | Result.Ok _ -> failwithf "Expected parse failure for %s" sql
    | Result.Error _ -> ()

[<Fact>]
let ``ALLOCATE DESCRIPTOR verification`` () =
    match parseStatement "ALLOCATE DESCRIPTOR d1 WITH MAX 10" with
    | AllocateDescriptor({ Kind = Identifier "D1" }, Some { Kind = Literal(Number 10m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected AllocateDescriptor, got %A" res)

[<Fact>]
let ``ALLOCATE SQL DESCRIPTOR verification`` () =
    match parseStatement "ALLOCATE SQL DESCRIPTOR d1" with
    | AllocateDescriptor({ Kind = Identifier "D1" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected AllocateDescriptor SQL, got %A" res)

[<Fact>]
let ``DEALLOCATE DESCRIPTOR verification`` () =
    match parseStatement "DEALLOCATE SQL DESCRIPTOR d1" with
    | DeallocateDescriptor { Kind = Identifier "D1" } -> ()
    | res -> Assert.Fail(sprintf "Expected DeallocateDescriptor, got %A" res)

[<Fact>]
let ``descriptor statement names are single identifiers`` () =
    // 20.2 / 20.3 <conventional descriptor name> ::= <non-extended descriptor name> | <extended
    // descriptor name>, and <non-extended descriptor name> ::= <identifier> (5.4) — no qualifier.
    parseStatementFails "ALLOCATE SQL DESCRIPTOR app.d1"
    parseStatementFails "ALLOCATE SQL DESCRIPTOR cat.app.d1"
    parseStatementFails "DEALLOCATE SQL DESCRIPTOR app.d1"
    // The <descriptor name> of 20.4 / 20.5 / 20.6 also admits a <PTF descriptor name>.
    parseStatementFails "DEALLOCATE SQL DESCRIPTOR PTF d1"

[<Fact>]
let ``GET DESCRIPTOR header verification`` () =
    match parseStatement "GET DESCRIPTOR d1 x = COUNT" with
    | GetDescriptor({ Kind = Identifier "D1" }, GetHeader [ ({ Kind = Identifier "X" }, "COUNT") ]) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDescriptor header, got %A" res)

[<Fact>]
let ``GET DESCRIPTOR VALUE verification`` () =
    match parseStatement "GET DESCRIPTOR d1 VALUE 1 x = DATA" with
    | GetDescriptor({ Kind = Identifier "D1" },
                    GetItem({ Kind = Literal(Number 1m) }, [ ({ Kind = Identifier "X" }, "DATA") ])) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDescriptor VALUE, got %A" res)

[<Fact>]
let ``GET DESCRIPTOR targets are simple target specifications (20.4)`` () =
    // 20.4 <get header information> / <get item information> take a <simple target specification>,
    // which admits a host parameter as well as a column reference. (A host parameter name is an
    // <identifier>, so a reserved word such as COUNT cannot follow the colon.)
    match parseStatement "GET DESCRIPTOR d1 :cnt = COUNT" with
    | GetDescriptor(_, GetHeader [ ({ Kind = Parameter ":CNT" }, "COUNT") ]) -> ()
    | res -> Assert.Fail(sprintf "Expected a host parameter target, got %A" res)

    match parseStatement "GET DESCRIPTOR d1 VALUE 1 :len = LENGTH" with
    | GetDescriptor(_, GetItem(_, [ ({ Kind = Parameter ":LEN" }, "LENGTH") ])) -> ()
    | res -> Assert.Fail(sprintf "Expected a host parameter item target, got %A" res)

    // a <dynamic parameter specification> is not part of <simple target specification>
    parseStatementFails "GET DESCRIPTOR d1 ? = COUNT"

[<Fact>]
let ``SET DESCRIPTOR header verification`` () =
    match parseStatement "SET DESCRIPTOR d1 COUNT = 2" with
    | SetDescriptor({ Kind = Identifier "D1" }, SetHeader [ ("COUNT", { Kind = Literal(Number 2m) }) ]) -> ()
    | res -> Assert.Fail(sprintf "Expected SetDescriptor header, got %A" res)

[<Fact>]
let ``SET DESCRIPTOR VALUE verification`` () =
    match parseStatement "SET DESCRIPTOR d1 VALUE 1 DATA = 'x'" with
    | SetDescriptor({ Kind = Identifier "D1" },
                    SetItem({ Kind = Literal(Number 1m) }, [ ("DATA", { Kind = Literal(String "x") }) ])) -> ()
    | res -> Assert.Fail(sprintf "Expected SetDescriptor VALUE, got %A" res)

[<Fact>]
let ``COPY DESCRIPTOR verification`` () =
    // 20.6 — the target descriptor name is a <PTF descriptor name> (PTF <simple value spec>).
    parseStatementFails "COPY d1 TO d2"

    match parseStatement "COPY d1 TO PTF ?" with
    | CopyDescriptor { Source = { Kind = Identifier "D1" }
                       SourceItem = None
                       Options = None
                       Target = { Kind = Parameter "?" }
                       TargetItem = None } -> ()
    | res -> Assert.Fail(sprintf "Expected CopyDescriptor, got %A" res)

[<Fact>]
let ``COPY DESCRIPTOR VALUE verification`` () =
    match parseStatement "COPY d1 VALUE 1 (NAME, TYPE) TO PTF ? VALUE 2" with
    | CopyDescriptor { Source = { Kind = Identifier "D1" }
                       SourceItem = Some { Kind = Literal(Number 1m) }
                       Options = Some [ "NAME"; "TYPE" ]
                       Target = { Kind = Parameter "?" }
                       TargetItem = Some { Kind = Literal(Number 2m) } } -> ()
    | res -> Assert.Fail(sprintf "Expected CopyDescriptor VALUE, got %A" res)

[<Fact>]
let ``PREPARE verification`` () =
    match parseStatement "PREPARE stmt FROM 'SELECT 1'" with
    | Prepare({ Kind = Identifier "STMT" }, None, { Kind = Literal(String "SELECT 1") }) -> ()
    | res -> Assert.Fail(sprintf "Expected Prepare, got %A" res)

[<Fact>]
let ``PREPARE with ATTRIBUTES verification`` () =
    match parseStatement "PREPARE stmt ATTRIBUTES 'a' FROM 'SELECT 1'" with
    | Prepare({ Kind = Identifier "STMT" }, Some { Kind = Literal(String "a") }, { Kind = Literal(String "SELECT 1") }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected Prepare with attributes, got %A" res)

[<Fact>]
let ``DEALLOCATE PREPARE verification`` () =
    match parseStatement "DEALLOCATE PREPARE stmt" with
    | DeallocatePrepare { Kind = Identifier "STMT" } -> ()
    | res -> Assert.Fail(sprintf "Expected DeallocatePrepare, got %A" res)

[<Fact>]
let ``DESCRIBE INPUT verification`` () =
    match parseStatement "DESCRIBE INPUT stmt USING SQL DESCRIPTOR d1" with
    | Describe { IsInput = true
                 IsCursor = false
                 Name = { Kind = Identifier "STMT" }
                 Descriptor = { Kind = Identifier "D1" }
                 Nesting = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Describe INPUT, got %A" res)

[<Fact>]
let ``DESCRIBE OUTPUT CURSOR verification`` () =
    match parseStatement "DESCRIBE OUTPUT CURSOR cur STRUCTURE USING DESCRIPTOR d1 WITH NESTING" with
    | Describe { IsInput = false
                 IsCursor = true
                 Name = { Kind = Identifier "CUR" }
                 Descriptor = { Kind = Identifier "D1" }
                 Nesting = Some true } -> ()
    | res -> Assert.Fail(sprintf "Expected Describe OUTPUT CURSOR, got %A" res)

    // 5.4 <cursor name> — at most two parts, MODULE being the only <local qualifier>.
    match parseStatement "DESCRIBE CURSOR MODULE.cur STRUCTURE USING DESCRIPTOR d1" with
    | Describe { IsCursor = true
                 Name = { Kind = ColumnReference [ "MODULE"; "CUR" ] } } -> ()
    | res -> Assert.Fail(sprintf "Expected a MODULE-qualified cursor name, got %A" res)

    parseStatementFails "DESCRIBE CURSOR a.b.c STRUCTURE USING DESCRIPTOR d1"

[<Fact>]
let ``DESCRIBE statement name verification`` () =
    match parseStatement "DESCRIBE stmt USING DESCRIPTOR d1 WITHOUT NESTING" with
    | Describe { IsInput = false
                 IsCursor = false
                 Name = { Kind = Identifier "STMT" }
                 Descriptor = { Kind = Identifier "D1" }
                 Nesting = Some false } -> ()
    | res -> Assert.Fail(sprintf "Expected Describe statement name, got %A" res)

[<Fact>]
let ``DESCRIBE INPUT rejects CURSOR`` () =
    // <describe input statement> ::= DESCRIBE INPUT <SQL statement name> <using descriptor> [ <nesting option> ]
    // INPUT commits to <describe input statement>; CURSOR is a reserved word and cannot be an <SQL statement name>.
    parseStatementFails "DESCRIBE INPUT CURSOR cur STRUCTURE USING DESCRIPTOR d1"

[<Fact>]
let ``EXECUTE verification`` () =
    // 20.11 <using argument> ::= <general value specification> — no <literal>.
    match parseStatement "EXECUTE stmt INTO a USING :x, ?" with
    | Execute({ Kind = Identifier "STMT" },
              Some(UsingArguments [ { Kind = Identifier "A" } ]),
              Some(UsingArguments [ { Kind = Parameter ":X" }; { Kind = Parameter "?" } ])) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute, got %A" res)

    parseStatementFails "EXECUTE stmt INTO a USING 1, 2"

[<Fact>]
let ``EXECUTE with descriptors verification`` () =
    match parseStatement "EXECUTE stmt INTO SQL DESCRIPTOR d1 USING SQL DESCRIPTOR d2" with
    | Execute({ Kind = Identifier "STMT" },
              Some(UsingDescriptor { Kind = Identifier "D1" }),
              Some(UsingDescriptor { Kind = Identifier "D2" })) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute with descriptors, got %A" res)

[<Fact>]
let ``EXECUTE with descriptors without SQL keyword verification`` () =
    // 20.11/20.12 allow the SQL keyword to be omitted.
    match parseStatement "EXECUTE stmt INTO DESCRIPTOR d1 USING DESCRIPTOR d2" with
    | Execute({ Kind = Identifier "STMT" },
              Some(UsingDescriptor { Kind = Identifier "D1" }),
              Some(UsingDescriptor { Kind = Identifier "D2" })) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute with descriptors without SQL, got %A" res)

[<Fact>]
let ``EXECUTE without clauses verification`` () =
    match parseStatement "EXECUTE stmt" with
    | Execute({ Kind = Identifier "STMT" }, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute without clauses, got %A" res)

[<Fact>]
let ``EXECUTE without statement name is rejected`` () = parseStatementFails "EXECUTE"

[<Fact>]
let ``SQL statement names are single identifiers`` () =
    // <SQL statement name> ::= <statement name> | <extended statement name>, and
    // <statement name> ::= <identifier> (5.4) — 20.9, 20.10 and 20.13.
    parseStatementFails "DEALLOCATE PREPARE app.stmt"
    parseStatementFails "DEALLOCATE PREPARE cat.app.stmt"
    parseStatementFails "DESCRIBE INPUT app.stmt USING DESCRIPTOR d1"
    parseStatementFails "DESCRIBE app.stmt USING DESCRIPTOR d1"
    parseStatementFails "EXECUTE app.stmt"

[<Fact>]
let ``EXECUTE IMMEDIATE verification`` () =
    match parseStatement "EXECUTE IMMEDIATE 'SELECT 1'" with
    | ExecuteImmediate { Kind = Literal(String "SELECT 1") } -> ()
    | res -> Assert.Fail(sprintf "Expected ExecuteImmediate, got %A" res)

[<Fact>]
let ``Dynamic SQL slots take only a simple value specification`` () =
    parseStatementFails "EXECUTE IMMEDIATE 1 + 1"
    parseStatementFails "PREPARE s FROM 1 + 1"
    parseStatementFails "GET DESCRIPTOR d1 VALUE 1 + 1 x = DATA"
    parseStatementFails "SET DESCRIPTOR d1 VALUE 1 + 1 DATA = 'x'"
    parseStatementFails "COPY d1 VALUE 1 + 1 (DATA) TO d2 VALUE 2"

[<Fact>]
let ``DYNAMIC DECLARE CURSOR verification`` () =
    // 20.15 <dynamic declare cursor> ::= DECLARE <cursor name> <cursor properties> FOR <statement name>
    // and <statement name> ::= <identifier> — the plain 5.4 identifier, not the 20.17
    // <extended statement name>.
    match parseStatement "DECLARE c CURSOR FOR s1" with
    | DynamicDeclareCursor dc ->
        match dc.Name.Kind, dc.Statement.Scope, dc.Statement.SimpleValue.Kind with
        | Identifier "C", None, Identifier "S1" -> ()
        | _ -> Assert.Fail(sprintf "Unexpected DynamicDeclareCursor %A" dc)

        Assert.Equal(None, dc.Properties.Sensitivity)
        Assert.Equal(None, dc.Properties.Scrollability)
        Assert.Equal(None, dc.Properties.Holdability)
        Assert.Equal(None, dc.Properties.Returnability)
    | res -> Assert.Fail(sprintf "Expected DynamicDeclareCursor, got %A" res)

    // 5.4 <cursor name> ::= <local qualified name> — at most two parts, MODULE being the
    // only <local qualifier> (MODULE is reserved, so the qualified form is the only one).
    match parseStatement "DECLARE MODULE.c CURSOR FOR s1" with
    | DynamicDeclareCursor dc ->
        match dc.Name.Kind with
        | ColumnReference [ "MODULE"; "C" ] -> ()
        | _ -> Assert.Fail(sprintf "Unexpected cursor name %A" dc.Name)
    | res -> Assert.Fail(sprintf "Expected DynamicDeclareCursor, got %A" res)

[<Fact>]
let ``DYNAMIC DECLARE CURSOR with properties verification`` () =
    match parseStatement "DECLARE c INSENSITIVE CURSOR WITH HOLD FOR s1" with
    | DynamicDeclareCursor dc ->
        Assert.Equal(Some Insensitive, dc.Properties.Sensitivity)
        Assert.Equal(Some WithHold, dc.Properties.Holdability)
        Assert.Equal(None, dc.Statement.Scope)

        match dc.Statement.SimpleValue.Kind with
        | Identifier "S1" -> ()
        | _ -> Assert.Fail(sprintf "Unexpected statement name %A" dc.Statement)
    | res -> Assert.Fail(sprintf "Expected DynamicDeclareCursor, got %A" res)

[<Fact>]
let ``DYNAMIC DECLARE CURSOR name arity is checked`` () =
    // <cursor name> admits at most two parts …
    parseStatementFails "DECLARE a.b.c CURSOR FOR s1"
    // … and a <statement name> is a bare <identifier>, so the 20.17 extended form,
    // a host parameter and a string literal are all rejected here.
    parseStatementFails "DECLARE c CURSOR FOR GLOBAL :s1"
    parseStatementFails "DECLARE c CURSOR FOR LOCAL :s1"
    parseStatementFails "DECLARE c CURSOR FOR :s1"
    parseStatementFails "DECLARE c CURSOR FOR 's1'"
    parseStatementFails "DECLARE c CURSOR FOR a.b"

[<Fact>]
let ``DYNAMIC DECLARE CURSOR without CURSOR keyword is rejected`` () = parseStatementFails "DECLARE c FOR s1"

[<Fact>]
let ``ALLOCATE EXTENDED DYNAMIC CURSOR verification`` () =
    match parseStatement "ALLOCATE :c SCROLL CURSOR FOR GLOBAL :s1" with
    | AllocateExtendedDynamicCursor ac ->
        Assert.Equal(Some Scroll, ac.Properties.Scrollability)
        Assert.Equal(Some ScopeGlobal, ac.Statement.Scope)

        match ac.Cursor.Scope, ac.Cursor.SimpleValue.Kind, ac.Statement.SimpleValue.Kind with
        | None, Parameter ":C", Parameter ":S1" -> ()
        | _ -> Assert.Fail(sprintf "Unexpected AllocateExtendedDynamicCursor %A" ac)
    | res -> Assert.Fail(sprintf "Expected AllocateExtendedDynamicCursor, got %A" res)

[<Fact>]
let ``ALLOCATE EXTENDED DYNAMIC CURSOR with dynamic names verification`` () =
    match parseStatement "ALLOCATE ? CURSOR FOR LOCAL :s1" with
    | AllocateExtendedDynamicCursor ac ->
        match ac.Cursor.Scope, ac.Cursor.SimpleValue.Kind, ac.Statement.Scope, ac.Statement.SimpleValue.Kind with
        | None, Parameter "?", Some ScopeLocal, Parameter ":S1" -> ()
        | _ -> Assert.Fail(sprintf "Unexpected AllocateExtendedDynamicCursor %A" ac)
    | res -> Assert.Fail(sprintf "Expected AllocateExtendedDynamicCursor, got %A" res)

[<Fact>]
let ``ALLOCATE RECEIVED CURSOR verification`` () =
    match parseStatement "ALLOCATE c CURSOR FOR PROCEDURE p" with
    | AllocateReceivedCursor ar ->
        match ar.Name.Kind, ar.Routine.Name.Kind with
        | Identifier "C", Identifier "P" -> ()
        | _ -> Assert.Fail(sprintf "Unexpected AllocateReceivedCursor %A" ar)
    | res -> Assert.Fail(sprintf "Expected AllocateReceivedCursor, got %A" res)

[<Fact>]
let ``ALLOCATE RECEIVED CURSOR without CURSOR keyword verification`` () =
    match parseStatement "ALLOCATE c FOR PROCEDURE p" with
    | AllocateReceivedCursor ar ->
        match ar.Name.Kind with
        | Identifier "C" -> ()
        | _ -> Assert.Fail(sprintf "Unexpected AllocateReceivedCursor %A" ar)
    | res -> Assert.Fail(sprintf "Expected AllocateReceivedCursor, got %A" res)

    // 20.18 <allocate received cursor statement> takes a <cursor name> (5.4) — at most two parts.
    match parseStatement "ALLOCATE MODULE.c FOR PROCEDURE p" with
    | AllocateReceivedCursor ar -> Assert.Equal<ExpressionKind>(ColumnReference [ "MODULE"; "C" ], ar.Name.Kind)
    | res -> Assert.Fail(sprintf "Expected AllocateReceivedCursor, got %A" res)

    parseStatementFails "ALLOCATE a.b.c FOR PROCEDURE p"

[<Fact>]
let ``ALLOCATE RECEIVED CURSOR with specific routine designator verification`` () =
    match parseStatement "ALLOCATE c CURSOR FOR PROCEDURE SPECIFIC FUNCTION f_spec" with
    | AllocateReceivedCursor ar ->
        match ar.Name.Kind, ar.Routine.IsSpecific, ar.Routine.RoutineType, ar.Routine.Name.Kind with
        | Identifier "C", true, Some RoutineType.Function, Identifier "F_SPEC" -> ()
        | _ -> Assert.Fail(sprintf "Unexpected AllocateReceivedCursor SPECIFIC %A" ar)
    | res -> Assert.Fail(sprintf "Expected AllocateReceivedCursor SPECIFIC, got %A" res)

[<Fact>]
let ``ALLOCATE without cursor name is rejected`` () = parseStatementFails "ALLOCATE c"

[<Fact>]
let ``ALLOCATE DESCRIPTOR WITH MAX rejects expression`` () =
    parseStatementFails "ALLOCATE DESCRIPTOR d1 WITH MAX 1 + 1"
    parseStatementFails "ALLOCATE DESCRIPTOR d1 WITH MAX a || b"
    parseStatementFails "ALLOCATE DESCRIPTOR d1 WITH MAX x = y"

[<Fact>]
let ``PIPE ROW verification`` () =
    // 20.28 <pipe row statement> ::= PIPE ROW <PTF descriptor name> ::= PTF <simple value specification>
    match parseStatement "PIPE ROW PTF :d1" with
    | PipeRow { Kind = Parameter ":D1" } -> ()
    | res -> Assert.Fail(sprintf "Expected PipeRow, got %A" res)

    parseStatementFails "PIPE ROW d1"
