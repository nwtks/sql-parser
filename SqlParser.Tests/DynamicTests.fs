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
    | AllocateDescriptor({ Scope = None
                           SimpleValue = { Kind = Identifier "D1" } },
                         Some { Kind = Literal(Number 10m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected AllocateDescriptor, got %A" res)

[<Fact>]
let ``ALLOCATE SQL DESCRIPTOR verification`` () =
    match parseStatement "ALLOCATE SQL DESCRIPTOR d1" with
    | AllocateDescriptor({ Scope = None
                           SimpleValue = { Kind = Identifier "D1" } },
                         None) -> ()
    | res -> Assert.Fail(sprintf "Expected AllocateDescriptor SQL, got %A" res)

[<Fact>]
let ``DEALLOCATE DESCRIPTOR verification`` () =
    match parseStatement "DEALLOCATE SQL DESCRIPTOR d1" with
    | DeallocateDescriptor { Scope = None
                             SimpleValue = { Kind = Identifier "D1" } } -> ()
    | res -> Assert.Fail(sprintf "Expected DeallocateDescriptor, got %A" res)

[<Fact>]
let ``descriptor statement names are single identifiers`` () =
    // 5.4 <non-extended descriptor name> still rejects PTF — only the 20.28 form admits it.
    parseStatementFails "DEALLOCATE SQL DESCRIPTOR PTF d1"

    // 5.4 <extended descriptor name> ::= [ <scope option> ] <simple value specification>
    // The extended form (with or without scope, plain simple-value-spec) accepts a
    // qualified identifier, a host parameter, etc.
    parseStatement "ALLOCATE SQL DESCRIPTOR app.d1" |> ignore
    parseStatement "ALLOCATE SQL DESCRIPTOR LOCAL :d" |> ignore
    parseStatement "ALLOCATE SQL DESCRIPTOR GLOBAL app.d1" |> ignore
    parseStatement "DEALLOCATE SQL DESCRIPTOR LOCAL :d" |> ignore

[<Fact>]
let ``GET DESCRIPTOR header verification`` () =
    match parseStatement "GET DESCRIPTOR d1 x = COUNT" with
    | GetDescriptor({ Scope = None
                      SimpleValue = { Kind = Identifier "D1" } },
                    GetHeader [ ({ Kind = Identifier "X" }, "COUNT") ]) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDescriptor header, got %A" res)

[<Fact>]
let ``GET DESCRIPTOR VALUE verification`` () =
    match parseStatement "GET DESCRIPTOR d1 VALUE 1 x = DATA" with
    | GetDescriptor({ Scope = None
                      SimpleValue = { Kind = Identifier "D1" } },
                    GetItem({ Kind = Literal(Number 1m) }, [ ({ Kind = Identifier "X" }, "DATA") ])) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDescriptor VALUE, got %A" res)

[<Fact>]
let ``PTF descriptor and cursor names (5.4)`` () =
    // 5.4 <descriptor name> ::= <conventional descriptor name> | <PTF descriptor name>
    // 5.4 <dynamic cursor name> ::= <conventional dynamic cursor name> | <PTF cursor name>
    // <PTF descriptor name> / <PTF cursor name> ::= PTF <simple value specification>, and
    // neither takes a <scope option>, so Scope is None.
    match parseStatement "GET DESCRIPTOR PTF :d :n = COUNT" with
    | GetDescriptor({ Scope = None
                      SimpleValue = { Kind = Parameter ":D" } },
                    GetHeader [ ({ Kind = Parameter ":N" }, "COUNT") ]) -> ()
    | res -> Assert.Fail(sprintf "Expected a PTF descriptor name, got %A" res)

    match parseStatement "SET DESCRIPTOR PTF :d COUNT = 1" with
    | SetDescriptor({ Scope = None
                      SimpleValue = { Kind = Parameter ":D" } },
                    SetHeader [ ("COUNT", { Kind = Literal(Number 1m) }) ]) -> ()
    | res -> Assert.Fail(sprintf "Expected a PTF descriptor name, got %A" res)

    match parseStatement "DESCRIBE OUTPUT s USING SQL DESCRIPTOR PTF :d" with
    | Describe { IsInput = false
                 Descriptor = { Scope = None
                                SimpleValue = { Kind = Parameter ":D" } } } -> ()
    | res -> Assert.Fail(sprintf "Expected a PTF descriptor name, got %A" res)

    match parseStatement "FETCH FROM PTF :c INTO :x" with
    | DynamicFetch(_,
                   { Scope = None
                     SimpleValue = { Kind = Parameter ":C" } },
                   _) -> ()
    | res -> Assert.Fail(sprintf "Expected a PTF cursor name, got %A" res)

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
    | SetDescriptor({ Scope = None
                      SimpleValue = { Kind = Identifier "D1" } },
                    SetHeader [ ("COUNT", { Kind = Literal(Number 2m) }) ]) -> ()
    | res -> Assert.Fail(sprintf "Expected SetDescriptor header, got %A" res)

[<Fact>]
let ``SET DESCRIPTOR VALUE verification`` () =
    match parseStatement "SET DESCRIPTOR d1 VALUE 1 DATA = 'x'" with
    | SetDescriptor({ Scope = None
                      SimpleValue = { Kind = Identifier "D1" } },
                    SetItem({ Kind = Literal(Number 1m) }, [ ("DATA", { Kind = Literal(String "x") }) ])) -> ()
    | res -> Assert.Fail(sprintf "Expected SetDescriptor VALUE, got %A" res)

[<Fact>]
let ``COPY DESCRIPTOR verification`` () =
    // 20.6 — the target descriptor name is a <PTF descriptor name> (PTF <simple value spec>).
    parseStatementFails "COPY d1 TO d2"

    // 6.4 <simple value specification> has no <dynamic parameter specification>, so `?` is out.
    parseStatementFails "COPY d1 TO PTF ?"

    match parseStatement "COPY d1 TO PTF :d2" with
    | CopyDescriptor { Source = { Scope = None
                                  SimpleValue = { Kind = Identifier "D1" } }
                       SourceItem = None
                       Options = None
                       Target = { Kind = Parameter ":D2" }
                       TargetItem = None } -> ()
    | res -> Assert.Fail(sprintf "Expected CopyDescriptor, got %A" res)

[<Fact>]
let ``COPY DESCRIPTOR VALUE verification`` () =
    // <item number 1> / <item number 2> are <simple value specification>s — no `?`.
    parseStatementFails "COPY d1 VALUE ? (NAME) TO PTF :d2 VALUE 2"

    match parseStatement "COPY d1 VALUE 1 (NAME, TYPE) TO PTF :d2 VALUE 2" with
    | CopyDescriptor { Source = { Scope = None
                                  SimpleValue = { Kind = Identifier "D1" } }
                       SourceItem = Some { Kind = Literal(Number 1m) }
                       Options = Some [ "NAME"; "TYPE" ]
                       Target = { Kind = Parameter ":D2" }
                       TargetItem = Some { Kind = Literal(Number 2m) } } -> ()
    | res -> Assert.Fail(sprintf "Expected CopyDescriptor VALUE, got %A" res)

[<Fact>]
let ``PREPARE verification`` () =
    match parseStatement "PREPARE stmt FROM 'SELECT 1'" with
    | Prepare({ Scope = None
                SimpleValue = { Kind = Identifier "STMT" } },
              None,
              { Kind = Literal(String "SELECT 1") }) -> ()
    | res -> Assert.Fail(sprintf "Expected Prepare, got %A" res)

[<Fact>]
let ``PREPARE with ATTRIBUTES verification`` () =
    match parseStatement "PREPARE stmt ATTRIBUTES 'a' FROM 'SELECT 1'" with
    | Prepare({ Scope = None
                SimpleValue = { Kind = Identifier "STMT" } },
              Some { Kind = Literal(String "a") },
              { Kind = Literal(String "SELECT 1") }) -> ()
    | res -> Assert.Fail(sprintf "Expected Prepare with attributes, got %A" res)

[<Fact>]
let ``DEALLOCATE PREPARE verification`` () =
    match parseStatement "DEALLOCATE PREPARE stmt" with
    | DeallocatePrepare { Scope = None
                          SimpleValue = { Kind = Identifier "STMT" } } -> ()
    | res -> Assert.Fail(sprintf "Expected DeallocatePrepare, got %A" res)

[<Fact>]
let ``DESCRIBE INPUT verification`` () =
    match parseStatement "DESCRIBE INPUT stmt USING SQL DESCRIPTOR d1" with
    | Describe { IsInput = true
                 IsCursor = false
                 Name = { Scope = None
                          SimpleValue = { Kind = Identifier "STMT" } }
                 Descriptor = { Scope = None
                                SimpleValue = { Kind = Identifier "D1" } }
                 Nesting = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Describe INPUT, got %A" res)

[<Fact>]
let ``DESCRIBE OUTPUT CURSOR verification`` () =
    match parseStatement "DESCRIBE OUTPUT CURSOR cur STRUCTURE USING DESCRIPTOR d1 WITH NESTING" with
    | Describe { IsInput = false
                 IsCursor = true
                 Name = { Scope = None
                          SimpleValue = { Kind = Identifier "CUR" } }
                 Descriptor = { Scope = None
                                SimpleValue = { Kind = Identifier "D1" } }
                 Nesting = Some true } -> ()
    | res -> Assert.Fail(sprintf "Expected Describe OUTPUT CURSOR, got %A" res)

    // 5.4 <cursor name> — at most two parts, MODULE being the only <local qualifier>.
    match parseStatement "DESCRIBE CURSOR MODULE.cur STRUCTURE USING DESCRIPTOR d1" with
    | Describe { IsCursor = true
                 Name = { Scope = None
                          SimpleValue = { Kind = ColumnReference [ "MODULE"; "CUR" ] } } } -> ()
    | res -> Assert.Fail(sprintf "Expected a MODULE-qualified cursor name, got %A" res)

    parseStatementFails "DESCRIBE CURSOR a.b.c STRUCTURE USING DESCRIPTOR d1"

[<Fact>]
let ``DESCRIBE statement name verification`` () =
    match parseStatement "DESCRIBE stmt USING DESCRIPTOR d1 WITHOUT NESTING" with
    | Describe { IsInput = false
                 IsCursor = false
                 Name = { Scope = None
                          SimpleValue = { Kind = Identifier "STMT" } }
                 Descriptor = { Scope = None
                                SimpleValue = { Kind = Identifier "D1" } }
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
    | Execute({ Scope = None
                SimpleValue = { Kind = Identifier "STMT" } },
              Some(UsingArguments [ { Kind = Identifier "A" } ]),
              Some(UsingArguments [ { Kind = Parameter ":X" }; { Kind = Parameter "?" } ])) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute, got %A" res)

    parseStatementFails "EXECUTE stmt INTO a USING 1, 2"

[<Fact>]
let ``EXECUTE with descriptors verification`` () =
    match parseStatement "EXECUTE stmt INTO SQL DESCRIPTOR d1 USING SQL DESCRIPTOR d2" with
    | Execute({ Scope = None
                SimpleValue = { Kind = Identifier "STMT" } },
              Some(UsingDescriptor { Scope = None
                                     SimpleValue = { Kind = Identifier "D1" } }),
              Some(UsingDescriptor { Scope = None
                                     SimpleValue = { Kind = Identifier "D2" } })) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute with descriptors, got %A" res)

[<Fact>]
let ``EXECUTE with descriptors without SQL keyword verification`` () =
    // 20.11/20.12 allow the SQL keyword to be omitted.
    match parseStatement "EXECUTE stmt INTO DESCRIPTOR d1 USING DESCRIPTOR d2" with
    | Execute({ Scope = None
                SimpleValue = { Kind = Identifier "STMT" } },
              Some(UsingDescriptor { Scope = None
                                     SimpleValue = { Kind = Identifier "D1" } }),
              Some(UsingDescriptor { Scope = None
                                     SimpleValue = { Kind = Identifier "D2" } })) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute with descriptors without SQL, got %A" res)

[<Fact>]
let ``EXECUTE without clauses verification`` () =
    match parseStatement "EXECUTE stmt" with
    | Execute({ Scope = None
                SimpleValue = { Kind = Identifier "STMT" } },
              None,
              None) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute without clauses, got %A" res)

[<Fact>]
let ``EXECUTE without statement name is rejected`` () = parseStatementFails "EXECUTE"

[<Fact>]
let ``SQL statement names are single identifiers`` () =
    // 5.4 <statement name> admits a bare identifier; the 20.17 extended form (below)
    // accepts any <simple value specification> including qualified identifiers and
    // host parameters, with or without a GLOBAL/LOCAL scope option. Arithmetic is
    // still rejected because <simple value specification> does not include a term.
    parseStatementFails "EXECUTE 1 + 1"

    parseStatement "DEALLOCATE PREPARE app.stmt" |> ignore
    parseStatement "DEALLOCATE PREPARE LOCAL :s" |> ignore
    parseStatement "PREPARE app.stmt FROM 'SELECT 1'" |> ignore
    parseStatement "PREPARE GLOBAL app.stmt FROM 'SELECT 1'" |> ignore
    parseStatement "EXECUTE LOCAL app.stmt" |> ignore
    parseStatement "DESCRIBE INPUT app.stmt USING DESCRIPTOR d1" |> ignore
    parseStatement "DESCRIBE app.stmt USING DESCRIPTOR d1" |> ignore

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

[<Fact>]
let ``DYNAMIC OPEN verification (20.19)`` () =
    // 20.19 <dynamic open statement> ::= OPEN <extended cursor name> [ <input using clause> ]
    // The plain 5.4 cursor name still routes via 14.4 (the static `Open` AST); only the
    // 20.17 <extended cursor name> (`[ <scope option> ] <simple value specification>`)
    // reaches `DynamicOpen`. The simple value spec is literal / host parameter / SQL
    // parameter reference — not a column reference (per spec).
    match parseStatement "OPEN GLOBAL :c" with
    | DynamicOpen({ Scope = Some ScopeGlobal
                    SimpleValue = { Kind = Parameter ":C" } },
                  None) -> ()
    | res -> Assert.Fail(sprintf "Expected DynamicOpen with GLOBAL scope, got %A" res)

    match parseStatement "OPEN LOCAL :c" with
    | DynamicOpen({ Scope = Some ScopeLocal
                    SimpleValue = { Kind = Parameter ":C" } },
                  None) -> ()
    | res -> Assert.Fail(sprintf "Expected DynamicOpen with LOCAL scope, got %A" res)

[<Fact>]
let ``DYNAMIC FETCH verification (20.20)`` () =
    // 20.20 <dynamic fetch statement> ::=
    //     FETCH [ [ <fetch orientation> ] FROM ] <extended cursor name> <output using clause>
    // The descriptor form (INTO [ SQL ] DESCRIPTOR) belongs to 20.20 — the static 14.5 path
    // rejects DESCRIPTOR (now a reserved word) and the dynamic path takes over.
    match parseStatement "FETCH cur INTO DESCRIPTOR d" with
    | DynamicFetch(None,
                   { Scope = None
                     SimpleValue = { Kind = Identifier "CUR" } },
                   UsingDescriptor { Scope = None
                                     SimpleValue = { Kind = Identifier "D" } }) -> ()
    | res -> Assert.Fail(sprintf "Expected DynamicFetch with DESCRIPTOR, got %A" res)

    match parseStatement "FETCH cur INTO SQL DESCRIPTOR d" with
    | DynamicFetch(None, _, UsingDescriptor _) -> ()
    | res -> Assert.Fail(sprintf "Expected DynamicFetch with SQL DESCRIPTOR, got %A" res)

    match parseStatement "FETCH GLOBAL :cur INTO a" with
    | DynamicFetch(None,
                   { Scope = Some ScopeGlobal
                     SimpleValue = { Kind = Parameter ":CUR" } },
                   _) -> ()
    | res -> Assert.Fail(sprintf "Expected DynamicFetch with GLOBAL cursor, got %A" res)

[<Fact>]
let ``DYNAMIC CLOSE verification (20.22)`` () =
    // 20.22 <dynamic close statement> ::= CLOSE <extended cursor name>
    // Only the 20.17 <extended cursor name> reaches `DynamicClose`; the plain 5.4 cursor
    // name routes via 14.6 (the static `Close` AST).
    match parseStatement "CLOSE LOCAL :c" with
    | DynamicClose { Scope = Some ScopeLocal
                     SimpleValue = { Kind = Parameter ":C" } } -> ()
    | res -> Assert.Fail(sprintf "Expected DynamicClose with LOCAL scope, got %A" res)

[<Fact>]
let ``DYNAMIC POSITIONED DELETE / UPDATE verification (20.23 / 20.24)`` () =
    // 20.23 / 20.24 are syntactically identical to 14.8 / 14.13; the dynamic dispatch
    // accepts the positioned forms (the preparable 20.25 / 20.27 forms are still rejected
    // by `rejectOmittedTarget`).

    match parseStatement "DELETE FROM t WHERE CURRENT OF c" with
    | Delete { Cursor = Some { Kind = Identifier "C" } } -> ()
    | res -> Assert.Fail(sprintf "Expected positioned Delete, got %A" res)

    match parseStatement "UPDATE t SET x = 1 WHERE CURRENT OF c" with
    | Update { Cursor = Some { Kind = Identifier "C" } } -> ()
    | res -> Assert.Fail(sprintf "Expected positioned Update, got %A" res)

    // 20.25 <preparable dynamic delete statement: positioned> — omitted target rejected.
    parseStatementFails "DELETE WHERE CURRENT OF c"
    // 20.27 <preparable dynamic update statement: positioned> — omitted target rejected.
    parseStatementFails "UPDATE SET x = 1 WHERE CURRENT OF c"

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
    // 20.17 <extended cursor name> ::= [ <scope option> ] <simple value specification> —
    // no <dynamic parameter specification>, so `ALLOCATE ? CURSOR` is rejected.
    parseStatementFails "ALLOCATE ? CURSOR FOR LOCAL :s1"

    match parseStatement "ALLOCATE :c1 CURSOR FOR LOCAL :s1" with
    | AllocateExtendedDynamicCursor ac ->
        match ac.Cursor.Scope, ac.Cursor.SimpleValue.Kind, ac.Statement.Scope, ac.Statement.SimpleValue.Kind with
        | None, Parameter ":C1", Some ScopeLocal, Parameter ":S1" -> ()
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
