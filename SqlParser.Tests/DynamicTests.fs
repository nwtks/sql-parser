module SqlParser.Tests.DynamicTests

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
let ``ALLOCATE DESCRIPTOR verification`` () =
    match parse "ALLOCATE DESCRIPTOR d1 WITH MAX 10" with
    | AllocateDescriptor({ Kind = Identifier "D1" }, Some { Kind = Literal(Number 10m) }) -> ()
    | res -> Assert.Fail(sprintf "Expected AllocateDescriptor, got %A" res)

[<Fact>]
let ``ALLOCATE SQL DESCRIPTOR verification`` () =
    match parse "ALLOCATE SQL DESCRIPTOR d1" with
    | AllocateDescriptor({ Kind = Identifier "D1" }, None) -> ()
    | res -> Assert.Fail(sprintf "Expected AllocateDescriptor SQL, got %A" res)

[<Fact>]
let ``DEALLOCATE DESCRIPTOR verification`` () =
    match parse "DEALLOCATE SQL DESCRIPTOR d1" with
    | DeallocateDescriptor { Kind = Identifier "D1" } -> ()
    | res -> Assert.Fail(sprintf "Expected DeallocateDescriptor, got %A" res)

[<Fact>]
let ``GET DESCRIPTOR header verification`` () =
    match parse "GET DESCRIPTOR d1 x = COUNT" with
    | GetDescriptor({ Kind = Identifier "D1" }, GetHeader [ ({ Kind = Identifier "X" }, "COUNT") ]) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDescriptor header, got %A" res)

[<Fact>]
let ``GET DESCRIPTOR VALUE verification`` () =
    match parse "GET DESCRIPTOR d1 VALUE 1 x = DATA" with
    | GetDescriptor({ Kind = Identifier "D1" },
                    GetItem({ Kind = Literal(Number 1m) }, [ ({ Kind = Identifier "X" }, "DATA") ])) -> ()
    | res -> Assert.Fail(sprintf "Expected GetDescriptor VALUE, got %A" res)

[<Fact>]
let ``SET DESCRIPTOR header verification`` () =
    match parse "SET DESCRIPTOR d1 COUNT = 2" with
    | SetDescriptor({ Kind = Identifier "D1" }, SetHeader [ ("COUNT", { Kind = Literal(Number 2m) }) ]) -> ()
    | res -> Assert.Fail(sprintf "Expected SetDescriptor header, got %A" res)

[<Fact>]
let ``SET DESCRIPTOR VALUE verification`` () =
    match parse "SET DESCRIPTOR d1 VALUE 1 DATA = 'x'" with
    | SetDescriptor({ Kind = Identifier "D1" },
                    SetItem({ Kind = Literal(Number 1m) }, [ ("DATA", { Kind = Literal(String "x") }) ])) -> ()
    | res -> Assert.Fail(sprintf "Expected SetDescriptor VALUE, got %A" res)

[<Fact>]
let ``COPY DESCRIPTOR verification`` () =
    match parse "COPY d1 TO d2" with
    | CopyDescriptor { Source = { Kind = Identifier "D1" }
                       SourceItem = None
                       Options = None
                       Target = { Kind = Identifier "D2" }
                       TargetItem = None } -> ()
    | res -> Assert.Fail(sprintf "Expected CopyDescriptor, got %A" res)

[<Fact>]
let ``COPY DESCRIPTOR VALUE verification`` () =
    match parse "COPY d1 VALUE 1 (NAME, TYPE) TO d2 VALUE 2" with
    | CopyDescriptor { Source = { Kind = Identifier "D1" }
                       SourceItem = Some { Kind = Literal(Number 1m) }
                       Options = Some [ "NAME"; "TYPE" ]
                       Target = { Kind = Identifier "D2" }
                       TargetItem = Some { Kind = Literal(Number 2m) } } -> ()
    | res -> Assert.Fail(sprintf "Expected CopyDescriptor VALUE, got %A" res)

[<Fact>]
let ``PREPARE verification`` () =
    match parse "PREPARE stmt FROM 'SELECT 1'" with
    | Prepare({ Kind = Identifier "STMT" }, None, { Kind = Literal(String "SELECT 1") }) -> ()
    | res -> Assert.Fail(sprintf "Expected Prepare, got %A" res)

[<Fact>]
let ``PREPARE with ATTRIBUTES verification`` () =
    match parse "PREPARE stmt ATTRIBUTES 'a' FROM 'SELECT 1'" with
    | Prepare({ Kind = Identifier "STMT" }, Some { Kind = Literal(String "a") }, { Kind = Literal(String "SELECT 1") }) ->
        ()
    | res -> Assert.Fail(sprintf "Expected Prepare with attributes, got %A" res)

[<Fact>]
let ``DEALLOCATE PREPARE verification`` () =
    match parse "DEALLOCATE PREPARE stmt" with
    | DeallocatePrepare { Kind = Identifier "STMT" } -> ()
    | res -> Assert.Fail(sprintf "Expected DeallocatePrepare, got %A" res)

[<Fact>]
let ``DESCRIBE INPUT verification`` () =
    match parse "DESCRIBE INPUT stmt USING SQL DESCRIPTOR d1" with
    | Describe { IsInput = true
                 IsCursor = false
                 Name = { Kind = Identifier "STMT" }
                 Descriptor = { Kind = Identifier "D1" }
                 Nesting = None } -> ()
    | res -> Assert.Fail(sprintf "Expected Describe INPUT, got %A" res)

[<Fact>]
let ``DESCRIBE OUTPUT CURSOR verification`` () =
    match parse "DESCRIBE OUTPUT CURSOR cur STRUCTURE USING DESCRIPTOR d1 WITH NESTING" with
    | Describe { IsInput = false
                 IsCursor = true
                 Name = { Kind = Identifier "CUR" }
                 Descriptor = { Kind = Identifier "D1" }
                 Nesting = Some true } -> ()
    | res -> Assert.Fail(sprintf "Expected Describe OUTPUT CURSOR, got %A" res)

[<Fact>]
let ``DESCRIBE statement name verification`` () =
    match parse "DESCRIBE stmt USING DESCRIPTOR d1 WITHOUT NESTING" with
    | Describe { IsInput = false
                 IsCursor = false
                 Name = { Kind = Identifier "STMT" }
                 Descriptor = { Kind = Identifier "D1" }
                 Nesting = Some false } -> ()
    | res -> Assert.Fail(sprintf "Expected Describe statement name, got %A" res)

[<Fact>]
let ``EXECUTE verification`` () =
    match parse "EXECUTE stmt INTO a USING 1, 2" with
    | Execute({ Kind = Identifier "STMT" },
              Some(UsingArguments [ { Kind = Identifier "A" } ]),
              Some(UsingArguments [ { Kind = Literal(Number 1m) }; { Kind = Literal(Number 2m) } ])) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute, got %A" res)

[<Fact>]
let ``EXECUTE with descriptors verification`` () =
    match parse "EXECUTE stmt INTO SQL DESCRIPTOR d1 USING SQL DESCRIPTOR d2" with
    | Execute({ Kind = Identifier "STMT" },
              Some(UsingDescriptor { Kind = Identifier "D1" }),
              Some(UsingDescriptor { Kind = Identifier "D2" })) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute with descriptors, got %A" res)

[<Fact>]
let ``EXECUTE without clauses verification`` () =
    match parse "EXECUTE stmt" with
    | Execute({ Kind = Identifier "STMT" }, None, None) -> ()
    | res -> Assert.Fail(sprintf "Expected Execute without clauses, got %A" res)

[<Fact>]
let ``EXECUTE IMMEDIATE verification`` () =
    match parse "EXECUTE IMMEDIATE 'SELECT 1'" with
    | ExecuteImmediate { Kind = Literal(String "SELECT 1") } -> ()
    | res -> Assert.Fail(sprintf "Expected ExecuteImmediate, got %A" res)

[<Fact>]
let ``PIPE ROW verification`` () =
    match parse "PIPE ROW d1" with
    | PipeRow { Kind = Identifier "D1" } -> ()
    | res -> Assert.Fail(sprintf "Expected PipeRow, got %A" res)

[<Fact>]
let ``EXECUTE without statement name is rejected`` () = parseFails "EXECUTE"
