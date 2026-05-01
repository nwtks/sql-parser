module SqlParser.Tests.DdlTests

open Xunit
open SqlParser

let parse sql =
    match SqlParser.parse sql with
    | Ok res -> res.Kind
    | Error(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``CREATE TABLE verification`` () =
    match parse "CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR(100) NOT NULL)" with
    | CreateTable { Table = { Kind = Identifier "USERS" }
                    Columns = [ { Name = { Kind = Identifier "ID" }
                                  DataType = Integer
                                  IsPrimaryKey = true }
                                { Name = { Kind = Identifier "NAME" }
                                  DataType = Varchar(Some 100)
                                  IsNullable = Some false } ] } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateTable, got %A" res)

[<Fact>]
let ``CREATE INDEX verification`` () =
    match parse "CREATE UNIQUE INDEX idx_name ON users (name)" with
    | CreateIndex { Name = { Kind = Identifier "IDX_NAME" }
                    Table = { Kind = Identifier "USERS" }
                    Columns = [ { Kind = Identifier "NAME" } ]
                    Unique = true } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateIndex, got %A" res)

[<Fact>]
let ``DROP statements verification`` () =
    match parse "DROP TABLE users" with
    | Drop(DropTable { Kind = Identifier "USERS" }) -> ()
    | res -> Assert.Fail(sprintf "Expected DropTable, got %A" res)

    match parse "DROP INDEX idx_name" with
    | Drop(DropIndex { Kind = Identifier "IDX_NAME" }) -> ()
    | res -> Assert.Fail(sprintf "Expected DropIndex, got %A" res)

    match parse "DROP VIEW my_view" with
    | Drop(DropView { Kind = Identifier "MY_VIEW" }) -> ()
    | res -> Assert.Fail(sprintf "Expected DropView, got %A" res)

[<Fact>]
let ``CREATE VIEW verification`` () =
    match parse "CREATE VIEW my_view AS SELECT * FROM t1" with
    | CreateView { Name = { Kind = Identifier "MY_VIEW" }
                   Query = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateView, got %A" res)

[<Fact>]
let ``ALTER TABLE verification`` () =
    match parse "ALTER TABLE users ADD COLUMN age INT" with
    | AlterTable { Table = { Kind = Identifier "USERS" }
                   Action = AddColumn { Name = { Kind = Identifier "AGE" }
                                        DataType = Integer } } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterTable, got %A" res)

[<Fact>]
let ``TRUNCATE TABLE verification`` () =
    match parse "TRUNCATE TABLE logs" with
    | Truncate { Kind = Identifier "LOGS" } -> ()
    | res -> Assert.Fail(sprintf "Expected Truncate, got %A" res)
