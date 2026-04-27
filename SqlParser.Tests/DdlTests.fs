module SqlParser.Tests.DdlTests

open Xunit
open SqlParser
open SqlParser.Ast

let parse sql =
    match SqlParser.parse sql with
    | Choice1Of2 { Kind = res } -> res
    | Choice2Of2(ParseError(msg, pos)) -> failwithf "Parse failed: %s at %d:%d" msg pos.Line pos.Column

[<Fact>]
let ``CREATE TABLE verification`` () =
    match parse "CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR(100) NOT NULL)" with
    | CreateTable { Table = "USERS"
                    Columns = [ id; name ] } ->
        Assert.Equal("ID", id.Name)
        Assert.Equal(Integer, id.DataType)
        Assert.True id.IsPrimaryKey
        Assert.Equal("NAME", name.Name)
        Assert.Equal(Varchar(Some 100), name.DataType)
        Assert.Equal(Some false, name.IsNullable)
    | res -> Assert.Fail(sprintf "Expected CreateTable, got %A" res)

[<Fact>]
let ``CREATE INDEX verification`` () =
    match parse "CREATE UNIQUE INDEX idx_name ON users (name)" with
    | CreateIndex { Name = "IDX_NAME"
                    Table = "USERS"
                    Columns = [ "NAME" ]
                    Unique = true } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateIndex, got %A" res)

[<Fact>]
let ``DROP statements verification`` () =
    Assert.Equal(Drop(DropTable "USERS"), parse "DROP TABLE users")
    Assert.Equal(Drop(DropIndex "IDX_NAME"), parse "DROP INDEX idx_name")
    Assert.Equal(Drop(DropView "MY_VIEW"), parse "DROP VIEW my_view")

[<Fact>]
let ``CREATE VIEW verification`` () =
    match parse "CREATE VIEW my_view AS SELECT * FROM t1" with
    | CreateView { Name = "MY_VIEW"
                   Query = SelectQuery _ } -> ()
    | res -> Assert.Fail(sprintf "Expected CreateView, got %A" res)

[<Fact>]
let ``ALTER TABLE verification`` () =
    match parse "ALTER TABLE users ADD COLUMN age INT" with
    | AlterTable { Table = "USERS"
                   Action = AddColumn { Name = "AGE"; DataType = Integer } } -> ()
    | res -> Assert.Fail(sprintf "Expected AlterTable, got %A" res)

[<Fact>]
let ``TRUNCATE TABLE verification`` () =
    match parse "TRUNCATE TABLE logs" with
    | Truncate "LOGS" -> ()
    | res -> Assert.Fail(sprintf "Expected Truncate, got %A" res)
