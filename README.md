# SQL Parser in F#

A SQL parser implemented in F# using [FParsec](https://www.quanttec.com/fparsec/), aligned with the **SQL-2016 foundation grammar**. This implementation is based on the [SQL-2016 Foundation Grammar](https://raw.githubusercontent.com/JakeWheat/sql-overview/refs/heads/master/sql-2016-foundation-grammar.txt) provided by the [sql-overview](https://github.com/JakeWheat/sql-overview) project.

## Features

### 🔍 Querying (SELECT)
- **Standard Clauses**: `SELECT` (including `DISTINCT`/`ALL`), `FROM`, `WHERE`, `GROUP BY`, `HAVING`, `WINDOW`, `ORDER BY`.
- **Advanced Clauses**: `OFFSET`, `FETCH FIRST/NEXT` (with `PERCENT` and `WITH TIES`).
- **Set Operations**: `UNION`, `INTERSECT`, `EXCEPT` (with `ALL`/`DISTINCT` and `CORRESPONDING`).
- **Window Functions**: Full support for `OVER` clauses, partition by, order by, and frame definitions (`ROWS`/`RANGE` between boundaries).
- **Common Table Expressions (CTEs)**: Support for `WITH` and `WITH RECURSIVE`.

### 📝 Data Manipulation (DML)
- `INSERT INTO ... VALUES / SELECT`
- `UPDATE ... SET ... WHERE`
- `DELETE FROM ... WHERE`
- `MERGE INTO ... USING ... ON ...`
- `TRUNCATE TABLE`

### 🏗️ Data Definition (DDL)
- `CREATE TABLE` (including column constraints like `PRIMARY KEY`, `UNIQUE`, `NOT NULL`, `CHECK`, `REFERENCES`).
- `CREATE INDEX` (including `UNIQUE`).
- `CREATE VIEW`.
- `DROP` (Table, View, Index).
- `ALTER TABLE` (Add/Drop column/constraint, Alter column).

### 🔢 Expressions & Types
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`), Comparison (`=`, `<>`, `<`, `<=`, `>`, `>=`), Logical (`AND`, `OR`, `NOT`), Concatenation (`||`).
- **Predicates**: `BETWEEN`, `IN`, `LIKE`, `SIMILAR TO`, `IS NULL`, `IS TRUE/FALSE/UNKNOWN`.
- **Functions**: `EXTRACT`, `POSITION`, `TRIM`, `CAST`, `CASE`, `COALESCE`, `NULLIF`.
- **Types**: Full SQL type system including `VARCHAR`, `NUMERIC`, `TIMESTAMP WITH TIME ZONE`, `INTERVAL`, `ARRAY`, `ROW`, etc.
- **Literals**: String, Hex (`X'...'`), Unicode (`U&'...'`), Binary, Date/Time, Numeric, Boolean.

## Usage

### Parsing a Statement

```fsharp
open SqlParser

let sql = "SELECT name, SUM(salary) OVER (PARTITION BY dept) FROM employees WHERE active = TRUE"

match SqlParser.parse sql with
| Choice1Of2 stmt ->
    printfn "Successfully parsed statement of kind: %A" stmt.Kind
| Choice2Of2 (ParseError(msg, pos)) ->
    printfn "Parse error: %s at line %d, col %d" msg pos.Line pos.Column
```

## Project Structure

- `Ast.fs`: Defines the Abstract Syntax Tree (AST) for SQL statements and expressions.
- `Lexer.fs`: Contains the lexing logic, reserved words, and literal parsers.
- `Types.fs`: Parsers for SQL data types.
- `ExpressionParser.fs`: Handles operator precedence and expression parsing.
- `QueryParser.fs`: Main logic for `SELECT` queries and set operations.
- `DmlParser.fs`: Parsers for `INSERT`, `UPDATE`, `DELETE`, `MERGE`.
- `DdlParser.fs`: Parsers for schema modification statements.
- `SqlParser.fs`: Main entry point and `WITH` clause handling.

## Running Tests

The project uses xUnit for testing. To run the tests:

```bash
dotnet test
```

## License

This project is licensed under the MIT License.
