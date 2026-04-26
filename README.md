# SQL-2003 Parser for F#

A robust SQL parser implemented in F# using the [FParsec](https://www.quanttec.com/fparsec/) library, specifically designed to align with the **ISO/IEC 9075:2003 (SQL-2003)** standard.

## Features

### 1. Lexical Analysis
- **Standard Keywords**: Full support for approx. 700 SQL-2003 reserved and non-reserved words.
- **Identifiers**: Support for regular identifiers, delimited identifiers (`"id"`), and Unicode escaping.
- **Literals**: Comprehensive literal parsing including:
    - Numeric (Exact and Approximate)
    - Character strings, National strings (`N'...'`), and Hex strings (`X'...'`)
    - DateTime literals (`DATE`, `TIME`, `TIMESTAMP`)
    - Boolean literals (`TRUE`, `FALSE`, `UNKNOWN`)
    - Intervals

### 2. Expression Parsing
- **Operator Precedence**: Correct handling of SQL operator precedence (Concatenation, Arithmetic, Comparison, Boolean).
- **Special Expressions**: Support for `CASE`, `CAST`, function calls, and subqueries within expressions.

### 3. Query & DML
- **SELECT**: Supports `JOIN` (Inner, Left, Right, Full, Cross), `WHERE`, `GROUP BY`, `HAVING`, and `ORDER BY`.
- **Set Operations**: `UNION`, `INTERSECT`, and `EXCEPT` (including `ALL` qualifier).
- **CTEs**: Common Table Expressions using `WITH` and `WITH RECURSIVE`.
- **DML**: Full support for `INSERT`, `UPDATE`, `DELETE`, and the standard `MERGE` statement.

### 4. Basic DDL
- **CREATE TABLE**: Support for column definitions, data types, and basic constraints.
- **CREATE INDEX**: Support for unique and standard indexes.

## Project Structure

The parser is designed with a modular architecture:
- `Lexer.fs`: Lexical tokens and basic elements.
- `Ast.fs`: Strongly typed Abstract Syntax Tree.
- `Types.fs`: SQL-2003 data types.
- `ExpressionParser.fs`: Scalar expression and operator precedence logic.
- `QueryParser.fs`: SELECT and set operation structures.
- `DmlParser.fs`: INSERT, UPDATE, DELETE, and MERGE logic.
- `DdlParser.fs`: CREATE TABLE and CREATE INDEX logic.
- `SqlParser.fs`: Main entry point and CTE handling.

## Getting Started

### Prerequisites
- .NET 8.0 SDK or later.

### Running Tests
The project includes a comprehensive test suite in `SqlParser.Tests`.
```bash
dotnet test
```

### Usage Example
```fsharp
open SqlParser

let sql = "SELECT id, name FROM users WHERE age > 18"
match SqlParser.parse sql with
| Choice1Of2 (stmt, pos) -> 
    printfn "Successfully parsed statement at line %d" pos.Line
| Choice2Of2 (ParseError (msg, pos)) -> 
    printfn "Parse error: %s at %d:%d" msg pos.Line pos.Column
```

## Current Status
- **Phase 1 (Foundation)**: 100% Complete.
- **Phase 2 (Scalar Expressions)**: 100% Complete.
- **Phase 3 (Query, DML & DDL)**: Core features implemented.
- **Verification**: 22/23 unit tests passing.

## License
This project is licensed under the MIT License.