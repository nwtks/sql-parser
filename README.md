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

### ↕️ Cursors & Locators
- `DECLARE <cursor> [SENSITIVE|INSENSITIVE|ASENSITIVE] [SCROLL|NO SCROLL] CURSOR [WITH|WITHOUT HOLD] [WITH|WITHOUT RETURN] FOR <query expression> [FOR READ ONLY | FOR UPDATE [OF <columns>]]`.
- Dynamic cursors: `DECLARE <cursor> [<cursor properties>] FOR <statement name>`, `ALLOCATE <extended cursor name> [<cursor properties>] FOR <extended statement name>`, `ALLOCATE <cursor name> [CURSOR] FOR PROCEDURE <specific routine designator>`.
- `OPEN`, `FETCH` (with `NEXT`/`PRIOR`/`FIRST`/`LAST`/`ABSOLUTE`/`RELATIVE`), `CLOSE`.
- `SELECT ... INTO ...` (single row).
- `DECLARE LOCAL TEMPORARY TABLE ... [ON COMMIT PRESERVE|DELETE ROWS]`.
- `FREE LOCATOR` / `HOLD LOCATOR`.
- Routine parameter defaults accept `DESCRIPTOR ( <column name> [ <data type> ] , ... )`.

### 🏗️ Data Definition (DDL)
- `CREATE TABLE` (column constraints like `PRIMARY KEY`, `UNIQUE`, `NOT NULL`, `CHECK`, `REFERENCES`, and table-level constraints: `PRIMARY KEY (...)`, `UNIQUE (...)`, `FOREIGN KEY (...) REFERENCES ... [ON UPDATE/DELETE ...]`, `CHECK (...)`).
- `CREATE INDEX` (including `UNIQUE`).
- `CREATE VIEW`.
- `CREATE ROLE` / `DROP ROLE`.
- `DROP` (Table, Index, View, Role).
- `ALTER TABLE` — the full `<alter table action>` set: `ADD [COLUMN]`, `DROP [COLUMN] ... CASCADE|RESTRICT`, `ALTER [COLUMN]` (`SET`/`DROP DEFAULT`, `SET`/`DROP NOT NULL`, `ADD`/`DROP SCOPE`, `SET DATA TYPE`, `SET GENERATED ...`, `RESTART`/`SET <sequence option>`, `DROP IDENTITY`, `DROP EXPRESSION`), `ADD`/`ALTER`/`DROP CONSTRAINT`, `ADD`/`DROP PERIOD FOR ...`, and `ADD`/`DROP SYSTEM VERSIONING`.
- `GRANT` / `REVOKE` (privileges and roles, including `ALL PRIVILEGES`, `WITH GRANT OPTION`, `WITH ADMIN OPTION`).

### 🔐 Transactions
- `START TRANSACTION` (with transaction modes like `ISOLATION LEVEL`, `READ ONLY`/`READ WRITE`).
- `COMMIT` / `ROLLBACK` (with `WORK`, `AND [NO] CHAIN`, and `TO SAVEPOINT`).
- `SAVEPOINT` / `RELEASE SAVEPOINT`.
- `SET [LOCAL] TRANSACTION` (isolation levels, `READ ONLY`/`READ WRITE`).
- `SET CONSTRAINTS ... { DEFERRED | IMMEDIATE }`.

### 🔢 Expressions & Types
- **Operators**: Arithmetic (`+`, `-`, `*`, `/`), Comparison (`=`, `<>`, `<`, `<=`, `>`, `>=`), Logical (`AND`, `OR`, `NOT`), Concatenation (`||`), Dereference (`->`).
- **Predicates**: `BETWEEN`, `IN`, `LIKE`, `SIMILAR TO`, `IS NULL`, `IS TRUE/FALSE/UNKNOWN`, `IS [NOT] NORMALIZED`, `IS [NOT] OF`, `IS [NOT] JSON`, `LIKE_REGEX`, `MATCH`, `MEMBER OF`, `SUBMULTISET OF`, `IS A SET`, period predicates.
- **Functions**: `EXTRACT`, `POSITION`, `TRIM`, `CAST`, `CASE`, `COALESCE`, `NULLIF`; numeric (`ABS`, `MOD`, `CEIL`/`CEILING`, `FLOOR`, `SQRT`, `POWER`, `LOG`/`LOG10`/`LN`/`EXP`, trigonometric, `WIDTH_BUCKET`, `CARDINALITY`, `CHAR_LENGTH`/`OCTET_LENGTH`); string (`SUBSTRING`, `OVERLAY`, `UPPER`/`LOWER`, `NORMALIZE`, `CONVERT`/`TRANSLATE` using, `TRIM_ARRAY`); regex (`OCCURRENCES_REGEX`, `POSITION_REGEX`, `SUBSTRING_REGEX`, `TRANSLATE_REGEX`); collection (`ARRAY`/`MULTISET`/`TABLE (query)` constructors and `MULTISET UNION`/`INTERSECT`/`EXCEPT`, `ELEMENT`, `SET`, element references); row pattern (`MATCH_NUMBER`, `CLASSIFIER`, `PREV`/`NEXT`/`FIRST`/`LAST`, `VALUE_OF`, `RUNNING`/`FINAL`, `GROUPING`); JSON (`JSON_VALUE`, `JSON_QUERY`, `JSON_OBJECT`, `JSON_ARRAY`, `JSON_OBJECTAGG`, `JSON_ARRAYAGG`).
- **Datetime**: `CURRENT_DATE`, `CURRENT_TIME`, `CURRENT_TIMESTAMP`, `LOCALTIME`, `LOCALTIMESTAMP`, plus `AT TIME ZONE` / `AT LOCAL`, and datetime-difference intervals (`(ts1 - ts2) DAY TO SECOND`).
- **Types**: Full SQL type system including `VARCHAR`, `NUMERIC`, `TIMESTAMP WITH TIME ZONE`, `INTERVAL`, `ARRAY`, `ROW`, nested collections (`INT ARRAY ARRAY`, `INT MULTISET ARRAY[3]`), etc.
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
- `CursorParser.fs`: Parsers for cursor declarations, `OPEN`/`FETCH`/`CLOSE`, cursor `SELECT ... INTO`, temporary table declarations, and locator statements.
- `DdlParser.fs`: Parsers for schema modification statements (including `GRANT`/`REVOKE`/role).
- `TransactionParser.fs`: Parsers for transaction statements.
- `SqlParser.fs`: Main entry point and `WITH` clause handling.

## Running Tests

The project uses xUnit for testing. To run the tests:

```bash
dotnet test
```

## License

This project is licensed under the MIT License.
