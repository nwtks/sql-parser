# SQL Parser in F#

A SQL parser implemented in F# using [FParsec](https://www.quanttec.com/fparsec/), aligned with the **SQL-2016 foundation grammar**. This implementation is based on the [SQL-2016 Foundation Grammar](https://raw.githubusercontent.com/JakeWheat/sql-overview/refs/heads/master/sql-2016-foundation-grammar.txt) provided by the [sql-overview](https://github.com/JakeWheat/sql-overview) project.

## Features

### 🔍 Querying (SELECT)
- **Standard Clauses**: `SELECT` (including `DISTINCT`/`ALL`), `FROM`, `WHERE`, `GROUP BY`, `HAVING`, `WINDOW`, `ORDER BY`.
- **Advanced Clauses**: `OFFSET`, `FETCH FIRST/NEXT` (with `PERCENT` and `WITH TIES`).
- **Set Operations**: `UNION`, `INTERSECT`, `EXCEPT` (with `ALL`/`DISTINCT` and `CORRESPONDING`).
- **Window Functions**: Full support for `OVER` clauses, partition by, order by, and frame definitions (`ROWS`/`RANGE` between boundaries).
- **Common Table Expressions (CTEs)**: Support for `WITH` and `WITH RECURSIVE`.
- **Row Value Constructors**: `(e1, e2, ...)` and `ROW(e1, ...)` as expressions, so row comparisons (`(a, b) = (c, d)`) and row `IN` lists work.
- **Select List**: `*`, `t.*`, `t.* AS (cols)`, and the general `<all fields reference>` (`(a + b).*`, `f(x).* AS (cols)`).
- **Table References**: `ONLY ( t ) [ AS alias [ (cols) ] ]`, `FOR SYSTEM_TIME AS OF | BETWEEN [SYMMETRIC|ASYMMETRIC] ... AND ... | FROM ... TO ...` (point-in-time expressions support `+`/`-` with intervals and `AT TIME ZONE`), `UNNEST ... [WITH ORDINALITY]`, `TABLE (...)`, `LATERAL (...)`, `TABLESAMPLE`, data-change delta tables (`FINAL|NEW|OLD TABLE ( <dml statement> ) [ AS alias [ (cols) ] ]`), and `MATCH_RECOGNIZE`.

### 📝 Data Manipulation (DML)
- `INSERT INTO ... VALUES / SELECT`
- `UPDATE [ <table> ] ... SET ... WHERE`, including `WHERE CURRENT OF <cursor>` (positioned and preparable-dynamic variants).
- `DELETE [ FROM <table> ] ... WHERE`, including `WHERE CURRENT OF <cursor>` (positioned and preparable-dynamic variants).
- `MERGE INTO ... USING ... ON ...`
- `TRUNCATE TABLE`
- `<target table>` accepts `ONLY ( <table> )`: `UPDATE ONLY (t) ...`, `DELETE FROM ONLY (t) ...`, `MERGE INTO ONLY (t) ...`.
- `FOR PORTION OF <period> FROM <point in time> TO <point in time>` for `UPDATE` / `DELETE`.

### ↕️ Cursors & Locators
- `DECLARE <cursor> [SENSITIVE|INSENSITIVE|ASENSITIVE] [SCROLL|NO SCROLL] CURSOR [WITH|WITHOUT HOLD] [WITH|WITHOUT RETURN] FOR <query expression> [FOR READ ONLY | FOR UPDATE [OF <columns>]]`.
- Dynamic cursors: `DECLARE <cursor> [<cursor properties>] FOR <statement name>`, `ALLOCATE <extended cursor name> [<cursor properties>] FOR <extended statement name>`, `ALLOCATE <cursor name> [CURSOR] FOR PROCEDURE <specific routine designator>`.
- `OPEN <cursor> [USING <args> | USING [SQL] DESCRIPTOR <descriptor>]`, `FETCH` (with `NEXT`/`PRIOR`/`FIRST`/`LAST`/`ABSOLUTE`/`RELATIVE`) `<cursor> INTO <args> | INTO [SQL] DESCRIPTOR <descriptor>`, `CLOSE`.
- `SELECT ... INTO ...` (single row).
- `DECLARE LOCAL TEMPORARY TABLE ... [ON COMMIT PRESERVE|DELETE ROWS]`.
- `FREE LOCATOR` / `HOLD LOCATOR`.
- Routine parameter defaults accept `DESCRIPTOR ( <column name> [ <data type> ] , ... )`.

### 🏗️ Data Definition (DDL)
- `CREATE TABLE` — `<table scope>` (`GLOBAL`/`LOCAL TEMPORARY`), typed tables (`OF <udt> [UNDER <supertable>]`), `<like clause>` (`LIKE <table> [INCLUDING|EXCLUDING IDENTITY|DEFAULTS|GENERATED]`), table period elements (`PERIOD FOR SYSTEM_TIME | <name> (begin, end)`), `WITH SYSTEM VERSIONING`, `ON COMMIT PRESERVE|DELETE ROWS`, and `<as subquery clause>` (`AS <query> WITH [NO] DATA`).
- Column definitions: data types and domain names; `<default clause>` restricted to the grammar's `<default option>` (`<literal>`, datetime value functions, `USER`/`CURRENT_USER`/…, `NULL`, `ARRAY[]`/`MULTISET[]`); `GENERATED {ALWAYS|BY DEFAULT} AS IDENTITY [(options)]`; generated columns (`GENERATED ALWAYS AS (expr)`); `GENERATED ALWAYS AS ROW START|END`; `CONSTRAINT <name> <column constraint> [<constraint characteristics>]`; and a column `COLLATE` clause.
- Table constraints: `PRIMARY KEY (...)`, `UNIQUE (...)`, `FOREIGN KEY (...) REFERENCES ... [ON UPDATE|ON DELETE ...]`, `CHECK (...)` — each with an optional `CONSTRAINT <name>` and `[<constraint characteristics>]` (`INITIALLY DEFERRED|IMMEDIATE`, `[NOT] DEFERRABLE`, `[NOT] ENFORCED`).
- `CREATE VIEW` — `CREATE [RECURSIVE] VIEW`, view column list, `OF <udt> [UNDER <table>]`, and `WITH [CASCADED|LOCAL] CHECK OPTION`.
- `CREATE SCHEMA` (character set / path, nested schema elements), `CREATE DOMAIN` / `ALTER DOMAIN` / `DROP DOMAIN`, `CREATE CHARACTER SET`, `CREATE COLLATION`, `CREATE TRANSLATION` / `DROP TRANSLATION`, `CREATE ASSERTION` / `DROP ASSERTION`.
- `CREATE CAST` / `DROP CAST`, `CREATE ORDERING` / `DROP ORDERING`, `CREATE TRANSFORM` / `ALTER TRANSFORM` / `DROP TRANSFORM`, `CREATE TYPE` / `ALTER TYPE` / `DROP TYPE` (with attributes, methods, `REF`/`CAST` options).
- `CREATE SEQUENCE` / `ALTER SEQUENCE` / `DROP SEQUENCE`; `CREATE PROCEDURE` / `CREATE FUNCTION` (parameter modes, `AS LOCATOR`, `TABLE`/`DESCRIPTOR` parameter types, `<returns table type>`, `<result cast>`, routine characteristics), `ALTER ROUTINE`, `DROP ROUTINE`; `CREATE TRIGGER` / `DROP TRIGGER`.
- `ALTER TABLE` — the full `<alter table action>` set: `ADD [COLUMN]`, `DROP [COLUMN] ... CASCADE|RESTRICT`, `ALTER [COLUMN]` (`SET`/`DROP DEFAULT`, `SET`/`DROP NOT NULL`, `ADD`/`DROP SCOPE`, `SET DATA TYPE`, `SET GENERATED ...`, `RESTART`/`SET <sequence option>`, `DROP IDENTITY`, `DROP EXPRESSION`), `ADD`/`ALTER`/`DROP CONSTRAINT`, `ADD`/`DROP PERIOD FOR ...`, and `ADD`/`DROP SYSTEM VERSIONING`.
- `DROP` (schema, table, view, domain, collation, character set, translation, assertion, cast, ordering, transform, routine, trigger, type) with `<drop behavior>`.
- `GRANT` / `REVOKE` (privileges and roles, including `ALL PRIVILEGES`, `SELECT (method list)`, `WITH GRANT OPTION`, `WITH ADMIN OPTION`).

### 🧩 Dynamic SQL, Diagnostics, Connections & Sessions
- Dynamic SQL: `PREPARE` / `EXECUTE` / `EXECUTE IMMEDIATE` / `DEALLOCATE PREPARE`, `DESCRIBE`, descriptor statements (`ALLOCATE` / `DEALLOCATE` / `GET` / `SET` / `COPY DESCRIPTOR`), and the dynamic cursor statements.
- `GET DIAGNOSTICS`.
- `CONNECT` / `SET CONNECTION` / `DISCONNECT`.
- Session management: `SET SESSION CHARACTERISTICS`, `SET SESSION AUTHORIZATION`, `SET ROLE`, `SET TIME ZONE`, `SET CATALOG`, `SET SCHEMA`, `SET NAMES`, `SET PATH`, `SET TRANSFORM GROUP`, `SET SESSION COLLATION`.

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

let sql = "SELECT name, SUM(salary) OVER (PARTITION BY dept) FROM employees WHERE active = TRUE;"

match SqlParser.parse sql with
| Ok stmt ->
    printfn "Successfully parsed statement of kind: %A" stmt.Kind
| Error (ParseError(msg, pos)) ->
    printfn "Parse error: %s at line %d, col %d" msg pos.Line pos.Column
```

The trailing `<semicolon>` is required — `parse` accepts a 22.1 `<direct SQL statement> ::= <directly executable statement> <semicolon>`.

## Project Structure

- `Ast.fs`: Defines the Abstract Syntax Tree (AST) for SQL statements and expressions.
- `Lexer.fs`: Contains the lexing logic, reserved words, and literal parsers.
- `Types.fs`: Parsers for SQL data types.
- `ExpressionParser.fs`: Handles operator precedence and expression parsing.
- `QueryParser.fs`: Main logic for `SELECT` queries and set operations.
- `DmlParser.fs`: Parsers for `INSERT`, `UPDATE`, `DELETE`, `MERGE`.
- `DdlParser.fs`: Parsers for schema modification statements (including `GRANT`/`REVOKE`/role).
- `TypeParser.fs`: Parsers for `CREATE`/`ALTER TYPE` and their attributes/methods.
- `RoutineParser.fs`: Parsers for `CREATE PROCEDURE`/`FUNCTION`/`TRIGGER`, `ALTER ROUTINE`.
- `ControlParser.fs`: Parsers for `CALL` and `RETURN`.
- `CursorParser.fs`: Parsers for cursor declarations, `OPEN`/`FETCH`/`CLOSE`, cursor `SELECT ... INTO`, temporary table declarations, and locator statements.
- `DynamicParser.fs`: Parsers for dynamic SQL (`PREPARE`, `EXECUTE`, descriptors, dynamic cursors).
- `DiagnosticsParser.fs`: Parser for `GET DIAGNOSTICS`.
- `ConnectionParser.fs`: Parsers for `CONNECT`, `SET CONNECTION`, `DISCONNECT`.
- `SessionParser.fs`: Parsers for session-management statements (`SET SESSION`, `SET ROLE`, `SET SCHEMA`, …).
- `TransactionParser.fs`: Parsers for transaction statements.
- `SqlParser.fs`: Main entry point and `WITH` clause handling.

## Running Tests

The project uses xUnit for testing. To run the tests:

```bash
dotnet test
```

## License

This project is licensed under the MIT License.
