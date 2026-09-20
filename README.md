# SQL Parser in F#

A parse-only SQL library implemented in F# with [FParsec](https://www.quanttec.com/fparsec/), aligned with the **SQL:2016 foundation grammar** (ISO/IEC 9075-2:2016). The [local copy of the grammar](sql-2016-grammar.txt) — from the [sql-overview](https://github.com/JakeWheat/sql-overview) project, annotated with clause-number provenance — is the design document: every production in `SqlParser/*.fs` cites the rule it implements as `// <clause> <rule name>`, and `RuleNumberingTests.fs` enforces the citations.

The library turns a statement into an AST (`Result<Statement, ParseError>`); it performs no name resolution, type checking or evaluation, and never throws on malformed input.

## Quick start

```bash
dotnet build   # .NET 10 (net10.0)
dotnet test    # xUnit v3 + coverlet
```

## Usage

Both entry points take the SQL text and return `Result<Statement, ParseError>`. The trailing `<semicolon>` is required.

- **`SqlParser.parse`** — a 22.1 `<direct SQL statement>`: the directly executable families only (`SELECT`, searched `INSERT` / `UPDATE` / `DELETE`, `MERGE`, `TRUNCATE`, `WITH ... <query>`, `<temporary table declaration>`, and schema, transaction, connection and session statements). The positioned forms of `UPDATE` (14.13) and `DELETE` (14.8) are rejected here.
- **`SqlParser.parseStatement`** — the superset admitted by 13.4 `<SQL procedure statement>`: everything `parse` accepts, plus `DECLARE CURSOR`, `OPEN` / `FETCH` / `CLOSE`, `SELECT ... INTO`, positioned `DELETE` / `UPDATE`, `FREE` / `HOLD LOCATOR`, `CALL` / `RETURN`, `GET DIAGNOSTICS` and all dynamic-SQL statements.

```fsharp
open SqlParser

let sql = "SELECT name, SUM(salary) OVER (PARTITION BY dept) FROM employees WHERE active = TRUE;"

match SqlParser.parse sql with
| Ok stmt ->
    printfn "Successfully parsed statement of kind: %A" stmt.Kind
| Error (ParseError(msg, pos)) ->
    printfn "Parse error: %s at line %d, col %d" msg pos.Line pos.Column
```

```fsharp
match SqlParser.parseStatement "DECLARE cur CURSOR FOR SELECT a FROM t;" with
| Ok stmt -> printfn "Successfully parsed statement of kind: %A" stmt.Kind
| Error (ParseError(msg, pos)) ->
    printfn "Parse error: %s at line %d, col %d" msg pos.Line pos.Column
```

A `Statement` carries a `Kind` (`StatementKind`) mirroring the grammar's alternatives and a `Pos` (`Line` / `Column`) for error reporting; see `SqlParser/Ast.fs`.

## Documentation

| Document | Contents |
|----------|----------|
| [docs/architecture.md](docs/architecture.md) | Module layout, parse pipeline, entry points, forward-reference wiring, AST and keyword strategy, coverage table, and the supported SQL features. |
| [docs/trade-off.md](docs/trade-off.md) | Design decisions and rejected alternatives. |
| [docs/gotchas.md](docs/gotchas.md) | Recurring F#/FParsec/grammar pitfalls. |
| [AGENTS.md](AGENTS.md) | Conventions for AI agents working in this repo (rule numbering, definition order, coding and testing rules). |

## License

This project is licensed under the MIT License.
