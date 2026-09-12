# SQL Parser in F#

A SQL parser implemented in F# using [FParsec](https://www.quanttec.com/fparsec/), aligned with the **SQL-2016 foundation grammar**.
This implementation is based on the [SQL-2016 Foundation Grammar](https://raw.githubusercontent.com/JakeWheat/sql-overview/refs/heads/master/sql-2016-foundation-grammar.txt) provided by the [sql-overview](https://github.com/JakeWheat/sql-overview) project
(a local copy with clause-number provenance is kept at [`sql-2016-grammar.txt`](sql-2016-grammar.txt)).

## Usage

```fsharp
open SqlParser

let sql = "SELECT name, SUM(salary) OVER (PARTITION BY dept) FROM employees WHERE active = TRUE;"

match SqlParser.parse sql with
| Ok stmt ->
    printfn "Successfully parsed statement of kind: %A" stmt.Kind
| Error (ParseError(msg, pos)) ->
    printfn "Parse error: %s at line %d, col %d" msg pos.Line pos.Column
```

The trailing `<semicolon>` is required.
There are two entry points, both taking the SQL text and returning `Result<Statement, ParseError>`:

- **`SqlParser.parse`** — a 22.1 `<direct SQL statement>`: the directly executable statement families (`SELECT`, searched `INSERT`/`UPDATE`/`DELETE`, `MERGE`, `TRUNCATE`, `WITH ... <query>`, schema, transaction, connection and session statements).
- **`SqlParser.parseStatement`** — a superset covering 13.4 `<SQL procedure statement>` as well: `DECLARE CURSOR`, `OPEN`/`FETCH`/`CLOSE`, `SELECT ... INTO`, positioned `DELETE`/`UPDATE`, locators, `CALL`/`RETURN`, `GET DIAGNOSTICS` and all dynamic SQL.

```fsharp
match SqlParser.parseStatement "DECLARE cur CURSOR FOR SELECT a FROM t;" with
| Ok stmt -> printfn "Successfully parsed statement of kind: %A" stmt.Kind
| Error (ParseError(msg, pos)) ->
    printfn "Parse error: %s at line %d, col %d" msg pos.Line pos.Column
```

## Build & Test

The project uses xUnit v3. To build and run the tests:

```bash
dotnet test
```

## Documentation

| Document | Contents |
|----------|----------|
| [docs/architecture.md](docs/architecture.md) | Module layout, parse pipeline, entry points, AST and keyword strategy, and the full list of supported SQL features. |
| [docs/trade-off.md](docs/trade-off.md) | Design decisions and rejected alternatives. |
| [docs/gotchas.md](docs/gotchas.md) | Recurring F#/FParsec/grammar pitfalls. |

## License

This project is licensed under the MIT License.
