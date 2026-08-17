# Gotchas & Common Mistakes

## F# union-case name clashes

Many SQL keywords map to natural F# union-case names that already exist elsewhere in `Ast.fs`. Adding a new DU case with a clashing name silently resolves to the wrong type (usually the most recently defined). Known clashes:

| Case name | Clashes between |
|-----------|-----------------|
| `Select`, `Insert`, `Update`, `Delete` | `StatementKind` vs `PrivilegeAction` |
| `Unique` | `ExpressionKind` (subquery predicate) vs `TableConstraint` |
| `DropRole` | `StatementKind` vs `DropStatement` |
| `SetDefault` | `ReferentialAction` vs `ColumnAlteration` |

**Fix:** qualify with the type name at the use site, e.g. `PrivilegeAction.Select`, `TableConstraint.Unique`, `DropStatement.DropRole`, `ReferentialAction.SetDefault`. Always qualify new case names that could clash with existing ones.

## `pKeyword` returns `Parser<string, unit>`, not `Parser<unit, unit>`

`pKeyword` keeps the matched string as its result. This is normally harmless because it is combined with `>>.`/`.>>`/`>>%`. But in an `<|>` (or other places where both branch result types must agree), mix it with a `Parser<unit, _>` and you get a type error. Coerce branches with `|>> ignore`.

## Nested `Some <record>` patterns in tests

F#'s pattern parser is finicky about `Some <record-pattern>` (e.g. `Some { Kind = ... }`) when it appears inside a union-case tuple pattern. It can fail with "Unexpected start of structured construct in pattern". Prefer binding to a variable and asserting on fields, e.g. match `Some name` then check `name.Kind`, instead of inlining the record pattern.

## `char list` to `string` conversion

`List.toArray |> string` on a `char list` yields `"System.Char[]"`, not the character contents. Use `System.String.Concat` (see `hexToInt32` in `Lexer.fs`). The old form made `Convert.ToInt32(..., 16)` throw `FormatException`, crashing every Unicode escape (`U&'\0041'`, `U&"\0041"`).

## Data type left recursion

`pCollectionType` must NOT be inside the same `choice` as the base type parser it calls, or unknown types (`MyType`, typos) trigger infinite recursion → `StackOverflowException`. Split into `pDataTypeElement` (all non-collection types) + `pDataType` = `pCollectionType <|> pDataTypeElement` (`Types.fs`).

## `decimal` overflow in approximate literals

`1E400` / `1.23E400` overflowed `decimal` with `OverflowException`. Clamp the exponent to ~28 (decimal max) before computing (`pApproximateNumericLiteral` in `Lexer.fs`).

## Time seconds must not accept repeated dots

`TIME '12:00:00.5.5'` used to throw `FormatException` from `decimal`. Parse seconds as digits + optional `.fraction` only, so invalid input fails cleanly (`pTimeValue` in `Lexer.fs`).

## Record patterns: no `; _`

F# record patterns ignore omitted fields automatically — do NOT write `{ Kind = X; _ }` (syntax error: "Unexpected symbol '_' in pattern"). Use `{ Kind = X }`.

## FParsec combinator precedence

`<|>` binds TIGHTER than `.>>`/`.>>.`/`>>.` in FParsec (all `>>.`-family operators are left-associative at equal precedence). Forgetting this produces wrong tuple arities (e.g. `pKeyword "X" >>. p >>. q` gives `p * q`, not a 3-tuple).

## Record type inference: `{ Kind = ...; Pos = ... }` may resolve to `Statement`

Both `Expression` and `Statement` have `Kind` + `Pos` fields. When a record literal like `{ Kind = Literal(Number 1m); Pos = ... }` is bound with `let` and its expected type isn't known at the binding site, F# can infer `Statement` (whose `Kind` is `StatementKind`) and report "expected StatementKind but here has type ExpressionKind". Fix: annotate explicitly, e.g. `let defaultCount: Expression = { ... }` (see `pFetch` in `QueryParser.fs`).

## `sepBy1` does not backtrack a consumed separator

`sepBy1 p sep` fails if `sep` succeeds but the following `p` fails — the consumed separator is not rolled back. For `t.*` (identifier chain ending in `*`), `sepBy1 pIdentifier (token ".")` consumes `t.` then fails on `*`. Use `p .>>. many (attempt (sep >>. p))` instead so a trailing separator is optional (see the qualified-star branch of `pColumnSource` in `QueryParser.fs`).

## `override` is a reserved F# keyword

`override` cannot be used as a lambda parameter or local binding name (used for `OVERRIDING` in `INSERT`). F# reports "Unexpected keyword 'override' in lambda expression". Rename the variable (e.g. `ovr`) — see `pInsertStatement` in `DmlParser.fs`.

## Record type inference ambiguity with identical field shapes

When two record types have the same field names/types, an unannotated record literal resolves to the *most recently defined* type. Adding `Columns: Expression list option` to `CreateViewStatement` made it identical to `Cte` (`{ Name: Expression; Columns: Expression list option; Query: Query }`), so `pCte`'s record literal silently became `CreateViewStatement` and broke `WithQuery`. Fix: qualify the first field, e.g. `{ Cte.Name = name; ... }` (see `pCte` in `ExpressionParser.fs`).

## `base` is a reserved F# keyword

`base` cannot be used as a lambda parameter or local binding (used for the table primary in `TABLESAMPLE`). F# reports "Unexpected keyword 'base' in pattern". Rename the variable (e.g. `tbl`) — see `pTablePrimary` in `QueryParser.fs`.

## `TableSource` record inference may resolve to `Statement`

Like `Expression`, `TableSource` has `Kind` + `Pos` fields, so an unannotated `{ Kind = TableSample(...); Pos = ... }` literal can be inferred as `Statement`. Qualify the first field: `{ TableSource.Kind = ...; Pos = ... }` (see the `TABLESAMPLE` suffix in `pTablePrimary`).

## `Range` clashes between `IntervalQualifier` and `WindowFrameUnit`

`Range` is a case of both `IntervalQualifier` (with arguments) and `WindowFrameUnit` (no arguments). In `Lexer.fs`, an unqualified `Range(Year, Month)` pattern resolves to `WindowFrameUnit.Range` and fails with "This union case does not take arguments". Qualify as `IntervalQualifier.Range(...)` (see `isValidIntervalValue` in `Lexer.fs`).

## `pint32` accepts a leading `-` in date components

`pDateValue` previously used `pint32`, so `DATE '2023--1-5'` parsed with a negative month. Use `pUnsignedInteger` (which rejects `-`) and validate month/day ranges explicitly (see `pDateValue` in `Lexer.fs`).

## Value restriction on parsers built with `withExprPosition`

A `let`-bound parser whose type isn't pinned to `Parser<_, unit>` can hit F#'s value restriction ("The value 'p' has an inferred generic function type"). `withExprPosition` is generic over the user-state type, so `let pDefaultValue = pKeyword "DEFAULT" >>% Default |> withExprPosition` fails. Fix: annotate explicitly, e.g. `let pDefaultValue: Parser<Expression, unit> = ...` (see `ExpressionParser.fs`).
