# Gotchas & Common Mistakes

Recurring pitfalls in this codebase. Entries are the ones worth knowing *before*
writing code here; [architecture.md](architecture.md) covers the design and
[trade-off.md](trade-off.md) the rationale behind the strictness.

## F# language pitfalls

### Union-case names clash — qualify at the use site

Many SQL keywords map to natural F# case names that already exist elsewhere in
`Ast.fs`. An unqualified name silently resolves to the wrong type (usually the
most recently defined).

| Case name | Clashes between |
|-----------|-----------------|
| `Select`, `Insert`, `Update`, `Delete` | `StatementKind` vs `PrivilegeAction` |
| `Unique`, `PrimaryKey`, `Check` | `ExpressionKind`/`ColumnConstraintKind` vs `TableConstraint` |
| `DropRole`, `DropType` | `StatementKind` vs `DropStatement` |
| `SetDefault`, `DropDefault`, `AddConstraint`, `DropConstraint` | `ReferentialAction`/`ColumnAlteration`/`AlterTableAction`/`DomainAlteration` |
| `Final` | `ResultOption` vs `TypeOption` vs `RunningOrFinal` |
| `Range` | `IntervalQualifier` vs `WindowFrameUnit` |
| `BeginAtomic` | `RoutineBody` vs `TriggeredStatement` |
| `First`, `Last`, `Next` | `Direction` vs `FirstOrLast`/`PrevOrNext` |

Always qualify (`PrivilegeAction.Select`, `TableConstraint.Unique`,
`IntervalQualifier.Range(…)`, `TypeOption.Final false`, `RunningOrFinal.Final`).
`ColumnConstraintKind.Null` was *removed* for this reason (it clashed with
`Literal.Null`).

### Reserved F# keywords cannot be identifiers or lambda parameters

`override`, `base`, `default`, `when`, `constraint` are reserved (some are
"reserved for future use"). Rename the binding: `ovr`, `tbl`, `defaultVal`,
`whenCond`, `typeConstraint`.

### `char list` → `string` is not `string`

`List.toArray |> string` on a `char list` yields `"System.Char[]"`, not the
characters. Use `System.String.Concat`.

### `List.fold` needs all three arguments

`p |>> List.fold (fun acc x -> …)` type-checks as a partial application and
yields a function, not an `Expression`. Bind the pair and fold explicitly:
`|>> fun (first, rest) -> rest |> List.fold folder first`.

### Record types with identical field shapes are inference-ambiguous

`Expression`, `TableSource` and `Statement` all have `Kind` + `Pos`, so an
unannotated record literal like `{ Kind = Literal(Number 1m); Pos = … }` can be
inferred as `Statement`. Adding `Columns` to `CreateViewStatement` made it
identical to `Cte`, so `pCte`'s literal silently became a `CreateViewStatement`.

**Fix:** qualify the first field (`{ Expression.Kind = …; … }`,
`{ TableSource.Kind = …; … }`, `{ Cte.Name = name; … }`) or annotate the binding
(`let defaultCount: Expression = …`).

### Value restriction: named parsers need an explicit type

A `let`-bound parser whose type is not pinned hits the value restriction
(`val pLeftBrace: Parser<char,'_a>`), because `pchar` returns `Parser<char,'u>`.
Annotate: `let pLeftBrace: Parser<char, unit> = pchar '{'` (use
`Parser<string, unit>` for `pstring`). Inline `token (pstring "{")` is unaffected.

### Record patterns: alignment and nesting rules

- Omitted fields are ignored — never write `{ Kind = X; _ }` (syntax error).
- Newline-separated fields must start at the **same column** (FS0010). Extract the
  record and match on it separately, or bind the payload and assert on fields.
- A `[` for a list value must start on the **same line** as the `=`:

  ```fsharp
  // FAILS
  | CreateProcedure { Parameters =
                          [ { Mode = Some In } ] } -> ()
  // WORKS
  | CreateProcedure { Parameters = [ { Mode = Some In } ] } -> ()
  ```

- An inline list-of-record pattern nested inside `Some(...)` fails even then
  (FS0010). Bind it first:
  `| CreateType { Representation = Some(MemberList attrs) } -> match attrs with …`.
- Deeply nested multi-line record patterns are fragile. Bind intermediate values
  and assert with `Assert.Equal` instead of matching the whole shape.

### Functions cannot appear in patterns

`Parameters = [ { ParameterType = dataTypeParam Integer } ]` does not compile — a
record pattern may contain only literals and constructors. Bind
(`Parameters = [ param ]`) and assert, or match the DU case directly.

## FParsec combinator pitfalls

### Operator precedence

`<|>` binds **tighter** than the `.>>`/`.>>.`/`>>.` family (which are
left-associative at equal precedence), and `>>%` shares F#'s precedence group
with `<|>` (both start `<`/`>`). Consequences:

- `pKeyword "X" >>. p >>. q` yields `p * q`, not a 3-tuple.
- `p >>% x <|> q >>% y` parses as `((p >>% x) <|> q) >>% y`. Parenthesise each
  alternative, or use `choice [ … ]`.
- `attempt pTargetTable |>> DmlTarget.TableTarget <|> preturn …` parses as
  `attempt pTargetTable |>> (DmlTarget.TableTarget <|> preturn …)`. Parenthesise
  the constructor branch — `|>>` binds looser than `<|>`.

### `.>>.` produces nested 2-tuples

`p1 .>>. p2 .>>. p3` is `(a * b) * c`, so the pattern must be
`fun ((x, y), z) -> …`. The compiler reports "Expecting a tuple of length 2 …
but given a tuple of length 3".

### `>>.` silently discards a result

`pKeyword "A" >>. opt p >>. q` drops the `opt`'s value. Use `.>>.` when the value
must survive: `>>. opt p .>>. q`. This produced the `BETWEEN` symmetry bug, where
the parser compiled but never saw the qualifier.

A `>>.`-chain also returns the *last* keyword's string, so
`pKeyword "DROP" >>. pKeyword "SYSTEM" >>. pKeyword "VERSIONING" .>>. pDropBehavior`
yields `string * bool`. Use `>>.` (not `.>>.`) for the final keyword.

### `|>>` swallows a following `>>=` into the lambda body

```fsharp
|>> fun x -> { … } >>= f    // parses as |>> (fun x -> { … } >>= f)
```

Parenthesise the lambda: `|>> (fun x -> { … }) >>= f`. Also note an `|>>`
projection can never fail — validation must use `>>=` and `fail`, and the
`StatementKind` wrapper must come *after* the validation step.

### `opt` does not backtrack partial consumption

`opt p` is `(p |>> Some) <|> preturn None`. If `p` consumes input and then fails,
`<|>` cannot recover. Wrap optional multi-keyword clauses in `attempt`:
`opt (attempt (pKeyword "WITH" >>. pKeyword "HIERARCHY" >>. pKeyword "OPTION"))`.
This is required for `pIdentitySpec`, `pCheckOption`, `pLockingClause`,
`pCursorHoldability`, `pCursorReturnability`, the `FOR UPDATE OF` group, and the
`WITH`-prefixed clauses generally.

### `notFollowedBy` makes the failure fatal — wrap in `attempt`

When the inner parser succeeds, `notFollowedBy p` marks the failure as fatal, and
`opt` does not catch fatal errors. Wrap with `attempt` so `opt` can return `None`
(see `pExistingWindowName` / `pWindowFrame`).

### `attempt` is required inside postfix loops

A branch that consumes a token and then fails aborts the enclosing `many` rather
than stopping it. `many (attempt pDereferenceReference <|> attempt pMethodOrFieldReference)`
and `many (attempt pPredicate <|> … <|> attempt pTimeZoneSuffix)` are the canonical
examples.

### `sepBy1` does not backtrack a consumed separator — and must not re-parse `p`

`sepBy1 p sep` is `p .>>. many (sep >>. p)`, so it fails if `sep` succeeds and `p`
then fails. Use `p .>>. many (attempt (sep >>. p))` when a trailing separator is
optional (e.g. `t.*` in `<derived column>`).

Also do not put `p` in `sep`: `sepBy1 pIdentifier (token "." >>. pIdentifier)`
double-parses. The separator should consume only `.`:
`pIdentifier .>>. many (attempt (token "." >>. pIdentifier .>>? notFollowedBy "("))`.

### Whitespace is not skipped automatically

`pKeyword` and most token parsers do not skip *leading* whitespace, and several
parsers do not consume *trailing* whitespace: `pchar '*'`, `pUnsignedInteger`,
and other raw character/digit parsers. Follow them with `.>> ws` / `token` (e.g.
`pchar '*' .>> ws`, `pUnsignedInteger .>> ws`), otherwise the next `pKeyword`
fails on the space.

`pKeyword` also refuses to match a keyword immediately followed by an identifier
character, which is why `SYSTEM` does not match `SYSTEM_TIME` and `C` does not
match `COBOL`.

### `pKeyword` returns `Parser<string, unit>`, not `Parser<unit, unit>`

Usually harmless (it feeds `>>.`/`.>>`/`>>%`), but in an `<|>` where both branches
must agree, mix it with a `Parser<unit, _>` and you get a type error. Coerce with
`|>> ignore` or `>>% ()`.

### There is no `pipe6`

FParsec only provides `pipe2`–`pipe5`. Split a six-way pipeline into `pipe5` plus
`pipe2` (see `pSelectBase` + `pQuerySpecification`).

## Definition order, forward references and dispatch

### Define before use, or use a forward reference

F# requires definitions before use. Adding a parser dependency means moving the
dependency *above* its user (`pIdentitySpec` and `pSequenceOption` before
`pColumnDefinition`; `pTransformsToBeDropped` before `pDropStatement`;
`pConstraintEnforcement` next to `pDropBehavior`). Cross-module recursion uses
`createParserForwardedToRef`.

### Forward-reference wiring happens in `SqlParser.fs`

`pSchemaElementImpl`, `pDataChangeStatementRef`, `pRoutineBodyStatementRefImpl`
and `pStatementRef` are assigned in `SqlParser.fs` *after* the target is defined.
Wiring them inside their own module fails (the target is compiled later). Use the
forwarding **parser**, never `…Ref.Value`, inside a combinator — reading `.Value`
at module-initialisation time captures FParsec's dummy parser.

### An optional-looking sub-rule must not match empty input

Transcribing `A | B...` as `opt A .>>. many B` makes the parser succeed on empty
input because both `opt` and `many` allow zero matches, silently shadowing later
alternatives. Transcribe the alternatives literally, e.g. 11.20:
`attempt (pSetIdentityColumnGeneration .>>. many option) <|> (many1 option)`.

### Dispatch order is load-bearing

Because `pKeyword` matches non-reserved words too, `choice` order keeps prefixes
apart. The pairs that must be ordered:

- `ALTER TYPE` **before** `ALTER ROUTINE`.
- `DROP TYPE` **after** `DROP ROUTINE`.
- `EXECUTE IMMEDIATE` **before** `EXECUTE <name>`.
- `DECLARE LOCAL TEMPORARY TABLE` **before** `DECLARE <cursor>`.
- `SELECT ... INTO` **before** `pQuery`.
- The PTF `DESCRIBE WITH …` body **before** the generic statement branch.
- 20.17 `ALLOCATE … FOR <statement>` **before** 20.18 `ALLOCATE … FOR PROCEDURE`.
- The `(VALUES …)` branch **before** the subquery branch in `pTablePrimary`.
- `SELECT ( <privilege method list> )` **before** `SELECT [ <column list> ]`.
- The `FINAL|NEW|OLD TABLE` alternative **before** the plain table alternative.
- The 6.26 navigation parser **before** `pRoutineInvocation` and
  `pColumnReferenceExpr`, tried Compound → Logical → Physical.
- 6.37's interval alternative **before**, and inside the same `attempt` as, the
  plain parenthesized `pExpression` branch.
- The NESTED branch before the regular-column branch in `pJsonTableColumn`.

### Do not reuse a parser whose grammar does not cover the slot

`INSERT` must keep `pQualifiedNameExpr` for its target: 14.11 `<insertion target>`
is a plain `<table name>` with no `ONLY` form, so `pTargetTable` would wrongly
accept `INSERT INTO ONLY (t) …`. Likewise `pPartitionBy` is column references
only.

The omitted DML target relies on `SET` and `WHERE` being reserved words —
`pTargetTable` fails cleanly on them. Do not "fix" a missing table name by
accepting `pIdentifierRaw`, which would parse `UPDATE SET …` with `SET` as the
table.

## Reserved words and keywords

### `pRoutineInvocation` only accepts reserved *function* keywords

`pReservedFunctionName` is a whitelist (`COUNT`, `ROW_NUMBER`,
`PERCENTILE_CONT`, `ABS`, …) plus non-reserved/delimited identifiers. Therefore:

- Reserved words that start a dedicated construct (`EXISTS`, `UNIQUE`,
  `JSON_EXISTS`, `PERIOD`, `VALUE_OF`) must stay off the whitelist, and their
  dedicated parsers must be listed **before** `pRoutineInvocation` in
  `pValueExpressionPrimary`.
- **Adding a reserved-name built-in requires adding it to `functionKeywords`**,
  or `SELECT ABS(x)` / `SELECT ROW_NUMBER() OVER (…)` fail with "reserved word".
  Non-reserved names (`foo(...)`) are unaffected.

### `pIdentifierRaw` is too permissive for closed enumerations

Item names that are reserved words (`NUMBER`, `ROW_COUNT`, `DATA`, …) cannot use
`pIdentifier`, but `pIdentifierRaw` also accepts `ALL`, `SELECT`, …. Use an
explicit `choice [ pKeyword "…" ]` enumeration (diagnostics/descriptor item names,
`<language name>`, `<parameter style>`).

### Non-reserved keywords need explicit handling

`TYPE`, `UNDER`, `OVERRIDING`, `INSTANCE`, `CONSTRUCTOR`, `INSTANTIABLE`,
`FINAL`, `OPTIONS`, `DERIVED`, `GENERATED`, `DESCRIBE`, `PRIVATE`, `FULFILL`,
`FINISH`, `SECURITY`, `DISPATCH`, `GENERAL`, `IMPLEMENTATION`, `DEFINER`,
`INVOKER`, `TRANSFORM`, `STYLE`, `LOCATOR`, `PRESERVE`, `TEMPORARY`, `EXTENDED`,
`ATTRIBUTES` are **not** reserved. Consequences:

- A `<typed table element>` cannot be dispatched on its first token —
  `<column options>` is only recognisable once the mandatory `WITH OPTIONS` has
  been consumed, so every alternative needs `attempt`.
- `DESCRIPTOR` must be tried before `<data type>` in `<parameter type>`,
  otherwise `(d DESCRIPTOR)` parses as a parameter of UDT type `DESCRIPTOR`.
- `INSTANCE`/`CONSTRUCTOR` must be `attempt`ed in `pMethodKind` so they can still
  be identifiers elsewhere.

`LOG` **is** reserved, so `INSERT INTO log …` fails — use another table name in
tests. `METHOD`, `REF`, `OUT`, `SYSTEM_TIME`, `VALUE_OF`, `DESCRIBE`, `START`,
`STATIC`, `GROUP`, `PARAMETER`, `SQL`, `EXTERNAL`, `DEFAULT` are reserved too.

### `<scope option>` is a 5.4 rule, `<semicolon>` a 5.1 rule

`RuleNumberingTests` matches a citation against the clauses that mention the name,
so `<scope option>` must cite 5.4 (not 20.15/20.17) and `<semicolon>` must cite
5.1 (not 5.2).

## Grammar-specific traps

### `pExpression` includes boolean operators

In slots where `AND` must not be consumed as a boolean operator, use a
boolean-free parser: `<point in time>` / `FOR PORTION OF` use the 6.35 datetime
parser, and the JSON argument slots use `pValueExpressionNoBoolean`
(`opp.ExpressionParser` without boolean operators). That parser is a forward ref
because the JSON parsers are defined before `opp` is built.

### `pDataType` accepts any identifier as a user-defined type

Any identifier is a valid UDT name, so `NESTED PATH '$.items'` would be read as a
column named `NESTED` of type `PATH`. The UDT branch ends with
`.>>? notFollowedBy pIdentifier` so a name immediately followed by another
identifier is rejected; the NESTED branch is also tried first.

### Recursive/ambiguous expression forms

- `JSON_ARRAY(NULL ON NULL)` — `sepBy pExpression ","` greedily consumes `NULL`,
  leaving `ON NULL` unconsumed. Guard each element with
  `pExpression .>>? notFollowedBy (attempt (pKeyword "ON" >>. pKeyword "NULL"))`.
- `pExplicitRowValueConstructor` (7.1) needs `attempt` so `(a)` can fall through
  to the plain parenthesized expression branch.
- 6.44 `SET ( … )` and `pMultisetValueExpression` are mutually recursive; the
  latter is a forward ref wired after `pValueExpressionPrimary`.
- `pIntervalSign` must not swallow the `-` of `->`: guard with
  `notFollowedBy (pchar '>')`.
- `<collection type>` suffixes must be folded (`pDataTypeElement .>>. many (…)
  |> List.fold`), never parsed self-referentially — `pDataType ARRAY …` is left
  recursive and recurses forever.
- `containsStandaloneQuantifiedSubquery` has a `| _ -> false` catch-all, so a new
  `ExpressionKind` case that can hold an `Expression` will silently escape the
  standalone-`ANY` rejection. **Add a branch for every new case** — the compiler
  will not warn you.

### `Condition` is an `Expression`, not an `ExpressionKind`

`RowPatternDefinition.Condition` and similar fields hold records with `Kind`+`Pos`,
so test patterns must wrap them: `Condition = { Kind = … }`.

### `pWhereClause` exists in two modules

`QueryParser.fs` defines `WHERE <expression>`; `DmlParser.fs` defines one returning
`(cursor, search condition)` for `WHERE CURRENT OF`. They do not clash, but
`CursorParser.fs` must open only `QueryParser` and not `DmlParser`.

### `LockingClause` must live inside the recursive type group

`LockingClause.ForUpdate` carries `Expression list option`, and `Expression` is
defined inside the `and`-chain starting at `DataType`. A standalone `type
LockingClause = …` before the group fails with FS0039.

## Test-writing pitfalls

### The `parse` helpers append `;` and need a type annotation

Every test file's helper is
`let parse (sql: string) = SqlParser.parse (sql.TrimEnd() + ";")`. The
`(sql: string)` annotation is required (FS0072 on `.TrimEnd()`). A test that calls
`SqlParser.parse` directly must append the semicolon itself, or it can pass for
the wrong reason.

Use `SqlParser.parse` for directly executable statements and
`SqlParser.parseStatement` for cursors / dynamic SQL / positioned DML — the two
entry points accept different statement sets.

### Prefer pattern matching over `Assert.Equal` on `Expression`s

`Expression` is a record with a `Pos` field, so constructing an expected value
requires a hand-written `Pos` that will never match the parsed one. Match the
pattern (which ignores `Pos`) or assert on individual fields.

### Validate numbering with the right regex

`RuleNumberingTests` extracts citations with `(\d+\.\d+)\s*<([^<>]+)>`. Capturing
`(\s*<…>)` and stripping with `Substring(1, len-2)` drops the closing `>` and
silently skips every citation, so the test passes vacuously.
