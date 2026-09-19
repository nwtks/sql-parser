# Gotchas & Common Mistakes

Recurring pitfalls in this codebase. [architecture.md](architecture.md) covers the
design and [trade-off.md](trade-off.md) the rationale behind the strictness.

## F# language pitfalls

### Union-case names clash — qualify at the use site

Many SQL keywords map to natural F# case names that already exist in `Ast.fs`; an
unqualified name silently resolves to the wrong type (usually the most recently
defined).

| Case name | Clashes between |
|-----------|-----------------|
| `Select`, `Insert`, `Update`, `Delete` | `StatementKind` vs `PrivilegeAction` |
| `Unique`, `PrimaryKey`, `Check` | `ExpressionKind`/`ColumnConstraintKind` vs `TableConstraint` |
| `SetDefault`, `DropDefault`, `AddConstraint`, `DropConstraint` | `ReferentialAction`/`ColumnAlteration`/`AlterTableAction`/`DomainAlteration` |
| `Final` | `ResultOption` vs `TypeOption` vs `RunningOrFinal` |
| `Between` | `ExpressionKind` vs `SystemTimeSpec` |
| `Range` | `IntervalQualifier` vs `WindowFrameUnit` |
| `BeginAtomic` | `RoutineBody` vs `TriggeredStatement` |
| `First`, `Last`, `Next` | `Direction` vs `FirstOrLast`/`PrevOrNext` |

Always qualify (`PrivilegeAction.Select`, `TableConstraint.Unique`,
`IntervalQualifier.Range(…)`, `TypeOption.Final false`, `RunningOrFinal.Final`);
`ColumnConstraintKind` has no bare `Null` for this reason. After moving
declarations in `Ast.fs`, re-check for newly ambiguous cases.

### Record types with identical field shapes are inference-ambiguous

`Expression`, `TableSource` and `Statement` all have `Kind` + `Pos`, so an
unannotated literal like `{ Kind = Literal(Number 1m); Pos = … }` can be inferred
as `Statement`; `CreateViewStatement` and `Cte` similarly collide in `pWithListElement`.

**Fix:** qualify the first field (`{ Expression.Kind = …; … }`,
`{ TableSource.Kind = …; … }`, `{ Cte.Name = name; … }`) or annotate the binding
(`let defaultCount: Expression = …`).

### Named parsers need an explicit type annotation

Value restriction: annotate `let pLeftBrace: Parser<char, unit> = pchar '{'` (use
`Parser<string, unit>` for `pstring`); inline `token (pstring "{")` is unaffected.

### Record patterns: alignment and nesting rules

- Omitted fields are ignored — never write `{ Kind = X; _ }`.
- Newline-separated fields must start at the **same column** (FS0010); extract the
  record and match separately, or bind and assert on fields.
- A `[` for a list value must start on the `=` line
  (`| CreateProcedure { Parameters = [ { Mode = Some In } ] } -> ()`); an inline
  list-of-record inside `Some(...)` fails even then — bind it first
  (`| CreateType { Representation = Some(MemberList attrs) } -> match attrs with …`).
- Deeply nested multi-line patterns are fragile — bind intermediates and
  `Assert.Equal`.

## FParsec combinator pitfalls

### Operator precedence

`<|>` binds **tighter** than the `.>>`/`.>>.`/`>>.` family, and `>>%` shares F#'s
precedence group with `<|>`. Parenthesise each alternative, and the constructor
branch of `attempt pTargetTable |>> DmlTarget.TableTarget <|> preturn …`, which
otherwise parses as `attempt pTargetTable |>> (DmlTarget.TableTarget <|> …)`
(`|>>` binds looser than `<|>`).

### `>>.` silently discards a result

Use `.>>.` when the value must survive (`>>. opt p .>>. q`).

A `>>.`-chain also returns the *last* keyword's string, so
`pKeyword "DROP" >>. pKeyword "SYSTEM" >>. pKeyword "VERSIONING" .>>. pDropBehavior`
yields `string * bool`. Use `>>.` (not `.>>.`) for the final keyword.

### `|>>` swallows a following `>>=` into the lambda body

```fsharp
|>> fun x -> { … } >>= f    // parses as |>> (fun x -> { … } >>= f)
```

Parenthesise the lambda: `|>> (fun x -> { … }) >>= f`. An `|>>` projection cannot
fail, so validation must use `>>=` + `fail`, and the `StatementKind` wrapper goes
*after* it.

### `opt` does not backtrack partial consumption

Wrap optional multi-keyword clauses in `attempt`
(`opt (attempt (pKeyword "WITH" >>. pKeyword "HIERARCHY" >>. pKeyword "OPTION"))`)
— required for `pIdentityColumnSpecification`, `pWithCheckOption`, `pUpdatabilityClause`,
`pCursorHoldability`, `pCursorReturnability`, the `FOR UPDATE OF` group and the
`WITH`-prefixed clauses.

A single optional token hits the same trap once an earlier parser has already
consumed input: `pCharacterSetSpecification` must use
`opt (attempt (pSqlLanguageIdentifier .>> token (pstring ".")))`, otherwise the
name is consumed and the missing `.` aborts the whole unqualified form, so
`_UTF8'abc'` fails to parse.

### `notFollowedBy` makes the failure fatal — wrap in `attempt`

`opt` cannot catch it, so wrap in `attempt` to let `opt` return `None` (see
`pExistingWindowName` / `pWindowFrameClause`).

### `attempt` is required inside postfix loops

A consumed token followed by a failure aborts the enclosing `many`. Canonical:
`many (attempt pDereferenceReference <|> attempt pMethodOrFieldReference)` and
`many (attempt pPredicate <|> … <|> attempt pTimeZoneSuffix)`.

### `sepBy1` does not backtrack a consumed separator — and must not re-parse `p`

Use `p .>>. many (attempt (sep >>. p))` when a trailing separator is optional (`t.*`
in `<derived column>`). Consume only `.` in the separator, never `p` itself:
`pIdentifier .>>. many (attempt (token "." >>. pIdentifier .>>? notFollowedBy "("))`.

### Whitespace is not skipped automatically

`pKeyword` skips no *leading* whitespace, and raw parsers (`pchar '*'`,
`pUnsignedInteger`) consume no *trailing* whitespace — follow them with `.>> ws` /
`token`, or the next `pKeyword` fails on the space.

`pKeyword` also refuses a keyword immediately followed by an identifier character
(`SYSTEM` ≠ `SYSTEM_TIME`, `C` ≠ `COBOL`).

### `pKeyword` returns `Parser<string, unit>`, not `Parser<unit, unit>`

Harmless with `>>.`/`.>>`/`>>%`, but mixing it with a `Parser<unit, _>` in an `<|>`
is a type error — coerce with `|>> ignore` or `>>% ()`.

## Definition order, forward references and dispatch

### Define before use, or use a forward reference

F# requires definitions before use. Move the dependency above its user
(`pSequenceGeneratorOption`/`pIdentityColumnSpecification` before `pColumnDefinition`), nest it when there
is exactly one consumer (`pTransformsToBeDropped` in `pDropStatement`), or use
`createParserForwardedToRef` across modules.

### Definitions are ordered by spec clause (best-effort)

Top-level definitions follow the ascending ISO/IEC 9075-2:2016 clause number cited
above them (sort key: the *first* citation; uncited helpers stay with what they
serve), but `define-before-use` wins — a helper cited under a later clause stays
above its user (`pReferentialTriggeredAction` 11.8 before `pColumnConstraintDefinition`
11.4). Where order is compiler-irrelevant it is kept strictly ascending (`Ast.fs`'s
`and` group 6.1 → 6.43, `ExpressionKind`/`Expression` at 6.28; the
`createParserForwardedToRef` declarations in `ExpressionParser.fs`, 6.1 → 7.17).
Single-use sub-parsers are nested inside their consumer, so they add no inversion.

### Forward-reference wiring happens in `SqlParser.fs`

`pDataChangeStatementRef`, `pPredicateRef`, `pPredicatePrimaryRef` and
`pStatementRef` are assigned in `SqlParser.fs` *after* the target is defined
(`pStatement`/`pStatementRef` are declared in `SchemaParser.fs` and reused there);
wiring them inside their own module fails. Use the forwarding **parser**, never
`…Ref.Value` — reading `.Value` at module-initialisation time captures FParsec's
dummy parser. The two §8 refs must also be assigned there: nothing else references
`PredicateParser`, so its initialiser would not run and the refs would stay dummy.
`SqlParser.fs` (initialised before `parse`) forces the module to load.

### An optional-looking sub-rule must not match empty input

`opt A .>>. many B` succeeds on empty input and silently shadows later
alternatives — transcribe them literally, e.g. 11.20:
`attempt (pSetIdentityColumnGeneration .>>. many option) <|> (many1 option)`.

### Dispatch order is load-bearing

Because `pKeyword` matches non-reserved words too, `choice` order keeps prefixes
apart. The pairs that must be ordered:

- `ALTER TYPE` **before** `ALTER ROUTINE`.
- `DROP TYPE` **before** `DROP ROUTINE`.
- `EXECUTE IMMEDIATE` **before** `EXECUTE <name>`.
- `DECLARE LOCAL TEMPORARY TABLE` **before** `DECLARE <cursor>`.
- `SELECT ... INTO` **before** `pQuery`.
- The PTF `DESCRIBE WITH …` body **before** the generic statement branch.
- 20.17 `ALLOCATE … FOR <statement>` **before** 20.18 `ALLOCATE … FOR PROCEDURE`.
- The `(VALUES …)` branch **before** the subquery branch in `pTablePrimary`.
- `SELECT ( <privilege method list> )` **before** `SELECT [ <column list> ]` — and the
  method list item (`pPrivilegeMethodItem`) re-checks that a `<routine type>` is present,
  otherwise `pSpecificRoutineDesignator`'s bare-name form (for `ALTER ROUTINE`) would
  swallow `SELECT (c1, c2)` as a method list.
- The `FINAL|NEW|OLD TABLE` alternative **before** the plain table alternative.
- The 6.26 navigation parser **before** `pRoutineInvocation` and
  `pColumnReferenceExpression`, tried Compound → Logical → Physical.
- 6.37's interval alternative **before**, and inside the same `attempt` as, the
  plain parenthesized `pExpression` branch.
- The NESTED branch before the regular-column branch in `pJsonTableColumnDefinition`.

### Do not reuse a parser whose grammar does not cover the slot

`INSERT` must keep `pSchemaQualifiedNameExpression` for its target: 14.11 `<insertion target>`
is a plain `<table name>` with no `ONLY` form, so `pTargetTable` would wrongly
accept `INSERT INTO ONLY (t) …`. Likewise `pPartitionedJoinColumnReferenceList` is column references only.

The omitted DML target relies on `SET` and `WHERE` being reserved words —
`pTargetTable` fails cleanly on them. Do not "fix" a missing table name with
`pIdentifierRaw`, which would parse `UPDATE SET …` with `SET` as the table.

## Reserved words and keywords

### `pRoutineInvocation` only accepts reserved *function* keywords

`pReservedFunctionName` is a whitelist (`COUNT`, `ROW_NUMBER`, `PERCENTILE_CONT`,
`ABS`, …) plus non-reserved/delimited identifiers. Therefore:

- Reserved words that start a dedicated construct (`EXISTS`, `UNIQUE`,
  `JSON_EXISTS`, `PERIOD`, `VALUE_OF`) must stay off the whitelist, and their
  dedicated parsers must be listed **before** `pRoutineInvocation` in
  `pValueExpressionPrimary`.
- **Adding a reserved-name built-in requires adding it to `functionKeywords`**, or
  `SELECT ABS(x)` / `SELECT ROW_NUMBER() OVER (…)` fail with "reserved word".
  Non-reserved names (`foo(...)`) are unaffected.

### `pIdentifierRaw` is too permissive for closed enumerations

Item names that are reserved words (`NUMBER`, `ROW_COUNT`, `DATA`, …) cannot use
`pIdentifier`, but `pIdentifierRaw` also accepts `ALL`, `SELECT`, …. Use an
explicit `choice [ pKeyword "…" ]` enumeration (diagnostics/descriptor item names,
`<language name>`, `<parameter style>`). The same applies to `<char length units>`
(`CHARACTERS | OCTETS`), which is `pCharLengthUnits` in `ExpressionParser.fs`.

### A citation must name the clause that *defines* the rule

`RuleNumberingTests` matches an unnumbered `<rule name>` against the clauses that
*mention* it, so the number must be the defining clause even when the rule is used
elsewhere: `<local qualified name>` is a **5.4** rule (not 14.1/20.1, where it is the
`<cursor name>` production) and `<char length units>` is a **6.1** rule (not
6.30/6.32, where it is used). Same class as the `<scope option>` (5.4) and
`<semicolon>` (5.1) entries above.

### Numeric conversions must be checked *and* culture-invariant

`uint64`/`int`/`decimal` conversions throw `OverflowException`/`FormatException` on
out-of-range input, and F#'s `decimal` reads a string with the **current culture**
(de-DE reads `1.5` as 15). Use the checked helpers (`toUnsignedInteger`,
`toDecimal`, `pUnsignedIntegerAsInt`) with `CultureInfo.InvariantCulture`; the
`runParser` try/with in `SqlParser.fs` is only a safety net, not the fix.

### `<literal>` includes `<signed numeric literal>`, but `pLiteral` does not

`pLiteral` covers the unsigned and general literal forms only, so `pLiteralExpression`
rejects a leading sign. Where the grammar requires a `<simple value specification>`
(which does admit `<signed numeric literal>`), add the sign form at that slot —
`pSimpleValueSpecification` does — instead of widening `pLiteral`, which would change
the AST of every `SELECT -1` (a unary-minus expression today, a literal after).

### NULL is not a `<literal>` — it is the 6.5 `<null specification>`

`pLiteral` has no `NULL` branch (2026-09-19): 5.3 `<literal>` does not admit NULL.
Do not "fix" a failing `x = NULL` test by re-adding NULL to `pLiteral` — that
over-accepts `SELECT 1 + NULL`. Add `pNullSpecification` (`ExpressionParser.fs`, 6.5)
to the contextually-typed slot instead; the full slot list is in trade-off.md.

### Non-reserved keywords need explicit handling

`TYPE`, `UNDER`, `OVERRIDING`, `INSTANCE`, `CONSTRUCTOR`, `INSTANTIABLE`, `FINAL`,
`OPTIONS`, `DERIVED`, `GENERATED`, `PRIVATE`, `FULFILL`, `FINISH`, `SECURITY`,
`DISPATCH`, `GENERAL`, `IMPLEMENTATION`, `DEFINER`, `INVOKER`, `TRANSFORM`, `STYLE`,
`LOCATOR`, `PRESERVE`, `TEMPORARY`, `EXTENDED`, `ATTRIBUTES` are **not** reserved.
Consequences:

- A `<typed table element>` cannot be dispatched on its first token —
  `<column options>` is only recognisable once the mandatory `WITH OPTIONS` has
  been consumed, so every alternative needs `attempt`.
- `DESCRIPTOR` must be tried before `<data type>` in `<parameter type>`, otherwise
  `(d DESCRIPTOR)` parses as a parameter of UDT type `DESCRIPTOR`.
- `INSTANCE`/`CONSTRUCTOR` must be `attempt`ed in `pMethodKind` so they can still
  be identifiers elsewhere.

`LOG` **is** reserved, so `INSERT INTO log …` fails — use another table name in
tests. `METHOD`, `REF`, `OUT`, `SYSTEM_TIME`, `VALUE_OF`, `DESCRIBE`, `START`,
`STATIC`, `GROUP`, `PARAMETER`, `SQL`, `EXTERNAL`, `DEFAULT` are reserved too.

### `<scope option>` is a 5.4 rule, `<semicolon>` a 5.1 rule

`RuleNumberingTests` matches a citation against the clauses that mention the name,
so `<scope option>` must cite 5.4 (not 20.15/20.17) and `<semicolon>` must cite 5.1
(not 5.2).

## Grammar-specific traps

### `pExpression` includes boolean operators

Where `AND` must not be consumed as a boolean operator, use a boolean-free parser:
`<point in time>` / `FOR PORTION OF` use the 6.35 datetime parser, and the JSON
argument slots use `pNonBooleanValueExpression` (`opp.ExpressionParser` without
boolean operators). That parser is a forward ref because the JSON parsers are
defined before `opp` is built.

### `pDataType` accepts any identifier as a user-defined type

Any identifier is a valid UDT name, so `NESTED PATH '$.items'` would be read as a
column named `NESTED` of type `PATH`. The UDT branch ends with
`.>>? notFollowedBy pIdentifier` so a name immediately followed by another
identifier is rejected; the NESTED branch is also tried first.

### Recursive/ambiguous expression forms

- `JSON_ARRAY(NULL ON NULL)` — `sepBy pExpression ","` greedily consumes `NULL`,
  leaving `ON NULL` unconsumed. Guard each element with
  `pExpression .>>? notFollowedBy (attempt (pKeyword "ON" >>. pKeyword "NULL"))`.
- `pExplicitRowValueConstructor` (7.1) needs `attempt` so `(a)` can fall through to
  the plain parenthesized expression branch.
- 6.44 `SET ( … )` and `pMultisetValueExpression` are mutually recursive; the
  latter is a forward ref wired after `pValueExpressionPrimary`.
- `pIntervalSign` must not swallow the `-` of `->`: guard with
  `notFollowedBy (pchar '>')`.
- `<collection type>` suffixes must be folded
  (`pDataTypeElement .>>. many (…) |> List.fold`), never parsed self-referentially —
  `pDataType ARRAY …` recurses forever.
- `expressionChildren` (the work-list traversal behind
  `containsStandaloneQuantifiedSubquery`) has a `| _ -> []` catch-all, so a new
  `ExpressionKind` case holding an `Expression` silently escapes the standalone-
  `ANY` rejection. **Add a branch for every new case** — the compiler will not warn.
  (`Parenthesized`, added 2026-09-19, forwards its child: `| Parenthesized inner -> [ inner ]`.)
- A boolean tree search cannot be tail-recursive with `||` / `List.exists`, so
  `[<TailCall>]` would warn (FS3569): collect children into a list
  (`expressionChildren`) and fold an explicit work list instead. `[<TailCall>]`
  also cannot be attached to a **local** `let rec` (FS0010), so the loop must be a
  module-level `private` function.

### `Condition` is an `Expression`, not an `ExpressionKind`

`RowPatternDefinition.Condition` and similar fields hold records with `Kind`+`Pos`,
so test patterns must wrap them: `Condition = { Kind = … }`.

### `pWhereClause` is shadowed inside `DataManipulationParser.fs`

`QueryParser.fs` defines `WHERE <expression>`; `DataManipulationParser.fs` defines a
local parser of the same name returning `(cursor, search condition)` for
`WHERE CURRENT OF` / search. The local binding shadows the imported one for every
unqualified use **after** its definition, so it must stay below
`pSelectStatementSingleRow` (14.7), which needs `QueryParser.pWhereClause`;
`pDeleteStatement`/`pUpdateStatement` sit below it and bind the local parser.
Hoisting the local `pWhereClause` above `pSelectStatementSingleRow` is a type error, not
a silent rebind.

### `LockingClause` must live inside the recursive type group

`LockingClause.ForUpdate` carries `Expression list option`, and `Expression` is
defined inside the `and`-chain starting at `DataType`. A standalone
`type LockingClause = …` before the group fails with FS0039.

## Test-writing pitfalls

### The `parse` helpers append `;` and need a type annotation

Every test file's helper is
`let parse (sql: string) = SqlParser.parse (sql.TrimEnd() + ";")`; the
`(sql: string)` annotation is required (FS0072). A test that calls
`SqlParser.parse` directly must append the semicolon itself, or it can pass for the
wrong reason.

Use `SqlParser.parse` for directly executable statements and
`SqlParser.parseStatement` for cursors / dynamic SQL / positioned DML — the two
entry points accept different statement sets.

Merging two test files is a trap: the moved blocks keep calling whatever `parse`
helper is in scope, not the one they were written against, so tests can fail — or
`... is rejected` tests can pass *vacuously* — for the wrong reason. Keep a
`parseStatement` / `parseStatementFails` helper alongside `parse` in any file that
exercises both entry points.

### `open FParsec` shadows `Result.Ok` / `Result.Error`

`ReplyStatus` has the same case names, so a test file that does `open FParsec`
resolves the `parse` helper's `| Ok res` / `| Error e` to `ReplyStatus` (FS3191,
plus a bogus `Result<Statement, ParseError>` mismatch). Qualify them —
`| Result.Ok res` / `| Result.Error e`.

### Prefer pattern matching over `Assert.Equal` on `Expression`s

`Expression` is a record with a `Pos` field, so an expected value needs a
hand-written `Pos` that never matches the parsed one. Match the pattern (ignores
`Pos`) or assert on individual fields.

### Validate numbering with the right regex

`RuleNumberingTests` extracts citations with `(\d+\.\d+)\s*<([^<>]+)>`. Capturing
`(\s*<…>)` and stripping with `Substring(1, len-2)` drops the closing `>` and
silently skips every citation, so the test passes vacuously.

### Reordering definitions means reordering their tests

`AGENTS.md` requires test functions to follow the source definitions they exercise
(compile order, then definition order). Because the order is review-only, a
definition reorder compiles and passes without warning while the tests silently
drift. When moving a definition, move the matching test block in the same change;
reordering a whole file is safest done with a block-level rewrite that re-inserts
every test exactly once (an insert+delete pair applied out of order can duplicate
or drop a block).

Order each block by the rule it names, not by the umbrella parser it happens to
call: `LexerTests.fs` drives `<regular identifier>` / `<delimited identifier>` (5.2)
through `pIdentifier` (5.4), and the blocks sit at the 5.2 position - next to
`pAnyRune`/`pUnicode*Escape` and before the numeric literals - mirroring `Lexer.fs`.

### `ROUTINE` is a regular identifier - try the designator branch first

In `<object name>` (12.3) the kind branch (`opt <object kind> <qualified name>`)
accepts `ROUTINE` as a plain name because `ROUTINE` is a non-reserved word.
When the routine-designator branch comes second, `GRANT EXECUTE ON ROUTINE add TO u`
therefore parses `ROUTINE` as the object name, fails at the missing `TO` (the
whole `attempt`ed branch backtracks) and the error surfaces as
`Expecting: . or TO`. The routine branch (`<routine type> <qualified name>`,
with the `<routine type>` keyword mandatory) must come first - `pRoutineType`
fails on ordinary names, so plain names still reach the kind branch.
