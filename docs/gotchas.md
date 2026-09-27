# Gotchas & Common Mistakes

Recurring pitfalls in this codebase. [architecture.md](architecture.md) covers the
design and [trade-off.md](trade-off.md) the rationale. Check this file before touching
parsers, AST patterns, or tests — every entry is a real failure mode from this codebase.

## F# language pitfalls

- **Union-case names clash across DUs** — `Select`/`Insert`/`Update`/`Delete`
  (`StatementKind` vs `PrivilegeAction`), `Final` (three DUs), `Range`, `BeginAtomic`,
  `First`/`Last`. An unqualified name silently resolves to the wrong type — qualify
  (`PrivilegeAction.Select`, …) and re-check after moving declarations in `Ast.fs`.
- **Identically-shaped records are inference-ambiguous** — `Expression`, `TableSource`,
  `Statement` all have `Kind` + `Pos`; `CreateViewStatement` and `Cte` collide in
  `pWithListElement`. Qualify the first field (`{ Expression.Kind = … }`).
- **Value restriction** — a *named* parser binding needs an explicit type
  (`let pLeftBrace: Parser<char, unit> = pchar '{'`); inline `token (pstring "{")` is fine.
- **Record patterns** — omitted fields are ignored (never `{ Kind = X; _ }`); fields on
  separate lines must align (FS0010); functions cannot appear in patterns. An inline
  record/list inside a record literal fails (FS0764/FS0001) — bind intermediates first.
- **`[<TailCall>]`** cannot attach to a local `let rec` (FS0010) and a *monadic* loop can
  never satisfy it (FS3569). Use a module-level collector with an explicit work list.

## FParsec combinator pitfalls

- **Precedence** — `<|>` binds tighter than `.>>`/`.>>.`/`>>.`, and `|>>` binds looser
  than `<|>`: parenthesise alternatives and constructor branches.
- **`>>.` discards the left result** — a `>>.`-chain returns the last keyword's string;
  use `.>>.` when the value must survive.
- **`|>>` swallows a following `>>=` into its lambda body** — parenthesise the lambda. An
  `|>>` projection cannot fail, so semantic validation needs `>>=` + `fail`.
- **`opt` does not backtrack partial consumption** — wrap optional multi-keyword clauses
  in `attempt` (the `WITH`-prefixed clauses, `FOR UPDATE OF`, cursor properties, …).
- **`notFollowedBy` makes failure fatal** — `opt` cannot catch it; wrap in `attempt`.
- **`many`/`sepBy1` do not backtrack a consumed token/separator** — postfix loops need
  `many (attempt …)`; an optional trailing separator needs `p .>>. many (attempt (sep >>. p))`.
- **Whitespace is not skipped automatically** — `pKeyword` skips no *leading* ws; raw
  parsers (`pchar '*'`, `pUnsignedInteger`) consume no *trailing* ws — add `.>> ws`.
- **`pstring` rejects newline characters in its argument** — `pstring "\r\n"` throws at
  module-initialisation time. Spell newlines with `pchar` (`pchar '\r' >>. pchar '\n'`).
- **`ws` is the separator consumer, so it eats comments too** (5.2) — a parser that must
  NOT accept a comment between two tokens (an `<introducer>`, intra-literal spaces of a
  datetime literal) must use `spaces`/`spaces1`, not `ws`.
- **`pKeyword` returns `Parser<string, unit>`** — mixing it with a `Parser<unit, _>` via
  `<|>` is a type error (coerce with `>>% ()`). The matched string keeps the *input
  casing*, so attach the meaning per branch; it refuses a keyword immediately followed by
  an identifier character (`SYSTEM` ≠ `SYSTEM_TIME`).

## Definition order, forward references and dispatch

- **Define before use** — move the dependency up, nest single-use sub-parsers, or use
  `createParserForwardedToRef`. Top-level definitions follow ascending ISO clause order
  *best-effort*; `define-before-use` wins, and uncited helpers stay next to their consumer.
- **Cross-module refs are wired where the target is defined** (inventory:
  [architecture.md](architecture.md) §5). Reference the forwarding *parser*, never
  `…Ref.Value` — reading `.Value` at initialisation captures FParsec's dummy parser.
- **An optional-looking sub-rule must not match empty input** — `opt A .>>. many B`
  succeeds on empty input and shadows later alternatives; transcribe the grammar literally.
- **Dispatch order is load-bearing** because `pKeyword` matches non-reserved words too:
  longer/more-specific forms first — `ALTER TYPE` < `ALTER ROUTINE`, `EXECUTE IMMEDIATE` <
  `EXECUTE <name>`, `(VALUES …)` < subquery, `FINAL|NEW|OLD TABLE` < plain table,
  `SELECT ( <privilege method list> )` < `SELECT [ <column list> ]`, 6.26 navigation <
  `pRoutineInvocation`, and in 12.3 `<object name>` the routine-designator branch first
  (`ROUTINE` is non-reserved).
- **The two entry points are not nested** — `parse` (22.1) rejects positioned DML/`OPEN`/
  `CALL`, `parseStatement` (13.4) rejects multi-row `SELECT`, `WITH`, temp table. A test
  that mixes them passes/fails for the wrong reason. Static `14.1 DECLARE CURSOR` is
  rejected by *both* — `pDeclareCursor` has no public caller (see trade-off.md).
- **Comparison operands must be `<row value predicand>`s, and the check reads the parser
  result's top node** — a term (`a + b = c`) is rejected too; `(a = b) = c` stays legal
  because the `Parenthesized` node survives. Desugared `=` (NULLIF) is nested inside its
  `Case` and must not be flagged.
- **Predicate suffixes are gated on the accumulated expression** —
  `pBooleanTestSuffixes` picks per shape (boolean → boolean-test only; term/row →
  no-boolean-test predicate; else full `pPredicate`). A new predicate in
  `PredicateParser.pPredicateImpl` needs three decisions: boolean primary? dropped when
  `includeBooleanTest = false`? part of 6.12 `<when operand>` (`forWhenOperand`)? Do not
  fold the suffixes back into `many ( … )` — the gating needs the current expression.
- **`<table argument>` vs its syntactic twins** — `<table function invocation>` and
  `TABLE ( <query> )` are also `<value expression>`s, so `pTableArgument` accepts them only
  when a table-argument clause follows. `COPARTITION` is *not* reserved — reject it
  explicitly as a correlation name and try it before the argument list.
- **Desugared nodes must not be re-checked post-parse** — `COALESCE`/`NULLIF` expand to
  shapes a naive traversal would reject; hence the predicate left-operand rule lives in
  parse-time suffix gating, and `findExpressionViolationIn` checks only standalone
  `QuantifiedSubquery` and the `<period predicate>` left operand.
- **`CAST ( NULL AS DESCRIPTOR )` is not a cast** — `<cast target>` excludes `DESCRIPTOR`;
  the form is `pDescriptorArgument`. `DESCRIPTOR ( … )` is the shared 20.16
  `pDescriptorValueConstructor` — do not duplicate it.
- **Do not reuse a parser whose grammar doesn't cover the slot** — e.g. `INSERT` keeps
  `pSchemaQualifiedNameExpression` (14.11 has no `ONLY` form). The omitted DML target
  relies on `SET`/`WHERE` being reserved — never "fix" it with a permissive identifier
  parser, which would read `UPDATE SET …`'s `SET` as the table.

## Reserved words and keywords

- **`pReservedFunctionName` is a whitelist** (`functionKeywords` in Lexer.fs);
  `pRoutineName` wraps it with non-reserved/delimited identifiers. Reserved words that
  start dedicated constructs (`EXISTS`, `UNIQUE`, `JSON_EXISTS`, `PERIOD`, `VALUE_OF`)
  must stay off it — their parsers are tried before `pRoutineInvocation`.
- **Closed enumerations need explicit `choice [ pKeyword "…" ]` lists** — a raw identifier
  parser also accepts `ALL`/`SELECT`/…, and reserved words cannot go through `pIdentifier`.
- **A citation must name the defining clause**, not the using one (`<local qualified
  name>` is 5.4, `<char length units>` is 6.1, …). Rule names must match the grammar
  file's casing too. Enforced by `RuleNumberingTests`.
- **Numeric conversions must be checked *and* culture-invariant** — use
  `toUnsignedInteger` / `toDecimal` / `pUnsignedIntegerAsInt` with
  `CultureInfo.InvariantCulture` (de-DE reads `"1.5"` as 15).
- **`pLiteral` excludes `<signed numeric literal>` and NULL** — 5.3 admits neither. Where
  the grammar wants a `<simple value specification>` (signs allowed), use
  `pSimpleValueSpecification` instead of widening `pLiteral`; NULL is 6.5
  `pNullSpecification`.
- **A host parameter name is an `<identifier>`**, so a reserved word cannot follow the
  colon: `GET DESCRIPTOR d :count = COUNT` fails because `COUNT` is reserved. Test with
  non-reserved names.
- **A bare column reference is `Identifier`, not `ColumnReference`** — `ColumnReference`
  appears only once a name is qualified.
- **Non-reserved keywords need explicit handling** — `TYPE`, `INSTANCE`, `CONSTRUCTOR`,
  `FINAL`, `OPTIONS`, `GENERATED`, `SECURITY`, `TRANSFORM`, `LOCATOR`, `TEMPORARY`,
  `COPARTITION`, `ROUTINE`, … are *not* reserved: alternatives starting with them need
  `attempt`. Reserved, by contrast: `LOG` (`INSERT INTO log …` fails — avoid it in tests),
  `METHOD`, `REF`, `OUT`, `SYSTEM_TIME`, `VALUE_OF`, `DESCRIBE`, `START`, `STATIC`,
  `GROUP`, `PARAMETER`, `SQL`, `EXTERNAL`, `DEFAULT`.

## Grammar-specific traps

- **`pExpression` includes boolean operators** — where `AND` must not be consumed, use the
  boolean-free parser: `<point in time>` / `FOR PORTION OF` use the 6.35 datetime parser;
  JSON slots use `pNonBooleanValueExpression`.
- **`pDataType` accepts any identifier as a UDT** — `NESTED PATH '$.items'` would read as
  column `NESTED` of type `PATH`; the UDT branch ends `.>>? notFollowedBy pIdentifier` and
  the `NESTED` branch is tried first.
- **Recursive/ambiguous forms** — `JSON_ARRAY(NULL ON NULL)` needs a `notFollowedBy` guard;
  `pExplicitRowValueConstructor` needs `attempt` so `(a)` falls through; `SET ( … )` ↔
  multiset recursion uses a forward ref; `pIntervalSign` must not swallow `->`;
  `<collection type>` suffixes are folded with `List.fold`, never self-referential.
- **`expressionChildren` has a `| _ -> []` catch-all** — a new `ExpressionKind` case
  holding an `Expression` silently escapes `findExpressionViolationIn`; add a branch for
  every new case (the compiler will not warn).
- **`Condition` is an `Expression`** — test patterns must wrap: `Condition = { Kind = … }`.
- **`pWhereClause` is shadowed in `DataManipulationParser.fs`** — the local
  `(cursor, search condition)` variant must stay below `pSelectStatementSingleRow`
  (14.7, which needs `QueryParser.pWhereClause`); hoisting it above is a type error.
- **`LockingClause` must live inside the recursive `and`-group** in `Ast.fs` — it carries
  `Expression list option`, and a standalone `type` before the group fails FS0039.

## Testing conventions

- **The `parse` helpers append `;` and need `(sql: string)`** (FS0072). A test calling
  `SqlParser.parse` directly must append the semicolon itself or pass for the wrong
  reason. Keep a `parseStatement`/`parseStatementFails` pair in any file exercising both
  entry points, or rejection tests pass vacuously.
- **`open FParsec` shadows `Result.Ok`/`Result.Error`** with `ReplyStatus` (FS3191) —
  qualify as `Result.Ok` / `Result.Error`.
- **Prefer pattern matching over `Assert.Equal` on `Expression`s** — `Pos` never matches a
  hand-written expected value.
- **Reordering source definitions means reordering their tests** (AGENTS.md: compile
  order, then definition order; review-only, so drift is silent).
- **A name slot is only as strict as the parser it names.** `pSchemaQualifiedNameExpression`
  accepts three parts; most 5.4 productions are narrower (`<schema name>`, `<cursor
  name>`, …). Use `pSchemaNameExpression` / `pCharacterSetNameExpression` /
  `pLocalQualifiedNameExpression`; add new narrow productions through `pNameOfArity`.
- **A permissive sub-parser inside a `choice` swallows its siblings.** 12.3 `<object
  name>`: using the bare-name-accepting designator parser made every `GRANT SELECT ON t1`
  resolve to `GrantRoutine`. Add a literal variant
  (`pTypedSpecificRoutineDesignator`) instead of reordering the alternatives.

## Known residual deviations (out of scope)

- **TRANSFORM GROUP `<multiple group specification>` types only the last group.** 11.60
  `pTransformGroupSpecification` accepts `g1, g2 FOR TYPE my_type` and types only the
  final group. The strict reading is rejected by an existing test; the lenient form is kept.
