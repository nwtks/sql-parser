# Gotchas & Common Mistakes

Recurring pitfalls in this codebase — every entry is a real failure mode. The design
lives in [architecture.md](architecture.md), the rationale in [trade-off.md](trade-off.md).

## F# language pitfalls

- **Union-case names clash across DUs** — `Select`/`Insert`/`Update`/`Delete`
  (`StatementKind` vs `PrivilegeAction`), `Final` (three DUs), `Range`, `BeginAtomic`,
  `First`/`Last`. An unqualified name silently resolves to the wrong type — qualify
  (`PrivilegeAction.Select`, …) and re-check after moving declarations in `Ast.fs`.
- **Identically-shaped records are inference-ambiguous** — `Expression`, `TableSource`,
  `Statement` all have `Kind` + `Pos`, and `CreateViewStatement`/`Cte` collide in
  `pWithListElement`. Qualify the first field (`{ Expression.Kind = … }`).
- **Value restriction** — a *named* parser binding needs an explicit type
  (`let pLeftBrace: Parser<char, unit> = pchar '{'`); inline `token (pstring "{")` is fine.
- **Record patterns** — omitted fields are ignored (never `{ Kind = X; _ }`); fields on
  separate lines must align (FS0010); functions cannot appear in patterns; an inline
  record/list literal fails (FS0764/FS0001) — bind intermediates first.
- **`[<TailCall>]`** cannot attach to a local `let rec` (FS0010), and a *monadic* loop can
  never satisfy it (FS3569) — use a module-level collector with an explicit work list.

## FParsec combinator pitfalls

- **Precedence** — in F#, `>>.`, `>>%`, `<|`, `|>>` and `>>=` all begin with a relational
  character, so they share ONE precedence level and associate **left to right**.
  Parenthesise alternatives and constructor branches. **`>>=` binds tighter than
  `.>>.`** — a `.>>.`-chain with a bind does not group left-associatively; use explicit
  `>>=` binds.
- **`>>.` discards the left result** (a `>>.`-chain returns the last keyword's string) —
  use `.>>.` when the value must survive.
- **`|>>` swallows a following `>>=` into its lambda body** — parenthesise the lambda.
  A projection cannot fail, so semantic validation needs `>>=` + `fail`.
- **`opt` does not backtrack partial consumption** — wrap optional multi-keyword clauses
  in `attempt` (the `WITH`-prefixed clauses, `FOR UPDATE OF`, …).
- **`attempt` swallows a committed failure, so an arity/shape guard inside it is
  unobservable** — `attempt (kw >>. body >>= fun x -> if bad x then fail …)` restores the
  input on the guard's failure and a later alternative re-parses the same tokens as a
  different production. A keyword branch that must REJECT a malformed suffix (7.9
  `PERMUTE ( <row pattern> { , … } )` with fewer than two patterns) must NOT be
  `attempt`-wrapped; committing is the only way the guard is visible.
- **`notFollowedBy` makes failure fatal** — `opt` cannot catch it; write
  `p .>> notFollowedBy q` inside `attempt`.
- **`many`/`sepBy1` do not backtrack a consumed token/separator** — postfix loops need
  `many (attempt …)`.
- **Whitespace is not skipped automatically** — `pKeyword` skips no *leading* ws; raw
  parsers (`pchar '*'`, `pUnsignedInteger`) consume no *trailing* ws — add `.>> ws`.
- **`pstring` rejects newline characters in its argument** — `pstring "\r\n"` throws at
  module-initialisation time; spell newlines with `pchar`.
- **`ws` is the separator consumer, so it eats comments too** (5.2) — where a comment
  must NOT sit between two tokens (an `<introducer>`, a datetime literal's interior),
  use `spaces`/`spaces1`.
- **`pKeyword` returns `Parser<string, unit>`** — coerce with `>>% ()` before combining
  with a `Parser<unit, _>`. It keeps the *input* casing and refuses a trailing identifier
  character (`SYSTEM` ≠ `SYSTEM_TIME`).

## Definition order, forward references and dispatch

- **Define before use** — move the dependency up, nest single-use sub-parsers, or use
  `createParserForwardedToRef`. Ascending ISO clause order is best-effort;
  `define-before-use` wins, and uncited helpers stay next to their consumer.
- **`<value expression primary>` and the non-boolean expression are forward refs** near
  the top of `ExpressionParser.fs`; operand slots needing the narrower production (6.17,
  6.23, 6.4, 6.25) must reference the ref, not a local parser (use-before-define).
- **Cross-module refs are wired where the target is defined** (inventory:
  [architecture.md](architecture.md) §5). Reference the forwarding *parser*, never
  `…Ref.Value` — reading `.Value` at initialisation captures FParsec's dummy parser.
- **An optional-looking sub-rule must not match empty input** — `opt A .>>. many B`
  succeeds on empty input and shadows later alternatives; transcribe the grammar literally.
- **Dispatch order is load-bearing** because `pKeyword` matches non-reserved words too —
  more-specific forms first: `ALTER TYPE` < `ALTER ROUTINE`, `EXECUTE IMMEDIATE` <
  `EXECUTE <name>`, `(VALUES …)` < subquery, `FINAL|NEW|OLD TABLE` < plain table,
  `SELECT ( <privilege method list> )` < `SELECT [ <column list> ]`, 6.26 navigation <
  `pRoutineInvocation`, and the 12.3 `<object name>` routine-designator branch first.
- **The two entry points are not nested** — `parse` (22.1) rejects positioned DML/`OPEN`/
  `CALL`; `parseStatement` (13.4) rejects multi-row `SELECT`, `WITH`, temp table. Static
  14.1 `DECLARE CURSOR` is rejected by *both* (`pDeclareCursor` has no caller). A test
  that mixes them passes/fails for the wrong reason.
- **Comparison operands must be `<row value predicand>`s, judged on the parser result's
  top node** — a top-level boolean is rejected (`1 = 2 = 3`, `x = EXISTS (…)`), a term is
  not (`a + b = c` is valid), and `(a = b) = c` survives through its `Parenthesized` node.
  The 6.39 `<boolean test>` gate (`isBooleanPredicand`) is narrower: `1 + 1 IS TRUE` is
  rejected, `1 + 1 IS NULL` (8.8) is not. Desugared `=` (NULLIF) sits inside a `Case` and
  must not be flagged.
- **6.4 has three value-specification widths** — `?` belongs to `<general value
  specification>` and `<target specification>` only, so a `<simple value specification>`
  slot must not accept it; 6.11 `<row marker offset>` re-adds it explicitly.
- **Greedy success can shadow a later alternative** — 14.4 `OPEN <cursor name>` must not
  consume a following `USING` (it would fail the enclosing statement instead of letting
  20.19 match); guard with `notFollowedBy (pKeyword "USING")`. When narrowing a parser,
  check that a broader sibling dispatched later still gets its turn.
- **Predicate suffixes are gated on the accumulated expression** — `pBooleanTestSuffixes`
  picks per shape (top-level boolean → boolean-test only; term/row → no-boolean-test
  predicate; else full `pPredicate`). A new predicate in `PredicateParser.pPredicateImpl`
  must decide: boolean primary? dropped when `includeBooleanTest = false`? part of 6.12
  `<when operand>`? Do not fold the suffixes back into `many ( … )`.
- **`<table argument>` vs its syntactic twins** — `<table function invocation>` and
  `TABLE ( <query> )` are also `<value expression>`s, so `pTableArgument` accepts them only
  with a following table-argument clause. `COPARTITION` is *not* reserved — reject it as a
  correlation name and try it before the argument list.
- **Desugared nodes must not be re-checked post-parse** — `COALESCE`/`NULLIF` expand to
  shapes a naive traversal would reject; the left-operand rule lives in parse-time suffix
  gating, and `findExpressionViolationIn` checks only `QuantifiedSubquery` and the
  `<period predicate>` left operand.
- **`CAST ( NULL AS DESCRIPTOR )` is not a cast** — `<cast target>` excludes `DESCRIPTOR`;
  the form is `pDescriptorArgument`, and `DESCRIPTOR ( … )` is the shared 20.16
  `pDescriptorValueConstructor`.
- **Do not reuse a parser whose grammar doesn't cover the slot** — `INSERT` uses the
  narrow `pTableNameExpression` (14.11's `<insertion target>` has no `ONLY`), and the
  omitted DML target relies on `SET`/`WHERE` being reserved: a permissive identifier
  parser would read `UPDATE SET …`'s `SET` as the table.

## Reserved words and keywords

- **`pReservedFunctionName` is a whitelist** (`functionKeywords` in `Lexer.fs`);
  `pRoutineName` wraps it with non-reserved/delimited identifiers. Reserved words that
  start dedicated constructs (`EXISTS`, `UNIQUE`, `JSON_EXISTS`, `PERIOD`, `VALUE_OF`)
  must stay off it — they parse through their own productions (`pPredicatePrimary`,
  `pValueOfExpressionAtRow`, …), never as a generic routine invocation.
- **Closed enumerations need explicit `choice [ pKeyword "…" ]` lists** — a raw identifier
  parser also accepts `ALL`/`SELECT`/…, and reserved words cannot go through `pIdentifier`.
- **A citation must name the defining clause** (`<local qualified name>` is 5.4,
  `<char length units>` is 6.1), and rule names must match the grammar file's casing —
  enforced by `RuleNumberingTests`.
- **Numeric conversions must be checked *and* culture-invariant** — use
  `toUnsignedInteger` / `toDecimal` / `pUnsignedIntegerAsInt` with
  `CultureInfo.InvariantCulture` (de-DE reads `"1.5"` as 15).
- **`pLiteral` excludes `<signed numeric literal>` and NULL** (5.3 admits neither) — use
  `pSimpleValueSpecification` where signs are allowed; NULL is 6.5 `pNullSpecification`.
- **A host parameter name is an `<identifier>`** — a reserved word cannot follow the
  colon: `GET DESCRIPTOR d :count = COUNT` fails because `COUNT` is reserved.
- **A bare column reference is `Identifier`, not `ColumnReference`** — `ColumnReference`
  appears only once a name is qualified.
- **Non-reserved keywords need explicit handling** — `TYPE`, `INSTANCE`, `CONSTRUCTOR`,
  `FINAL`, `OPTIONS`, `GENERATED`, `SECURITY`, `TRANSFORM`, `LOCATOR`, `TEMPORARY`,
  `COPARTITION`, `ROUTINE`, … are *not* reserved, so alternatives starting with them need
  `attempt`. Reserved, by contrast: `LOG` (`INSERT INTO log …` fails), `METHOD`, `REF`,
  `OUT`, `SYSTEM_TIME`, `VALUE_OF`, `DESCRIBE`, `START`, `STATIC`, `GROUP`, `PARAMETER`,
  `SQL`, `EXTERNAL`, `DEFAULT`.

## Grammar-specific traps

- **`pExpression` includes boolean operators** — where `AND` must not be consumed, use
  the boolean-free parser: `<point in time>` / `FOR PORTION OF` use the 6.35 datetime
  parser (`pDatetimeValueExpression`), JSON slots use `pNonBooleanValueExpression`.
- **`pDataType` accepts any identifier as a UDT** — `NESTED PATH '$.items'` would read as
  column `NESTED` of type `PATH`; `pUserDefinedType` ends `.>>? notFollowedBy pIdentifier`
  and the JSON_TABLE `NESTED` branch is tried first.
- **Recursive/ambiguous forms** — `JSON_ARRAY(NULL ON NULL)` needs a `notFollowedBy` guard;
  `pExplicitRowValueConstructor` needs `attempt` so `(a)` falls through; `SET ( … )` ↔
  multiset recursion uses a forward ref; `pIntervalSign` must not swallow `->`;
  `<collection type>` suffixes are folded with `List.fold`, never self-referential.
- **`expressionChildren` has a `| _ -> []` catch-all** — a new `ExpressionKind` case
  holding an `Expression` silently escapes `findExpressionViolationIn`; add a branch for
  every new case (the compiler will not warn).
- **`Condition` is an `Expression`** — test patterns must wrap: `Condition = { Kind = … }`.
- **`pWhereClause` is shadowed in `DataManipulationParser.fs`** — the local
  `(cursor, search condition)` variant must stay below `pSelectStatementSingleRow` (14.7
  needs the different-typed `QueryParser.pWhereClause`).
- **`LockingClause` must live inside the recursive `and`-group** in `Ast.fs` — it carries
  `Expression list option`, and a standalone `type` before the group fails FS0039.

## Testing conventions

- **The `parse` helpers append `;` and need `(sql: string)`** (FS0072). A test calling
  `SqlParser.parse` directly must append the semicolon itself or pass for the wrong
  reason; keep a `parseStatement`/`parseStatementFails` pair in any file exercising both
  entry points, or rejection tests pass vacuously.
- **`open FParsec` shadows `Result.Ok`/`Result.Error`** with `ReplyStatus` (FS3191) —
  qualify as `Result.Ok` / `Result.Error`.
- **Prefer pattern matching over `Assert.Equal` on `Expression`s** — `Pos` never matches a
  hand-written expected value.
- **Reordering source definitions means reordering their tests** (AGENTS.md: compile
  order, then definition order; review-only, so drift is silent).
- **A name slot is only as strict as the parser it names** — `pSchemaQualifiedNameExpression`
  accepts three parts; most 5.4 productions are narrower (`<schema name>`, `<character
  set name>`, `<cursor name>`, `<table name>`, `<external routine name>`, `<group
  name>`, …). Use `pSchemaNameExpression` / `pCharacterSetNameExpression` /
  `pLocalQualifiedNameExpression` / `pTableNameExpression` / `pIdentifierExpression`;
  add new narrow productions through `pNameOfArity`.
- **A permissive sub-parser inside a `choice` swallows its siblings** — in 12.3
  `<object name>` the bare-name-accepting designator parser made every `GRANT SELECT ON
  t1` resolve to `GrantRoutine`; use the literal `pTypedSpecificRoutineDesignator`
  instead of reordering the alternatives.
- **`Assert.Fail` is not `unit`-only but also not polymorphic** — it is usable as the
  last branch of a `unit` match, but a match that must *produce a value* cannot use it
  (`type constraint mismatch: 'unit' is not compatible with 'string'`). Write the helper
  as `unit`-returning, or extract with `failwithf`, or assert inside the branch.
- **Record and list patterns split across lines need same-column alignment (FS0010)** —
  `{ DataType = Some(…) }` written over several lines inside `[ … ]` fails to parse.
  Bind first (`| CreateTable { Columns = [ column ] } -> match column.DataType with …`)
  instead of nesting the pattern.
- **Inside `[ … ]` a comma makes a TUPLE, not two list elements** — use `;`:
  `[ _; pair ]`, not `[ _, pair ]`.

## Known residual deviations (out of scope)

- **20.26 `<preparable dynamic cursor name>` scope option** — `WHERE CURRENT OF`
  shares one parser between the static 14.8/14.13 (bare `<cursor name>`) and the
  dynamic 20.23–20.27 forms, so `WHERE CURRENT OF LOCAL c` also parses in static
  statements. Deliberate; documented in [trade-off.md](trade-off.md).
