# Gotchas & Common Mistakes

Recurring pitfalls in this codebase. [architecture.md](architecture.md) covers the
design and [trade-off.md](trade-off.md) the rationale. Check this file before touching
parsers, AST patterns, or tests — every entry is a real failure mode from this codebase.

## F# language pitfalls

- **Union-case names clash across DUs** — `Select`/`Insert`/`Update`/`Delete`
  (`StatementKind` vs `PrivilegeAction`), `Unique`/`PrimaryKey`/`Check`,
  `SetDefault`/`AddConstraint`/`DropConstraint`, `Final` (`ResultOption` vs
  `TypeOption` vs `RunningOrFinal`), `Between`, `Range` (`IntervalQualifier` vs
  `WindowFrameUnit`), `BeginAtomic`, `First`/`Last`/`Next`. An unqualified name
  silently resolves to the wrong (usually most-recently-defined) type — qualify
  (`PrivilegeAction.Select`, `TableConstraint.Unique`, …), and re-check after moving
  declarations in `Ast.fs`.
- **Identically-shaped records are inference-ambiguous** — `Expression`, `TableSource`
  and `Statement` all have `Kind` + `Pos`; `CreateViewStatement` and `Cte` collide in
  `pWithListElement`. Qualify the first field (`{ Expression.Kind = … }`) or annotate
  the binding.
- **Value restriction**: a *named* parser binding needs an explicit type
  (`let pLeftBrace: Parser<char, unit> = pchar '{'`; `Parser<string, unit>` for
  `pstring`). Inline `token (pstring "{")` is unaffected.
- **Record patterns**: omitted fields are ignored (never `{ Kind = X; _ }`);
  newline-separated fields must start at the same column (FS0010); a list `[` must
  start on the `=` line; an inline list-of-record inside `Some(...)` fails even then —
  bind intermediates and assert on fields. Functions cannot appear in patterns
  (`dataTypeParam Integer` fails).
- **`[<TailCall>]`** cannot be attached to a local `let rec` (FS0010), and a *monadic*
  loop can never satisfy it (FS3569 — `suffix >>= pBooleanTestSuffixes`). Move
  tail-recursive collectors to module level with an explicit work list
  (`collectSqlArgumentChildren`).

## FParsec combinator pitfalls

- **Precedence**: `<|>` binds tighter than `.>>`/`.>>.`/`>>.`; `>>%` shares `<|>`'s
  group; `|>>` binds looser than `<|>` — parenthesise alternatives and constructor
  branches (`attempt p |>> C <|> …` otherwise parses as `|>> (C <|> …)`).
- **`>>.` discards the left result**; a `>>.`-chain returns the *last* keyword's string
  (`pKeyword "DROP" >>. … .>>. pDropBehavior` yields `string * bool`). Use `.>>.` when
  the value must survive, `>>.` for the final keyword.
- **`|>>` swallows a following `>>=` into its lambda body** — parenthesise the lambda.
  An `|>>` projection cannot fail, so semantic validation needs `>>=` + `fail`, with
  the `StatementKind` wrapper *after* it.
- **`opt` does not backtrack partial consumption** — wrap optional multi-keyword
  clauses in `attempt` (`pIdentityColumnSpecification`, `pWithCheckOption`,
  `pUpdatabilityClause`, `pCursorHoldability`/`pCursorReturnability`, `FOR UPDATE OF`,
  the `WITH`-prefixed clauses). Even a single optional token hits the trap after input
  was consumed: `pCharacterSetSpecification` needs
  `opt (attempt (pSqlLanguageIdentifier .>> token (pstring ".")))` or `_UTF8'abc'` fails.
- **`notFollowedBy` makes failure fatal** — `opt` cannot catch it; wrap in `attempt`
  (`pExistingWindowName`, `pWindowFrameClause`).
- **Postfix loops need `many (attempt …)`** — a consumed token followed by a failure
  aborts the enclosing `many` (dereference/method chains, predicate suffixes).
- **`sepBy1` does not backtrack a consumed separator** — use
  `p .>>. many (attempt (sep >>. p))` when a trailing separator is optional (`t.*`),
  and consume only the separator in the loop, never `p` itself.
- **Whitespace is not skipped automatically** — `pKeyword` skips no *leading* ws; raw
  parsers (`pchar '*'`, `pUnsignedInteger`) consume no *trailing* ws — follow them with
  `.>> ws` / `token`.
- **`pKeyword` returns `Parser<string, unit>`** — mixing it with a `Parser<unit, _>`
  via `<|>` is a type error; coerce with `|>> ignore` / `>>% ()`. The matched string
  keeps the *input casing* (`pstringCI`), so never match it case-sensitively — attach
  the meaning per branch (`pKeyword "WITH" >>% true`). It also refuses a keyword
  immediately followed by an identifier character (`SYSTEM` ≠ `SYSTEM_TIME`).

## Definition order, forward references and dispatch

- **Define before use**: move the dependency up, nest single-use sub-parsers inside
  their consumer, or use `createParserForwardedToRef` across modules. Top-level
  definitions follow ascending ISO clause order *best-effort* — `define-before-use`
  wins (`pReferentialTriggeredAction` 11.8 stays above `pColumnConstraintDefinition` 11.4).
- **Cross-module refs are wired in `SqlParser.fs`** after the target is defined
  (`pDataChangeStatementRef`; the §8 refs `pPredicateRef` / `pPredicateNoBooleanTestRef` /
  `pPredicatePrimaryRef`; `pBooleanTestPart2Ref`, `pWhenOperandPart2Ref`; `pStatementRef`).
  Reference the forwarding *parser*, never `…Ref.Value` — reading `.Value` at
  initialisation captures FParsec's dummy parser. The §8 refs must live in `SqlParser.fs`
  because nothing else loads `PredicateParser`.
- **An optional-looking sub-rule must not match empty input** — `opt A .>>. many B`
  succeeds on empty input and shadows later alternatives; transcribe literally (11.20:
  `attempt (pSetIdentityColumnGeneration .>>. many option) <|> (many1 option)`).
- **Dispatch order is load-bearing** because `pKeyword` matches non-reserved words too.
  Keep apart: `ALTER TYPE` < `ALTER ROUTINE`; `DROP TYPE` < `DROP ROUTINE`;
  `EXECUTE IMMEDIATE` < `EXECUTE <name>`; `DECLARE LOCAL TEMPORARY TABLE` <
  `DECLARE <cursor>`; `SELECT ... INTO` < `pQuery`; PTF `DESCRIBE WITH …` body <
  generic branch; 20.17 `ALLOCATE … FOR <statement>` < 20.18 `… FOR PROCEDURE`;
  `(VALUES …)` < subquery in `pTablePrimary`; `FINAL|NEW|OLD TABLE` < plain table;
  `SELECT ( <privilege method list> )` < `SELECT [ <column list> ]` (and
  `pPrivilegeMethodItem` must re-check a `<routine type>` is present, else
  `pSpecificRoutineDesignator`'s bare-name form swallows `SELECT (c1, c2)`);
  6.26 navigation < `pRoutineInvocation`/`pColumnReferenceExpression`; 6.37 interval
  alternative < plain parenthesized `pExpression` (same `attempt`); NESTED <
  regular-column in `pJsonTableColumnDefinition`. In `<object name>` (12.3) the
  routine-designator branch must come **first**: `ROUTINE` is non-reserved, so the
  kind branch would swallow `GRANT EXECUTE ON ROUTINE add TO u` as a plain name.
- **Predicate suffixes are gated on the accumulated expression** —
  `pBooleanTestSuffixes` picks per shape: `IsBoolean` → none; other top-level boolean
  → `pBooleanTestPart2` only; `BinaryOp`/`UnaryOp`/`RowValueConstructor` →
  `pPredicateNoBooleanTest`; everything else → full `pPredicate`. A new predicate in
  `PredicateParser.pPredicateImpl` needs three decisions: boolean-primary set? dropped
  by `includeBooleanTest = false`? included in 6.12 `<when operand>` (`forWhenOperand`)?
  Do not fold the suffixes back into `many ( … )` — the gating needs the current
  expression at each step. Operand categories are checked alongside: `pOperand`
  (`<row value predicand>`), `pValueOperand` (value-shaped), `pInValueItem`
  (`<row value expression>`), tested by `isBooleanTopLevel` / `isRowValueExpression` /
  `isValueShaped`.
- **`<table argument>` vs its syntactic twins** — `<table function invocation>` and
  `TABLE ( <query> )` are also `<value expression>`s and `expr AS <name>` is also a
  `<generalized expression>`, so `pTableArgument` accepts them only when a
  table-argument clause follows (`PARTITION BY` / `PRUNE|KEEP WHEN EMPTY` / `ORDER BY`,
  or correlation + derived column list). `COPARTITION` is *not* reserved, so
  `pCorrelationName` and the routine-name parse must reject it explicitly.
  `f(a => b => 1)` is rejected (no nested `<named argument specification>` in 10.4);
  `f(a => f2(b => 1))` is legal — collectors unwrap one level of `SqlArgumentNamed`
  only.
- **Desugared nodes must not be re-checked post-parse** — `COALESCE` expands to a
  searched `Case` with `IsNull` conditions, `NULLIF` to `BinaryOp(Equal, …)`; a
  traversal rejecting boolean operands would reject the legal forms. Hence the
  predicate left-operand rule is enforced at parse time by the suffix gating, and
  `findExpressionViolationIn` checks only standalone `QuantifiedSubquery` and the
  `<period predicate>` left operand (top node of an `opp` parse only).
- **`CAST ( NULL AS DESCRIPTOR )` is not a cast** — `<cast target>` is a
  `<domain name>` or `<data type>`, so `pCastSpecification` rejects `AS DESCRIPTOR`;
  the form belongs to `pDescriptorArgument` (`ExpressionKind.DescriptorCast`).
  `DESCRIPTOR ( … )` in the same slots is the shared 20.16
  `pDescriptorValueConstructor` — do not duplicate it.
- **Do not reuse a parser whose grammar doesn't cover the slot** — `INSERT` keeps
  `pSchemaQualifiedNameExpression` (14.11 `<insertion target>` has no `ONLY` form, so
  `pTargetTable` would wrongly accept `INSERT INTO ONLY (t) …`);
  `pPartitionedJoinColumnReferenceList` is column references only. The omitted DML
  target relies on `SET`/`WHERE` being reserved — never "fix" it with `pIdentifierRaw`,
  which would read `UPDATE SET …`'s `SET` as the table.

## Reserved words and keywords

- **`pReservedFunctionName` is a whitelist** (`functionKeywords`: `COUNT`,
  `ROW_NUMBER`, `ABS`, …); `pRoutineName` wraps it with non-reserved/delimited
  identifiers. Reserved words that start dedicated constructs (`EXISTS`, `UNIQUE`,
  `JSON_EXISTS`, `PERIOD`, `VALUE_OF`) must stay off the whitelist, with their parsers
  listed *before* `pRoutineInvocation` in `pValueExpressionPrimaryImpl`. Adding a
  reserved-name built-in requires adding it to `functionKeywords` or `SELECT ABS(x)`
  fails.
- **`pIdentifierRaw` is too permissive for closed enumerations** (item names that are
  reserved words can't use `pIdentifier`, but `pIdentifierRaw` also accepts
  `ALL`/`SELECT`/…). Use explicit `choice [ pKeyword "…" ]` lists
  (diagnostics/descriptor items, `<language name>`, `<parameter style>`,
  `<char length units>` = `pCharLengthUnits`).
- **A citation must name the defining clause**, not the using one: `<local qualified
  name>` is 5.4, `<char length units>` is 6.1, `<scope option>` is 5.4, `<semicolon>`
  is 5.1 — `RuleNumberingTests` matches against mentioning clauses, so any number works
  there but only the defining clause is correct.
- **Numeric conversions must be checked *and* culture-invariant** — use
  `toUnsignedInteger` / `toDecimal` / `pUnsignedIntegerAsInt` with
  `CultureInfo.InvariantCulture` (de-DE reads `"1.5"` as 15); `runParser`'s try/with is
  a safety net, not the fix.
- **`pLiteral` excludes `<signed numeric literal>` and NULL** — 5.3 admits neither.
  Where the grammar wants a `<simple value specification>` (which does admit signs),
  add the sign form at that slot (`pSimpleValueSpecification`) instead of widening
  `pLiteral` (that would change every `SELECT -1` AST). NULL is the 6.5
  `pNullSpecification` — don't re-add it to `pLiteral` to fix `x = NULL`; that
  over-accepts `SELECT 1 + NULL`.
- **Non-reserved keywords need explicit handling** — `TYPE`, `UNDER`, `OVERRIDING`,
  `INSTANCE`, `CONSTRUCTOR`, `FINAL`, `OPTIONS`, `DERIVED`, `GENERATED`, `SECURITY`,
  `DEFINER`, `INVOKER`, `TRANSFORM`, `STYLE`, `LOCATOR`, `PRESERVE`, `TEMPORARY`,
  `EXTENDED`, `ATTRIBUTES`, `COPARTITION`, `ROUTINE` are *not* reserved: typed-table
  elements need `attempt` on every alternative (dispatchable only after `WITH
  OPTIONS`); `DESCRIPTOR` must be tried before `<data type>` in `<parameter type>`;
  `INSTANCE`/`CONSTRUCTOR` need `attempt` in `pMethodKind`. Conversely `LOG` *is*
  reserved (`INSERT INTO log …` fails — avoid it in tests), as are `METHOD`, `REF`,
  `OUT`, `SYSTEM_TIME`, `VALUE_OF`, `DESCRIBE`, `START`, `STATIC`, `GROUP`,
  `PARAMETER`, `SQL`, `EXTERNAL`, `DEFAULT`.

## Grammar-specific traps

- **`pExpression` includes boolean operators** — where `AND` must not be consumed,
  use the boolean-free parser: `<point in time>` / `FOR PORTION OF` use the 6.35
  datetime parser; JSON slots use `pNonBooleanValueExpression` (a forward ref — the
  JSON parsers are defined before `opp` is built).
- **`pDataType` accepts any identifier as a UDT** — `NESTED PATH '$.items'` would
  read as column `NESTED` of type `PATH`; the UDT branch ends
  `.>>? notFollowedBy pIdentifier` and the NESTED branch is tried first.
- **Recursive/ambiguous forms**: `JSON_ARRAY(NULL ON NULL)` — guard elements with
  `pExpression .>>? notFollowedBy (attempt (pKeyword "ON" >>. pKeyword "NULL"))`;
  `pExplicitRowValueConstructor` needs `attempt` so `(a)` falls through; 6.44
  `SET ( … )` ↔ `pMultisetValueExpression` mutual recursion uses a forward ref;
  `pIntervalSign` must not swallow `->` (`notFollowedBy (pchar '>')`); `<collection
  type>` suffixes are folded (`pDataTypeElement .>>. many … |> List.fold`), never
  self-referential.
- **`expressionChildren` has a `| _ -> []` catch-all** — a new `ExpressionKind`
  case holding an `Expression` silently escapes `findExpressionViolationIn`'s
  checks. Add a branch for every new case; the compiler will not warn. (The
  traversal cannot use `||`/`List.exists` tail-recursively — collect into a list and
  fold an explicit work list.)
- **`Condition` is an `Expression`, not an `ExpressionKind`** — test patterns must
  wrap: `Condition = { Kind = … }`.
- **`pWhereClause` is shadowed in `DataManipulationParser.fs`** — the local
  `(cursor, search condition)` variant must stay below `pSelectStatementSingleRow`
  (14.7, which needs `QueryParser.pWhereClause`); hoisting it above is a type error,
  not a silent rebind.
- **`LockingClause` must live inside the recursive `and`-group** — it carries
  `Expression list option`, and a standalone `type` before the group fails FS0039.

## Test-writing pitfalls

- **The `parse` helpers append `;` and need `(sql: string)`** (FS0072). Tests calling
  `SqlParser.parse` directly must append the semicolon themselves or pass for the
  wrong reason. Use `parse` (22.1) for directly executable statements and
  `parseStatement` (13.4) for cursors / dynamic SQL / positioned DML. When merging
  test files, moved blocks keep whatever helper is in scope — keep a
  `parseStatement`/`parseStatementFails` pair in any file exercising both entry
  points, or rejection tests pass vacuously.
- **`open FParsec` shadows `Result.Ok`/`Result.Error`** with `ReplyStatus` (FS3191) —
  qualify as `Result.Ok` / `Result.Error`.
- **Prefer pattern matching over `Assert.Equal` on `Expression`s** — the `Pos` field
  never matches a hand-written expected value; match the pattern or assert fields.
- **`RuleNumberingTests` regex**: `(\d+\.\d+)\s*<([^<>]+)>` — capturing the leading
  space and stripping with `Substring(1, len-2)` drops the closing `>` and makes the
  test pass vacuously.
- **Reordering source definitions means reordering their tests** (AGENTS.md: compile
  order, then definition order; review-only, so drift is silent). Move the matching
  test block in the same change; rewrite whole files at block level so every test is
  re-inserted exactly once. Order each block by the rule it names, not the umbrella
  parser it calls (`LexerTests.fs` drives 5.2 identifiers through `pIdentifier`).
