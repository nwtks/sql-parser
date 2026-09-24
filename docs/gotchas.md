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
  silently resolves to the wrong type — qualify (`PrivilegeAction.Select`,
  `TableConstraint.Unique`, …) and re-check after moving declarations in `Ast.fs`.
- **Identically-shaped records are inference-ambiguous** — `Expression`, `TableSource`
  and `Statement` all have `Kind` + `Pos`; `CreateViewStatement` and `Cte` collide in
  `pWithListElement`. Qualify the first field (`{ Expression.Kind = … }`) or annotate
  the binding.
- **Value restriction** — a *named* parser binding needs an explicit type
  (`let pLeftBrace: Parser<char, unit> = pchar '{'`; `Parser<string, unit>` for
  `pstring`); inline `token (pstring "{")` is unaffected.
- **Record patterns** — omitted fields are ignored (never `{ Kind = X; _ }`); fields on
  separate lines must start at the same column (FS0010); a list `[` must start on the
  `=` line; an inline list-of-record inside `Some(...)` fails even then — bind
  intermediates and assert on fields. Functions cannot appear in patterns
  (`dataTypeParam Integer` fails).
- **`[<TailCall>]`** cannot be attached to a local `let rec` (FS0010), and a *monadic*
  loop can never satisfy it (FS3569 — `suffix >>= pBooleanTestSuffixes`). Use a
  module-level collector with an explicit work list (`collectSqlArgumentChildren`).

## FParsec combinator pitfalls

- **Precedence** — `<|>` binds tighter than `.>>`/`.>>.`/`>>.`, and `|>>` binds looser
  than `<|>`: parenthesise alternatives and constructor branches (`attempt p |>> C <|> …`
  otherwise parses as `|>> (C <|> …)`).
- **`>>.` discards the left result** — a `>>.`-chain returns the *last* keyword's string
  (`pKeyword "DROP" >>. … .>>. pDropBehavior` yields `string * bool`). Use `.>>.` when
  the value must survive.
- **`|>>` swallows a following `>>=` into its lambda body** — parenthesise the lambda.
  An `|>>` projection cannot fail, so semantic validation needs `>>=` + `fail`.
- **`opt` does not backtrack partial consumption** — wrap optional multi-keyword
  clauses in `attempt` (`pIdentityColumnSpecification`, `pWithCheckOption`,
  `pCursorHoldability`, `FOR UPDATE OF`, the `WITH`-prefixed clauses). Even a single
  optional token hits the trap once input was consumed: `pCharacterSetSpecification`
  needs `opt (attempt (pSqlLanguageIdentifier .>> token (pstring ".")))` or
  `_UTF8'abc'` fails.
- **`notFollowedBy` makes failure fatal** — `opt` cannot catch it; wrap in `attempt`
  (`pExistingWindowName`).
- **`many`/`sepBy1` do not backtrack a consumed token/separator** — postfix loops need
  `many (attempt …)` (dereference/method chains, predicate suffixes), and an optional
  trailing separator needs `p .>>. many (attempt (sep >>. p))` (`t.*`).
- **Whitespace is not skipped automatically** — `pKeyword` skips no *leading* ws; raw
  parsers (`pchar '*'`, `pUnsignedInteger`) consume no *trailing* ws — add `.>> ws`.
- **`pKeyword` returns `Parser<string, unit>`** — mixing it with a `Parser<unit, _>`
  via `<|>` is a type error (coerce with `|>> ignore` / `>>% ()`). The matched string
  keeps the *input casing* (`pstringCI`), so attach the meaning per branch
  (`pKeyword "WITH" >>% true`); it also refuses a keyword immediately followed by an
  identifier character (`SYSTEM` ≠ `SYSTEM_TIME`).

## Definition order, forward references and dispatch

- **Define before use** — move the dependency up, nest single-use sub-parsers inside
  their consumer, or use `createParserForwardedToRef`. Top-level definitions follow
  ascending ISO clause order *best-effort*; `define-before-use` wins
  (`pReferentialTriggeredAction` 11.8 above `pColumnConstraintDefinition` 11.4), and
  uncited helpers stay next to the definition they serve (`withTablePosition`,
  `withExprPosition`).
- **Cross-module refs are wired where the target is defined** (inventory:
  [architecture.md](architecture.md) §5). Reference the forwarding *parser*, never
  `…Ref.Value` — reading `.Value` at initialisation captures FParsec's dummy parser. A
  ref wired inside its defining module only runs because something else forces that
  module's initialiser first (for `pPredicatePrimaryRef`, the §8 refs that
  `SqlParser.fs` wires).
- **An optional-looking sub-rule must not match empty input** — `opt A .>>. many B`
  succeeds on empty input and shadows later alternatives; transcribe literally (11.20:
  `attempt (pSetIdentityColumnGeneration .>>. many option) <|> (many1 option)`).
- **Dispatch order is load-bearing** because `pKeyword` matches non-reserved words too:
  `ALTER TYPE` < `ALTER ROUTINE`; `DROP TYPE` < `DROP ROUTINE`; `EXECUTE IMMEDIATE` <
  `EXECUTE <name>`; `SELECT ... INTO` < `pQuery`; 11.60 `pRoutineBody`'s PTF
  branch < its `<SQL routine spec>` branch (or `DESCRIBE WITH …` resolves to the
  20.10 `<describe statement>`); 20.17 `ALLOCATE … FOR <statement>` < 20.18
  `… FOR PROCEDURE`; `(VALUES …)` < subquery in `pTablePrimary`; `FINAL|NEW|OLD TABLE` <
  plain table; `SELECT ( <privilege method list> )` < `SELECT [ <column list> ]`;
  6.26 navigation < `pRoutineInvocation`/`pColumnReferenceExpression`; 6.37's
  `pDatetimeDifference` < the plain parenthesised expression; in `<object name>` (12.3)
  the routine-designator branch first (`ROUTINE` is non-reserved, else
  `GRANT EXECUTE ON ROUTINE add TO u` reads as a plain name). `pPrivilegeMethodItem`
  must also re-check that a `<routine type>` is present, or the bare-name form of
  `pSpecificRoutineDesignator` swallows `SELECT (c1, c2)`.
- **The two entry points are not nested** — do not write a `parseStatement` test
  expecting a 22.1 form to pass (multi-row `SELECT`, `WITH`, temp table) or a
  `parse` test expecting a 13.4 form (positioned DML, `OPEN`, `CALL`). They
  reject different families; a test that mixes them fails for the wrong reason.
  Static `14.1 DECLARE CURSOR` is rejected by *both* — `pDeclareCursor` has no
  public caller (see [trade-off.md](trade-off.md)).
- **Predicate suffixes are gated on the accumulated expression** —
  `pBooleanTestSuffixes` picks per shape: `IsBoolean` → none; other top-level boolean →
  `pBooleanTestPart2` only; `BinaryOp`/`UnaryOp`/`RowValueConstructor` →
  `pPredicateNoBooleanTest`; else full `pPredicate`. A new predicate in
  `PredicateParser.pPredicateImpl` needs three decisions: boolean primary? dropped when
  `includeBooleanTest = false`? part of 6.12 `<when operand>` (`forWhenOperand`)? Do not
  fold the suffixes back into `many ( … )` — the gating needs the current expression at
  each step. Operand categories are checked alongside: `pOperand` (`<row value
  predicand>`), `pValueOperand` (value-shaped), `pInValueItem` (`<row value
  expression>`), via `isBooleanTopLevel`/`isRowValueExpression`/`isValueShaped`.
- **`<table argument>` vs its syntactic twins** — `<table function invocation>` and
  `TABLE ( <query> )` are also `<value expression>`s, and `expr AS <name>` is also a
  `<generalized expression>`, so `pTableArgument` accepts them only when a
  table-argument clause follows (`PARTITION BY` / `PRUNE|KEEP WHEN EMPTY` / `ORDER BY`,
  or correlation + derived column list). `COPARTITION` is *not* reserved, so it must be
  rejected explicitly as a correlation name and tried before the argument list.
  `f(a => b => 1)` is rejected (10.4 has no nested `<named argument specification>`);
  the collectors unwrap one level of `SqlArgumentNamed` only.
- **Desugared nodes must not be re-checked post-parse** — `COALESCE` expands to a
  searched `Case` with `IsNull` conditions, `NULLIF` to `BinaryOp(Equal, …)`, so a
  traversal rejecting boolean operands would reject legal forms. Hence the predicate
  left-operand rule lives in the parse-time suffix gating, and
  `findExpressionViolationIn` checks only standalone `QuantifiedSubquery` and the
  `<period predicate>` left operand (top node of an `opp` parse).
- **`CAST ( NULL AS DESCRIPTOR )` is not a cast** — `<cast target>` is a
  `<domain name>` or `<data type>`, so `pCastSpecification` rejects `AS DESCRIPTOR`;
  the form is `pDescriptorArgument` (`ExpressionKind.DescriptorCast`).
  `DESCRIPTOR ( … )` in the same slots is the shared 20.16
  `pDescriptorValueConstructor` — do not duplicate it.
- **Do not reuse a parser whose grammar doesn't cover the slot** — `INSERT` keeps
  `pSchemaQualifiedNameExpression` (14.11 `<insertion target>` has no `ONLY` form);
  `pPartitionedJoinColumnReferenceList` is column references only. The omitted DML
  target relies on `SET`/`WHERE` being reserved — never "fix" it with a permissive
  identifier parser, which would read `UPDATE SET …`'s `SET` as the table.

## Reserved words and keywords

- **`pReservedFunctionName` is a whitelist** (`functionKeywords`: `COUNT`,
  `ROW_NUMBER`, …); `pRoutineName` wraps it with non-reserved/delimited identifiers. A
  reserved-name built-in without a dedicated parser must be listed there. Reserved
  words that start dedicated constructs (`EXISTS`, `UNIQUE`, `JSON_EXISTS`, `PERIOD`,
  `VALUE_OF`) must stay off it — their parsers are bundled into `pPredicatePrimary`,
  tried before `pRoutineInvocation` in `pValueExpressionPrimaryImpl`.
- **Closed enumerations need explicit `choice [ pKeyword "…" ]` lists** — a raw
  identifier parser also accepts `ALL`/`SELECT`/…, and reserved words cannot go through
  `pIdentifier`. Used for diagnostics/descriptor items, `<language name>`,
  `<parameter style>` and `<char length units>` (`pCharLengthUnits`).
- **A citation must name the defining clause**, not the using one: `<local qualified
  name>` is 5.4, `<char length units>` is 6.1, `<scope option>` is 5.4, `<semicolon>` is
  5.1 — `RuleNumberingTests` matches against *mentioning* clauses, so any number works
  there but only the defining clause is correct.
- **Numeric conversions must be checked *and* culture-invariant** — use
  `toUnsignedInteger` / `toDecimal` / `pUnsignedIntegerAsInt` with
  `CultureInfo.InvariantCulture` (de-DE reads `"1.5"` as 15); `runParser`'s try/with is
  a safety net, not the fix.
- **`pLiteral` excludes `<signed numeric literal>` and NULL** — 5.3 admits neither.
  Where the grammar wants a `<simple value specification>` (signs allowed), add the sign
  form at that slot (`pSimpleValueSpecification`) instead of widening `pLiteral`, which
  would change every `SELECT -1` AST. NULL is the 6.5 `pNullSpecification` — folding it
  into `pLiteral` over-accepts `SELECT 1 + NULL`.
- **Non-reserved keywords need explicit handling** — `TYPE`, `UNDER`, `OVERRIDING`,
  `INSTANCE`, `CONSTRUCTOR`, `FINAL`, `OPTIONS`, `DERIVED`, `GENERATED`, `SECURITY`,
  `DEFINER`, `INVOKER`, `TRANSFORM`, `STYLE`, `LOCATOR`, `PRESERVE`, `TEMPORARY`,
  `EXTENDED`, `ATTRIBUTES`, `COPARTITION`, `ROUTINE` are *not* reserved: typed-table
  elements need `attempt` on every alternative, `DESCRIPTOR` must precede `<data type>`
  in `<parameter type>`, and `pMethodKind` needs `attempt` on `INSTANCE`/`CONSTRUCTOR`.
  Reserved, by contrast: `LOG` (`INSERT INTO log …` fails — avoid it in tests),
  `METHOD`, `REF`, `OUT`, `SYSTEM_TIME`, `VALUE_OF`, `DESCRIBE`, `START`, `STATIC`,
  `GROUP`, `PARAMETER`, `SQL`, `EXTERNAL`, `DEFAULT`.

## Grammar-specific traps

- **`pExpression` includes boolean operators** — where `AND` must not be consumed, use
  the boolean-free parser: `<point in time>` / `FOR PORTION OF` use the 6.35 datetime
  parser; JSON slots use `pNonBooleanValueExpression` (a forward ref — the JSON parsers
  are defined before `opp` is built).
- **`pDataType` accepts any identifier as a UDT** — `NESTED PATH '$.items'` would read
  as column `NESTED` of type `PATH`; the UDT branch ends
  `.>>? notFollowedBy pIdentifier` and the `NESTED` branch is tried first in
  `pJsonTableColumnDefinition`.
- **Recursive/ambiguous forms** — `JSON_ARRAY(NULL ON NULL)` needs
  `pExpression .>>? notFollowedBy (attempt (pKeyword "ON" >>. pKeyword "NULL"))`;
  `pExplicitRowValueConstructor` needs `attempt` so `(a)` falls through; 6.44
  `SET ( … )` ↔ `pMultisetValueExpression` recursion uses a forward ref; `pIntervalSign`
  must not swallow `->`; `<collection type>` suffixes are folded
  (`pDataTypeElement .>>. many … |> List.fold`), never self-referential.
- **`expressionChildren` has a `| _ -> []` catch-all** — a new `ExpressionKind` case
  holding an `Expression` silently escapes `findExpressionViolationIn`; add a branch for
  every new case (the compiler will not warn). The traversal folds an explicit work list
  — it cannot use `||`/`List.exists` tail-recursively.
- **`Condition` is an `Expression`** — test patterns must wrap: `Condition = { Kind = … }`.
- **`pWhereClause` is shadowed in `DataManipulationParser.fs`** — the local
  `(cursor, search condition)` variant must stay below `pSelectStatementSingleRow`
  (14.7, which needs `QueryParser.pWhereClause`); hoisting it above is a type error.
- **`LockingClause` must live inside the recursive `and`-group** — it carries
  `Expression list option`, and a standalone `type` before the group fails FS0039.

## SQL:2016 conformance implementation notes

- `pSimpleValueSpecification` now includes SQL parameter references, so identifiers are
  intentionally valid in OFFSET/FETCH and other value-specification slots. Keep tests
  explicit: `OFFSET x` is positive, while arithmetic terms remain invalid there.
- `pNonBooleanValueExpression` is a distinct operator-precedence parser. Reusing the full
  `pExpression` in a value-only slot can re-admit comparisons and predicate suffixes.
- `CASE` part-2 predicates are intentionally enabled only in the 6.12 when-operand
  dispatch; the general predicate path remains unchanged.
- `TRUNCATE` now stores the target-table `isOnly` flag. Updating an additive record field
  requires updating every test pattern that constructs the DU case.
- The delta-table and collection-table correlation requirements are parser-level
  constraints, not AST validation; their optional AST fields remain for other forms.


- **The `parse` helpers append `;` and need `(sql: string)`** (FS0072). A test calling
  `SqlParser.parse` directly must append the semicolon itself or pass for the wrong
  reason. Use `parse` (22.1) for directly executable statements and `parseStatement`
  (13.4) for `OPEN`/`FETCH`/`CLOSE`, dynamic SQL and positioned DML — keep a
  `parseStatement`/`parseStatementFails` pair in any file exercising both entry points,
  or rejection tests pass vacuously. The entry points reject *different* families (see
  the dispatch section above), so pick the one whose clause the test targets.
- **`open FParsec` shadows `Result.Ok`/`Result.Error`** with `ReplyStatus` (FS3191) —
  qualify as `Result.Ok` / `Result.Error`.
- **Prefer pattern matching over `Assert.Equal` on `Expression`s** — `Pos` never matches
  a hand-written expected value.
- **`RuleNumberingTests` regex**: `(\d+\.\d+)\s*<([^<>]+)>` — if the name group ever
  captures the leading space, `Substring(1, len-2)` drops the closing `>` and every
  citation is skipped silently.
- **Reordering source definitions means reordering their tests** (AGENTS.md: compile
  order, then definition order; review-only, so drift is silent). Order each test block
  by the rule it names, not the umbrella parser it calls.
