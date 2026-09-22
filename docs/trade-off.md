# Design Trade-offs

Significant design decisions and the alternatives rejected, organised by theme.
The architecture is in [architecture.md](architecture.md); recurring pitfalls in
[gotchas.md](gotchas.md). Check here before "fixing" a rejection or an AST shape.

## Module organisation

- **One module per grammar section** (`SchemaParser` §11, `AccessControlParser` §12,
  `DataManipulationParser` §14 incl. cursors/locators, `PredicateParser` §8, …) rather
  than by "DDL/DML/other"; shared sub-parsers are hoisted to the earliest module all
  callers reach (`<scope clause>` → `ExpressionParser.fs`; `<language clause>`,
  `<method kind>`, `pDropBehavior` → `SchemaParser.fs`; `pGrantor` →
  `AccessControlParser.fs`). **Compile order is leaf-first**; cross-module recursion is
  wired via `createParserForwardedToRef` (statement-level and §8 refs in `SqlParser.fs`)
  — a dummy parser until assignment, in exchange for keeping rules in their natural
  module.
- **Definition order follows the spec clause order**, best-effort (`define-before-use`
  wins; dependency level, then production order; helpers next to their consumer).
  Dispatch/alternative lists keep their historical order — they drive the parse.
  Review-enforced; see [AGENTS.md](../AGENTS.md).

## Entry points and statement dispatch

- **Two entry points, each exact for its clause.** `parse` (22.1) takes only the
  directly executable families and requires the trailing `<semicolon>`;
  `parseStatement` (13.4) takes `<SQL executable statement>` — cursors
  (`OPEN`/`FETCH`/`CLOSE`), `SELECT ... INTO`, positioned DML, `CALL`,
  diagnostics, dynamic SQL. Neither is a superset of the other: `parse` excludes
  the positioned `UPDATE`/`DELETE` forms by explicit guards, `parseStatement`
  excludes multi-row `SELECT`, `WITH`, `DECLARE CURSOR` (14.1) and
  `<temporary table declaration>` (14.16) because 13.4 lists none of them.
  `pStatement` (shared with routine bodies 11.60 and triggers 11.49) is the same
  strict 13.4 choice, so a routine body is `RETURN` / `SELECT ... INTO` / DML —
  never a bare `SELECT`.
- **`DECLARE CURSOR` (14.1) is parsed but unreachable.** Static `DECLARE CURSOR`
  belongs to an SQL-client module (21), which this library does not expose;
  adding a third entry point was rejected in favour of keeping exactly two
  clause-exact public functions. `pDeclareCursor` (DataManipulationParser.fs)
  stays implemented for a future §21 surface; both entry points reject the
  syntax today. Dynamic `DECLARE ... FOR` (20.15) remains reachable through
  `parseStatement`.
- **`CREATE SCHEMA`'s `<schema element>` list is an explicit `choice`** (CREATE-family +
  `GRANT`), so `DROP` / `ALTER` / `TRUNCATE` / `REVOKE` cannot appear there.

## Grammar-faithful strictness

The parser rejects what the standard does not permit, even in common vendor dialects:

- **Mandatory clauses stay mandatory:** `<drop behavior>` (`DROP TABLE t CASCADE`, never
  bare), `<with or without data>` after `CREATE TABLE ... AS`, `ROW`/`ROWS` after
  `OFFSET`, `TABLE` in `TRUNCATE`, ≥ 1 characteristic + `RESTRICT` on `ALTER ROUTINE`,
  no column list/override on `INSERT ... DEFAULT VALUES`.
- **Closed sets stay closed:** `<default option>` (11.5) admits literals, datetime value
  functions and the listed built-ins (`DEFAULT (1 + 2)` / `DEFAULT ?` rejected); no bare
  identifier in `<simple value specification>`; `<language name>`, `<parameter style>`
  and `<char length units>` are keyword enumerations; `<grantor>` is
  `CURRENT_USER | CURRENT_ROLE`; `<sample method>` is `BERNOULLI | SYSTEM`.
- **No non-standard syntax:** no `CREATE`/`DROP INDEX`, `ALTER TABLE ... RENAME`,
  `FOR SHARE` or `NATURAL CROSS JOIN` (the grammar has no such alternative).
- **`pRoutineInvocation` is gated by the reserved *function* keyword whitelist**
  (`functionKeywords` in `Lexer.fs`), so `EXISTS`/`UNIQUE`/`PERIOD`-style words cannot
  degrade to `FunctionCall`; `OVER`/`WITHIN GROUP` suffixes are enforced, and dedicated
  parsers pin arity down (`ABS(a, b)` rejected).
- **Values are validated at the lexer:** numeric literals must fit `decimal` (checked,
  invariant-culture; nothing throws); datetime and interval values are range-checked
  (hours 0–23, time zone within ±14:00…). Calendar validity (February 30) is
  deliberately *not* checked — semantic.
- **Trade-off:** real-world SQL that omits standard-mandated clauses fails to parse
  (consumers would layer extensions on top), but an accepted string is much more likely
  to be valid SQL-2016.

## Expressions and data types

- **Types avoid left recursion by construction** — `pDataTypeElement` + a folded
  `pCollectionType` chain, so typo'd names fail cleanly and `INT ARRAY ARRAY` works.
  Varying types REQUIRE their length; `CharacterLength`/`LargeObjectLength` keep the
  grammar's model, `CharacterTypeWithModifiers` carries `[ CHARACTER SET ] [ COLLATE ]`,
  and a `<collate clause>`'s position decides between that 6.1 type-level slot and the
  enclosing rule's `Collation` field.
- **Quantified comparison uses an intermediate node** — `ANY/SOME/ALL (subquery)` parses
  as a term and the comparison-operator mapping rewrites it; `findExpressionViolationIn`
  rejects any survivor. One AST case instead of 18 operator×quantifier infixes.
- **Reserved built-ins get dedicated AST cases** (datetime functions, `SUBSTRING FROM/FOR`,
  navigation, `RUNNING`/`FINAL`, …); the four regex functions (6.30/6.32) share one
  argument record, but each production's parser only accepts its own optional clauses
  (`WITH` / `OCCURRENCE` / `GROUP` — grammar `sql-2016-grammar.txt` 1819–2088).
- **Postfix constructs reuse existing layers** (`COLLATE` as predicate suffix, multiset
  set-ops as a postfix fold, `<time zone specifier>` over `<interval primary>`) — slightly
  more permissive parents, no new precedence levels.
- **Interval / point-in-time parsing.** 6.37 tries the narrow `(d1 - d2) <qualifier>`
  alternative (`pDatetimeDifference`) first, or `pIntervalPrimary` swallows it; the ±
  chain is left-folded. The 6.35/6.37/6.43/7.16 chain uses the conforming
  `pValueExpressionPrimary` — only `opp.TermParser` uses
  `pValueExpressionPrimaryWithPredicates`, whose sole grammar-exceeding approximation is
  the §8 predicate atoms. `<point in time>` is the 6.35 `<datetime value expression>`
  chain (no `*`, `||`, `=`). The `*` wildcard is not a value expression: `COUNT(*)`, the
  select list and row patterns parse it in their own clauses.
- **JSON.** Paths are opaque strings; JSON argument slots use the boolean-free
  `pNonBooleanValueExpression`; `FORMAT <representation>` is preserved on context and
  passing arguments; behaviour DUs are split (`JsonValueBehavior` / `JsonQueryBehavior`)
  and `JsonType*` prefixed against `ExpressionKind` clashes.
- **Host-language names are not modelled:** `<embedded variable name>` degrades to the
  host-parameter form (`:name` / `?`) the slot already accepts (6.4, 14.17, 20.4).

## Query AST shape

- **`FROM` is a `TableSource list`** (7.5) — the comma stays distinct from `CROSS JOIN`;
  7.4 `<table expression>` requires its `<from clause>`, so bare `SELECT 1` is rejected.
- **Set-operation tails live in a `QueryExpression` case** carrying
  `ORDER BY`/`OFFSET`/`FETCH`/`LOCKING`; plain `SELECT ... ORDER BY` folds into
  `SelectStatement`; `INTERSECT` binds tighter than `UNION`/`EXCEPT`.
- **`GROUP BY` is `GroupingElement list`** so `(a, b)` is one grouping set.
- **Correlation handling per source**: `Only`/`DataChangeDelta` optional aliases,
  `Lateral`/`Unnest` mandatory, `TableSample` wraps a `TableSource`; parenthesized table
  refs are only `<joined table>`s.
- **`TABLE (expr)` is disambiguated by shape** (`PtfTable` iff `FunctionCall`) — no
  parse-only classifier can do better.

## DDL and DML AST shape

- **Constraints**: `ColumnDefinition.Constraints` plus derived convenience accessors
  (redundant by design); `ColumnConstraintKind` has no bare `NULL`; 11.4's slot is one
  `opt` over a `Choice` (`GENERATED ALWAYS AS IDENTITY DEFAULT 5` rejected);
  `ConstraintCharacteristics` is three `bool option`s in grammar order.
- **`CREATE TABLE`** carries optional clauses as dedicated fields (`Under`/`Like`/
  `Periods`/`AsQuery`/`TypedElements`…) rather than exploding DU cases; `<table element>`
  is a four-way `Choice`.
- **Sequence options** are one shared `SequenceOption` DU with per-slot subsets (no
  option kind leaks into the wrong clause).
- **`ALTER TABLE`** models every 11.10 action; `<drop behavior>` is a `bool`;
  `AddTablePeriod`'s column list holds exactly 0 or 2 entries (11.27 requires both).
- **DML**: `SetClause` tried MultipleSet → MutatedSet → SingleSet; `DEFAULT` is only an
  insert/update value, never a general expression; `DmlTarget = TableTarget |
  OmittedTarget` guards positioned forms; `ONLY ( t )` applies to UPDATE/DELETE/MERGE,
  not `INSERT` (14.11 has no ONLY form).
- **Flat `StatementKind` cases** for every `DROP` variant and every 12.3 `<object name>`
  kind of `GRANT`/`REVOKE` (`GrantObject`, `GrantTable`, …, `GrantRoutine`), wrapping
  shared payload records; `GrantRoles`/`RevokeRoles` stay separate. `PrivilegeSelectTarget`
  separates method lists from column lists.

## Routines, triggers and types

- **One `CreateRoutine` record** for procedures/functions (`Returns = None` ⇔ procedure);
  `CREATE METHOD` stays separate (no `<routine characteristics>` slot there).
- **11.60/11.61 share one duplicate check but have different characteristic sets**
  (`NAME` is 11.61-only; `ALTER ROUTINE` rejects `SPECIFIC`/deterministic/savepoint-level).
- **`RoutineBody`** = `SqlRoutine` | `ExternalRoutine` | `PolymorphicTableFunction` |
  `BeginAtomic` (an extension — 13.4 has no `<compound statement>`); the PTF branch is
  tried first so a body starting with `DESCRIBE` is not read as a §20 statement.
- **`<specific routine designator>` is one record** shared by every designator slot
  (ALTER ROUTINE, CREATE CAST, ORDERING, TRANSFORM, PTF components, privilege method
  lists); `IsSpecific`/`RoutineType` are optional because a bare name is legal — hence
  `pPrivilegeMethodItem`'s re-check (see gotchas.md).
- **Parameter/return types**: generic-table and descriptor types are tried before
  `<data type>` because `DESCRIPTOR` is non-reserved; the parameter name backtracks, so
  `IN mytype` is an unnamed parameter of type `mytype`, `IN p1 mytype` the named form.

## Lexical modelling

- **SQL terminals are modelled as they are used (5.1)** — only `{ } ^ | $` and the
  `{-`/`-}` compound tokens (row patterns) get parsers; `<percent>` / `<reverse solidus>`
  occur only inside opaque embedded languages (8.6 regex, 9.38/9.39 JSON path) or
  nowhere, so parsers for them would only suggest the parser understands that text.

## NULL is a null specification, not a literal

5.3 `<literal>` has no NULL alternative — NULL is the 6.5 `<null specification>`, so
`pLiteral` rejects it and `pNullSpecification` is OR-ed in at the contextually-typed slots
the grammar allows (6.13 cast operand, 10.4 SQL argument, 11.5 default option, 11.60
parameter default, 14.11/14.12/14.15 DML values, 16.2 return value); the CASE `<result>`
takes a bare `NULL`. Keyword slots (`IS NULL`, `SET NULL`, `NULLS FIRST`, JSON `ON NULL`)
are untouched, and `WHERE x = NULL`, `SELECT 1 + NULL`, `ABS(NULL)`, `COALESCE(NULL, 1)`
are rejected. `Literal Null` stays in the AST — it is what `pNullSpecification` and the
`NULLIF` desugar produce.

## Parenthesized expressions keep their parens

6.3's parenthesized alternative wraps its result in `ExpressionKind.Parenthesized` instead
of flattening, so `(1 = 1)` is a `<boolean predicand>` and `x BETWEEN (1 = 1) AND 2` /
`(a = b) IS TRUE` parse; `expressionChildren` forwards through it transparently.

## Predicate, comparison and period operands

- **Suffix gating**: after a top-level boolean only the 6.39 boolean test remains; a
  boolean test takes no further suffix; terms/signed primaries are not `<boolean
  primary>`s (`1 = 2 IS NULL`, `x IS TRUE IS FALSE`, `1 + 1 IS TRUE` all rejected) —
  hence one suffix at a time (`pBooleanTestSuffixes`) and a separate
  `pPredicateNoBooleanTest`.
- **Operand categories**: `pOperand` = `<row value predicand>` (BETWEEN / IS DISTINCT /
  OVERLAPS), `pInValueItem` = `<row value expression>` (`x IN (1 + 1)` / `x IN ((1), 2)` /
  `x IN (-1)` rejected), `pValueOperand` = value-shaped (LIKE/SIMILAR/regex pattern,
  escape, FLAG, multiset operands reject explicit rows). Type-level distinctions stay
  unchecked — semantic.
- **Comparison / period**: `pValueExpressionChecked` rejects top-level boolean operands of
  comparisons; a `<period predicate>`'s left operand is checked post-parse
  (`findExpressionViolationIn`) since it parses before the suffix runs.
- **Desugars narrow the checks deliberately**: `COALESCE` → searched case with `IsNull`
  conditions, `NULLIF` → `BinaryOp(Equal, …)` — hence parse-time gating for the null
  predicate and a top-node-only comparison check (`COALESCE(1 = 2, TRUE)` and
  `NULLIF(1 = 2, 3)` stay legal).

## 6.12 `<when operand>` part-2 forms

`CASE x WHEN = 1 / IS NULL / BETWEEN 1 AND 2 / IN (…) …` parses: the `<case operand>`
supplies part 1, so such a case is represented as a **searched case** (each simple when
clause becomes the OR of its predicates) — not round-trippable, no AST case added.
`pPredicateImpl`'s `forWhenOperand` flag carries 6.12's narrower alternative list.

## CAST FORMAT and the 10.4 descriptor argument

`CAST` carries the optional `FORMAT <cast template>` (third field); `AS DESCRIPTOR` is
rejected by `pCastSpecification` (a `<cast target>` is a domain name or data type). The
descriptor forms live in 10.4's `<descriptor argument>`: `DESCRIPTOR ( a INT, b )` (the
20.16 constructor, shared with 11.60 `<parameter default>`) and
`CAST ( NULL AS DESCRIPTOR )` (`ExpressionKind.DescriptorCast`); a standalone
`CAST ( NULL AS DESCRIPTOR )` stays rejected.

## §10.4 SQL arguments are structured

Invocation nodes carry a `SqlArgumentList`; each argument is `SqlArgumentValue |
Generalized | Named | Table | Descriptor`. Heuristics, since a parse-only library cannot
resolve names: a table-function/`TABLE (query)` proper counts as a `<table argument>` only
with a following clause; `expr AS name` is generalized unless a column list/clause
follows; `COPARTITION` (non-reserved) is excluded from correlation/routine-name
positions; reserved built-ins take value arguments only (`SUM(a, TABLE(t))` rejected).

## Deliberate deviations (documented, not fixed)

- **6.37 in a general `<value expression>`**: `INTERVAL '1' DAY * ? DAY` parses under
  6.35 `<point in time>` / 19.4 `SET TIME ZONE` but not in a select list, where `*` is
  numeric multiplication. The same type-level blind spot covers interval-vs-datetime
  operands, calendar validity, binary `POSITION ... USING` (only the character form has
  the slot) and character-vs-numeric predicate operands.
- **Syntactic ambiguities**: kind-less `GRANT ... ON <name>` reads as a table grant;
  `TABLE (expr)` PTF classification is shape-based; a lone `TRANSFORM GROUP g` is
  reported as `<single group specification>`; `NORMALIZE`'s result length is parsed as
  a plain expression.
- **Opaque embedded languages**: the SQL/JSON path grammar (9.38/9.39) and XQuery-regex
  patterns (8.6) are kept as strings by design.
- **`BEGIN ATOMIC`** bodies go beyond 13.4 (no `<compound statement>` in the grammar).
