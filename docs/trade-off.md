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

## 2026-09 SQL:2016 conformance changes

- 6.4 `<SQL parameter reference>` is represented by the existing `Identifier` /
  `ColumnReference` AST; parameter-vs-column resolution remains semantic. 6.5 shares
  `<implicitly typed value specification>` and `<contextually typed value specification>`
  across CAST, SQL arguments, INSERT/MERGE values, UPDATE/merge assignment, and
  parameter defaults. `ARRAY[]` / `MULTISET[]` therefore use the existing constructor
  nodes rather than adding an information-losing marker.
- Boolean-free parser layers are separate from the full expression operator parser.
  This keeps non-boolean slots (character/numeric/JSON/point-in-time) from consuming
  comparisons or predicate suffixes while preserving the full parser for search conditions.
  ORDER BY sort keys are NOT in that set: 10.10 `<sort key>` is a `<value expression>`, so
  the full parser serves them and boolean sort keys are accepted.
- 6.10 is represented by the existing `WindowFunction` record. `RESPECT NULLS` /
  `IGNORE NULLS` are parsed for LEAD/LAG, FIRST_VALUE/LAST_VALUE and NTH_VALUE;
  `FROM FIRST` / `FROM LAST` are parsed for NTH_VALUE only. Both modifier choices are
  retained in the AST, and unsupported functions or invalid modifier ordering are rejected.
  The grammar-specific aggregate families are validated in the shared routine validation
  block.
- `JSON_ARRAY` now has an additive query-constructor AST case; query bodies remain
  opaque to expression traversal, like the other query-bearing constructor nodes.
- 6.32 `<normalize function result length>` gets a typed `NormalizeResultLength`
  (`<character length> | <character large object length>`) instead of a free expression,
  so `NORMALIZE(x, NFC, 10 + 1)` is rejected. A bare integer is ambiguous between the
  two alternatives, so `<character length>` wins and the large-object case is reached
  only by a `<multiplier>` (`2K OCTETS`).
- 10.9 `<listagg overflow clause>` is a seventh `FunctionCall` field
  (`ListaggError | ListaggTruncate of Expression option * bool`). It is parsed inside
  the argument parentheses, per the production, and rejected for any routine other than
  `LISTAGG`.

## 2026-09-25 SQL:2016 conformance sweep

- **5.2 `<separator>` now includes comments.** `ws` — the separator consumer that follows
  every token — consumes `<simple comment>` and `<bracketed comment>` as well as white
  space, so `SELECT/*c*/1` is `SELECT 1`. A `<simple comment>` is terminated by LF, CR or
  CRLF and, as a documented extension, by the end of the input, so a trailing `-- …`
  comment needs no final newline: `<newline>` is implementation-defined (5.2), which is
  what makes that extension defensible. Bracketed comments are NOT nested — the checked-in
  grammar file defers the nesting rule to the Syntax Rules, which are not part of this
  repository, so the conservative reading is kept; the old 10000-character search limit is
  gone, and each comment alternative is atomic so an unterminated `/*` backtracks cleanly.
- **10.10 `<sort key>` is a `<value expression>`** — boolean sort keys are accepted.
- **6.30/6.32 numeric slots** (`<start position>`, `<regex occurrence>`,
  `<regex capture group>`) use the numeric value expression parser.
- **10.9**: `COUNT ( <asterisk> )` rejects a `<set quantifier>`; `<listagg set function>`
  accepts one (the binary set functions still do not). 6.10 `<lead or lag function>`'s
  `<offset>` is an `<exact numeric literal>`, so exponent notation is rejected —
  approximate literals are a distinct `Literal.ApproximateNumber` case.
- **6.10 `<window row pattern measure>`** — a bare `<measure name>` followed by OVER is a
  `<window function>`; it reuses `WindowFunction` with an empty argument list.
- **11.8**: `<references specification>` is shared by 11.4 and 11.8 and now carries
  `[ MATCH <match type> ]`, the referencing/referenced `<period specification>`s, and a
  `<table name>` (5.4, at most two parts) for the referenced table.
- **11.51** `<partial method specification>` requires a `<returns clause>` and stores the
  full 11.60 `<returns type>`; 11.61 `NAME <external routine name>` accepts the 5.4
  `<character string literal>` form (`Choice<string, Expression>`).
- **5.4 `<schema name>`** is at most two parts, so `CREATE SCHEMA cat.sch.name` is rejected.
- **14.3 `<cursor specification>`** — `parse` (22.2) accepts `[ <updatability clause> ]`
  after a query expression and stores it on the innermost `SelectStatement.Locking`
  (SQL-2016 has no `<lock clause>` in a `<query expression>`, so the slot was free). A
  subquery or `INSERT ... SELECT` is a bare `<query expression>` and does not accept it.
- **20.25/20.27 are preparable-only.** The omitted `<target table>` forms are the text
  handed to PREPARE, so both entry points reject them (13.4 lists 20.23/20.24, which carry
  a target). The DML parsers keep the form for a future preparable-statement surface.
- **20.4** GET DESCRIPTOR targets are `<simple target specification>`s (host parameters
  allowed); **23.1** GET DIAGNOSTICS targets are the same production, so `?` is rejected.
- **20.11 `<using argument>` is a `<general value specification>`** (6.4) — no `<literal>`,
  so `OPEN c USING 1` is rejected; host parameters (with an indicator), `?`, identifiers
  and the CURRENT_*/USER/VALUE keywords are accepted.
- **14.15 `<update target>`** admits the array-element form in all three set-clause shapes.
- **Still open:** 11.4's optional `<data type or domain name>` (typed-table columns) and
  the 20.x extended `<SQL statement name>` / `<dynamic cursor name>` forms
  (`[ GLOBAL | LOCAL ] :c`, `PTF :c`), which need scope information the
  `Expression`-shaped name fields cannot hold.


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
  reported as `<single group specification>`.
- **Opaque embedded languages**: the SQL/JSON path grammar (9.38/9.39) and XQuery-regex
  patterns (8.6) are kept as strings by design.
- **`BEGIN ATOMIC`** bodies go beyond 13.4 (no `<compound statement>` in the grammar).
