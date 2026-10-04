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
  `AccessControlParser.fs`; `pExtendedName` → `DataManipulationParser.fs`).
  **Compile order is leaf-first**; cross-module recursion is wired via
  `createParserForwardedToRef` (statement-level and §8 refs in `SqlParser.fs`)
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
- **20.25/20.27 (omitted `<target table>`) are preparable-only.** Both entry points
  reject them (13.4 lists the targeted 20.23/20.24); the dynamic dispatch reroutes
  the DML parsers through `rejectOmittedTarget` (`SqlParser.fs`). The DML parsers
  keep the form for a future preparable-statement surface.
- **`CREATE SCHEMA`'s `<schema element>` list is an explicit `choice`** (CREATE-family +
  `GRANT`), so `DROP` / `ALTER` / `TRUNCATE` / `REVOKE` cannot appear there.
- **`TRUNCATE` (14.10) is dispatched through `pSqlSchemaStatement`** (an 11.x
  dispatcher) as a routing convenience, although it is not an 11.x `<schema element>`.

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
- **5.4 name arity is enforced per production.** `pSchemaQualifiedNameExpression`
  (three parts) is shared, but narrower slots use a dedicated parser in
  `ExpressionParser.fs`: `pIdentifierNameExpression` (bare `<identifier>` — the 20.15
  `<statement name>` and `<non-extended descriptor name>`, so `EXECUTE a.b` is
  rejected); `pSchemaNameExpression` (≤ 2 parts — `DROP SCHEMA`, 10.3
  `PATH <schema name list>`); `pCharacterSetNameExpression` (`[ <schema name> . ]
  <SQL language identifier>`, so up to THREE parts with an ASCII-only final part —
  `CREATE SCHEMA … DEFAULT CHARACTER SET`, 11.41/11.42, the 11.43 `FOR` slot, 11.45's
  `FOR`/`TO` slots, 12.3's `CHARACTER SET`, and the 6.1 `CHARACTER SET` modifier);
  `<collation name>` keeps three; `pLocalQualifiedNameExpression` (`<local qualified
  name>`, every `<cursor name>` slot — `MODULE.c` yes, `a.b.c` no); `pTableNameExpression`
  (5.4 `<table name> ::= <local or schema qualified name>` — **both** `a.b.c` and
  `MODULE.c`, used by 11.3/11.8/11.10/11.17/11.31/11.32/11.49, 12.3's bare
  `[ TABLE ] <table name>`, 14.x targets and the 7.6/7.17 table-name slots); and
  `pIdentifierExpression` where the production is a bare `<identifier>` — 5.4
  `<external routine name>` (11.60/11.61 `EXTERNAL NAME`) and 11.67 `<group name>`
  (11.67/11.68/11.71 transforms). Checked *after* parsing (`pNameOfArity`) so the failure
  can name the production. The other direction: `<constraint name>` (10.8) is a
  `<schema qualified name>`, so 11.4/11.6/11.24–11.26 and 17.4 `SET CONSTRAINTS` accept
  qualified names, and 11.8's referenced `<table name>` may be three parts or `MODULE.t`.
- **6.4 value-specification widths:** `?` is a `<dynamic parameter specification>` and
  belongs to `<general value specification>` / `<target specification>` only — a slot
  whose BNF says `<simple value specification>` rejects it (`OFFSET ? ROWS`,
  `CONNECT TO ?`, …); 6.11 `<row marker offset>` re-adds it explicitly.
- **6.1 type slots are exact.** `<predefined type>` has no `<row type>` alternative
  (`CREATE DOMAIN d AS ROW(a INT)` rejected), and `<referenced type>` is only a
  `<path-resolved user-defined type name>` (`REF(INTEGER)` rejected). 6.26
  `<logical offset>` / `<physical offset>` re-add `?` beside `<simple value
  specification>` (`FIRST(x, ?)` parses).
- **Operand slots use the grammar's narrower production.** 6.4 `<current collation
  specification>` takes a `<string value expression>` (`COLLATION FOR (1 = 1)`
  rejected), 6.25 `<multiset element reference>` a `<multiset value expression>`
  (`ELEMENT(1 = 1)` rejected), and 6.17 `<generalized invocation>` / 6.23
  `<reference resolution>` take a `<value expression primary>` (`(1 + 1 AS t).m` /
  `DEREF(1 = 1)` rejected). 6.27 `<JSON value empty/error behavior>` `DEFAULT` takes a
  full `<value expression>` (`DEFAULT (1 = 1)` parses).
- **Comparison operands must be `<row value predicand>`s (8.2/7.2).** 7.2 reaches
  `<common value expression>` through 7.1 `<row value constructor predicand>`, so terms
  and signed primaries are valid (`a + b = c`, `-x = 1`); the only rejection is a
  **top-level** boolean (`isBooleanTopLevel`), and `(1 = 1)` survives parenthesized. The
  6.39 `<boolean test>` has a separate, narrower gate (`isBooleanPredicand`): `1 + 1 IS
  TRUE` is rejected while `1 + 1 IS NULL` (8.8, a wider operand) parses. 8.4 `IN`-list
  items are `<row value expression>` and stay strict (`x IN (1 + 1)` rejected,
  `isRowValueExpression`); a `<period predicate>`'s left operand is checked post-parse
  (`findExpressionViolationIn`).
- **`<search condition>` (8.21) rejects a `<term>` but not a bare primary.** 6.39
  `<boolean value expression>` bottoms out at `<boolean primary> ::= <predicate> |
  <boolean predicand>`, and `<boolean predicand>` includes any
  `<nonparenthesized value expression primary>`. So `WHERE 1`, `CHECK (1)` and
  `HAVING 'x'` are **grammar-valid** (rejecting them would be a type check — semantic,
  and deliberately out of scope), while `WHERE 1 + 1` and `CHECK (a + b)` are not: a
  6.29 `<term>` or a signed `<numeric primary>` is not a primary. `pSearchCondition`
  (`ExpressionParser.fs`) draws exactly that line and gates 7.9 `<row pattern
  definition>`, 7.12 `<where clause>`, 7.14 `<having clause>`, 10.9 `<filter clause>`,
  the `CHECK` clauses of 11.4/11.6/11.34/11.47, 11.49 `<triggered action>` and the
  `WHERE`/`ON`/`AND` slots of 14.9/14.12/14.14. 11.60 `<parameter default>` uses the
  mirrored rule (`isBooleanTopLevel`), because 6.28 `<value expression>` there excludes
  booleans entirely.
- **`pRoutineInvocation` is gated by the reserved *function* keyword whitelist**
  (`functionKeywords` in `Lexer.fs`), so `EXISTS`/`UNIQUE`/`PERIOD`-style words cannot
  degrade to `FunctionCall`; `OVER`/`WITHIN GROUP` suffixes are enforced, and dedicated
  parsers pin arity down (`ABS(a, b)` rejected).
- **Values are validated at the lexer:** numeric literals must fit `decimal` (checked,
  invariant-culture; nothing throws); datetime and interval values are range-checked
  (hours 0–23, time zone within ±14:00…). Calendar validity (February 30) is
  deliberately *not* checked — semantic.
- **6.5 `<implicitly typed value specification>` reaches CAST** (6.13 `<cast operand>`),
  so `CAST(NULL AS t)` and `CAST(ARRAY[] AS INTEGER)` parse — 6.42/6.45 require at least
  one element, so only `<empty specification>` reaches them.
- **Trade-off:** real-world SQL that omits standard-mandated clauses fails to parse
  (consumers would layer extensions on top), but an accepted string is much more likely
  to be valid SQL-2016.

## Intentional deviations from SQL:2016

Single source of truth for every knowing departure from `sql-2016-grammar.txt`, by
direction.

### Extensions (accept syntax the grammar does not)

- `BEGIN ATOMIC` in routine bodies (11.60) — a compound-statement extension mirroring
  the 11.49 `<triggered SQL statement>` arm (`{ <SQL procedure statement> <semicolon> }... END`,
  trailing `;` included); 13.4 has no `<compound statement>`.
- Trailing `--` comment without a final newline (5.2) — `<newline>` is
  implementation-defined, which makes it defensible.
- 20.15 dynamic `DECLARE CURSOR` is reachable through `parseStatement` although the
  grammar places `<declare cursor>` only in §21.
- Flat `StatementKind` DU vs the grammar's nested dispatch — structural relaxation.

### Omissions (grammar productions not implemented)

- `<direct implementation-defined statement>` (22.1) is not wired into
  `pDirectSqlStatement` — there are no implementation-defined statements to accept.
- `<embedded variable specification>` (6.4) is not parsed by
  `pGeneralValueSpecification` / `pSimpleValueSpecification` — embedded SQL is out of
  scope; host-language names degrade to host parameters elsewhere.
- 14.1 `<declare cursor>` / 14.16 `<temporary table declaration>` are parsed but
  unreachable — see "Entry points".
- **11.4's optional `<data type or domain name>` inside 11.3 `<table element list>`**
  — the type stays REQUIRED there, because `CREATE TABLE t (id, name) AS SELECT …`
  needs `pColumnDefinition` to reject `(id, name)` so the `( <column name list> )` slot
  of 11.3 `<as subquery clause>` can match. Typed-table columns route through
  `ColumnOptions`, which has no type slot. The optional form **is** used by 11.11
  `<add column definition>` and 11.27 `<add system time period column list>`, which
  have no competing alternative (`pColumnDefinitionNoType`).
- **11.27 with UNTYPED period columns** is genuinely ambiguous —
  `… ADD PERIOD FOR SYSTEM_TIME (s, e) ADD COLUMN s ADD COLUMN e` reads as columns
  `s`/`s` or `s`/`e`, and greedy parsing takes the first. Only the typed form is
  exercised by the tests.
- §13.1–13.3 (SQL-client module definition) and all of §21 (embedded SQL) are out of
  scope — no public surface.
- **`MATCH_RECOGNIZE` after a non-`<table or query name>`** (7.6). 7.6 puts
  `<row pattern recognition clause and name>` in the `<correlation or recognition>` slot
  of *every* `<table primary>`, but only `<table or query name>` has an AST node
  (`TableSourceKind.MatchRecognize`) that can represent it — `Subquery`, `ValuesTable`,
  `Lateral`, `Unnest` and `JsonTable` all record a correlation **name**. Accepting
  `FROM UNNEST(a) MATCH_RECOGNIZE (…)` would hand consumers a plain `Unnest` with the
  whole clause silently dropped, so those forms are **rejected** with an explanatory
  message instead (`pNamedCorrelation` / `pOptionalNamedCorrelation` in
  `QueryParser.fs`). `FROM t MATCH_RECOGNIZE (…)` works and folds the table name into the
  clause's optional input-name slot.
- **`PERMUTE` inside a row pattern primary is a committed keyword** (7.9). `PERMUTE` is
  not in `Lexer.reservedWords` (keeping it usable as an ordinary identifier, like
  `MEASURES`), so the `PERMUTE ( … )` branch of `pRowPatternPrimary` is deliberately NOT
  `attempt`-wrapped: fewer than two comma-separated `<row pattern>`s (e.g.
  `PATTERN (PERMUTE (A))`) fails the whole pattern instead of backtracking into a
  variable-named-`PERMUTE` + parenthesized-group reading.

### Relaxations (accept input the grammar rejects)

- Predicate atoms / interval / multiset-operand widening (6.3/6.37/6.43) and `<when
  operand>` accepting predicate part-2 (6.12) — see "Interval / point-in-time parsing"
  and "Predicate, comparison and period operands".
- `<embedded variable name>` degrades to a host parameter (6.4/14.17).
- **7.17 set-operation associativity follows the left-recursive grammar.** Repeated
  `UNION`/`EXCEPT` within a query-expression body and repeated `INTERSECT` within a query
  term are accepted and folded left; `INTERSECT` still binds more tightly than
  `UNION`/`EXCEPT`. Parentheses can override that grouping.
- **AST collapses** (parsed, but the node cannot represent every alternative):
  - 8.22 `<JSON key uniqueness constraint> [ KEYS ]` — the optional `KEYS` keyword is
    parsed and discarded, so `IS JSON WITH UNIQUE` and `IS JSON WITH UNIQUE KEYS` yield
    an identical AST.
  - 7.11 `[ <quotes behavior> QUOTES [ ON SCALAR STRING ] ]` — `JsonQueryQuotes` is
    `Keep | Omit`, so the `ON SCALAR STRING` qualifier is parsed and discarded.

---

The remaining known departures are kept deliberately and documented where they arise:
the `TRUNCATE` routing above, the §6.37-in-a-value-expression blind spot ("Deliberate
deviations" below), and the syntactic ambiguities listed there.

## Expression and type parsing

- **Types avoid left recursion by construction** — `pDataTypeElement` + a folded
  `pCollectionType` chain, so typo'd names fail cleanly and `INT ARRAY ARRAY` works.
  Varying types REQUIRE their length; `CharacterLength`/`LargeObjectLength` keep the
  grammar's model, `CharacterTypeWithModifiers` carries `[ CHARACTER SET ] [ COLLATE ]`,
  and a `<collate clause>`'s position decides between that 6.1 type-level slot and the
  enclosing rule's `Collation` field.
- **Boolean-free parser layers are separate from the full expression operator parser**,
  keeping non-boolean slots (character/numeric/JSON/point-in-time) from consuming
  comparisons or predicate suffixes. ORDER BY sort keys are NOT in that set: 10.10
  `<sort key>` is a `<value expression>`, so boolean sort keys are accepted.
  6.30/6.32 numeric slots (`<start position>`, `<regex occurrence>`,
  `<regex capture group>`) use the numeric value expression parser.
- **Quantified comparison uses an intermediate node** — `ANY/SOME/ALL (subquery)` parses
  as a term and the comparison-operator mapping rewrites it; `findExpressionViolationIn`
  rejects any survivor. One AST case instead of 18 operator×quantifier infixes.
- **Reserved built-ins get dedicated AST cases** (datetime functions, `SUBSTRING FROM/FOR`,
  navigation, `RUNNING`/`FINAL`, …); the four regex functions (6.30/6.32) share one
  argument record, but each production's parser only accepts its own optional clauses
  (`WITH` / `OCCURRENCE` / `GROUP`).
- **Postfix constructs reuse existing layers** (`COLLATE` as a primary postfix — 6.31
  `<character factor>` — so it binds tighter than every operator and a comparison operand
  like `x = 'a' COLLATE c` parses; multiset set-ops as a postfix fold; `<time zone
  specifier>` over `<interval primary>`) — slightly more permissive parents, no new
  precedence levels.
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
  and `JsonType*` prefixed against `ExpressionKind` clashes. `JSON_ARRAY` has an
  additive query-constructor AST case; query bodies remain opaque to expression
  traversal, like the other query-bearing constructor nodes.
- **Host-language names are not modelled:** `<embedded variable name>` degrades to the
  host-parameter form (`:name` / `?`) the slot already accepts (6.4, 14.17, 20.4).
- **6.4 `<SQL parameter reference>` is represented by the existing `Identifier` /
  `ColumnReference` AST**; parameter-vs-column resolution remains semantic. 6.5 shares
  `<implicitly typed value specification>` and `<contextually typed value specification>`
  across CAST, SQL arguments, INSERT/MERGE values, UPDATE/merge assignment, and
  parameter defaults. `ARRAY[]` / `MULTISET[]` therefore use the existing constructor
  nodes rather than adding an information-losing marker.

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
- **Operand categories**: `pOperand` (BETWEEN / IS DISTINCT / OVERLAPS part-2) and the
  `<when operand>` / `<case operand>` slots accept a `<row value predicand>` — terms
  included (`x BETWEEN a + b AND c`), and `(1 = 1)` / `(1 + 2)` stay legal because the
  `Parenthesized` node survives — but never a TOP-LEVEL boolean (`1 = 1`);
  `isPredicateOperand` (ExpressionParser.fs) is exactly "not top-level boolean".
  `pInValueItem` = `<row value expression>` (`x IN (1 + 1)` / `x IN ((1), 2)` /
  `x IN (-1)` rejected — parenthesized items are NOT accepted there), `pValueOperand` =
  value-shaped (LIKE/SIMILAR/regex pattern, escape and FLAG — an explicit row value
  constructor AND a 6.29 `<term>` / signed primary are rejected, while a 6.31
  concatenation or a `COLLATE` suffix stays legal), and a `<multiset value expression>`
  rejects an explicit row value constructor at its base (`x MEMBER OF (1, 2)` /
  `x SUBMULTISET OF ROW(1, 2)` rejected). Type-level distinctions stay unchecked —
  semantic.
- **Desugars narrow the checks deliberately**: `COALESCE` → searched case with `IsNull`
  conditions, `NULLIF` → `BinaryOp(Equal, …)` — hence parse-time gating for the null
  predicate and a top-node-only comparison check (`COALESCE(1 = 2, TRUE)` and
  `NULLIF(1 = 2, 3)` stay legal).
- **6.12 `<when operand>` part-2 forms**: `CASE x WHEN = 1 / IS NULL / BETWEEN 1 AND 2 /
  IN (…) …` parses — the `<case operand>` supplies part 1, so such a case is represented
  as a **searched case** (each simple when clause becomes the OR of its predicates);
  not round-trippable, no AST case added. `pPredicateImpl`'s `forWhenOperand` flag
  carries 6.12's narrower alternative list.

## Query AST shape

- **`FROM` is a `TableSource list`** (7.5) — the comma stays distinct from `CROSS JOIN`;
  7.4 `<table expression>` requires its `<from clause>`, so bare `SELECT 1` is rejected.
- **Set-operation tails live in a `QueryExpression` case** carrying
  `ORDER BY`/`OFFSET`/`FETCH`/`LOCKING`; plain `SELECT ... ORDER BY` folds into
  `SelectStatement`; `INTERSECT` binds tighter than `UNION`/`EXCEPT`.
- **`GROUP BY` is `GroupingElement list`** so `(a, b)` is one grouping set; the
  quantifier is `GroupByQuantifier: SetQuantifier`.
- **Correlation handling per source**: `Only`/`DataChangeDelta` optional aliases,
  `Lateral`/`Unnest` mandatory, `TableSample` wraps a `TableSource`; `Table` takes a
  derived column list, `Unnest` an operand `Expression list` (7.6); parenthesized table
  refs are only `<joined table>`s.
- **`TABLE (expr)` is disambiguated by shape** (`PtfTable` iff `FunctionCall`) — no
  parse-only classifier can do better.
- **14.3 `<cursor specification>`** — `parse` (22.2) accepts `[ <updatability clause> ]`
  after a query expression and stores it on the innermost `SelectStatement.Locking`
  (SQL-2016 has no `<lock clause>` in a `<query expression>`, so the slot was free). A
  subquery or `INSERT ... SELECT` is a bare `<query expression>` and does not accept it.

## DDL and DML AST shape

- **Constraints**: `ColumnDefinition.Constraints` plus derived convenience accessors
  (redundant by design); `ColumnConstraintKind` has no bare `NULL`; 11.4's slot is one
  `opt` over a `Choice` (`GENERATED ALWAYS AS IDENTITY DEFAULT 5` rejected);
  `ConstraintCharacteristics` is three `bool option`s in grammar order.
  `<references specification>` is shared by 11.4 and 11.8 and carries
  `[ MATCH <match type> ]`, the referencing/referenced `<period specification>`s, and a
  `<table name>` (5.4 — up to three parts, or `MODULE.t`) for the referenced table.
- **`CREATE TABLE`** carries optional clauses as dedicated fields (`Under`/`Like`/
  `Periods`/`AsQuery`/`TypedElements`…) rather than exploding DU cases; `<table element>`
  is a four-way `Choice`. The 11.3 `<as subquery clause>` requires the `<subquery>`
  parentheses (`AS (SELECT …)`, never bare `AS SELECT …`) — a real-world regression
  risk accepted for conformance; the view form (11.32) is unaffected.
- **Sequence options** are one shared `SequenceOption` DU with per-slot subsets (no
  option kind leaks into the wrong clause).
- **`ALTER TABLE`** models every 11.10 action; `<drop behavior>` is a `bool`;
  `AddTablePeriod`'s column list holds exactly 0 or 2 entries (11.27 requires both).
- **DML**: `SetClause` tries MultipleSet → MutatedSet → SingleSet; `DEFAULT` is only an
  insert/update value, never a general expression; `DmlTarget = TableTarget |
  OmittedTarget` guards positioned forms; `ONLY ( t )` applies to UPDATE/DELETE/MERGE,
  not `INSERT` (14.11 has no ONLY form); 14.15 `<update target>` admits the
  array-element form where the grammar permits it, multiple-assignment targets retain
  their ordinary-vs-mutated distinction, and `MergeAction.MergeUpdate`
  holds a `SetClause list`. 14.11 `<from constructor>` admits a nonparenthesized
  `<contextually typed value specification>` row (`INSERT INTO t VALUES NULL` parses).
- **Flat `StatementKind` cases** for every `DROP` variant and every 12.3 `<object name>`
  kind of `GRANT`/`REVOKE` (`GrantTable`, …, `GrantRoutine`), wrapping
  shared payload records; `GrantRoles`/`RevokeRoles` stay separate. `PrivilegeSelectTarget`
  separates method lists from column lists. 12.3's routine alternative carries the whole
  `SpecificRoutineDesignator` (`SPECIFIC <routine type> …`, `… FOR <type>`), with a
  literal variant `pTypedSpecificRoutineDesignator` (mandatory `<routine type>`, no
  `<data type list>`) so the permissive parser cannot swallow the kind-keyword and
  `TABLE` alternatives of the same `<object name>`; 10.6's `<data type list>` is parsed
  (`GRANT EXECUTE ON ROUTINE add (INTEGER)`).

## Routines, triggers and types

- **One `CreateRoutine` record** for procedures/functions (`Returns = None` ⇔ procedure);
  `CREATE METHOD` stays separate (no `<routine characteristics>` slot there).
- **11.60/11.61 share one duplicate check but have different characteristic sets**
  (`NAME` is 11.61-only; `ALTER ROUTINE` rejects `SPECIFIC`/deterministic/savepoint-level).
  11.51 `<partial method specification>` requires a `<returns clause>` and stores the
  full 11.60 `<returns type>`; 11.61 `NAME <external routine name>` accepts the 5.4
  `<character string literal>` form (`Choice<string, Expression>`).
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

## §20 dynamic SQL names and dispatch

- **§20 name slots carry `ExtendedName`** (`{ Scope: ScopeOption option; SimpleValue:
  Expression }`, parsed by `pExtendedName` = `[ <scope option> ] <simple value
  specification>`, 20.17) on `Prepare`, `DeallocatePrepare`, `Execute`,
  `DescribeStatement`, `AllocateDescriptor`, `DeallocateDescriptor`, `GetDescriptor`,
  `SetDescriptor`, `CopyDescriptorStatement.Source`, and `UsingClause.UsingDescriptor`.
  The scope slot stays `None` where the grammar has no extended form — `ALLOCATE
  DESCRIPTOR` takes only a `<conventional descriptor name>` (the scope option belongs to
  the cursor form, `ALLOCATE <extended cursor name> FOR <extended statement name>`).
  Exceptions kept strict: the CURSOR branch of 20.10 uses the 5.4
  `<local qualified name>`; 20.15 uses the plain `<statement name>` (`Scope = None`) —
  `DECLARE c CURSOR FOR LOCAL :s` is rejected while 20.17 `ALLOCATE … FOR LOCAL :s`
  works; `PTF :c` keeps its separate `pTargetDescriptorName` (20.6, 20.28).
- **The five `<SQL dynamic data statement>` alternatives (20.19/20.20/20.22/20.23/20.24)
  are wired into `pSqlDynamicStatement`.** `pDynamicOpenStatement`,
  `pDynamicFetchStatement` and `pDynamicCloseStatement` (`DynamicParser.fs`) accept the
  20.17 `<extended cursor name>`; the positioned 20.23/20.24 forms reuse the DML parsers
  through `rejectOmittedTarget`. `DynamicOpen`/`DynamicFetch`/
  `DynamicClose` carry the scope option. To stop the static 14.4/14.5/14.6 `<cursor
  name>` from absorbing dynamic-only inputs (e.g. `FETCH cur INTO DESCRIPTOR d`),
  `DESCRIPTOR` is reserved per SQL-2016 5.2.
- **Static `FETCH` (14.5) takes only `INTO <fetch target list>`** — the descriptor form
  belongs to the dynamic 20.20 `<output using clause>`, so `pFetchIntoClause` is
  dedicated (`DataManipulationParser.fs`).
- **20.11 `<using argument>` is a `<general value specification>`** (6.4) — no
  `<literal>`, so `OPEN c USING 1` is rejected; host parameters (with an indicator),
  `?`, identifiers and the CURRENT_*/USER/VALUE keywords are accepted.
- **20.4** GET DESCRIPTOR targets are `<simple target specification>`s (host parameters
  allowed); **23.1** GET DIAGNOSTICS targets are the same production, so `?` is rejected.

## Set functions and window functions

- **6.10 is represented by the existing `WindowFunction` record.** `RESPECT NULLS` /
  `IGNORE NULLS` are parsed for LEAD/LAG, FIRST_VALUE/LAST_VALUE and NTH_VALUE;
  `FROM FIRST` / `FROM LAST` for NTH_VALUE only. Both modifier choices are retained,
  unsupported functions or invalid ordering are rejected; grammar-specific aggregate
  families are validated in the shared routine validation block. A bare `<measure
  name>` followed by OVER (6.10 `<window row pattern measure>`) reuses `WindowFunction`
  with an empty argument list.
- **10.9**: the `<set quantifier>` belongs to `<general set function>` and
  `<listagg set function>` only — binary / array / JSON / row-pattern-count alternatives
  reject one (`my_func(DISTINCT 1)` too). The `[ <filter clause> ]` is admitted on every
  `<aggregate function>` alternative (binary set functions, `ARRAY_AGG`, the JSON
  aggregates), and `OVER` additionally on the aggregate families that
  `<window function type>` reaches. A bare `<rank function type>` or
  `<inverse distribution function>` is rejected — the former needs OVER or WITHIN GROUP,
  the latter its mandatory WITHIN GROUP. `COUNT ( <asterisk> )` and the 10.9 row pattern
  count `COUNT ( <row pattern variable name> . <asterisk> )` are the only `*` arguments.
  6.10 `<lead or lag function>`'s `<offset>` is an `<exact numeric literal>` (exponent
  notation rejected — approximate literals are a distinct `Literal.ApproximateNumber`
  case).
- **10.9 `<array aggregate function>`** — the ORDER BY form carries a dedicated
  `ArrayAgg` node (`Argument` / `OrderBy` / `Filter`); a plain `ARRAY_AGG(x)` stays a
  `FunctionCall`. `<listagg overflow clause>` is a seventh `FunctionCall` field
  (`ListaggError | ListaggTruncate of Expression option * bool`), parsed inside the
  argument parentheses and rejected for any routine other than `LISTAGG`; the JSON
  aggregate constructors take the `<filter clause>` as their fifth tuple field.
- **6.32 `<normalize function result length>`** gets a typed `NormalizeResultLength`
  (`<character length> | <character large object length>`), so
  `NORMALIZE(x, NFC, 10 + 1)` is rejected. A bare integer is ambiguous, so
  `<character length>` wins and the large-object case needs a `<multiplier>`
  (`2K OCTETS`).

## CAST FORMAT and the 10.4 descriptor argument

`CAST` carries the optional `FORMAT <cast template>` (third field); `AS DESCRIPTOR` is
rejected by `pCastSpecification` (a `<cast target>` is a domain name or data type). The
descriptor forms live in 10.4's `<descriptor argument>`: `DESCRIPTOR ( a INT, b )` (the
20.16 constructor, shared with 11.60 `<parameter default>`) and
`CAST ( NULL AS DESCRIPTOR )` (`ExpressionKind.DescriptorCast`); a standalone
`CAST ( NULL AS DESCRIPTOR )` stays rejected.

## §10.4 SQL arguments are structured

Invocation nodes carry a `SqlArgumentList` (`StaticMethodInvocation`'s is optional —
`my_type::prune` parses without parentheses, 6.18); each argument is `SqlArgumentValue |
Generalized | Named | Table | Descriptor`. Heuristics, since a parse-only library cannot
resolve names: a table-function invocation counts as a `<table argument>` only with a
following clause (`TABLE ( <query> )` needs none); `expr AS name` is generalized unless a
column list/clause
follows; `COPARTITION` (non-reserved) is excluded from correlation/routine-name
positions; reserved built-ins take value arguments only (`SUM(a, TABLE(t))` rejected).

## Lexical modelling

- **SQL terminals are modelled as they are used (5.1)** — only `{ } ^ | $` and the
  `{-`/`-}` compound tokens (row patterns) get parsers; `<percent>` / `<reverse solidus>`
  occur only inside opaque embedded languages (8.6 regex, 9.38/9.39 JSON path) or
  nowhere, so parsers for them would only suggest the parser understands that text.
- **5.2 `<separator>` includes comments.** `ws` consumes `<simple comment>` and
  `<bracketed comment>` as well as white space, so `SELECT/*c*/1` is `SELECT 1`. A
  `<simple comment>` is terminated by LF, CR or CRLF and, as a documented extension, by
  end of input. Bracketed comments are NOT nested — the checked-in grammar file defers
  nesting to the Syntax Rules, so the conservative reading is kept; each comment
  alternative is atomic so an unterminated `/*` backtracks cleanly.

## Deliberate deviations (documented, not fixed)

- **6.37 in a general `<value expression>`**: `INTERVAL '1' DAY * ? DAY` parses under
  6.35 `<point in time>` / 19.4 `SET TIME ZONE` but not in a select list, where `*` is
  numeric multiplication. The same type-level blind spot covers interval-vs-datetime
  operands, calendar validity, binary `POSITION ... USING` (only the character form has
  the slot), character-vs-numeric predicate operands and collection-vs-numeric
  arguments (`CARDINALITY`).
- **Syntactic ambiguities**: kind-less `GRANT ... ON <name>` reads as a table grant;
  `TABLE (expr)` PTF classification is shape-based; a lone `TRANSFORM GROUP g` is
  reported as `<single group specification>` (11.60).
- **Opaque embedded languages**: the SQL/JSON path grammar (9.38/9.39) and XQuery-regex
  patterns (8.6) are kept as strings by design.
- **Aggregate window forms beyond `pRoutineInvocation`** — `ARRAY_AGG(x ORDER BY y)` and
  `JSON_ARRAYAGG(x ORDER BY y)` parse, and a plain `ARRAY_AGG(x) OVER (…)` follows the
  grammar through `pRoutineInvocation`; but a window suffix on the ORDER BY / FILTER
  variants (`ARRAY_AGG(x ORDER BY y) OVER (…)`) and `JSON_ARRAYAGG(x) OVER (…)` are not
  wired — the dedicated parsers leave the trailing OVER unconsumed and the statement is
  rejected.
- **20.26 `<preparable dynamic cursor name>` scope option.** 20.23–20.27 share their
  `WHERE CURRENT OF` parser with the static 14.8/14.13 (a bare `<cursor name>`), so the
  scope option is accepted there too — `WHERE CURRENT OF LOCAL c` parses in static
  statements as well. Chosen over rejecting the grammar-valid dynamic forms; the scope
  is validated but not stored (the AST slot is an `Expression`).
- **14.11** `<from subquery>` query expressions beginning with a table value constructor
  are parsed as the query alternative, preserving any following set operations.
