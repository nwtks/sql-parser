# Design Trade-offs

Significant design decisions and the alternatives rejected, organised by theme.
The architecture is in [architecture.md](architecture.md); recurring pitfalls in
[gotchas.md](gotchas.md). Check here before "fixing" a rejection or an AST shape.

## Module organisation

- **One module per grammar section** (`SchemaParser` §11, `AccessControlParser` §12,
  `DataManipulationParser` §14 incl. cursors/locators, `PredicateParser` §8, …) rather
  than by "DDL/DML/other": more files, but each mirrors the standard's section names.
  Shared sub-parsers are hoisted to the earliest module all callers reach
  (`<scope clause>` / `<method kind>` → `ExpressionParser.fs`, `pDropBehavior` →
  `SchemaParser.fs`, `pGrantor` → `AccessControlParser.fs`).
- **Compile order is part of the design** (leaf-first in `SqlParser.fsproj`);
  cross-module recursion is wired via `createParserForwardedToRef` in `SqlParser.fs` —
  a dummy parser until assignment, in exchange for keeping rules in their natural
  module.
- **Definition order follows the spec clause order**, best-effort: `define-before-use`
  wins; single-use sub-parsers are nested inside their consumer, so they cause no
  inversion. Review-enforced convention, not a hard rule.

## Entry points and statement dispatch

- **Two entry points.** `parse` (22.1) takes only the directly executable families and
  requires the trailing `<semicolon>`; `parseStatement` (13.4) is the superset (cursors,
  positioned DML, `CALL`, diagnostics, dynamic SQL). Breaking for callers, but `parse`
  stays standard-conforming; the positioned `UPDATE`/`DELETE` forms are excluded by
  explicit guards.
- **`CREATE SCHEMA`'s `<schema element>` list is an explicit `choice`** (CREATE-family +
  `GRANT`), not a reuse of the dispatcher, so `DROP` / `ALTER` / `TRUNCATE` / `REVOKE`
  cannot appear.

## Grammar-faithful strictness

The parser rejects constructs the standard does not permit, even in common vendor
dialects. Representative examples:

- `<drop behavior>` is required (`DROP TABLE t CASCADE`, never bare `DROP TABLE t`);
  `<with or without data>` is mandatory after `CREATE TABLE ... AS`; `OFFSET` requires
  `ROW`/`ROWS`; `NATURAL CROSS JOIN` is rejected; `<sample method>` is
  `BERNOULLI | SYSTEM` only.
- **No non-standard syntax:** no `CREATE`/`DROP INDEX`, no `ALTER TABLE ... RENAME`,
  no `FOR SHARE`.
- **Mandatory clauses stay mandatory:** `ALTER ROUTINE` needs ≥ 1 characteristic *and*
  `RESTRICT` (11.61 brackets neither); `TRUNCATE` needs `TABLE`;
  `INSERT ... DEFAULT VALUES` accepts no column list or override.
- **Closed sets stay closed:** `<default option>` (11.5) admits literals, datetime value
  functions and the listed built-in values (`DEFAULT (1 + 2)` and `DEFAULT ?` are
  rejected); `<simple value specification>` admits no bare identifier; `<language name>`,
  `<parameter style>` and `<char length units>` are keyword enumerations, not
  identifiers; `<grantor>` is `CURRENT_USER | CURRENT_ROLE` only.
- **`pRoutineInvocation` accepts only reserved *function* keywords** (whitelist), so
  `EXISTS`/`UNIQUE`/`PERIOD`-style words cannot degrade to `FunctionCall` and wrong
  arities (`ABS(a, b)`) are rejected; window/ordered-set built-ins keep their mandatory
  `OVER` / `WITHIN GROUP` suffix.
- **Values are validated at the lexer:** numeric literals must fit `decimal` (checked,
  invariant-culture conversions — nothing throws out of the public API); datetime and
  interval values are range-checked (hours 0–23, time zone within ±14:00, interval
  digits vs precision…). Calendar validity (February 30) is deliberately *not* checked —
  semantic.
- **Trade-off:** real-world SQL omitting standard-mandated clauses fails to parse, so
  consumers would layer extensions on top. In exchange, an accepted string is much more
  likely to be valid SQL-2016.

## Expressions and data types

- **Types avoid left recursion by construction** — `pDataTypeElement` + a folded
  `pCollectionType` suffix chain, so typo'd type names fail cleanly and `INT ARRAY ARRAY`
  works. Varying types REQUIRE their length; lengths keep the grammar's model
  (`CharacterLength { Value; Unit }`, `LargeObjectLength { Value; Multiplier; Unit }`),
  and a character type can carry its type-level `[ CHARACTER SET ] [ COLLATE ]` clauses
  via `CharacterTypeWithModifiers`. A `<collate clause>` directly after a character type
  is the 6.1 type-level one; after other clauses it lands in the enclosing rule's
  `Collation` field (position disambiguates).
- **Quantified comparison uses an intermediate node** — `ANY/SOME/ALL (subquery)` parses
  as a term and the comparison-operator mapping rewrites it; `findExpressionViolationIn`
  rejects any surviving standalone node. One AST case instead of 18 operator×quantifier
  infixes.
- **Reserved built-ins get dedicated AST cases** (datetime functions, `SUBSTRING FROM/FOR`,
  navigation, `RUNNING`/`FINAL`, …) so structure survives; arity is enforced by the parser.
- **Postfix constructs reuse existing layers** (`COLLATE` as predicate suffix, multiset
  set-ops as a postfix fold, `<time zone specifier>` over `<interval primary>`) — slightly
  more permissive parents, no new precedence levels.
- **Interval / point-in-time parsing.** `<point in time>` is a dedicated 6.35 parser
  (rejects `*`, `||`, `=`); 6.37 tries the narrow `(d1 - d2) <qualifier>` alternative
  (`pDatetimeDifference`) first because `pIntervalPrimary` would otherwise swallow it.
  Interval- vs datetime-valued operands are syntactically indistinguishable, so `1 + 2`
  parses in interval slots — semantic. The 6.35/6.37/6.43/7.16 chain uses the conforming
  `pValueExpressionPrimary`; only `opp.TermParser` uses the predicate-including variant,
  whose single grammar-exceeding approximation is the §8 predicate atoms. The 7.16 `*`
  wildcard is not in the expression parser — `COUNT(*)`, select-list `*` and row-pattern
  `*` are each parsed by their own clause.
- **JSON.** Paths are plain strings kept opaque; JSON argument slots use the boolean-free
  `pNonBooleanValueExpression` (stricter than the grammar, keeps comma lists
  unambiguous); `FORMAT <representation>` is preserved on context and passing arguments
  (`JsonApiCommon.ContextFormat`, `JsonPassingArgument.InputFormat`); behaviour DUs are
  split (`JsonValueBehavior` / `JsonQueryBehavior`) and `JsonType*` prefixed against
  `ExpressionKind` clashes.

## Query AST shape

- **`FROM` is a `TableSource list`** (7.6's `<table reference list>`) — the comma stays
  distinct from `CROSS JOIN`. 7.16 requires a `<from clause>` at the `<table expression>`
  level, so bare `SELECT 1` is rejected.
- **Set-operation tails live in a `QueryExpression` case** carrying
  `ORDER BY`/`OFFSET`/`FETCH`/`LOCKING`; plain `SELECT ... ORDER BY` folds into
  `SelectStatement`; `INTERSECT` binds tighter than `UNION`/`EXCEPT`.
- **`GROUP BY` is `GroupingElement list`** so `(a, b)` is one grouping set, not two
  columns.
- **Correlation handling per source**: `Only`/`DataChangeDelta` carry optional aliases,
  `Lateral`/`Unnest` require one, `TableSample` wraps a `TableSource`; parenthesized
  table refs are only `<joined table>`s.
- **`TABLE (expr)` is disambiguated by shape** (`PtfTable` iff `FunctionCall`) — no
  parse-only classifier can do better.

## DDL and DML AST shape

- **Constraints**: `ColumnDefinition.Constraints` plus derived convenience accessors
  (redundant by design); `ColumnConstraintKind` has no bare `NULL`; 11.4's slot is one
  `opt` over a `Choice` (so `GENERATED ALWAYS AS IDENTITY DEFAULT 5` is rejected);
  `ConstraintCharacteristics` is three `bool option`s in grammar order.
- **`CREATE TABLE`** carries optional clauses as dedicated fields
  (`Under`/`Like`/`Periods`/`AsQuery`/`TypedElements`…) rather than exploding DU cases;
  `<table element>` is a four-way `Choice`.
- **Sequence options** are one shared `SequenceOption` DU, with per-slot subsets (no
  option kind leaks into the wrong clause).
- **`ALTER TABLE`** models every 11.10 action; `<drop behavior>` is a `bool`;
  `AddTablePeriod`'s column list holds exactly 0 or 2 entries.
- **DML**: `SetClause` tried MultipleSet → MutatedSet → SingleSet; `DEFAULT` is only an
  insert/update value (`InsertSource.DefaultValues` + `ExpressionKind.Default`), never a
  general expression; `DmlTarget = TableTarget | OmittedTarget` guards positioned forms;
  `ONLY ( t )` is modelled for UPDATE/DELETE/MERGE but not `INSERT` (14.11 has no ONLY
  form).
- **Flat `StatementKind` cases** for every `DROP` variant and every 12.3 `<object name>`
  kind of `GRANT`/`REVOKE` (`GrantTable`, …, `GrantObject`, `GrantRoutine`), wrapping
  shared payload records — matching how `CreateTable`/`AlterTable` are modelled; no
  nested `DropStatement`/`GrantStatement` DU. `GrantRoles`/`RevokeRoles` stay separate
  (GRANT vs ADMIN OPTION differ). `PrivilegeSelectTarget` separates method lists from
  column lists.

## Routines, triggers and types

- **One `CreateRoutine` record** for procedures/functions (`Returns = None` ⇔ procedure);
  `CREATE METHOD` stays separate (no `<routine characteristics>` slot there).
- **11.60 and 11.61 have different characteristic sets** sharing one duplicate check
  (`NAME` is 11.61-only; `ALTER ROUTINE` rejects `SPECIFIC`/deterministic/savepoint-level).
  `RoutineCharacteristic` is a flat DU (BNF allows any order); `<language name>` /
  `<parameter style>` are closed keyword sets.
- **`RoutineBody`** = `SqlRoutine` | `ExternalRoutine` | `PolymorphicTableFunction` |
  `BeginAtomic` (an extension — 13.4 has no `<compound statement>`); the PTF branch is
  tried first because `DESCRIBE` also starts `<describe statement>`.
- **`<specific routine designator>` is one record** shared across every designator slot
  (ALTER ROUTINE, CREATE CAST, ORDERING, TRANSFORM, PTF components, privilege method
  lists); `IsSpecific`/`RoutineType` are optional because `ALTER ROUTINE add` (bare name)
  is legal — which is why `pPrivilegeMethodItem` re-checks the designator (see
  gotchas.md).
- **Parameter/return types** (`ParameterType`, `ReturnsType` DUs): generic-table and
  descriptor types are tried before `<data type>` because `DESCRIPTOR` is non-reserved;
  a leading identifier is always the parameter name, so `IN mytype` is rejected.

## Lexical modelling

- **SQL terminals are modelled as they are used (5.1)** — only `{ } ^ | $` and the
  `{-`/`-}` compound tokens (row patterns) get parsers; `<percent>` / `<reverse solidus>`
  occur only inside opaque embedded languages (8.6 regex, 9.38/9.39 JSON path) or
  nowhere, so parsers for them would be unreachable code suggesting the parser
  understands what it treats as text.

## NULL is a null specification, not a literal

5.3 `<literal>` has no NULL alternative — NULL is the 6.5 `<null specification>`, so
`pLiteral` rejects it and `pNullSpecification` is OR-ed in at exactly the
contextually-typed slots the grammar allows (6.13 cast operand, 10.4 SQL argument,
11.5 default option, 11.60 parameter default, 14.11/14.12/14.15 DML value slots, 16.2
return value, 6.12 result). Keyword slots (`IS NULL`, `SET NULL`, `NULLS FIRST`, JSON
`ON NULL`) are untouched. Breaking: `WHERE x = NULL`, `SELECT 1 + NULL`, `ABS(NULL)`,
`COALESCE(NULL, 1)` are rejected. `Literal Null` stays in the AST — it is what
`pNullSpecification` and the `NULLIF` desugar produce.

## Parenthesized expressions keep their parens

6.3's parenthesized alternative wraps its result in `ExpressionKind.Parenthesized`
instead of flattening, so `(1 = 1)` is a `<boolean predicand>` and
`x BETWEEN (1 = 1) AND 2` / `(a = b) IS TRUE` parse; `expressionChildren` forwards
through it transparently.

## Predicate, comparison and period operands

- **Suffix gating**: after a top-level boolean only the 6.39 boolean test remains
  (`1 = 2 IS NULL`, `x LIKE 'a' IS NULL`, `1 = 2 COLLATE c` rejected); a boolean test
  takes no further suffix (`x IS TRUE IS FALSE` rejected); terms/signed primaries are
  not `<boolean primary>`s (`1 + 1 IS TRUE` rejected). Hence `pBooleanTestSuffixes`
  applies one suffix at a time and `pPredicateNoBooleanTest` exists as a separate §8
  parser.
- **Operand categories**: `pOperand` = `<row value predicand>` (BETWEEN / IS DISTINCT /
  OVERLAPS), `pInValueItem` = `<row value expression>` (`x IN (1 + 1)`, `x IN ((1), 2)`,
  `x IN (-1)` rejected), `pValueOperand` = value-shaped (LIKE/SIMILAR/regex pattern,
  escape, FLAG, multiset operands reject explicit rows). Only type-level distinctions
  (character vs numeric) remain unchecked — semantic.
- **Comparison / period**: `pValueExpressionChecked` rejects top-level boolean operands
  of comparisons; a `<period predicate>`'s left operand is checked post-parse
  (`findExpressionViolationIn`) since the operand parses before the suffix runs.
- **Desugars narrow the checks deliberately**: `COALESCE` → searched case with `IsNull`
  conditions, `NULLIF` → `BinaryOp(Equal, …)` — hence parse-time gating for the null
  predicate and a top-node-only comparison check (`COALESCE(1 = 2, TRUE)` and
  `NULLIF(1 = 2, 3)` stay legal).

## 6.12 `<when operand>` part-2 forms

`CASE x WHEN = 1 / IS NULL / BETWEEN 1 AND 2 / IN (…) …` parses: the `<case operand>`
supplies part 1, so such a case is represented as a **searched case** (each simple when
clause becomes the OR of its predicates) — surface syntax is not round-trippable, no AST
case added. `pPredicateImpl`'s `forWhenOperand` flag carries 6.12's narrower alternative
list (boolean test, distinct, collate, type, JSON, member, submultiset, set, period stay
rejected there).

## CAST FORMAT and the 10.4 descriptor argument

`CAST` carries the optional `FORMAT <cast template>` (third field); `AS DESCRIPTOR` is
rejected by `pCastSpecification` (a `<cast target>` is a domain name or data type). The
grammar's descriptor forms live in 10.4's `<descriptor argument>`:
`DESCRIPTOR ( a INT, b )` (the 20.16 constructor, shared with 11.60 `<parameter default>`)
and `CAST ( NULL AS DESCRIPTOR )` (`ExpressionKind.DescriptorCast`). A standalone
`CAST ( NULL AS DESCRIPTOR )` stays rejected.

## §10.4 SQL arguments are structured

Invocation nodes carry a `SqlArgumentList`; each argument is
`SqlArgumentValue | Generalized | Named | Table | Descriptor`. Heuristics, since a
parse-only library cannot resolve names: a table-function/`TABLE (query)` proper counts
as a `<table argument>` only when a table-argument clause follows; `expr AS name` is
generalized unless a column list/clause follows; `COPARTITION` (non-reserved) is excluded
from correlation/routine-name positions; reserved built-ins take value arguments only
(`SUM(a, TABLE(t))` rejected).

## Deliberate deviations (documented, not fixed)

- **6.37 in a general `<value expression>`**: `INTERVAL '1' DAY * ? DAY` parses under
  6.35 `<point in time>` / 19.4 `SET TIME ZONE` but not in a select list, where `*` is
  numeric multiplication — interval-vs-numeric operands are a type distinction.
- **Semantic distinctions**: interval vs datetime operands, calendar validity, binary
  `POSITION ... USING` (only the character form has the slot), character-vs-numeric
  predicate operands.
- **Syntactic ambiguities**: kind-less `GRANT ... ON <name>` reads as a table grant;
  `TABLE (expr)` PTF classification by shape.
- **Opaque embedded languages**: the SQL/JSON path grammar (9.38/9.39) and XQuery-regex
  patterns (8.6) are kept as strings by design.
- **`BEGIN ATOMIC`** bodies go beyond 13.4 (no `<compound statement>` in the foundation
  grammar).
