# Design Trade-offs

Significant design decisions and the alternatives rejected, organised by theme.
The architecture is described in [architecture.md](architecture.md); recurring
pitfalls in [gotchas.md](gotchas.md).

## Module organisation

**One module per grammar section.** Statements are grouped by the grammar section
they belong to rather than by "DDL/DML/other": `SchemaParser` (§11 SQL-schema
statements: DDL, UDTs and routines/triggers), `AccessControlParser` (§12),
`DataManipulationParser` (§14: DML plus cursors, temporary tables and locators),
`ControlParser` (§16), `TransactionParser` (§17), `ConnectionParser` (§18),
`SessionParser` (§19), `DynamicParser` (§20), `DiagnosticsParser` (§23) and
`PredicateParser` (§8).

- **Trade-off:** More files and `Compile Include` entries, but each module mirrors
  the standard's section names. `SchemaParser` and `DataManipulationParser` accept
  a broader responsibility in exchange for keeping the shared §11 / §14 parsers in
  one place. `SchemaParser` is named for the whole `<SQL-schema statement>` family
  (routines and triggers are only a small part); every module keeps the `…Parser`
  suffix except `Lexer`.

**Definition order follows the spec clause order.** Top-level definitions are
ordered by the ascending ISO/IEC 9075-2:2016 clause number in their leading
comment, so the file reads like `sql-2016-grammar.txt`. The sort key is the *first*
citation above a definition; uncited helpers stay with what they serve.

- **Trade-off:** The order is **best-effort** — `define-before-use` wins, so a rule
  whose sub-parsers are cited later stays where the dependency requires. Where
  order is compiler-irrelevant it is strictly ascending: `Ast.fs`'s recursive `and`
  group (6.1 → 6.43, with `ExpressionKind`/`Expression` at its 6.28 position) and
  the `createParserForwardedToRef` declarations at the top of `ExpressionParser.fs`
  (6.1 → 8.9). Single-use sub-parsers are nested inside their consumer and so
  cause no inversion (`pForeignKeyConstraint` in `pTableConstraintDefinition`, `pMemberList`
  in `pRepresentation`, `pSearchClause`/`pCycleClause` in `pWithListElement`, …).

**Compile order is part of the design.** F# needs definition before use, so
`SqlParser.fsproj` lists modules leaf-first: `PredicateParser.fs` (§8) after
`QueryParser.fs` (§7) and before `SchemaParser.fs` (§11); `AccessControlParser.fs`
(§12) after `SchemaParser.fs`; `DataManipulationParser.fs` before
`DynamicParser.fs`.

- **Trade-off:** Moving a parser to a "more logical" module can force a compile-
  order change or a forward reference. Shared sub-parsers are hoisted to the
  earliest module all callers reach (`<scope clause>` / `<method kind>` in
  `ExpressionParser.fs`, `pConstraintEnforcement` / `pDropBehavior` in
  `SchemaParser.fs`, `pGrantor` in `AccessControlParser.fs`).

**Forward references carry cross-module recursion.** Mutually recursive rules in
different modules are wired with `createParserForwardedToRef` and assigned in
`SqlParser.fs` once every participant is defined; [architecture.md](architecture.md)
§5 tabulates them.

- **Trade-off:** Indirection (a dummy parser until assignment) in exchange for
  keeping rules in their natural module. Never read `.Value` at module-init time.

## Entry points and statement dispatch

**Two entry points.** `parse` (22.1 `<direct SQL statement>`) takes the directly
executable families and requires the trailing `<semicolon>`; `parseStatement`
(13.4) is a superset adding cursors, positioned DML, `CALL`, diagnostics and
dynamic SQL.

- **Trade-off:** Requiring the `<semicolon>` and restricting `parse` breaks callers
  (every test helper appends `;`), but `parse` stays standard-conforming and the
  positioned `UPDATE`/`DELETE` forms are excluded by explicit guards.

**Schema elements are CREATE-family statements plus `GRANT`.** `CREATE SCHEMA`'s
`<schema element>` list is an explicit `choice` rather than a reuse of the
dispatcher, so `DROP` / `ALTER` / `TRUNCATE` / `REVOKE` cannot appear.

## Grammar-faithful strictness

The parser rejects constructs the standard does not permit, even in common vendor
dialects:

- **`<drop behavior>` is required** where the grammar requires it, so bare
  `DROP TABLE t`, `REVOKE ... FROM u` and `ALTER TABLE t DROP COLUMN c` are
  rejected. It is a `bool` (`true` = `CASCADE`, `false` = `RESTRICT`).
- **Non-standard syntax is absent:** no `CREATE`/`DROP INDEX`, no
  `ALTER TABLE ... RENAME`, no `FOR SHARE` (`<updatability clause>` is only
  `FOR READ ONLY | FOR UPDATE`).
- **`<with or without data>` is mandatory** (11.3), so
  `CREATE TABLE t AS SELECT 1` is rejected.
- **`NATURAL CROSS JOIN` is rejected** (7.10 `<join type>` has no `CROSS`);
  `<sample method>` is `BERNOULLI | SYSTEM` only.
- **`OFFSET` requires `ROW`/`ROWS`; `FETCH` quantity is optional** (defaults to 1).
- **`<partitioned join column reference list>` is column references only**
  (`PARTITION BY (a + b)` is rejected).
- **`<default option>` is a closed set** (11.5): `DEFAULT (1 + 2)`, `DEFAULT a + b`
  and `DEFAULT ?` are rejected.
- **`pRoutineInvocation` accepts only reserved *function* keywords**, so reserved
  words that start dedicated constructs (`EXISTS`, `UNIQUE`, `JSON_EXISTS`,
  `PERIOD`, `VALUE_OF`, …) cannot degrade to a generic `FunctionCall`; `ABS(a, b)`
  is rejected for the same reason. Adding a reserved-name built-in means extending
  `functionKeywords`.

- **A numeric literal whose value does not fit `decimal` is rejected**, not clamped to a different
  number (`1E29` used to become `1e28`). Every numeric conversion is checked and reads the
  *invariant* culture, so a malformed or out-of-range value fails the parse instead of throwing.
  A numeric token must also not run into a following identifier character, so `1E`, `1E5x` and
  `0x10` are rejected rather than re-read as a number plus an alias.
- **Datetime and interval values are range-checked:** hours 0-23, minutes 0-59, seconds 0-60 (leap
  second), `<time zone interval>` within +-14:00, years 0001-9999, month 1-12, day 1-31, and an
  `<interval literal>`'s digits must fit its leading / fractional-seconds precision. Calendar
  validity (February 30, leap years) is deliberately *not* checked — that distinction is semantic.
- **`<simple value specification>` admits no bare identifier.** `CONNECT TO 'server'` and
  `SET ROLE 'admin'` are the standard spellings, so the compatibility extension that accepted
  `CONNECT TO server` / `SET ROLE admin` was removed. `<literal>` includes `<signed numeric
  literal>`, so `FETCH RELATIVE -1` is still accepted (as one literal, not a unary minus).
- **Reserved built-ins keep their mandatory suffix.** A `<window function>` needs `OVER` and a
  `<hypothetical set function>` / `<listagg set function>` needs `WITHIN GROUP`, so `ROW_NUMBER()`
  and `LISTAGG(x, ',')` are rejected instead of degrading to a generic `FunctionCall`.
- **A clause stays in the clause it belongs to.** `<char length units>` is the closed set
  `CHARACTERS | OCTETS` and `OCTET_LENGTH` has no `USING` slot; a `JSON_TABLE` column list admits
  `NESTED` but not `FOR CHAINING` and a `JSON_TABLE_PRIMITIVE` list the reverse; `WRAPPER`,
  `QUOTES` and `EMPTY ARRAY`/`EMPTY OBJECT` require `FORMAT JSON` while `DEFAULT` excludes it.
- **Mandatory clauses are parsed as mandatory:** `ALTER ROUTINE` needs at least one characteristic
  and the `RESTRICT` behavior (11.61 brackets neither), `TRUNCATE TABLE` needs `TABLE`, a
  `SELECT ... INTO` needs its `<table expression>`, and `INSERT ... DEFAULT VALUES` accepts neither
  an `<insert column list>` nor an `<override clause>`.
- **Nothing throws out of the public API.** The per-parser checks above are the primary defence;
  `runParser` additionally converts any escaping exception into `Result.Error`, so a value the
  grammar does not model can never surface as a raw .NET exception.

- **Trade-off (all of the above):** Real-world SQL that omits standard-mandated
  clauses no longer parses, so consumers would have to layer extensions on top. In
  exchange, an accepted string is much more likely to be valid SQL-2016.

## Expressions and data types

- **Types avoid left recursion by construction.** `pDataTypeElement` parses every
  non-collection type and `pCollectionType` folds the `ARRAY`/`MULTISET` suffixes
  left-to-right, so typo'd type names fail cleanly instead of overflowing, and
  nested collections (`INT ARRAY ARRAY`) work. `<collection type>` is postfix-only.
- **Quantified comparison uses an intermediate node.** `ANY/SOME/ALL (subquery)` is
  parsed as a term (`QuantifiedSubquery`) and rewritten to `QuantifiedComparison`
  by the comparison-operator mapping, because FParsec's `opp` consumes `=` and does
  not backtrack; `pExpression` rejects any surviving standalone node. This avoids
  18 per-operator×quantifier infix operators at the cost of a case leaking into the
  AST. (`SomeQuantifier` avoids shadowing F#'s `Some`.)
- **Reserved built-ins get dedicated cases.** `CURRENT_DATE`/…/`LOCALTIMESTAMP`,
  `SUBSTRING(x FROM a FOR b)`, `OVERLAY`, the function catalogue, row-pattern
  navigation, `RUNNING`/`FINAL` and `GROUPING` are reserved, so they cannot be
  identifiers or generic calls. More AST cases, but the FROM/FOR structure
  survives; zero/one-argument built-ins share `NumericValueFunction` (arity
  enforced by the parser, not the type) and `SetFunction` wraps the aggregate call.
- **Postfix constructs reuse existing layers:** `COLLATE` is a predicate suffix,
  `MULTISET UNION|INTERSECT|EXCEPT` is a left-folded postfix, and `<time zone
  specifier>` reuses `<interval primary>`. The parent layer is slightly more
  permissive, but no new precedence level is needed.
- **Interval and point-in-time parsing.** `<point in time>` uses a dedicated 6.35
  parser (left-folded over `+`/`-`, with `AT TIME ZONE`), so `a * b` / `a || b` /
  `a = b` are rejected there. `<interval term>`'s `*`/`/` right operand approximates
  the grammar's `<factor>` with `<value expression primary>`, so
  `INTERVAL '1' DAY * ? DAY` fails; interval literals are shape-validated against
  their qualifier — coarse (no per-month check) but it rejects
  `INTERVAL 'abc' YEAR`. The 6.37 `<interval value expression>` has a full
  dedicated parser (`pIntervalValueExpression`, after `pIntervalTerm`) with the
  4th alternative — `( <datetime> - <datetime> ) <interval qualifier>` — kept as a
  separate narrow parser (`pDatetimeDifference`) that is tried first, because
  `pIntervalPrimary` would otherwise fold the parenthesized difference into
  `IntervalPrimary`. The narrow form remains a `<value expression primary>`
  alternative; `SET TIME ZONE` (19.4) consumes the full parser. Interval- vs
  datetime-valued operands are syntactically indistinguishable, so non-interval
  arithmetic (`1 + 2`) still parses — the distinction is semantic. The 6.35/6.37
  chain (and `<interval primary>`, `<datetime term>`) uses
  `pValueExpressionPrimary`, which excludes the two grammar-exceeding
  approximations that `pValueExpressionPrimaryWithPredicates` adds — the §8
  predicate atoms (`pPredicatePrimary`) and the 7.16 `*` wildcard — so
  predicates/star are rejected inside interval and datetime operands. The same
  grammar-shaped primary is used by the 6.43 multiset sites (`<multiset primary>`
  in `pMultisetSetOperatorSuffix` and `pMultisetValueExpressionRef`) and the 7.16
  `<all fields reference>`; only `opp.TermParser` keeps the full form
  (`pValueExpressionPrimaryWithPredicates`), since it needs the predicate atoms
  for the quantified-comparison rewrite and the `*` wildcard for select item lists.
- **Row value constructors and JSON.** 7.1's `<explicit row value constructor>` is
  an expression case (the parenthesized form needs ≥ 2 elements, so `(a)` still
  means the plain parenthesized expression). JSON paths are plain `string`s and the
  JSON argument slots use the boolean-free `pNonBooleanValueExpression` —
  deliberately stricter than the grammar, to keep comma-separated lists
  unambiguous. The `<JSON input clause>` (`FORMAT <JSON representation>`) on both
  the context item and each passing argument is preserved
  (`JsonApiCommon.ContextFormat`, `JsonPassingArgument.InputFormat`).
  `JsonValueBehavior` and `JsonQueryBehavior` are separate DUs, and the
  the `<JSON predicate type constraint>` cases are prefixed `JsonType*` to avoid
  clashing with `ExpressionKind`. `pJsonKeyUniqueness` / `pPadCharacteristic`
  return `bool`, not `bool option`.

## Query AST shape

- **`FROM` is a list, not a join tree.** 7.6 is `FROM <table reference list>`, so
  `SelectStatement.From` is `TableSource list` (empty = no `FROM`) and the comma
  stays distinct from `CROSS JOIN`.
- **Set-operation `ORDER BY` scope.** `Query` has a `QueryExpression` case carrying
  the trailing `ORDER BY`/`OFFSET`/`FETCH`/`LOCKING` when the body is a set
  operation or a `WITH` query. Plain `SELECT ... ORDER BY` still folds into
  `SelectStatement`; `INTERSECT` binds tighter than `UNION`/`EXCEPT`.
- **`GROUP BY` uses grouping elements.** `GroupBy` is `GroupingElement list`
  (`GroupingSet | Rollup | Cube | GroupingSets | EmptyGroupingSet`) plus
  `GroupByDistinct`, so `(a, b)` is one grouping set rather than two columns.
- **Table sources hold their correlation names.** `Only` and `DataChangeDelta`
  carry the optional alias/column list, `TableSample` wraps a `TableSource`,
  `Lateral`/`Unnest` require an alias, and `SystemTime` takes a full `TableSource`.
  The `<row pattern recognition clause>` half of `<correlation or recognition>` is
  not accepted there.
- **`TABLE (expr)` is disambiguated by shape** — `PtfTable` when the expression is
  a `FunctionCall`, `TableFunction` otherwise. A non-invocation PTF would be
  misclassified, but no parser can tell them apart without semantic knowledge.

## DDL and DML AST shape

- **Constraints.** `ColumnDefinition` carries `Constraints` and keeps
  `IsNullable`/`IsPrimaryKey`/`IsUnique`/`References`/`Check` as derived
  convenience accessors (redundant by design). `ColumnConstraintKind` has no bare
  `NULL` (11.4 lacks the alternative; this also avoids clashing with
  `Literal.Null`). 11.4's optional slot is one `opt` over a `Choice`, so
  `GENERATED ALWAYS AS IDENTITY DEFAULT 5` is rejected. `ConstraintCharacteristics`
  is three `bool option`s tried in grammar order and must not swallow a following
  `COLLATE`.
- **`CREATE TABLE`.** `CreateTableStatement` carries
  `Under`/`Like`/`Periods`/`WithSystemVersioning`/`OnCommit`/`AsQuery`/`AsColumns`/
  `WithData`/`TypedElements`, and `<table element>` is a four-way `Choice`.
  Optional fields keep the common shape unchanged, at the cost of a 6-tuple in the
  content-source alternatives.
- **Sequence options** are one `SequenceOption` DU shared by `CREATE`/`ALTER
  SEQUENCE` and `<identity column specification>`, composed from per-rule parsers;
  `MaxValue`/`MinValue` use `decimal option` (`None` = `NO MAXVALUE`/`NO MINVALUE`).
  `CREATE TABLE` and `ALTER SEQUENCE` take the permissive `pSequenceGeneratorOption`, while
  11.20 takes the narrowed parsers.
- **`ALTER TABLE`** models every 11.10 action (`ADD`/`ALTER`/`DROP [COLUMN]`,
  constraints, periods, system versioning). `<drop behavior>` is a `bool`;
  `AlterConstraint` carries `[NOT] ENFORCED` as a `bool` (11.25 allows only
  `<constraint enforcement>`); `AddTablePeriod`'s optional column list holds
  exactly 0 or 2 entries.
- **DML.** `SetClause` is a DU tried MultipleSet → MutatedSet → SingleSet, with
  `MutatedSet` folding the dotted target into a `FieldReference` chain.
  `InsertSource.DefaultValues` plus `ExpressionKind.Default` cover `DEFAULT VALUES`
  and `VALUES (DEFAULT)` (`DEFAULT` is deliberately not a general expression).
  `pOverrideClause` is shared by `INSERT`/`MERGE` (`bool option`), and `MergeInsert`
  keeps a single row. `FOR PORTION OF` is a `PortionOfSpec` using the 6.35 parser.
  `DmlTarget = TableTarget | OmittedTarget` lets positioned statements omit the
  target, guarded against `<portion of>`, aliases and search conditions (`MERGE`
  has no omitted form). `ONLY ( <table> )` is a pair of flags, and `INSERT` keeps a
  plain name (14.11 has no `ONLY` form).
- **`DROP`, `GRANT` and `REVOKE`.** One drop parser covers every `DROP` in the
  grammar; each variant is a flat `StatementKind` case (`DropTable`, `DropView`,
  `DropSchema`, …) rather than a nested `DropStatement` DU, matching how
  `CreateTable`/`CreateView`/`AlterTable` are modelled. `DROP ROLE` is therefore
  the single `StatementKind.DropRole`. The payload is `Expression * bool` except
  `DropRole`, `DropCharacterSet`, `DropTransliteration` and `DropTrigger`
  (`Expression`) and `DropAssertion` (`bool option`). `GRANT`/`REVOKE` follow the
  same flat-case model: one `StatementKind` case per 12.3 `<object name>` kind
  (`GrantTable`, `GrantDomain`, …, plus `GrantObject`/`RevokeObject` for the
  optional `[ TABLE ]` form and `GrantRoutine`/`RevokeRoutine` for the
  `<specific routine designator>` form, which keeps the 10.6 `<routine type>`)
  wrapping a shared `GrantPrivilegeStatement` / `RevokePrivilegeStatement` record;
  `GrantRoles` / `RevokeRoles` stay separate because `WITH GRANT OPTION` vs
  `WITH ADMIN OPTION` differ. `GRANTED BY` / `WITH ADMIN` keep the 12.3 `<grantor>`
  as `Grantor = CurrentUser | CurrentRole | AuthorizationId` — the grammar allows
  only the two keywords, but the parser also accepts an `<authorization identifier>`
  in the `<grantor>` position (over-permissive). `PrivilegeSelectTarget` separates
  method lists from column lists.

## Routines, triggers and types

- **One `CreateRoutine` record** for procedures and functions; `Returns = None` ⇔
  procedure, and only a function reports `Dispatch = true`. `CREATE METHOD` is not
  folded in (`<method specification designator>` has no `<routine characteristics>`
  slot), so it gets `MethodSpecificationDesignator` and
  `StatementKind.CreateMethod`.
- **11.60 and 11.61 are different sets** sharing one duplicate check
  (`rejectDuplicateCharacteristics`): `NAME <external routine name>` is 11.61-only,
  and `ALTER ROUTINE` rejects `SPECIFIC`, `<deterministic characteristic>` and
  `<savepoint level indication>`. `RoutineCharacteristic` is a flat DU because the
  BNF allows any order but the syntax rules allow each at most once; `<language
  name>` and `<parameter style>` are closed keyword sets parsed in
  `SchemaParser.fs`.
- **`RoutineBody`** is `SqlRoutine` (carrying the optional `<rights clause>`),
  `ExternalRoutine`, or `PolymorphicTableFunction`. `BEGIN ATOMIC` with
  `sepEndBy1` matches the grammar but is an extension of this parser — 13.4 has no
  `<compound statement>`; the PTF branch is tried first because `DESCRIBE` also
  starts `<describe statement>`; `validateRoutine` rejects a repeated
  `PARAMETER STYLE`.
- **`<specific routine designator>` is a record** (`IsSpecific`, `RoutineType`,
  `Name`, `DataTypeList`, `ForType`) so it is round-trippable across its seven AST
  positions; `RoutineType` stays an option, and `pRoutineDesignator` is now
  only used by `DROP ... <routine>` — the `GRANT`/`REVOKE` object name uses a local
  `pRoutineType .>>. <qualified name>` parser that keeps the `<routine type>`.
- **Parameter and return types.** `ParameterType` (`DataTypeParameter |
  GenericTableParameter | DescriptorParameter`) and `ReturnsType` (`ReturnsData |
  ReturnsTable | ReturnsOnlyPassThrough`) make return tables, result casts and
  locator indications representable. The generic-table/descriptor types are tried
  before `<data type>` because `DESCRIPTOR` is non-reserved, and a leading
  identifier is always the parameter name, so `IN mytype` is rejected.

## Lexical and JSON modelling

**SQL terminal characters are modelled as they are used (5.1).** Only terminals
referenced by a production get a parser: `{`, `}`, `^`, `|`, `$` and the 5.2
compound tokens `{-`/`-}` (used by the 7.9 row pattern parser). `<percent>` and
`<reverse solidus>` are deliberately not modelled — they occur only inside the
embedded XQuery-regex (8.6) and SQL/JSON path (9.38/9.39) languages, whose text is
kept opaque, or in no production at all. A parser for them would be unreachable
code that suggests the parser understands syntax it deliberately treats as text.

## Spec-compliance strictness sweep (2026-09-17)

A full audit against `sql-2016-grammar.txt` closed the over-permissive (accepts-invalid)
gaps below. Each is a breaking change: SQL that previously parsed is now rejected.

- **7.10** — a `<qualified join>` requires a `<join specification>`; `<cross join>` and
  `<natural join>` reject one. `FROM a JOIN b` and `FROM a CROSS JOIN b ON ...` are rejected.
- **7.6** — `<derived table>`, `<lateral derived table>`, `<collection derived table>`,
  `<JSON table>` and `<JSON table primitive>` require their `<correlation or recognition>` /
  `<correlation name>`; only `<PTF derived table>`, `<only spec>` and `<data change delta
  table>` bracket it. Only `<joined table>`s may be parenthesized (`FROM (t)` is rejected).
  `<row pattern input name>` is a `<correlation name>` — no schema-qualified input name and
  no second name group before the recognition clause.
- **7.16** — `<table expression>` requires a `<from clause>` (`SELECT 1` is rejected);
  a bare `<asterisk>` is an alternative to the whole `<select list>`, not a sublist
  (`SELECT *, a` is rejected).
- **7.17** — `<query expression>` has no `<updatability clause>` slot; `FOR UPDATE` /
  `FOR READ ONLY` belong to the 14.3 `<cursor specification>` only (the `DeclareCursor`
  AST now carries `Updatability`). `<fetch first percentage>` requires its quantity.
- **7.11** — `<JSON table plan union>` / `<plan cross>` need at least two operands; a
  single path name is a `<JSON table plan>` (`JsonPlanName`), a parenthesized plan is a
  `<plan primary>` and cannot stand alone as `PLAN ((p OUTER q))`.
- **6.10 / 10.9** — `pRoutineInvocation` validates arity and argument shapes per function
  family (`RANK()` empty in the OVER form, `SUM(a,b)` rejected, `LEAD` offset must be an
  `<exact numeric literal>`, `LISTAGG` separator must be a `<character string literal>`,
  …), restricts `OVER` to `<window function type>`s, `WITHIN GROUP` to `<ordered set
  function>`s and `FILTER` to `<set function>`s, and enforces the clause order
  `args → WITHIN GROUP → FILTER → OVER`.
- **6.9 / 6.21** — `GROUPING` takes plain `<column reference>`s (no `COLLATE`); the
  dereference right-hand side is a single `<qualified identifier>`.
- **6.12 / 6.42 / 6.45** — `COALESCE` needs ≥ 2 arguments; `ARRAY[]` / `MULTISET[]`
  enumerations need ≥ 1 element (the empty forms are only `<empty specification>`s).
- **6.30 / 6.32** — numeric argument slots (`ABS`, `MOD`, trig, `WIDTH_BUCKET`,
  `SUBSTRING ... FROM/FOR`, `OVERLAY ... FROM/FOR`, `POSITION` operands, length
  expressions) use `<numeric value expression>` / non-boolean parsers, so
  `ABS(1 = 2)` is rejected.
- **8.20** — `PERIOD ( ... )` is a `<period predicand>`: it must be followed by a
  period-predicate operator (lookahead), so it cannot leak as a standalone atom;
  `EQUALS`/`PRECEDES`/`SUCCEEDS`/`IMMEDIATELY ...` require a `<period predicand>` on the
  right (only `CONTAINS` admits a point in time).
- **5.4** — `<schema qualified name>` allows at most three parts (catalog.schema.identifier).
- **11.8** — `<referential triggered action>` admits at most one `<update rule>` and one
  `<delete rule>`, in either order.
- **11.72 / 11.73 / 11.4** — the sequence-generator option sets no longer leak:
  `CREATE SEQUENCE` has no `RESTART`, `ALTER SEQUENCE` has no `AS`/`START WITH`, and the
  `<identity column specification>` has neither.
- **11.49** — `REFERENCING` requires at least one `<transition table or variable>`.
- **20.6** — the `COPY ... TO` target is a `<PTF descriptor name>` (`PTF <simple value
  specification>`).

**Remaining deliberate deviations (documented, not fixed):**

- Predicate part-2 operands (`BETWEEN`/`IN`/`LIKE`/... right-hand sides), `<when operand>`s
  and the left operand of a period predicate accept the full `<value expression>` chain —
  the precedence-parser architecture applies predicates as postfix to the full `opp`
  expression. The top-level check (`isBooleanTopLevel`) approximates the
  `<row value predicand>` requirement; a parenthesized boolean operand is legal
  (`Parenthesized` keeps the parens), a bare one is rejected.
- The `<binary position expression>` form admits `USING <char length units>` — binary and
  character operands are syntactically indistinguishable, so one parser serves both.
- `GRANT ... ON <name>` without a kind keyword is parsed as a table grant even when the
  name is meant for another object kind (syntactically unavoidable).

## Spec-compliance follow-up (2026-09-18)

Four of the remaining deliberate deviations were closed:

- **12.3** — `<grantor>` is now the closed keyword set `CURRENT_USER | CURRENT_ROLE`; an
  `<authorization identifier>` in the `GRANTED BY` / `WITH ADMIN` position is rejected.
- **11.34 / 11.51** — `<domain definition>` and `<representation>` take a dedicated
  `pPredefinedType` (the built-in alternatives of `<data type>`, no UDT name, no `REF`),
  so `CREATE DOMAIN d my_udt` and `CREATE TYPE t AS my_udt` are rejected. A bare UDT name
  with a collection suffix (`CREATE TYPE t AS my_udt ARRAY`) is still a valid
  `<collection type>` (`pCollectionTypeStrict` requires ≥ 1 suffix), and
  `REF USING <predefined type>` (11.51) uses the same parser.
- **8.x / 6.12** — predicate part-2 operands and `<when operand>`s reject a TOP-LEVEL
  boolean-producing expression (`isBooleanTopLevel` in `ExpressionParser.fs`):
  `1 BETWEEN 1 = 1 AND 2`, `'a' LIKE 'b' = 'c'`, `CASE x WHEN 1 AND 2 THEN 1 END` are
  rejected. This is an approximation, not a full `<row value predicand>` parser. The
  parenthesized over-strictness noted here was resolved on 2026-09-19 — the AST now
  keeps a `Parenthesized` node (see that section below).
- **6.37** — `<interval term>`'s `*`/`/` right operand is now `pIntervalFactor`
  (`[ <sign> ] <interval primary>`), which covers both the `<factor>` form (no qualifier)
  and the 4th alternative's `<interval factor>` (optional qualifier):
  `INTERVAL '1' DAY * ? DAY` now parses.

Still open (syntactically indistinguishable or semantic): interval vs datetime operands,
calendar validity, binary `POSITION ... USING`, kind-less `GRANT ON <name>`,
`TABLE (expr)` PTF classification, and the JSON path grammar (kept opaque by design).

## NULL is a null specification, not a literal (2026-09-19)

`pLiteral` no longer accepts `NULL`: 5.3 `<literal>` has no NULL alternative — NULL is
the **6.5 `<null specification>`**, the implicitly-typed half of a `<contextually typed
value specification>`. `ExpressionParser.pNullSpecification` (next to
`pDefaultSpecification`) is OR-ed in at exactly the slots the grammar allows:

- **6.13** `<cast operand>` (`CAST(NULL AS INT)`), **10.4** `<SQL argument>` (routine,
  method, static-method and `CALL` arguments), **11.60** `<parameter default>`,
  **11.5** `<default option>`, **14.11** `<contextually typed row value constructor
  element>` (INSERT VALUES), **14.12** `<merge insert value element>` and the merge
  `UPDATE SET` values, **14.15** `<update source>` / `<assigned row>`, **16.2**
  `<return value>` (`RETURN NULL`) and the **6.12** `<result>` alternative
  (`THEN NULL` / `ELSE NULL`, which already had its own alternative).
- The keyword slots (`IS [NOT] NULL`, `SET NULL`, `NOT NULL`, `NULLS FIRST|LAST`,
  the JSON `NULL`/`ON NULL` behaviors) are keyword forms and are untouched.

Breaking: `SELECT 1 + NULL`, `WHERE x = NULL`, `ABS(NULL)`, `COALESCE(NULL, 1)` and
`CASE x WHEN NULL` are now rejected. `ExpressionKind.Literal Null` stays in the AST —
it is what `pNullSpecification` and the `NULLIF` desugar produce.

## Parenthesized value expressions keep their parens (2026-09-19)

`pValueExpressionPrimaryImpl`'s parenthesized alternative now wraps its result in a new
`ExpressionKind.Parenthesized of Expression` (cited as 6.3, defined inside the 6.3 rule)
instead of flattening, so a parenthesized boolean expression (`x BETWEEN (1 = 1) AND 2`,
`CASE x WHEN (1 = 1) THEN 1`) is a 6.39 `<boolean predicand>` and is accepted —
`isBooleanTopLevel` treats `Parenthesized` as non-boolean. This resolves the
over-strictness documented in the 2026-09-18 follow-up. `expressionChildren` forwards
the inner child transparently, so the standalone-`ANY` guard still sees through it.
`(ts1 - ts2)` without a qualifier is now a `Parenthesized` subtraction (the qualified
4th alternative of 6.37 is tried first and unchanged).

## The privilege method list requires a routine type (2026-09-19)

12.3's `<privilege method list>` items are `<specific routine designator>`s, whose
`<routine type>` is mandatory — but `pSpecificRoutineDesignator` accepts a bare name
(for `ALTER ROUTINE add`, a deliberate trade-off), which leaked into `pPrivileges`:
`GRANT SELECT (c1, c2)` was classified as `PrivilegeMethods` (the column-list branch
was unreachable) and `SELECT (a INT, b INT)` was accepted. `pPrivilegeMethodItem` now
re-checks the designator (`IsSpecific || RoutineType.IsSome`), so a bare name list
falls through to the `<privilege column list>` branch and typed lists are rejected.
`pSpecificRoutineDesignator` itself is unchanged — `ALTER ROUTINE add` still parses.

## The `*` wildcard is out of the expression parser (2026-09-18)

`opp.TermParser` no longer accepts a bare `*` (`ExpressionKind.Star`). The 7.16
`<asterisk>` is not a `<value expression primary>`; its three legitimate positions are
now parsed where they belong:

- `COUNT ( * )` is its own 10.9 `<aggregate function>` alternative — the bare `*` is
  parsed directly in `pRoutineInvocation`'s argument list and rejected for every other
  function name (`SUM(*)`, `my_func(*)` are rejected; `COUNT(*, x)` fails the arity check).
- The select list's bare `<asterisk>` is parsed by `pSelectList` (7.16), and the
  `<qualified asterisk>` / `<all fields reference>` forms by `pQualifiedAsterisk` —
  the workaround guard in `pSelectSublist` is gone.
- The row-pattern quantifier `*` (7.9) and the `*`/`/` operators (6.29/6.37) are
  unrelated and unchanged.

`pValueExpressionPrimaryImpl` now carries only ONE grammar-exceeding approximation (the
§8 predicate atoms); `pValueExpressionPrimary` excludes it for the 6.35/6.37
datetime & interval chain, while `pValueExpressionPrimaryWithPredicates` (the
`opp.TermParser` form) includes it.

## Renaming the value-expression primaries (2026-09-18)

The spec-faithful parser is now named `pValueExpressionPrimary` (the grammar's
6.3 rule name); the permissive form that adds the §8 predicate atoms is
`pValueExpressionPrimaryWithPredicates` (the `opp.TermParser` form). The old
`pValueExpressionPrimaryStrict` name was misleading — "strict" suggested a
deviation, when it was in fact the conforming parser. Pure rename, no behavior
change.
