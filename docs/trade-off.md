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
  (6.1 → 7.17). Single-use sub-parsers are nested inside their consumer and so
  cause no inversion (`pForeignKeyConstraint` in `pTableConstraint`, `pMemberList`
  in `pRepresentation`, `pSearchClause`/`pCycleClause` in `pCte`, …).

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
  `INTERVAL 'abc' YEAR`.
- **Row value constructors and JSON.** 7.1's `<explicit row value constructor>` is
  an expression case (the parenthesized form needs ≥ 2 elements, so `(a)` still
  means the plain parenthesized expression). JSON paths are plain `string`s and the
  JSON argument slots use the boolean-free `pValueExpressionNoBoolean` —
  deliberately stricter than the grammar, to keep comma-separated lists
  unambiguous. `JsonValueBehavior` and `JsonQueryBehavior` are separate DUs, and
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
  `CREATE TABLE` and `ALTER SEQUENCE` take the permissive `pSequenceOption`, while
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
  `pOverride` is shared by `INSERT`/`MERGE` (`bool option`), and `MergeInsert`
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
  positions; `RoutineType` stays an option, and `pRoutineDesignatorWithType` is now
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
