# Design Trade-offs

This document records significant design decisions and the trade-offs considered.

## Transaction statements live in a dedicated module

Transactions (`START TRANSACTION`, `COMMIT`, `ROLLBACK`, `SAVEPOINT`, `SET TRANSACTION`, `SET CONSTRAINTS`) are neither data manipulation nor data definition statements, so they were placed in a new `TransactionParser.fs` module rather than `DmlParser.fs` or `DdlParser.fs`.

- **Trade-off:** An extra file and `Compile Include` entry, but keeps each parser module conceptually cohesive and mirrors the SQL-2016 grammar's "SQL transaction statement" section.
- Alternative (folding into `DmlParser`) was rejected because it would blur the module's responsibility.

## GRANT / REVOKE modeled as discriminated unions distinguishing privileges from roles

`GrantStatement` is `GrantPrivileges` | `GrantRoles` (and `RevokeStatement` likewise). The two forms share the `GRANT ... TO ...` shape but differ in the `ON <object>` clause and `WITH GRANT OPTION` vs `WITH ADMIN OPTION`.

- **Trade-off:** A single record with optional fields would be simpler but loses exhaustiveness and makes the two forms harder to distinguish in consumers. The DU keeps each variant's data precise.

## `ALTER TABLE ... ADD CONSTRAINT` reuses `pTableConstraint`

The optional `CONSTRAINT <name>` prefix is owned by `pTableConstraint` (so `ADD CONSTRAINT fk ...` and `ADD PRIMARY KEY (...)` both work). The `ADD` action parser is simply `ADD <pTableConstraint>`.

- **Trade-off:** This avoids double-consuming the constraint name (which would happen if the `ADD` action also tried to parse an optional `CONSTRAINT` keyword). The `CONSTRAINT <name>` grammar is defined once, in one place.

## Set-operation `ORDER BY` scope via `QueryExpression` wrapper

`SELECT a UNION SELECT b ORDER BY c` — the `ORDER BY` applies to the whole set-operation result, not the last `SELECT`. To represent this, `Query` gained a `QueryExpression` case that carries the top-level `ORDER BY`/`OFFSET`/`FETCH`/`LOCKING` when the body is a set operation or `WITH` query.

- **Trade-off:** A new `Query` union case (consumers must handle it) vs. attaching `ORDER BY` to the last operand (semantically wrong scope) or growing `WithQuery` with optional fields (bloats that case). The wrapper only appears when the body is *not* a plain `SELECT` AND has trailing clauses; plain `SELECT ... ORDER BY` still folds into `SelectStatement`, so the common AST shapes are unchanged.

## Data type parser avoids left recursion

The type grammar is mutually recursive (`<data type>` can be a collection of `<data type>`), which is left-recursive as written. FParsec cannot parse direct left recursion, so `pDataTypeElement` (all non-collection types) is parsed first, then optional `ARRAY`/`MULTISET` suffixes (`pCollectionType`).

- **Trade-off:** This removes the `StackOverflowException` on user-defined/unknown types but means nested collections (`INT ARRAY ARRAY`) are not supported — a rare case in practice. `ROW(...)` fields still allow collection element types via `pDataType`.

## Datetime value functions and string functions as dedicated AST cases

`CURRENT_DATE`/`CURRENT_TIME`/`CURRENT_TIMESTAMP`/`LOCALTIME`/`LOCALTIMESTAMP` are reserved words, so they cannot be parsed as identifiers or generic function calls. They get dedicated `ExpressionKind` cases (`CurrentDate`, `CurrentTime of int option`, ...) with optional precision. Likewise `SUBSTRING(x FROM a FOR b)` and `OVERLAY(x PLACING y FROM n)` get dedicated cases.

- **Trade-off:** More AST cases vs. forcing these into `FunctionCall` (which would lose the FROM/FOR structure and require reserved-word identifiers). `SUBSTRING`/`OVERLAY`/`TRIM`/`POSITION`/`EXTRACT` are deliberately **not** in the `pRoutineInvocation` reserved-function whitelist, so the non-standard comma form (`SUBSTRING(x, a, b)`) is now rejected (see "`functionKeywords` trimmed to the shapes that have no dedicated parser" below).

## `FETCH` quantity optional, `OFFSET` ROW/ROWS required

Per the grammar, `FETCH FIRST [<quantity>] ROW ONLY` allows omitting the quantity (defaults to 1), while `OFFSET <n> ROW|ROWS` requires the row-count unit. `pFetch` now defaults the count to a literal `1` when omitted; `pOffset` requires `ROW`/`ROWS`.

- **Trade-off:** `OFFSET 5` (no unit) is now rejected, matching the grammar and closing a silent-acceptance gap.

## `FROM` as a table reference list

The grammar's `<from clause> ::= FROM <table reference list>` is a comma-separated list of table references. `SelectStatement.From` is therefore `TableSource list` (empty = no `FROM`), not a single `TableSource option`. `FROM a, b, c` is `[a; b; c]` rather than a nested join tree.

- **Trade-off:** A list is the honest representation of the grammar and lets consumers distinguish `FROM a, b` from `FROM a CROSS JOIN b`. It changed the AST shape (all `From` consumers updated), but avoids conflating comma with `CROSS JOIN`.

## `GROUP BY` as grouping elements

`GROUP BY` accepts ordinary grouping sets, `ROLLUP`/`CUBE`, `GROUPING SETS`, empty grouping sets `()`, and a `DISTINCT`/`ALL` quantifier. `SelectStatement.GroupBy` is `GroupingElement list` (`GroupingSet of Expression list | Rollup | Cube | GroupingSets | EmptyGroupingSet`) plus a `GroupByDistinct: bool` flag.

- **Trade-off:** A dedicated DU keeps the grammar's structure (a parenthesized `(a, b)` is a single grouping set, not two columns) and is exhaustively matchable. `GROUP BY a, b` is `[GroupingSet [a]; GroupingSet [b]]`.

## Column-level constraints folded into `ColumnDefinition` fields

Column-level `UNIQUE`, `REFERENCES`, and `CHECK` were previously rejected. `ColumnDefinition` now carries `IsUnique: bool`, `References: ForeignKeyConstraint option`, and `Check: Expression option` (alongside the existing `IsNullable`/`IsPrimaryKey`/`DefaultValue`). `pColumnDefinition` parses `many pColumnConstraint` and folds each constraint into the matching field.

- **Trade-off:** Flat boolean/option fields keep consumers simple and match how `IsPrimaryKey` was already modeled, at the cost of not preserving constraint order. A `ColumnConstraint list` would preserve order but force every consumer to filter. `ReferentialAction`/`ForeignKeyConstraint` had to move *before* `ColumnDefinition` in `Ast.fs` so the new `References` field can reference them.

## `DROP`/`TRUNCATE` behavior as `bool`

`DROP TABLE t CASCADE|RESTRICT` and `TRUNCATE TABLE t RESTART|CONTINUE IDENTITY` now parse. `DropTable` is `Expression * bool` and `Truncate` is `Expression * bool option` (`true` = CASCADE/RESTART, `false` = RESTRICT/CONTINUE). The grammar marks `<drop behavior>` as required, so `DROP TABLE t` without a behavior is now rejected.

- **Trade-off:** `DROP TABLE t` (the common form without behavior) is now rejected to match the grammar. `TRUNCATE` keeps `bool option` because the grammar's `<identity column restart option>` is genuinely optional. A dedicated DU would be more descriptive but a `bool` is enough for the two-way choice.

## `INSERT` source and override as dedicated AST shapes

`InsertSource` gained a `DefaultValues` case (`INSERT INTO t DEFAULT VALUES`), and `InsertStatement` gained `Override: bool option` (`OVERRIDING USER|SYSTEM VALUE`). `VALUES (DEFAULT)` and `UPDATE ... SET col = DEFAULT` are handled by a new `ExpressionKind.Default` case.

- **Trade-off:** `DEFAULT` is *not* a general expression — `SELECT DEFAULT` is rejected. It is only accepted in the contexts where the grammar allows `<default value>`: `INSERT ... VALUES (...)` and `UPDATE ... SET col = ...`. A dedicated `pDefaultValue` parser is used in those two spots instead of adding `DEFAULT` to the expression term parser. The `Override` field is `bool option` (`Some true` = USER, `Some false` = SYSTEM).

## `UPDATE` set clause as a `SetClause` DU

`UPDATE t SET (a, b) = (1, 2)` (multiple assignment) is distinct from `SET col = value`. `UpdateStatement.Set` is now `SetClause list` where `SetClause = SingleSet of Expression * Expression | MultipleSet of Expression list * Expression list`.

- **Trade-off:** A DU keeps the two forms distinguishable and exhaustively matchable, at the cost of changing the `Set` shape (consumers must unwrap `SingleSet`). `UPDATE`/`DELETE` also gained `TableAlias: Expression option` for `UPDATE t AS x` / `DELETE FROM t AS x`.

## `CREATE TABLE AS SELECT` and `CREATE VIEW` column lists

`CREATE TABLE t AS SELECT ... [WITH [NO] DATA]` and `CREATE TABLE t (a, b) AS SELECT ...` are supported via `AsQuery: Query option`, `AsColumns: Expression list option`, and `WithData: bool option` on `CreateTableStatement`. `CREATE VIEW v (a, b) AS ...` gained `Columns: Expression list option`.

- **Trade-off:** Optional fields on the existing record keep the common `CREATE TABLE (col defs)` shape unchanged, but the `(a, b)` column-name list in the AS-SELECT form is stored separately from `Columns` (which holds `ColumnDefinition`s). `pCreateTableStatement` tries the table-element list first, then the column-name-list + AS form, then bare AS.

## Schema-qualified names via `pSchemaQualifiedName` / `pQualifiedNameExpr`

The 5.4 `<schema qualified name>` rule (`[ <schema name> <period> ] <qualified identifier>`, i.e. `catalog.schema.name`) is implemented in the Lexer as `pSchemaQualifiedName`, which returns the parts as a `string list`. `app.users` (and deeper `a.b.c`) now parse in table-name positions (`FROM`, `INSERT INTO`, `UPDATE`, `DELETE FROM`, `MERGE INTO`, `CREATE TABLE/INDEX/VIEW`, `DROP`, `TRUNCATE`, `ALTER TABLE`, `GRANT`/`REVOKE` objects). `pQualifiedNameExpr` (in `ExpressionParser`) maps that list to a single `Expression` — `Identifier` for one part, `ColumnReference parts` for two or more — reusing the same shape as column references.

- **Trade-off:** Reusing `ColumnReference` avoids a new AST case but means a table name and a column reference are indistinguishable by `Kind` alone (consumers must rely on position). A dedicated `QualifiedName` case would be clearer but adds a case for a shape already modeled. The Lexer returns a plain `string list` (not a dedicated type) so the same parser can feed both the expression path and any future name-only consumers.

## Quantified comparison via an intermediate `QuantifiedSubquery` term

`x = ANY (SELECT ...)` / `x > ALL (...)` / `x = SOME (...)` failed because FParsec's `OperatorPrecedenceParser` consumes the `=` and does not backtrack when the right operand fails. The fix parses `ANY/SOME/ALL (subquery)` as a term (`QuantifiedSubquery of Quantifier * Query`), then the comparison-operator mappings (`comparisonOp`) rewrite `BinaryOp(op, l, QuantifiedSubquery(q, sub))` into `QuantifiedComparison(op, q, l, sub)`.

- **Trade-off:** A `QuantifiedSubquery` intermediate case leaks into the AST for standalone `ANY (SELECT ...)` (semantically invalid but parseable). To close that gap, `pExpression` runs a recursive `containsStandaloneQuantifiedSubquery` check and rejects any surviving `QuantifiedSubquery` (i.e. one not rewritten by a comparison operator). The alternative — special infix operators per operator×quantifier (18 combinations) — was rejected as too noisy. `Quantifier` uses `SomeQuantifier` (not `Some`) because the case name would shadow F#'s option constructor.

## `FILTER` / `WITHIN GROUP` as optional `FunctionCall` fields

`COUNT(*) FILTER (WHERE x > 0)` and `PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY x)` now parse. `FunctionCall` gained a `filter: Expression option` and a `withinGroup: (Expression * bool * NullsOrder option) list option` field (after the existing window field).

- **Trade-off:** Growing the tuple keeps one case for all function calls (plain, aggregate-with-filter, ordered-set) at the cost of a 6-field tuple. A dedicated `AggregateFunction` case would be cleaner but would split `COUNT(*)` into two different AST shapes depending on whether a suffix is present.

## `COLLATE` as a predicate-suffix

`x COLLATE "C"` parses via a new `Collate of Expression * Expression` case handled in `pPredicate` (postfix on an expression).

- **Trade-off:** COLLATE is not a predicate, but it is a postfix on a value expression, so reusing the suffix mechanism is structurally correct and cheap. The collation name is parsed as an identifier expression.

## `LATERAL` / `UNNEST` / `TABLESAMPLE` as `TableSourceKind` cases

`FROM LATERAL (SELECT ...) AS u`, `FROM UNNEST(arr) WITH ORDINALITY AS u`, and `FROM t TABLESAMPLE BERNOULLI (10)` now parse. `TableSourceKind` gained `Lateral of Query * Expression * Expression list option`, `Unnest of Expression * bool * Expression * Expression list option`, and `TableSample of TableSource * string * Expression * Expression option` (method, percentage, repeat argument). `pTablePrimary` parses the base then applies an optional `TABLESAMPLE` suffix.

- **Trade-off:** `TableSample` wraps a `TableSource` (recursive), which is honest about the grammar (`<table sample clause>` is a suffix on `<table primary>`). The alias for `Lateral`/`Unnest` is required (`Expression`, matching `Subquery`), so `FROM LATERAL (...)` / `FROM UNNEST(arr)` without an alias are rejected per the grammar.

## Hex string literals allow spaces between hexit pairs

`X'01 AF 02'` (spaces between hexit pairs, and trailing spaces before the closing quote) now parses. `pSegment` skips `spaces` before each pair and after the whole segment.

- **Trade-off:** `spaces` (which also accepts tabs/newlines) is used rather than a literal space, matching FParsec's usual whitespace handling. The `attempt` wrapper lets `many` stop cleanly at the closing quote without consuming trailing whitespace.

## `DATE` literal values are validated

`pDateValue` now uses `pUnsignedInteger` (so a leading `-` is rejected) and validates month ∈ 1..12 and day ∈ 1..31, failing with `invalid date` otherwise.

- **Trade-off:** Range validation is done at parse time (rejecting `DATE '2023-13-01'`) rather than deferring to a semantic layer. The check is deliberately coarse (day ≤ 31, not per-month) to keep the lexer simple; exact calendar validation is left to consumers.

## SEQUENCE options shared between SEQUENCE statements and IDENTITY columns

`CREATE SEQUENCE` / `ALTER SEQUENCE` (11.72/11.73) and the `<identity column specification>` (11.4) share the same `<common sequence generator options>`. All of them are parsed by a single `SequenceOption` DU (`DataTypeOption` / `StartWith` / `IncrementBy` / `MaxValue of decimal option` / `MinValue of decimal option` / `Cycle of bool` / `Restart of decimal option`), defined in `DdlParser.fs` *before* `pColumnDefinition` so both parsers can reference it.

The sub-rules are separate parsers so each caller can take exactly what its clause allows:

- `pBasicSequenceGeneratorOption` — `<basic sequence generator option>` (11.72): `INCREMENT BY`, `MAXVALUE`, `NO MAXVALUE`, `MINVALUE`, `NO MINVALUE`, `CYCLE`, `NO CYCLE`.
- `pSequenceGeneratorStartWithOption` — `<sequence generator start with option>` (11.72).
- `pAlterSequenceGeneratorRestartOption` — `<alter sequence generator restart option>` (11.73).
- `pSequenceOption` — `<sequence generator option>`: `<sequence generator data type option>` (`AS <data type>`) or `<common sequence generator options>` (start-with + basic). Used by `CREATE SEQUENCE`.

- **Trade-off:** One options DU keeps the features consistent and avoids near-identical types, while the split parsers keep each production to its own grammar. `MaxValue`/`MinValue` use `decimal option` where `None` means `NO MAXVALUE`/`NO MINVALUE`. `CREATE TABLE`'s `<identity column specification>` and `ALTER SEQUENCE` still reuse the permissive `pSequenceOption` (so `RESTART`/`AS <data type>`/`START WITH` are not restricted there); the new `<alter identity column specification>` (11.20) instead uses the narrowed parsers, since it is the only place where the standard is explicit about `SET <basic sequence generator option>`. `CreateTableStatement.TableScope` (`TableScope option`: `GLOBAL TEMPORARY`/`LOCAL TEMPORARY`) and `ColumnDefinition.Identity` (`IdentitySpec option` = `{ IsAlways; Options }`) are plain option fields, so the absent forms (`CREATE TABLE ...`, `id INT`) are `None`.

## Full `<alter table action>` coverage (11.10)

`AlterTableAction` now covers every alternative of `<alter table action>`, so `ALTER TABLE` is complete against 11.10:

| Action | AST case | Rule |
|--------|----------|------|
| `ADD [COLUMN] <column definition>` | `AddColumn` | 11.11 |
| `ALTER [COLUMN] <column name> <alter column action>` | `AlterColumn` | 11.12 |
| `DROP [COLUMN] <column name> <drop behavior>` | `DropColumn of Expression * bool` | 11.23 |
| `ADD <table constraint definition>` | `AddConstraint` | 11.24 |
| `ALTER CONSTRAINT <name> <constraint enforcement>` | `AlterConstraint of Expression * bool` | 11.25 |
| `DROP CONSTRAINT <name> <drop behavior>` | `DropConstraint of Expression * bool` | 11.26 |
| `ADD <table period definition> [...]` | `AddTablePeriod of TablePeriodDefinition * ColumnDefinition list` | 11.27 |
| `DROP <period specification> <drop behavior>` | `DropTablePeriod of TimePeriodSpecification * bool` | 11.28 |
| `ADD SYSTEM VERSIONING` | `AddSystemVersioning` | 11.29 |
| `DROP SYSTEM VERSIONING <drop behavior>` | `DropSystemVersioning of bool` | 11.30 |

`ColumnAlteration` likewise gained `AddColumnScope` (11.17), `DropColumnScope` (11.18), `AlterIdentityColumn` (11.20), `DropIdentity` (11.21), and `DropExpression` (11.22), in `<alter column action>` order.

- **Trade-off:** `<drop behavior>` is modelled as a bare `bool` (`true` = CASCADE, `false` = RESTRICT) rather than a DU, matching `DropTable`/`DropView`/`DropSequence`. `AlterConstraint` carries `[NOT] ENFORCED` as a `bool` too, even though the AST already has a general `ConstraintCharacteristics` record: 11.25 only permits `<constraint enforcement>`, so reusing that record would advertise `INITIALLY`/`DEFERRABLE` support the parser does not provide. `AddTablePeriod`'s optional `<add system time period column list>` is a `ColumnDefinition list` holding exactly 0 or 2 entries (the grammar fixes the arity), which keeps absent/`[]` and present unambiguous.
- **Trade-off:** Because the grammar marks `<drop behavior>` as required, `ALTER TABLE t DROP COLUMN c` and `ALTER TABLE t DROP CONSTRAINT c` (the common bare forms) are now rejected — the same decision already taken for `DROP TABLE`/`DROP VIEW`/`DROP SEQUENCE`/`REVOKE`. Making the new actions parse-only (no `<generation clause>` or `GENERATED ALWAYS AS ROW START|END` support in `<column definition>`) means `DROP EXPRESSION` and the `ADD ... ADD COLUMN` pair are accepted without the corresponding column *definitions* being expressible yet; those belong to 11.3/11.4 and are tracked separately in `docs/audit-report.md`.
- **Trade-off:** `TimePeriodSpecification` and `TablePeriodDefinition` are shared types placed on the `AlterTableAction` side of `Ast.fs`. They mirror the grammar's `<table element>` alternatives (11.3), so a future `CREATE TABLE ... WITH SYSTEM VERSIONING` / `PERIOD FOR ...` element can reuse them without an AST change.

## `CALL` and `SET ROLE` get their own modules

`CALL` (13.1, a SQL-control statement) and `SET ROLE` (17.9, a SQL-session statement) are neither DML, DDL, nor transaction statements, so they were placed in new `ControlParser.fs` and `SessionParser.fs` modules (mirroring the `TransactionParser.fs` decision) rather than folded into `DmlParser.fs`/`TransactionParser.fs`.

- **Trade-off:** Two more files and `Compile Include` entries, but keeps module responsibility aligned with the grammar's "SQL-control statement" and "SQL-session statement" sections. These modules will be extended to cover cursors/connection/session/diagnostics/dynamic SQL. `SetRole of Expression option` uses `None` for `SET ROLE NONE`; the role is parsed as a value expression, so `SET ROLE CURRENT_USER` also works.

## `DROP VIEW` / `DROP SEQUENCE` require a drop behavior

`DROP VIEW v CASCADE|RESTRICT` and `DROP SEQUENCE s CASCADE|RESTRICT` now parse; `DropView`/`DropSequence` are `Expression * bool` (`true` = CASCADE, `false` = RESTRICT). The grammar marks `<drop behavior>` as required for views and sequences, so `DROP VIEW v` without a behavior is rejected (the same rule already applied to `DROP TABLE`).

- **Trade-off:** Breaking change for the bare `DROP VIEW v` form (previously accepted), but matches the grammar and closes a silent-acceptance gap. `DropStatement` is now uniformly `Expression * bool` for `DropTable`/`DropView`/`DropSequence`, with only `DropRole` (no behavior in the grammar) as a single-arg case.

## `INTERVAL` literal values are validated against the qualifier

`INTERVAL 'abc' YEAR` and `INTERVAL '1-2' YEAR` (value shape not matching the qualifier) are now rejected. `isValidIntervalValue` matches the value string against a regex per `IntervalQualifier` (single field vs. each `Range` combination).

- **Trade-off:** Regex-based shape validation is pragmatic and compact, but it only checks the *shape* (e.g. `1-2` for `YEAR TO MONTH`), not numeric bounds (e.g. month ≤ 12). The `_ -> "^$"` fallback rejects any qualifier combination the grammar doesn't define. `IntervalQualifier.Range` must be fully qualified in `Lexer.fs` because `Range` also names a `WindowFrameUnit` case.

## Non-standard syntax removed

`CREATE INDEX` / `DROP INDEX`, `ALTER TABLE ... RENAME TO` / `RENAME COLUMN`, and `FOR SHARE` were previously accepted but do not exist in the SQL-2016 grammar (`sql-2016-grammar.txt` contains no `INDEX`, `RENAME`, or `SHARE`; the `<updatability clause>` is only `FOR READ ONLY | FOR UPDATE`). They were removed:

- `CreateIndexStatement` type, `CreateIndex` statement case, and `DropIndex` removed from `Ast.fs`/`DdlParser.fs`/`SqlParser.fs`.
- `RenameTo` / `RenameColumn` removed from `AlterTableAction`.
- `ForShare` removed from `LockingClause` (only `ForUpdate` remains).

- **Trade-off:** Removing these closes the "invalid SQL silently accepted" gap for vendor extensions. `FOR UPDATE` is kept because it *is* in the spec's `<updatability clause>`. Indexes are implementation-defined in the standard, so a consumer needing them must add their own extension layer.

## GRANT / REVOKE aligned with grammar 12.2 / 12.5 / 12.7

`GRANT` now accepts `[ WITH HIERARCHY OPTION ] [ WITH GRANT OPTION ] [ GRANTED BY <grantor> ]` (privileges) and `[ WITH ADMIN OPTION ] [ GRANTED BY <grantor> ]` (roles). `GrantPrivileges` gained a `withHierarchyOption: bool` field; `GRANTED BY <grantor>` is parsed and discarded (like `CREATE ROLE ... WITH ADMIN`). `REVOKE` gained the `<revoke option extension>` (`GRANT OPTION FOR` / `HIERARCHY OPTION FOR` for privileges, `ADMIN OPTION FOR` for roles) and now **requires** the `<drop behavior>` (`CASCADE` / `RESTRICT`), so `REVOKE ... FROM u` without a behavior is rejected. `RevokePrivileges`/`RevokeRoles` carry the option and a `cascade: bool`. `PrivilegeAction` gained `Under` (12.3 `<action>`).

- **Trade-off:** `REVOKE SELECT ON t FROM u` (the common form without a drop behavior) is now rejected to match the grammar, mirroring the earlier `DROP TABLE` decision. `GRANTED BY` is accepted but not surfaced in the AST — it is authorization metadata with no consumer yet, so discarding it keeps the AST minimal. `pGrantor`/`pDropBehavior` were hoisted to module level so `CREATE ROLE`, `GRANT`, `REVOKE`, and `DROP` share them.

## `GROUPS` window frame unit

`<window frame units> ::= ROWS | RANGE | GROUPS` — `WindowFrameUnit` gained `Groups` and the frame parser accepts `GROUPS` alongside `ROWS`/`RANGE`.

- **Trade-off:** A third union case is required for exhaustiveness; no behavior change beyond accepting the keyword.

## `ExplicitTable` and `TableValueConstructor` as `Query` cases

Per 7.17 `<simple table> ::= <query specification> | <table value constructor> | <explicit table>`, `Query` gained `ExplicitTable of Expression` (`TABLE t`) and `TableValueConstructor of Expression list list` (`VALUES (1, 'a'), (2, 'b')`). Both are valid `<query primary>`s, so they can appear as set-operation operands (`TABLE a UNION TABLE b`) and at the top level.

- **Trade-off:** Two new `Query` cases (consumers must handle them). `applyOrderByOffsetFetch` wraps them in `QueryExpression` when trailing `ORDER BY`/`OFFSET`/`FETCH` are present, since they are not plain `SELECT`s. In `FROM`, a parenthesized `(VALUES ...)` still yields the dedicated `ValuesTable` table source — the `pTablePrimary` VALUES branch is tried *before* the subquery branch so the existing `ValuesTable` shape is preserved (see gotchas).

## `NATURAL CROSS JOIN` rejected

7.10 `<natural join>` uses `<join type>` (which has no `CROSS`), so `NATURAL CROSS JOIN` is invalid. `pJoinedTableSuffix` now selects the join-type parser based on whether a `NATURAL` prefix was consumed.

- **Trade-off:** A small conditional in the suffix parser (`if Option.isSome nat then pJoinTypeWithoutCross else pJoinType`) instead of a single permissive parser, closing the silent-acceptance gap.

## `TABLESAMPLE` method restricted to `BERNOULLI | SYSTEM`

7.6 `<sample method> ::= BERNOULLI | SYSTEM`. `pSampleMethod` now accepts only those two keywords (previously any identifier was accepted).

- **Trade-off:** Vendor methods (e.g. `RANDOM`) are rejected per the spec; consumers needing them must extend `pSampleMethod`.

## Parenthesized `<query primary>` allows trailing clauses

7.17 `<query primary> ::= <simple table> | ( <query expression body> [ <order by clause> ] [ <result offset clause> ] [ <fetch first clause> ] )`. The parenthesized branch now parses optional `ORDER BY`/`OFFSET`/`FETCH`/`LOCKING` inside the parens and folds them via `applyOrderByOffsetFetch`, so `(SELECT 1 ORDER BY 1) UNION SELECT 2` is valid.

- **Trade-off:** The parenthesized branch is now a 4-tuple pipeline; the trailing clauses are folded into the inner query (or wrapped in `QueryExpression` for set operations), keeping scope correct.

## `ONLY` / data-change-delta keep the optional correlation name

7.6 `<only spec>` / `<data change delta table>` allow an optional `<correlation or recognition>` suffix. `TableSourceKind.Only` and `TableSourceKind.DataChangeDelta` now carry the correlation name plus the parenthesized derived column list, so `FROM ONLY (t) AS u (a)` and `FROM OLD TABLE (DELETE FROM t) AS d (x)` no longer discard them.

- **Trade-off:** Both cases now hold one more field than `Table` (which keeps only the alias), because `pCorrelationOrRecognition` yields the pair. The other half of `<correlation or recognition>` — a `<row pattern recognition clause>` — is still not accepted in this slot.

## `TABLE (expr)` classified as `TableFunction` vs `PtfTable` by a `FunctionCall` check

`<table function derived table>` and `<PTF derived table>` are syntactically identical (`TABLE ( <value expression> )`), so `pTablePrimary` parses the expression once and classifies it: a `FunctionCall` → `PtfTable`, anything else → `TableFunction`.

- **Trade-off:** A shape-based heuristic. A PTF that is not a plain routine invocation would be misclassified, but in practice PTFs are function calls. A dedicated parser cannot distinguish the two without semantic knowledge.

## `SYMMETRIC` / `ASYMMETRIC` modelled by a `SystemTimeSymmetry` DU

7.6 `<query system time period specification>` — `FOR SYSTEM_TIME BETWEEN [ ASYMMETRIC | SYMMETRIC ] p1 AND p2` stores the qualifier as `SystemTimeSymmetry option` on `SystemTimeSpec.Between` (`None` = unspecified).

- **Trade-off:** The qualifier is semantic metadata with no consumer yet, but carrying it is cheap and stops the parser from accepting and silently dropping a token. A DU `option` is used instead of a bare `bool` so that "absent" and "explicitly asymmetric" stay distinguishable.

## `<point in time>` uses a dedicated `<datetime value expression>` parser

6.35 `<datetime value expression>` is implemented as `pDatetimeTerm` (`<datetime primary> [ AT TIME ZONE … ]`) left-folded over `+`/`-`. `<point in time>` (7.6 `FOR SYSTEM_TIME`, 14.9/14.14 `FOR PORTION OF`) now uses it instead of `pValueExpressionNoBoolean`, so `a * b`, `a || b` and `a = b` are rejected where a point in time is expected, while `CURRENT_DATE - INTERVAL '1' DAY`, `CURRENT_TIMESTAMP AT TIME ZONE x` and `? DAY` work.

- **Trade-off:** The right operand of `+`/`-` may be an `<interval term>` or a `<datetime term>` and the two are syntactically indistinguishable, so `attempt pIntervalTerm <|> pDatetimeTerm` is tried in that order. `<interval term>`'s `*`/`/` right operand is the grammar's `<factor>` (6.29) — `[ <sign> ] <numeric primary>`, approximated by `[ <sign> ] <value expression primary>` — not an `<interval factor>`, so the `[ <interval qualifier> ]` suffix is rejected there (`INTERVAL '1' DAY * ? DAY`). `pValueExpressionNoBoolean` is used by the JSON slots, which is deliberately stricter than the grammar's `<value expression>`.

## DML `<target table>` supports `ONLY ( <table name> )`

`UPDATE` / `DELETE` / `MERGE` parse their target with `DmlParser.pTargetTable`, which accepts `ONLY ( <table name> )` and records it in the new `TableIsOnly` / `TargetIsOnly` record fields. `INSERT` deliberately keeps `pQualifiedNameExpr`: its `<insertion target>` is a plain `<table name>` (14.11).

- **Trade-off:** A `bool` flag is used instead of a dedicated target DU so that the existing record patterns in the tests keep compiling; the flag is only meaningful next to the name it qualifies.

## `<all fields reference>` accepts any `<value expression primary>`

7.16 `<all fields reference> ::= <value expression primary> <period> <asterisk> [ AS ( <all fields column name list> ) ]` is parsed by a second branch of `pQualifiedAsterisk`, so `(a + b).*`, `f(x).*` and `ROW(a, b).*` work. The `<asterisked identifier chain>` branch is still tried first, which keeps `t.*` a `QualifiedStar` and `t.* AS (x)` an `AllFieldsReference` — both shapes predate the general form.

- **Trade-off:** The general branch parses a full `<value expression primary>` before discovering that no `.` follows, so a select sublist is parsed twice. It is `attempt`ed, so this only costs time. `AllFieldsReference`'s column list is now optional (`None` = no `AS` clause), which the type already allowed.

## `<explicit row value constructor>` (7.1) is an expression case

`(e1, e2, …)` and `ROW(e1, …)` are `ExpressionKind.RowValueConstructor`, added to `pValueExpressionPrimary` so that `<row value predicand>` positions work: `SELECT (1, 2)`, `SELECT (1, 2) = (3, 4)` and `SELECT (1, 2) IN ((1, 2), (3, 4))`.

- **Trade-off:** The parenthesized form needs two or more elements (`pExpression .>>. many1`) and is wrapped in `attempt`, so `(a)` still falls through to the plain parenthesized `<value expression>` branch; `ROW ( … )` uses `sepBy1`, so one element is accepted there. `<row subquery>` stays `SubqueryExpression`, and the `<contextually typed row value constructor>` uses in `VALUES` / `SET (a, b) = …` keep their existing `Expression list` shapes.

## `<interval primary>` (6.37) wraps only when a qualifier is present

`<interval primary> ::= <value expression primary> [ <interval qualifier> ]` is `IntervalPrimary(expr, qualifier)`, produced only when a qualifier actually follows (`? DAY`). A qualifier-less `<interval primary>` returns its inner `<value expression primary>` unchanged, so `INTERVAL '1' DAY` and plain operands keep their previous AST shapes.

- **Trade-off:** `<interval primary>` is reachable only through `<datetime value expression>` (the point-in-time slots and the `( <datetime value expression> - <datetime term> ) <interval qualifier>` alternative); a general-expression `<interval primary>` such as `SELECT ? DAY` is still rejected. `pTimeZoneSuffix` now reuses `pIntervalPrimary` for its `<time zone specifier>`, which is what its grammar comment always claimed.

## `pPartitionBy` accepts only column references

`<partitioned join column reference list>` is a list of column references, and `pPartitionBy` parses `sepBy1 pColumnReferenceExpr (token ",")` to enforce that (it previously used `pExpression`).

- **Trade-off:** Arbitrary expressions such as `PARTITION BY (a + b)` are now rejected, matching the grammar. A column-reference-only parser is stricter than the codebase's other column-list parsers, but the grammar is explicit here.

## `pJoinSpecification` returns `(JoinCondition * Expression option)`

`<named columns join> ::= USING ( <join column list> ) [ AS <join correlation name> ]` — the `USING` alias is folded into `JoinSource.UsingAlias: Expression option` via the tuple returned by `pJoinSpecification`.

- **Trade-off:** A tuple return keeps the `ON`/`USING` choice and the optional alias in one parser; the caller (`pJoinedTableSuffix`) destructures it into the `JoinSource` fields.

## `pRoutineInvocation` accepts only reserved *function* keywords

`pRoutineInvocation` derives the routine name from `pReservedFunctionName`, an explicit whitelist of the reserved keywords the grammar spells as functions: the `<aggregate function>` names, `<inverse distribution function type>`, `<window function type>`, and the built-ins that have no dedicated parser. Every other reserved word is rejected by `pRegularIdentifier` (`Lexer.fs`), so `EXISTS (SELECT ...)`, `UNIQUE (...)`, `JSON_EXISTS(...)`, `PERIOD (s, e)` and `VALUE_OF(...)` cannot degrade to a generic function call; their dedicated parsers are also listed BEFORE `pRoutineInvocation` in `pValueExpressionPrimary`.

- **Trade-off:** Adding a new built-in or vendor function whose name is reserved requires adding it to `functionKeywords` in `ExpressionParser.fs` (see docs/gotchas.md). Non-reserved names (`foo(...)`, `app.foo(...)`) are unaffected.

## `JSON_ARRAY(NULL ON NULL ...)` ambiguity

`JSON_ARRAY`'s element list is `[ <JSON value expression list> ]` and the null clause is `NULL ON NULL` / `ABSENT ON NULL`. A bare `NULL` (or `ABSENT`, which is not reserved and parses as an identifier) followed by `ON NULL` is the null clause, not an element. `sepBy pExpression ","` greedily consumes `NULL` as an element, leaving `ON NULL` to fail. Guard: `pExpression .>>? notFollowedBy (attempt (pKeyword "ON" >>. pKeyword "NULL"))` per element.

- **Trade-off:** The `.>>?` + `notFollowedBy` guard makes the element parser reject a leading `NULL`/`ABSENT` that starts the null clause, so `JSON_ARRAY(NULL ON NULL)` yields an empty element list + `JsonNullOnNull`. `JSON_ARRAY(NULL)` and `JSON_ARRAY(NULL, 1)` still parse `NULL` as an element.

## `pJsonKeyUniqueness` returns `bool`, not `bool option`

`WITH UNIQUE [KEYS]` / `WITHOUT UNIQUE [KEYS]` is optional in `JSON_OBJECT`/`JSON_OBJECTAGG`. The parser returns `bool` (`true` = WITH, `false` = WITHOUT) and callers wrap it in `opt` to get `bool option` (None = absent). Returning `bool option` from the parser would double-wrap under `opt` (`bool option option`).

- **Trade-off:** A `bool` return keeps the optionality at the call site, avoiding `Option.flatten` noise. The `IS JSON` predicate's uniqueness clause is handled separately (it returns `bool option` and is wrapped in `opt (attempt ...)` + `Option.flatten`).

## JSON type constraint renamed to `JsonType*`

The `<JSON predicate type constraint>` (`VALUE | ARRAY | OBJECT | SCALAR`) would clash with `ExpressionKind.JsonValue`/`JsonArray`/`JsonObject` if named `Value`/`Array`/`Object`. The DU is `JsonTypeConstraint = JsonTypeValue | JsonTypeArray | JsonTypeObject | JsonTypeScalar`.

- **Trade-off:** Prefixing avoids the clash and keeps the cases self-documenting. Similarly `JsonQueryBehavior` (not `JsonValueBehavior`) for `JSON_QUERY`'s `ERROR | NULL | EMPTY ARRAY | EMPTY OBJECT`.

## Period predicates reuse the existing `Overlaps` case

`<period predicate>` includes `OVERLAPS`, which is already covered by the existing `ExpressionKind.Overlaps`. The new `PeriodPredicate of PeriodPredicateKind * Expression * Expression` covers `EQUALS | CONTAINS | PRECEDES | SUCCEEDS | IMMEDIATELY PRECEDES | IMMEDIATELY SUCCEEDS` only.

- **Trade-off:** No duplicate `Overlaps` case. The period predicate operands are `Expression` (a `<period reference>` is a column reference; `PERIOD (s, e)` is the new `PeriodValue of Expression * Expression` case), so the right operand is `attempt pPeriodValue <|> pExpr`.

## JSON slots use non-boolean value expressions and a plain-string path

`<JSON API common syntax>`'s context item, `<JSON name and value>` name/value, `<JSON passing argument>`, and the `DEFAULT <value expression>` behavior use `pValueExpressionNoBoolean` (the grammar's `<JSON value expression>` is a value expression, not a boolean one). `<JSON path specification>` is a `<character string literal>`, so `JsonApiCommon.Path`, `JsonRegularColumn.Path`, `JsonFormattedColumn.Path`, and `JsonNestedColumns.Path` are now `string`/`string option` rather than `Expression`.

- **Trade-off:** The AST no longer wraps the JSON path in a synthetic `Literal(String ...)` node, so consumers read the path directly. Because `pValueExpressionNoBoolean` is the operator-precedence parser without the boolean operators, boolean expressions (`a AND b`) are rejected in these slots — deliberately *stricter* than the grammar, which permits `<boolean value expression>` inside `<JSON value expression>`, because a comma-separated argument list must stay unambiguous.

## `JSON_QUERY` wrapper/quotes/behavior clauses

`<JSON query wrapper behavior>` (`WITHOUT [ARRAY] | WITH [CONDITIONAL|UNCONDITIONAL] [ARRAY]`) is a record `{ WithWrapper; Conditional; Array }`; `<JSON query quotes behavior>` is `Keep | Omit`; `<JSON query empty/error behavior>` is `JsonQueryError | JsonQueryNull | JsonQueryEmptyArray | JsonQueryEmptyObject`. `JSON_VALUE`'s empty/error behavior is `JsonError | JsonNull | JsonDefault of Expression`.

- **Trade-off:** Two distinct behavior DUs (`JsonValueBehavior` vs `JsonQueryBehavior`) because the grammar defines different alternatives for each. `JsonOutput = { Returning: DataType; Format: JsonRepresentation option }` and `JsonRepresentation = JsonEncoding of JsonEncoding option` (None = no `ENCODING` clause).

## Parser names mirror grammar non-terminals

As a readability refactor, parser functions were renamed to match the SQL-2016 non-terminals they implement, and each module gained a banner listing the grammar sections it covers. Examples: `pTableSource`→`pTableReference`, `pColumnExpr`→`pDerivedColumn`, `pSelectCore`→`pQuerySpecification`, `pTerm`→`pValueExpressionPrimary`, `pPredicateSuffix`→`pPredicate`, `pCastExpr`→`pCastSpecification`, `pSubstringExpr`→`pCharacterSubstringFunction`, `pFunctionCallExpr`→`pRoutineInvocation`, `pParameterExpr`→`pGeneralValueSpecification`.

- **Trade-off:** Renames touch every reference (cross-file references like `DmlParser`'s use of `pTableReference` must be updated in lockstep), but the payoff is that a reader can map each parser to the grammar rule it implements. FParsec's define-before-use constraint means files are still ordered leaf-first rather than grammar-top-down; the banners and names bridge that gap.

## Schema elements restricted to CREATE-family statements and GRANT

`<schema element>` (inside `CREATE SCHEMA`) is an explicit list of *definition* statements. `pSchemaElementImpl` is a `createParserForwardedToRef` whose `.Value` is wired in `SqlParser.fs` (after the individual `CREATE` parsers are defined) to a `choice` of the CREATE-family parsers (`pCreateTableStatement`, `pCreateViewStatement`, `pCreateRoleStatement`, `pCreateSequenceStatement`, `pCreateDomainStatement`, `pCreateCharacterSetStatement`, `pCreateCollationStatement`, `pCreateTransliterationStatement`, `pCreateAssertionStatement`, `pCreateCastStatement`, `pCreateOrderingStatement`, `pCreateTransformStatement`, `pCreateTypeStatement`, `pCreateProcedureStatement`, `pCreateFunctionStatement`, `pCreateTriggerStatement`) plus `pGrantStatement`.

- **Trade-off:** `DROP`/`ALTER`/`TRUNCATE`/`REVOKE` are no longer accepted as schema elements (they were when the forward ref pointed at the whole `pDdl`). The list duplicates the CREATE alternatives rather than reusing `pDdl`, but it closes the "any DDL is a schema element" gap. `CREATE SCHEMA s CREATE TABLE t (id INT)` yields `Elements = [CreateTable _]`.

## `ConstraintCharacteristics` matches the grammar's three alternatives

`<constraint characteristics> ::= <constraint check time> [ [ NOT ] DEFERRABLE ] [ <constraint enforcement> ] | [ NOT ] DEFERRABLE [ <constraint check time> ] [ <constraint enforcement> ] | <constraint enforcement>`. `ConstraintCharacteristics = { InitiallyDeferred: bool option; Deferrable: bool option; Enforced: bool option }` and `pConstraintCharacteristics` tries the three alternatives in order, ending with an empty `preturn` because the whole clause is optional in its enclosing production.

- **Trade-off:** The record stays flat and easy to consume, but the grammar's combination rules are enforced: a bare `[ NOT ] DEFERRABLE` is valid (the second alternative allows the check time to be omitted), while invalid orders such as `ENFORCED DEFERRABLE` are rejected. The parser must leave any following keyword that belongs to the enclosing production (e.g. a domain's `COLLATE` clause) unconsumed — the second alternative requires `DEFERRABLE`, so it cannot silently swallow `COLLATE`.

## Specific routine designator simplified

`<specific routine designator>` is `SPECIFIC <routine type> <name> | <routine type> <name> | <name> [ FOR <udt name> ]` where `<routine type>` is `ROUTINE | FUNCTION | PROCEDURE | [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD`. `pSpecificRoutineDesignator` handles the first three forms but drops the `[ FOR <udt name> ]` clause and the method member data-type list.

- **Trade-off:** The `FOR <udt name>` suffix (used by `CREATE CAST ... WITH SPECIFIC FUNCTION f FOR t`) is discarded — the AST keeps only the routine name. This keeps `CreateCast`/`CreateOrdering`/`CreateTransform` shapes simple; the udt name has no consumer yet.

## Transliteration source parsed as a name, not a routine designator

`<transliteration definition>`'s source is a `<specific routine designator>`, but `CreateTransliteration of Expression * Expression * Expression * Expression` (name, source, target, path) parses the source as a plain `pQualifiedName`.

- **Trade-off:** `CREATE TRANSLATION tr FROM src TO tgt USING path` treats `src` as a name. The grammar's routine-designator form (`FROM SPECIFIC FUNCTION f`) is not supported — a simplification consistent with the specific-routine-designator decision above.

## Drops added to the existing `pDropStatement` choice

`DROP SCHEMA/DOMAIN/COLLATION/CHARACTER SET/TRANSLITERATION/ASSERTION/CAST/ORDERING/TRANSFORM` were added as new alternatives of the existing `pDropStatement` (which already handled table/view/role/sequence). `DropAssertion` uses `bool option` (the grammar's `<drop behavior>` is optional there); `DropCast`/`DropTransform` require a behavior.

- **Trade-off:** One drop parser keeps `DROP` dispatch in a single place. `DropCast`'s source/target are `DataType` (parsed via `pDataType`), so `DROP CAST (INT AS BIGINT) CASCADE` works. `DropTransform` reuses `pTransformsToBeDropped` (`ALL | <group name>`), which must be defined *before* `pDropStatement` (F# define-before-use).

## Transform groups are whitespace-separated

`<transform definition>` / `<alter group>` allow multiple groups with no separator: `CREATE TRANSFORM FOR t g1 (...) g2 (...)`. `pCreateTransformStatement`/`pAlterTransformStatement` use `many1 pTransformGroup` / `many1 pAlterTransformGroup` with no separator between groups.

- **Trade-off:** Matches the grammar exactly (`<transform group>...`). The `TRANSFORM | TRANSFORMS` keyword choice is parenthesized (`(pKeyword "TRANSFORM" <|> pKeyword "TRANSFORMS")`) so the plural form works.

## Schema charset-or-path allows either order

`CREATE SCHEMA ... [ <default character set specification> ] [ <path specification> ]` — the grammar fixes the order, but `pCreateSchemaStatement` parses `many (pCharset <|> pPath)` (each returning `Choice1Of2`/`Choice2Of2`) so either order is accepted.

- **Trade-off:** Slightly over-permissive (accepts `PATH ... DEFAULT CHARACTER SET ...` in either order) but robust to consumer input variations. The `AUTHORIZATION` clause is handled separately: `name [AUTHORIZATION id] | AUTHORIZATION id` (name optional when `AUTHORIZATION` is present).

## `ALTER TRANSFORM` drop behavior is inside the parens

Per 11.70 `<drop transform element list> ::= DROP ( <transform kind> [ , <transform kind> ] <drop behavior> )`, the drop behavior is *inside* the parentheses: `ALTER TRANSFORM FOR t g (DROP (TO SQL RESTRICT))`. The parser follows the grammar; the initial test wrote `DROP (TO SQL) RESTRICT` (behavior outside) and failed.

- **Trade-off:** The grammar's placement is unusual (behavior inside the parens), so it is easy to write the wrong input. The parser is correct per the spec; tests must put `CASCADE`/`RESTRICT` inside the parens.

## Routines and triggers live in a dedicated `RoutineParser.fs` module

`CREATE PROCEDURE` / `CREATE FUNCTION` (11.60), `ALTER ROUTINE` (11.61), and `CREATE TRIGGER` (11.49) are neither DML, DDL, nor transaction statements, so they were placed in a new `RoutineParser.fs` module (compiled after `DdlParser.fs`, which it needs for `pSpecificRoutineDesignator`). `DROP ROUTINE` / `DROP TRIGGER` were added to the existing `pDropStatement` in `DdlParser.fs`.

- **Trade-off:** One more file and `Compile Include` entry, but keeps module responsibility aligned with the grammar's "SQL-invoked routine" / "trigger definition" sections. `RoutineParser.fs` needs `pSpecificRoutineDesignator` (for `ALTER ROUTINE`), so it must be compiled after `DdlParser.fs`.

## `CREATE PROCEDURE` / `CREATE FUNCTION` share one `CreateRoutine` record

Both are `<SQL-invoked routine>`s differing only in the `<returns clause>` (functions require `RETURNS <data type>`, procedures never have one). `CreateRoutine = { Name; Parameters; Returns: DataType option; Characteristics; Body }` is shared, with `StatementKind.CreateProcedure` / `CreateFunction` wrapping it; `Returns` is `None` for procedures.

- **Trade-off:** One record avoids two near-identical types. `Returns: DataType option` encodes the procedure/function distinction at the AST level, so consumers can rely on `Returns = None` ⇔ procedure.

## Routine body is a forward reference to the full statement parser

`<SQL routine body> ::= <SQL procedure statement>` and `<triggered SQL statement>` can be *any* statement (including nested routine/trigger definitions). `pRoutineBodyStatementRef` is a `createParserForwardedToRef` wired to `pStatement` in `SqlParser.fs` (after `pStatementRef.Value` is set), mirroring `pSchemaElementImpl`/`pDataChangeStatementRef`.

- **Trade-off:** Reusing `pStatement` means a routine body can contain any statement — slightly over-permissive versus the grammar's `<SQL procedure statement>` (which excludes e.g. `SELECT` in some interpretations) but consistent with the codebase's permissive statement slots. `RoutineBody.SqlRoutine`/`TriggeredStatement.SingleStatement` store `StatementKind` (not `Statement`), so the position is dropped inside bodies.

## `BEGIN ATOMIC` bodies use `sepEndBy1` for the statement list

`<SQL routine body> ::= BEGIN ATOMIC { <SQL procedure statement> <semicolon> }... END` — every statement is followed by a semicolon, including the last one before `END`. `sepEndBy1 pStmt (token ";")` accepts the trailing separator, so `BEGIN ATOMIC SELECT 1; SELECT 2; END` parses.

- **Trade-off:** `sepEndBy1` (trailing separator allowed) matches the grammar exactly; `sepBy1` would reject the final `;` before `END`. The `BeginAtomic` case is shared by `RoutineBody` and `TriggeredStatement`, so it must be fully qualified (`RoutineBody.BeginAtomic` / `TriggeredStatement.BeginAtomic`) in both the parser and tests.

## `RoutineCharacteristic` as a flat DU

`<routine characteristic>` is `LANGUAGE | PARAMETER STYLE | SPECIFIC | DETERMINISTIC | SQL-data access | null-call | DYNAMIC RESULT SETS | savepoint level`. `RoutineCharacteristic` is a flat DU (`Language of string`, `ParameterStyle of string`, `SpecificName of Expression`, `Deterministic of bool`, `SqlDataAccess of SqlDataAccess`, `NullCall of bool`, `DynamicResultSets of uint64`, `SavepointLevel of bool`, `ExternalName of Expression`). `pRoutineCharacteristics` collects them and rejects duplicates.

- **Trade-off:** The BNF's `[ <routine characteristic>... ]` permits any order (so no order is enforced), but 11.60's syntax rule allows each characteristic at most once. `pRoutineCharacteristics` groups the parsed characteristics by category and fails on a duplicate (e.g. two null-call clauses, or `LANGUAGE SQL LANGUAGE SQL`). `Deterministic`/`NullCall`/`SavepointLevel` use `bool` (`NOT DETERMINISTIC` → `false`); `DynamicResultSets` uses `uint64` (from `pUnsignedInteger`, which must be followed by `.>> ws` since it does not consume trailing whitespace). `NAME <external routine name>` is included so `ALTER ROUTINE ... NAME f` works.

## Parameter declaration treats a leading identifier as the name

`<SQL parameter declaration> ::= [ <parameter mode> ] [ <SQL parameter name> ] <parameter type> [ RESULT ] [ DEFAULT <parameter default> ]` — both the mode and the name are optional, and `<parameter type>` can be a user-defined type name (an identifier). `pParameterDeclaration` parses `opt mode .>>. opt pIdentifierExpr .>>. pDataType`, so a leading identifier is always the name.

- **Trade-off:** `IN mytype` (a nameless parameter whose type is a UDT) is ambiguous and fails — the identifier is consumed as the name. This is a documented simplification; real-world routines virtually always name their parameters. `<parameter type>` is limited to `<data type>` (no `TABLE`/`DESCRIPTOR` parameters, no `AS LOCATOR`).

## Trigger transitions reuse `Expression` for names

`<transition table or variable>` is `OLD [ROW] [AS] <name> | NEW [ROW] [AS] <name> | OLD TABLE [AS] <name> | NEW TABLE [AS] <name>`. `TransitionTableOrVariable` is `OldRow | NewRow | OldTable | NewTable of Expression`, with the `TABLE` forms tried before the `[ROW]` forms (so `OLD TABLE` isn't consumed as `OLD` + name).

- **Trade-off:** Four cases mirror the grammar. The `AS` keyword is optional and discarded. `TriggerEvent.Update of Expression list option` uses `None` for `UPDATE` without `OF <column list>`.

## `PrivilegeSelectTarget` distinguishes the two `SELECT` privilege forms

12.3 `<action>` includes `SELECT ( <privilege method list> )` alongside `SELECT [ <privilege column list> ]`. The method-list alternative is tried *before* the column-list one (otherwise its `opt pPrivilegeColumnList` succeeds with `None`, leaving the `(` unconsumed and failing on the following `ON`). `PrivilegeAction.Select` now carries a `PrivilegeSelectTarget option` (`PrivilegeColumns of Expression list | PrivilegeMethods of Expression list`) instead of a bare `Expression list option`.

- **Trade-off:** The two forms are now distinguishable in the AST. Ambiguous input `SELECT (col1, col2)` still parses as the method-list form (the first alternative wins), but consumers can at least tell which parser produced it; use `SELECT ( SPECIFIC FUNCTION f )` for an unambiguous method list.

## `pWhereClause` returns `(Expression option * Expression option)`

Positioned `DELETE`/`UPDATE` (`WHERE CURRENT OF <cursor>`) and the searched form (`WHERE <search condition>`) share one parser. `pWhereClause` (defined locally in `DmlParser.fs`) returns a tuple `(cursor, search condition)` — exactly one element is `Some`. Both `pUpdateStatement`/`pDeleteStatement` destructure it into separate `Cursor: Expression option` and `Where: Expression option` fields.

- **Trade-off:** A tuple return keeps one parser for both forms, but the two DML statements must unpack it. The `Cursor`/`Where` split at the AST level keeps each field's meaning explicit and lets `UPDATE`/`DELETE` share `pWhereClause` unchanged. The `CURRENT`/`OF` branch is `attempt`-wrapped and tried *before* the search-condition branch because `CURRENT` could otherwise parse as a column reference.

## `pOverride` hoisted and shared by `INSERT` and `MERGE`

`<override clause> ::= OVERRIDING USER VALUE | OVERRIDING SYSTEM VALUE` appears in both the `<insert statement>` (14.5) and MERGE's `<merge when not matched clause>` (14.12). `pOverride` is defined once at module level and reused, returning `bool option` (`Some true` = USER, `Some false` = SYSTEM, `None` = absent). `InsertStatement.Override` and `MergeInsert`'s second field both hold it.

- **Trade-off:** One definition avoids two near-identical parsers, but the `bool option` encoding requires callers to remember the USER=true/SYSTEM=false convention (matching how the drop-behavior booleans are used elsewhere). A dedicated DU would be self-documenting but adds a type for a two-way choice.

## `FOR PORTION OF` as a dedicated `PortionOfSpec`

14.9/14.14 `<application time period specification> ::= FOR PORTION OF <period name> FROM <point in time 1> TO <point in time 2>` is modeled as `PortionOfSpec = { PeriodName; From; To }` (all `Expression`), stored in `UpdateStatement.PortionOf`/`DeleteStatement.PortionOf` as an option.

- **Trade-off:** A record keeps the three components named. The point-in-time operands use `pDatetimeValueExpression` (6.35) so a `BETWEEN ... AND ...` inside the value isn't consumed as a boolean and only datetime/interval arithmetic is accepted — the same decision as the `<point in time>` for `FOR SYSTEM_TIME`. `pPortionOf` is `attempt`-wrapped at the call sites so a plain `UPDATE t SET ...` isn't broken when `FOR` fails.

## Mutated set clause folds the target into a `FieldReference` chain

14.15 `<mutated set clause> ::= <mutated target> <period> <method name> <equals operator> <update source>` (e.g. `SET a.b = value`, `SET a.b.c = value`). `SetClause.MutatedSet of Expression * Expression * Expression` (target, method name, value). The dotted target `a.b.c` is folded into a nested `FieldReference(FieldReference(a, b), c)` and the last segment is the method name.

- **Trade-off:** Reusing `FieldReference` avoids a new "mutated target" AST case — a dotted path is a field-reference chain. The `List.fold` construction needs explicit `Expression` type annotations because both `Expression` and `Statement` have `Kind`/`Pos` fields (see gotchas). The parser is ordered MultipleSet → MutatedSet → SingleSet so `SET (a,b)=(1,2)`, `SET a.b = 1`, and `SET a = 1` each hit the right branch.

## `MergeInsert` keeps a single VALUES row

`MergeAction.MergeInsert of Expression list option * bool option * Expression list` (insert column list, override, values). The values are a **single** `Expression list` — MERGE's INSERT takes one `VALUES (...)` row, unlike top-level `INSERT` which takes `Expression list list`.

- **Trade-off:** The third field mirrors the grammar (one row). `pDefaultValue` is accepted alongside `pExpression` in the values so `VALUES (DEFAULT)` works, matching the top-level `INSERT` behavior. The override field reuses the hoisted `pOverride`.

## `DROP ROUTINE` reuses the routine designator

`<drop routine statement> ::= DROP <specific routine designator> <drop behavior>`. `pDropStatement` gained `DropRoutine of Expression * bool` via `pRoutineDesignatorWithType .>>. pDropBehavior` (so `DROP FUNCTION f CASCADE`, `DROP PROCEDURE p RESTRICT`, and bare `DROP f CASCADE` all work), and `DropTrigger of Expression` via `DROP TRIGGER <name>` (no drop behavior per 11.50).

- **Trade-off:** `pRoutineDesignatorWithType` (the `<routine type> <name> | <name>` subset of the specific routine designator) is shared by `pObjectName` (GRANT/REVOKE targets), `pDropStatement`, and `pSpecificRoutineDesignator`. `DROP TRIGGER` is a separate case because it has no drop behavior and `TRIGGER` is not a `<routine type>`.

## UDT statements live in a new `TypeParser.fs` module

`CREATE TYPE` (11.40), `ALTER TYPE` (11.59), and the attribute/method/option sub-parsers are neither DML nor DDL in the usual sense, so they were placed in a new `TypeParser.fs` module (compiled after `RoutineParser.fs`, before `TransactionParser.fs`), mirroring the `TransactionParser.fs`/`ControlParser.fs`/`SessionParser.fs` decisions. `DROP TYPE` stays in `DdlParser.fs`'s `pDropStatement` (it is a drop statement).

- **Trade-off:** One more file and `Compile Include` entry, but keeps module responsibility aligned with the grammar's "data type definition" section. `TypeParser.fs` opens `DdlParser`/`RoutineParser` for `pDataType`/`pParameterDeclaration`/`RoutineCharacteristic` reuse.

## `CreateTypeStatement` as a single record with optional fields

`<create type statement>` is `CREATE TYPE <name> [ UNDER <name> ] [ AS <representation> ] [ <type option list> ] [ <method specification list> ]`. `CreateTypeStatement` is `{ Name; Under: Expression option; Representation: TypeRepresentation option; Options: TypeOption list; Methods: MethodSpecification list }`.

- **Trade-off:** A single record keeps the many optional parts in one place and lets consumers inspect any combination. `TypeRepresentation` is `Predefined of DataType | MemberList of AttributeDefinition list` (the `AS <predefined type>` and `AS ( <member list> )` forms). `TypeOption` is a flat DU of the nine options (`Instantiable of bool` / `Final of bool` / `RefUsing of DataType` / `RefFrom of Expression list` / `RefIsSystemGenerated` / `CastToRef` / `CastToType` / `CastToDistinct` / `CastToSource of Expression`).

## Method specifications reuse `ParameterDeclaration` and `RoutineCharacteristic`

`<method specification>` reuses the routine parameter/characteristic machinery: `MethodSpecification` is `{ Kind: MethodKind option; Name: Expression; Parameters: ParameterDeclaration list; Returns: DataType option; Specific: Expression option; SelfAsResult: bool; SelfAsLocator: bool; Characteristics: RoutineCharacteristic list }`. `MethodKind` is `Instance | Static | Constructor` (from `INSTANCE`/`STATIC`/`CONSTRUCTOR`). `pMethodSpecification` is `OVERRIDING <partial method spec> | <original method spec>`.

- **Trade-off:** Reusing `ParameterDeclaration`/`RoutineCharacteristic` avoids near-duplicate types, but the method-characteristic set is restricted to `LANGUAGE`/`DETERMINISTIC`/`NO SQL`/`CONTAINS SQL`/`READS SQL DATA`/`MODIFIES SQL DATA` (the grammar's `<method characteristics>`), so a method cannot carry e.g. `DYNAMIC RESULT SETS`. `SelfAsResult`/`SelfAsLocator` are `bool` flags for `SELF AS RESULT`/`SELF AS LOCATOR`.

## `ALTER TYPE` actions as a DU

`<alter type statement> ::= ALTER TYPE <name> <alter type action>`. `AlterTypeAction` is `AddAttribute of AttributeDefinition | DropAttribute of Expression | AddMethod of MethodSpecification * bool | DropMethod of MethodKind option * Expression * DataType list`. The `bool` on `AddMethod` is the `RESTRICT`/`CASCADE` drop behavior (required by the grammar); `DropMethod` carries the optional method kind, name, and the `( <data type list> )` signature.

- **Trade-off:** A DU keeps the four actions exhaustively matchable. `DROP METHOD`'s signature is `between "(" ")" (sepBy1 pDataType ",")` — the parens are part of the action, matching 11.59.

## `DROP TYPE` as a `DropStatement` case

`<drop type statement> ::= DROP TYPE <name> <drop behavior>`. `DropStatement` gained `DropType of Expression * bool` (`true` = CASCADE, `false` = RESTRICT), placed *after* `DropRoutine` in `pDropStatement` because `TYPE` is not a reserved word and `DROP TYPE` must not be consumed as a routine designator.

- **Trade-off:** `DROP TYPE t` without a behavior is rejected (matching the grammar's required `<drop behavior>`), consistent with `DROP TABLE`/`DROP VIEW`/`DROP SEQUENCE`.

## Method invocation / static method / `NEW` / field reference as expression cases

`ExpressionKind` gained `MethodInvocation of Expression * Expression * Expression list` (receiver, method name, args), `StaticMethodInvocation of Expression * Expression * Expression list` (type name, method name, args), `NewSpecification of Expression * Expression list` (type, args), and `FieldReference of Expression * Expression` (receiver, field name). `pValueExpressionPrimary` parses a base expression then applies `many (attempt pDereferenceReference <|> pMethodOrFieldReference)` (`.name(args)` → `MethodInvocation`, `.name` → `FieldReference`, `-> name[args]` → `Dereference`), folding left.

- **Trade-off:** `FieldReference` keeps the field name as an `Expression` (not a `string`) for consistency with the rest of the AST. `pStaticMethodInvocation` (`type::method(args)`) and `pNewSpecification` (`NEW type(args)`) are tried *before* `pRoutineInvocation` and `pColumnReferenceExpr` in `pValueExpressionPrimary` so `NEW`/`::` are not consumed as identifiers. The generalized invocation and the `->` dereference operator were added later — see the `<dereference operation>` and generalized-method-invocation entries below.

## `pColumnReferenceExpr` rewritten to avoid `sepBy1` double-parse

The original `pColumnReferenceExpr` used `sepBy1 pIdentifier (attempt (token "." >>. pIdentifier .>>? notFollowedBy "("))`. Because `sepBy1 p sep = p .>>. many (sep >>. p)`, the separator already parsed `pIdentifier`, causing a *double* identifier parse that broke `SELECT t.id FROM t`, `JOIN ... ON a.id = b.id`, and DML aliases. It is now `pIdentifier .>>. many (attempt (token "." >>. pIdentifier .>>? notFollowedBy "("))`.

- **Trade-off:** The rewrite is the correct `sepBy1`-equivalent (separator parses only the `.`), and the `notFollowedBy "("` keeps `a.b.c(x)` from being misread as a column reference (it becomes a method invocation). This was a latent bug exposed by the method-invocation work.

## Typed tables / views keep only `OfType`

`<typed table clause> ::= OF <UDT name> [ UNDER <supertable> ]` and `<referenceable view specification> ::= OF <UDT name> [ UNDER <supertable> ]`. `CreateTableStatement.OfType` and `CreateViewStatement.OfType` are `Expression option`; the `UNDER <supertable>` part is parsed and discarded.

- **Trade-off:** The supertable is not surfaced in the AST (no consumer yet). `pCreateTableStatement`'s content-source alternatives now produce a 4-tuple `(elems, asCols, asQuery, ofType)` with the typed-table clause as the fourth alternative; `pCreateViewStatement`'s `pViewSpecification` is `Choice1Of2 (column list) | Choice2Of2 (OF type)`.

## `REF(type)` data type

`<reference type> ::= REF ( <data type> ) [ SCOPE <table name> ]`. `DataType` gained `ReferenceType of DataType * Expression option` (the `SCOPE` name). It is added to `pDataTypeElementRef.Value` *before* `pIdentifierExpr |>> UserDefinedType`, so `REF(...)` is not consumed as a user-defined type name.

## `<dereference operation>` / `<attribute or method reference>` / `<method reference>` (6.20–6.22)

`ExpressionKind` gained `Dereference of Expression * Expression * Expression list option`. `pValueExpressionPrimary` applies `many (attempt pDereferenceReference <|> pMethodOrFieldReference)`, where `pDereferenceReference` parses `<right arrow> <qualified identifier> [ <SQL argument list> ]`. The dereference operator is matched *inside the term parser*, so the operator-precedence parser's prefix/infix `-` never sees the leading `-` of `->`.

- **Trade-off:** One AST case covers all three rules: the `Expression list option` is `None` for 6.21 (attribute access) and `Some args` for 6.22 (method reference). Telling 6.21 from 6.22 needs name resolution, which a parser cannot do.
- **Trade-off:** `FieldReference`/`MethodInvocation` (the `.` forms) are deliberately not reused: `->` dereferences a reference while `.` navigates a value, so keeping the cases apart preserves the distinction the grammar draws.

## Generalized method invocation (6.17)

`ExpressionKind` gained `GeneralizedInvocation of Expression * DataType * Expression * Expression list option` for `( <value expression primary> AS <data type> ) <period> <method name> [ <SQL argument list> ]`. It is tried before the plain parenthesized-expression alternative in `pValueExpressionPrimary`.

- **Trade-off:** The operand is parsed with `pExpression` (an existing forward ref) so the parser can be defined before `pValueExpressionPrimary` without yet another forward reference. That makes the operand slot slightly more permissive than the grammar (`( <arithmetic expression> AS t ).m()` is accepted).
- **Trade-off:** The optional `<SQL argument list>` stays an option, so the bare `(x AS t).m` form is still distinguishable from `(x AS t).m()`.

## `SPECIFICTYPE` (6.32) is an attempt branch of `pMethodOrFieldReference`

6.32 `<specific type method> ::= <user-defined type value expression> <period> SPECIFICTYPE [ ( ) ]` cannot be routed through `pIdentifierExpr`, because `SPECIFICTYPE` is a reserved word (Lexer.fs). `pSpecificTypeMethod` is therefore `attempt`ed first inside `pMethodOrFieldReference`, and the trailing `()` is recorded as a `bool` on `SpecificTypeMethod`.

## Numeric / string function catalogue (6.30, 6.32, 6.38, 6.41, 6.44)

The reserved built-in keywords that used to fall through to the generic `pRoutineInvocation` now have dedicated parsers and AST cases:

| Rule | AST |
|------|-----|
| 6.30 `<length expression>` | `LengthExpression of LengthFunction * Expression * string option` |
| 6.30 `<numeric value function>` | `NumericValueFunction of NumericFunction * Expression list` |
| 6.30 `<regex occurrences function>` / `<regex position expression>` | `RegexOccurrences` / `RegexPosition of RegexStart option * RegexArgument` |
| 6.32 `<regular expression substring function>` | `SubstringSimilar` |
| 6.32 `<fold>` / `<transcoding>` / `<character transliteration>` | `Fold` / `Transcoding` / `CharacterTransliteration` |
| 6.32 `<regex substring function>` / `<regex transliteration>` | `RegexSubstring` / `RegexTransliterate` |
| 6.32 `<normalize function>` / `<classifier function>` | `NormalizeFunction` / `Classifier` |
| 6.41 `<trim array function>` / 6.44 `<multiset set function>` | `TrimArray` / `MultisetSetFunction` |

- **Trade-off:** 6.30 `<absolute value expression>` and 6.38 `<interval absolute value function>` are both spelled `ABS ( <expression> )`, so they share a single `NumericValueFunction(NumericFunction.AbsoluteValue, _)` case with a dual citation. The interval and numeric forms cannot be told apart without type information.
- **Trade-off:** The four regex functions share one `RegexArgument` record and one `pRegexArgument` parser that accepts the *union* of their optional clauses (`WITH`, `FROM`, `USING`, `OCCURRENCE`, `GROUP`) in `TRANSLATE_REGEX` order. Each individual rule permits fewer clauses than the parser accepts; the extra slots are parsed and retained rather than rejected.
- **Trade-off:** `<char length units>` is a `string option` (`pIdentifierRaw`), matching the existing `Substring`/`Position` representation. `OCTET_LENGTH` therefore also accepts a `USING` clause that 6.30 does not allow, because all three length functions share one parser body.
- **Trade-off:** `<normalize function result length>` is parsed as an `Expression`, so `NORMALIZE(x, NFC, CHARACTER_LENGTH(10))` nests a `LengthExpression` there instead of a dedicated length-specification type. A dedicated type would carry no information a consumer could act on.
- **Trade-off:** Zero- and one-argument built-ins are all modelled as `NumericValueFunction` with an `Expression list`, so arity is enforced by the parser rather than by the type.
- **Trade-off:** The 6.32 `<binary value function>` alternatives (binary `SUBSTRING`/`TRIM`/`OVERLAY`) are syntactically identical to the character forms and are served by the existing `Substring`/`Trim`/`Overlay` cases; only the citation was added.

## `functionKeywords` trimmed to the shapes that have no dedicated parser

The 10.4 reserved-function whitelist used to list every built-in name, so `ABS`, `SET`, `TRIM_ARRAY`, `CHAR_LENGTH`, `UPPER`, `NORMALIZE`, the `*_REGEX` family, the trigonometric/exponential set and `GROUPING` all parsed as generic `FunctionCall`s. Now that each has a dedicated parser those entries are removed; what remains is `<aggregate function>`, `<inverse distribution function type>` and `<window function type>`.

- **Trade-off:** This is a deliberate behaviour change. A malformed call such as `ABS(a, b)` or `TRIM_ARRAY(x)` is now rejected instead of degrading to a `FunctionCall`, which is what the grammar requires. Add a name back to the whitelist only when the built-in has no dedicated parser.

## `RUNNING` / `FINAL` and `GROUPING` (6.9)

`ExpressionKind` gained `SetFunction of RunningOrFinal option * Expression` and `Grouping of Expression list`. `pSetFunctionSpecification` parses the prefix and then requires the following `FunctionCall` name to be a member of `aggregateFunctionKeywords` (now the single source of truth shared with the 10.4 whitelist); otherwise it `fail`s, so `FINAL my_routine(x)` is rejected while `FINAL SUM(x)` is accepted.

- **Trade-off:** `SetFunction` wraps the aggregate's `FunctionCall` instead of adding a seventh field to `FunctionCall`. A new field would have changed every existing pattern match on that case for a modifier that only means anything in row-pattern contexts.
- **Trade-off:** `pRunningOrFinal` is defined once and shared with 6.26's `<running or final>`.

## `<row pattern navigation operation>` (6.26) takes over `PREV`/`NEXT`/`FIRST`/`LAST`

`pRowPatternNavigationOperation` is `attempt`ed before `pRoutineInvocation` *and* before `pColumnReferenceExpr`, and produces `ExpressionKind.RowPatternNavigation of RowPatternNavigation` (`Logical` / `Physical` / `Compound`). The three forms are tried Compound → Logical → Physical, so `PREV(FIRST(x), 2)` gets the flat compound shape instead of a logical nested inside a physical.

- **Trade-off:** `PREV`, `NEXT`, `FIRST` and `LAST` are *not* reserved words, so `first(a)` now parses as navigation rather than as a call to a user-defined routine named `first`. A bare `first` is unaffected (navigation requires `(`), so `SELECT first FROM t` still yields `Identifier "FIRST"`. This is accepted as the spec-aligned reading: the four names are non-reserved and no built-in uses them.
- **Trade-off:** The offsets are parsed with `pSimpleValueSpecification` (6.4), matching `<logical offset>`/`<physical offset>`, rather than with a full `<value expression>`.

## `MULTISET UNION` / `INTERSECT` / `EXCEPT` (6.43) as a postfix

`ExpressionKind` gained `MultisetSetOperation of MultisetSetOperator * bool option * Expression * Expression` (`bool option` = `Some true` ALL / `Some false` DISTINCT / `None` unspecified). It is applied as a left-folded postfix in `pBooleanTest` alongside the predicates and `<array element reference>`, and also as the self-contained `pMultisetValueExpression` used by 6.44 `SET ( ... )` — reached through a forward ref, because `SET(...)` is itself a `<value expression primary>`.

- **Trade-off:** Because the layer is a postfix on whatever the operator-precedence parser produced, the *left* operand may be any `<value expression>` rather than strictly a `<multiset term>`. The right operand *is* a `<multiset term>`, which is what makes `MULTISET INTERSECT` bind tighter than `MULTISET UNION`/`MULTISET EXCEPT`; repeated `MULTISET INTERSECT` consequently groups to the right (harmless for an idempotent operation).
- **Trade-off:** `<array concatenation>` (6.40) needs no new case — `||` is already `BinaryOp(Concatenate, _, _)`.

## `<empty specification>` (6.5) needs no new AST case

`<empty specification> ::= ARRAY <left bracket or trigraph> <right bracket or trigraph> | MULTISET <left bracket or trigraph> <right bracket or trigraph>`. It is already produced by the 6.42/6.45 enumeration constructors, because `sepBy` accepts zero elements: `ARRAY[]` is `ArrayConstructor []` and `MULTISET[]` is `MultisetConstructor []`. The rules are now cited on those parsers and covered by tests.

- **Trade-off:** 6.45 keeps a recorded gap: its third alternative, `<table value constructor by query>` (`TABLE ( <query expression> )`), is a §7.3 construct reachable through query expressions, not through an expression primary, so it is not modelled as a multiset constructor.

- **Trade-off:** `REF` is a reserved word, so `pKeyword "REF"` disambiguates cleanly. The `SCOPE` name is an `Expression option` (None = no scope).

## `SELECT INTO` reuses `QueryParser`'s clause parsers chained with `>>=`

`pSelectIntoStatement` chains `pSetQuantifier` / `pSelectSublist` / `pFromClause` / `pWhereClause` / `pGroupByClause` / `pHavingClause` / `pWindowClause` with `>>=` (bind) to build the `SelectIntoStatement` record. Optional clauses are normalized to empty values with `Option.defaultValue`.

- **Trade-off:** `SelectIntoStatement` holds `IsDistinct` / `Columns` / `Into` / `From` / `Where` / `GroupBy` / `GroupByDistinct` / `Having` / `Window`, mirroring the `SELECT` clause structure. Because `pWhereClause` exists with different signatures in both `QueryParser.fs` and `DmlParser.fs`, `CursorParser.fs` opens only `QueryParser`.

## Shorthand keywords are normalized to `None`

`CONNECT TO DEFAULT` / `SET TIME ZONE LOCAL` / `SET NO COLLATION` are normalized to `ConnectStatement.Server = None` / `SetTimeZone None` / `SetSessionCollation(None, _)` respectively.

- **Trade-off:** `ConnectStatement` uses `Server: Expression option` (None = DEFAULT), `SetTimeZone` uses `Expression option` (None = LOCAL), and `SetSessionCollation` uses `Expression option` (None = NO COLLATION), unifying the shorthand forms to `None` to reduce branching on the consumer side.

## `UsingClause` models argument lists and descriptors with a DU

`EXECUTE`'s `INTO` / `USING` clauses (20.13), `OPEN`'s `<input using clause>` (20.19) and `FETCH`'s `<output using clause>` (20.20) can each take either an argument list or an `[ SQL ] DESCRIPTOR <descriptor name>`.

- **Trade-off:** A `UsingClause = UsingArguments of Expression list | UsingDescriptor of Expression` DU models both forms; `Execute` holds `UsingClause option * UsingClause option` (INTO, USING), `Open` holds `UsingClause option` and `Fetch` holds a mandatory `UsingClause` (both 14.5 and 20.20 require the output clause).
- **Trade-off:** `pUsingClause` (20.11) and `pIntoClause` (20.12) live in `CursorParser.fs`, not `DynamicParser.fs`: `CursorParser.fs` is compiled first and `pOpenStatement` / `pFetchStatement` need them, while `DynamicParser.fs` merely opens `SqlParser.CursorParser` again.

## `DescribeStatement` distinguishes its three forms with bool flags

The three forms `DESCRIBE INPUT` / `DESCRIBE OUTPUT CURSOR ... STRUCTURE` / `DESCRIBE <name>` are distinguished by the `IsInput` / `IsCursor` bool flags.

- **Trade-off:** `DescribeStatement` holds `{ IsInput; IsCursor; Name; Descriptor; Nesting }`, where `Nesting` is `bool option` (`WITH/WITHOUT NESTING`). `pDescribeStatement` assembles the branches with `>>=`.

## `GetDiagnosticsStatement` models its three forms with a DU

`GET DIAGNOSTICS` has three forms: statement information / condition information / all information.

- **Trade-off:** Modeled as a `StatementInfo of (Expression * string) list | ConditionInfo of Expression * (Expression * string) list | AllInfo of Expression * AllQualifier option` DU. The information item names (`NUMBER` / `ROW_COUNT` / `MESSAGE_TEXT` etc.) include reserved words but are a closed enumeration, so they are parsed by `choice` lists of `pKeyword`s (`pStatementInfoItemName` / `pConditionInfoItemName`) and kept as `string` — `pIdentifierRaw` would also accept `ALL`, `SELECT`, ….

## `pJsonTableColumn` tries the NESTED branch before the regular-column branch

`JSON_TABLE`'s `<JSON table column>` has several forms: NESTED / ORDINALITY / CHAINING / regular and formatted columns. Because `pDataType` accepts any identifier as a user-defined type (`UserDefinedType`), `NESTED PATH '$.items' ...` would be misread as a regular column (name `NESTED`, type `PATH`) and then fail on the following string literal.

- **Trade-off:** The NESTED branch is placed first in the `choice`, followed by ORDINALITY → CHAINING → regular/formatted-column branches. Given `pDataType`'s permissiveness (any identifier accepted as a UDT), trying the keyword-like column-name forms first resolves the ambiguity.

## `<JSON query wrapper behavior>` is `pJsonQueryWrapper .>> pKeyword "WRAPPER"` in that order

The grammar is `WITHOUT [ ARRAY ] WRAPPER | WITH [ UNCONDITIONAL | CONDITIONAL ] [ ARRAY ] WRAPPER` — the behavior (WITH/WITHOUT etc.) comes first and `WRAPPER` closes the clause. It was initially written in the reverse order (`pKeyword "WRAPPER" >>. pJsonQueryWrapper`), which made `WITH WRAPPER` fail.

- **Trade-off:** The behavior parser comes first and the `WRAPPER` keyword last. `pJsonQueryWrapper` returns `{ WithWrapper; Conditional; Array }` and `WRAPPER` is discarded with `>>.`.

## `pExistingWindowName` rejects `MEASURES` as a window name

In the `<window name or specification>` `( <existing window name> ... )` branch, using `opt pIdentifierExpr` would let the `<row pattern measures>` inside a window frame (`MEASURES y AS m`) be swallowed as an existing window name. `pExistingWindowName` rejects with `notFollowedBy` the case where `MEASURES` is followed by `<expr> AS`.

- **Trade-off:** When the inner parser succeeds, `notFollowedBy` marks the failure as **fatal**, so inside `opt` it cannot be caught and `opt pWindowFrame` is never reached. Wrapping it in `attempt` converts the fatal error back into an ordinary failure so `opt` can return `None`.

## `OUT` is a reserved word — `MATCH_RECOGNIZE (...) AS out` is correctly rejected

7.6/7.7 — `<row pattern output name> ::= <correlation name> ::= <identifier>`, and §5.2 makes `OUT` a reserved word, so `AS out` is **not** valid SQL and `pCorrelationName` (via `pIdentifierExpr`) must fail on it; `MATCH_RECOGNIZE (...) AS out_t` is the valid spelling.

- **Trade-off:** None — this matches the grammar. To use the literal name `out` it must be delimited (`"out"`).

## `VALUE_OF(x)` without `AT` is rejected

`VALUE_OF ( <expr> AT <row marker expr> [ , <expr> ] )` — when `AT` is omitted, `pValueOfFunction` fails and the parser must not fall through to a generic function call. `VALUE_OF` is a reserved word (Lexer.fs) and is deliberately **not** in `pRoutineInvocation`'s reserved-function whitelist, so `VALUE_OF(x)` is now rejected.

- **Trade-off:** `VALUE_OF(x)` is no longer a valid invalid-syntax test input (it now genuinely fails). Tests can still use `VALUE_OF(x AT 5)` for the error path. This is a side effect of restricting `pRoutineInvocation` to reserved *function* keywords.

## JSON_TABLE / JSON_TABLE_PRIMITIVE share `pJsonApiCommon`

Both statements share the `<JSON API common syntax>` (context expression / PATH / AS name / PASSING).

- **Trade-off:** `pJsonApiCommon` is extracted as a shared parser and reused by both `pJsonTableStatement` and `pJsonTablePrimitiveStatement`. They differ only in that `JSON_TABLE` has an optional PLAN clause and optional `ON ERROR`, while `JSON_TABLE_PRIMITIVE` requires `ON ERROR`.

## MATCH_RECOGNIZE shares the row-pattern common parser between QueryParser / ExpressionParser

`MATCH_RECOGNIZE` (QueryParser) and the `<row pattern measures>` / `<row pattern common syntax>` inside window frames (ExpressionParser) share the same row-pattern syntax.

- **Trade-off:** `pRowPatternMeasures` / `pRowPatternCommon` are defined as forward references with `createParserForwardedToRef` and referenced from both modules. `WindowFrame` gained `Measures: RowPatternMeasure list option` and `RowPattern: RowPatternCommon option` fields to hold the window row pattern.

## `<declare cursor>` / `<cursor properties>` / `<cursor specification>` (14.1–14.3)

`pDeclareCursorStatement` parses `DECLARE <cursor name> <cursor properties> FOR <cursor specification>`. The `<cursor specification>` reuses `QueryParser.pQuery`, which already consumes the optional trailing `<updatability clause>`; there is therefore no dedicated `CursorSpecification` AST type and `DeclareCursorStatement.Specification` is a `Query`.

`<cursor properties>` (`CursorProperties`) holds four independent optional attributes: sensitivity (`SENSITIVE | INSENSITIVE | ASENSITIVE`), scrollability (`SCROLL | NO SCROLL`), holdability (`WITH HOLD | WITHOUT HOLD`) and returnability (`WITH RETURN | WITHOUT RETURN`). The `CURSOR` keyword is mandatory between scrollability and holdability, matching the grammar.

- **Trade-off:** The `<updatability clause>` was extended from `FOR UPDATE` to `FOR UPDATE [ OF <column name list> ]`; `LockingClause.ForUpdate` now carries `Expression list option`. The clause is shared by plain `SELECT`, set operations and `DECLARE CURSOR`, so the `OF` list works everywhere the clause is allowed.
- **Trade-off:** The four `<cursor properties>` attributes are separate DUs rather than one flat keyword list, so an absent attribute is `None` and the AST preserves exactly what was written (including the implicit `ASENSITIVE` default).

## `<temporary table declaration>` (14.16)

`pTemporaryTableDeclarationStatement` parses `DECLARE LOCAL TEMPORARY TABLE <name> ( <table element list> ) [ ON COMMIT { PRESERVE | DELETE } ROWS ]`, reusing `DdlParser.pColumnDefinition` / `pTableConstraint` for the elements.

- **Trade-off:** This module-level declaration is distinct from `CREATE LOCAL TEMPORARY TABLE` (11.3, which uses `TableScope`). `TemporaryTableDeclarationStatement` keeps columns and constraints as separate lists (mirroring `CreateTableStatement`) with `OnCommit` as a `TableCommitAction option`.

## `<locator reference>` is limited to host/dynamic parameters (14.17–14.18)

`FREE LOCATOR` / `HOLD LOCATOR` parse a comma-separated `<locator reference>` list. The grammar allows `<host parameter name> | <embedded variable name> | <dynamic parameter specification>`; only `:name` and `?` have standalone syntax, so `<embedded variable name>` is not modelled.

- **Trade-off:** Each reference is an `Expression` of `Parameter` kind, reusing the representation from `6.4 <general value specification>`. A host-language `<embedded variable name>` is treated as `<host parameter name>`.

## `<cursor attributes>` (20.8) is a free-standing rule

`<cursor attributes> ::= <cursor attribute>...` is defined in 20.8 but is not referenced by any production in `sql-2016-grammar.txt` — `<cursor properties>` (14.2, used by `DECLARE CURSOR` and `ALLOCATE`) is the ordered form actually reached by the grammar. `CursorParser.pCursorAttributes` is therefore implemented as a reusable parser (`many1` of the four `<cursor attribute>` alternatives) and is exercised directly rather than through a statement.

- **Trade-off:** 20.8 is not reachable from `SqlParser.parse`; the rule is kept as a library-facing parser so the design document and the implementation agree rule-for-rule. `pCursorProperties` deliberately does **not** reuse it, because `<cursor properties>` fixes the order (`sensitivity`, `scrollability`, `CURSOR`, `holdability`, `returnability`) while `<cursor attributes>` is an unordered repetition.

## Dynamic cursors (20.15 / 20.17 / 20.18) reuse `CursorProperties` and add `ExtendedName`

`pDynamicDeclareCursorStatement`, `pAllocateExtendedDynamicCursorStatement` and `pAllocateReceivedCursorStatement` live in `DynamicParser.fs` and reuse `CursorParser.pCursorProperties` (14.2) so the four cursor attributes are parsed identically for static and dynamic cursors.

- **Trade-off:** `<extended statement name>` / `<extended cursor name>` are both `[ <scope option> ] <simple value specification>`, so one record serves both: `ExtendedName { Scope: ScopeOption option; SimpleValue: Expression }`. A plain `<statement name>` (`<identifier>`) is the same shape with `Scope = None`. `<simple value specification>` (6.4) is a new parser in `ExpressionParser.fs`.
- **Trade-off:** The two `ALLOCATE` forms are distinguished by requiring `<cursor properties>` (20.17) vs. the optional `CURSOR` plus `FOR PROCEDURE` (20.18). `<specific routine designator>` is stored as an `Expression`, matching the existing simplification used by `ALTER ROUTINE` and `GRANT`.

## `<descriptor value constructor>` (20.16) is wired into `<parameter default>` only

The grammar admits `DESCRIPTOR ( <descriptor column list> )` in two slots: `<descriptor argument>` (PTF `<copartition specification>`, not implemented) and `<parameter default>` (11.60). Only the latter is reachable, so `RoutineParser.pDescriptorValueConstructor` is used there and the result is carried as `ExpressionKind.DescriptorValueConstructor`.

- **Trade-off:** The constructor is intentionally **not** an alternative of `<value expression primary>` — the grammar does not allow it as a general value expression, and adding it would make `DESCRIPTOR` (a non-reserved word) bind more eagerly than the existing identifier fallback.

## Nested collection types (6.1)

`<collection type>` is a postfix chain: `<array type>` / `<multiset type>` wrap a `<data type>`, and that inner `<data type>` may itself be a collection. `Types.pCollectionType` parses the collection-free element with `pDataTypeElement` and then folds `many (ARRAY [ [ <maximum cardinality> ] ] | MULTISET)` left-to-right with `List.fold`.

- **Trade-off:** The previous shape (`pDataTypeElement .>>. choice [...]`) accepted a single suffix, so `INT ARRAY ARRAY` and `INT MULTISET ARRAY[3]` failed. Folding keeps the parser free of left recursion — a self-referential `<data type> ARRAY` production would recurse on the same input position forever, which is why `pDataTypeElement` deliberately excludes collection types.
- **Trade-off:** An arbitrary identifier is still accepted as a `<path-resolved user-defined type name>` (the over-permissiveness recorded in §5.3 of the audit); the collection suffix is only recognised after a complete element type.

## `<interval value expression>` from a datetime difference (6.37)

The fourth alternative of 6.37 — `( <datetime value expression> <minus sign> <datetime term> ) <interval qualifier>` — is parsed by `ExpressionParser.pIntervalValueExpression`: it parses `( <value expression> )`, requires an `<interval qualifier>` (10.1, the same parser used by `INTERVAL` literals) and accepts the node only if the parenthesised expression is a subtraction. The AST is `ExpressionKind.DatetimeDifference of Expression * Expression * IntervalQualifier`.

- **Trade-off:** The qualifier is kept in the AST, so `(ts1 - ts2) DAY TO SECOND` is distinguishable from a plain subtraction. The other three alternatives of 6.37 are ordinary interval arithmetic and remain `BinaryOp`/literals.
- **Trade-off:** The parser is an `attempt` alternative placed **before** the generic parenthesised `pExpression` branch of `pValueExpressionPrimary`, so the inner expression is parsed twice whenever no qualifier follows (and the qualifier attempt itself runs after every parenthesised expression). A non-subtraction such as `(a + b) DAY` fails the guard and falls back to the plain parenthesised expression, leaving `DAY` to be rejected by the enclosing rule.
- **Not modelled:** `<interval primary> ::= <value expression primary> [ <interval qualifier> ]` — a qualifier attached to an arbitrary primary, as in the embedded-SQL `? DAY` — has no AST case; only the datetime-difference form of 6.37 is recognised.

## `<table value constructor by query>` (6.45) as a value expression primary

The third alternative of `<multiset value constructor>` is `<table value constructor by query>`, i.e. `TABLE <table subquery>`. `ExpressionParser.pTableValueConstructorByQuery` parses `TABLE ( <query expression> )` and yields `ExpressionKind.TableQuery of Query`, reachable from `<value expression primary>` — so `SELECT TABLE (SELECT ...)` is accepted.

- **Trade-off:** `Query` is reused for the subquery and the node is a distinct case rather than `MultisetQuery`, because 6.45 lists it as a constructor by query independently of the `MULTISET` keyword.
- **Trade-off:** The node is only reachable from an expression. The `<table reference>` position (`FROM ...`, 7.6) is handled by `QueryParser.pTablePrimary`, whose own `TABLE ( <value expression> )` branch for `<collection derived table>` consumes `TABLE` before calling `pExpression`; that path is unaffected and does not consult the new parser. Note that in `sql-2016-grammar.txt` the 7.3 `<table value constructor>` production is `VALUES`-only, so a `TABLE ( <query> )` table reference is not required by the design document.

## `<specific routine designator>` (10.6) is a record

`pSpecificRoutineDesignator` used to yield a bare `Expression`, so `<routine type>`, the `<data type list>` of a `<member name>` and the trailing `[ FOR <schema-resolved user-defined type name> ]` were all discarded. It now produces `SpecificRoutineDesignator { IsSpecific; RoutineType; Name; DataTypeList; ForType }`, and every caller carries the record: `PrivilegeSelectTarget.PrivilegeMethods`, `OrderingCategory.Relative` / `Map`, `TransformElement.ToSql` / `FromSql`, `StatementKind.CreateCast`, `StatementKind.CreateTransliteration`, `AlterRoutineStatement.Routine` and `AllocateReceivedCursorStatement.Routine`. 11.45's `<transliteration source>` also uses the designator now, so `FROM SPECIFIC FUNCTION f` parses.

- **Trade-off:** Retyping seven AST positions is a wide change, but it makes the designator round-trippable. `IsSpecific` distinguishes the `SPECIFIC <routine type> <specific name>` alternative; `DataTypeList = Some []` means the parenthesised `<data type list>` is present but empty and `None` means it is absent.
- **Trade-off:** `RoutineType` is an `option` because a bare `<schema qualified routine name>` is still accepted (`CREATE CAST (INT AS BIGINT) WITH f` gives `None`). Note that `ALTER ROUTINE add` yields `Some RoutineType.Routine, Name = "add"` — `ROUTINE` is consumed as the `<routine type>`. Requiring a `<routine type>` would break `ALTER ROUTINE` / `GRANT ... ON FUNCTION`.
- **Kept separate:** `pRoutineDesignatorWithType` (an `Expression`) is still what `pObjectName` uses for `GRANT ... ON FUNCTION f`, because 12.3's `<object name>` carries a plain name rather than a designator.

## Strict `<default option>` (11.5) for every `<default clause>`

`CREATE TABLE ... DEFAULT`, `ALTER TABLE ... SET DEFAULT`, `CREATE DOMAIN ... DEFAULT` and `ALTER DOMAIN ... SET DEFAULT` now share `pDefaultOption`, a closed parser for `<literal>`, `<datetime value function>`, `USER`, `CURRENT_USER`, `CURRENT_ROLE`, `SESSION_USER`, `SYSTEM_USER`, `CURRENT_CATALOG`, `CURRENT_SCHEMA`, `CURRENT_PATH` and `<implicitly typed value specification>` (`NULL`, `ARRAY[]` / `MULTISET[]`). `pSignedNumericLiteral` is tried separately because `pLiteralExpr` only accepts the unsigned form.

- **Trade-off:** `pGeneralValueSpecification` was deliberately **not** reused — it also accepts `VALUE`, `?` / `:name` and `COLLATION FOR (...)`, none of which are `<default option>`s. The cost is that `DEFAULT (1 + 2)`, `DEFAULT a + b` and `DEFAULT ?` are now rejected (they used to parse as arbitrary expressions), because `<default option>` has no general value expression.

## `<column definition>` (11.4) models its single optional clause explicitly

The `[ <default clause> | <identity column specification> | <generation clause> | <system time period start column specification> | <system time period end column specification> ]` slot is one `opt` over a `Choice`, so at most one alternative is accepted. The four are modelled by `ColumnGeneration = IdentityColumn | GeneratedColumn | SystemTimePeriodColumn`, and `ColumnDefinition` gained `Generation`, `SystemTimePeriod`, `Collation` (10.7) and `Constraints`.

- **Trade-off:** Previously `GENERATED ALWAYS AS IDENTITY` was an `opt` before the constraint list and `DEFAULT` was parsed *as a column constraint*, so `c INT GENERATED ALWAYS AS IDENTITY DEFAULT 5` was accepted. Making the slot a single choice rejects it, matching 11.4 where `<default clause>` and `<identity column specification>` are alternatives.
- **Trade-off:** A bare `NULL` was dropped from `ColumnConstraintKind`: 11.4's `<column constraint>` is only `NOT NULL | <unique specification> | <references specification> | <check constraint definition>`, so `c INT NULL` is rejected. This also removed an F# union-case clash with `Literal.Null` (see `docs/gotchas.md`); the alternative was qualifying every existing `Literal Null` use site.
- **Kept:** `IsNullable`, `IsPrimaryKey`, `IsUnique`, `References` and `Check` stay on `ColumnDefinition` as convenience accessors derived from `Constraints`, so existing consumers keep compiling. They are redundant with `Constraints` by design; `IsNullable` can now only be `Some false` (a `NOT NULL` constraint) or `None`.

## `<column constraint definition>` and `<table constraint definition>` carry name + characteristics (11.4 / 11.6)

`ColumnConstraint { Name; Kind; Characteristics }` and `TableConstraintDefinition { Constraint; Characteristics }` model `[ <constraint name definition> ] <…> [ <constraint characteristics> ]`. `CreateTableStatement.Constraints`, `AlterTableAction.AddConstraint` and `TemporaryTableDeclarationStatement.Constraints` all hold `TableConstraintDefinition`, and `pTableConstraint` returns it directly.

- **Trade-off:** Wrapping `TableConstraint` (rather than adding a third tuple element to each of its cases) keeps `TableConstraint.PrimaryKey` / `Unique` / `ForeignKey` / `Check` intact, so 11.24 `ADD <table constraint definition>` composes unchanged; the cost is that consumers destructure `.Constraint`.
- **Trade-off:** `pConstraintCharacteristics` and `pConstraintEnforcement` were hoisted to the top of `DdlParser.fs` so 11.4 can reuse them, and the old duplicate near `pDomainConstraint` was removed. `ConstraintCharacteristics` is in the same recursive `and` group as `ColumnDefinition` in `Ast.fs`, so no type reordering was needed.

## `CREATE TABLE` contents source (11.3): `UNDER`, `LIKE`, period elements, `SYSTEM VERSIONING`, `ON COMMIT`

`CreateTableStatement` gained `Under`, `Like`, `Periods`, `WithSystemVersioning` and `OnCommit`. `<table element>` is now `Choice<ColumnDefinition, TablePeriodDefinition, TableConstraintDefinition, (Expression * LikeOption list)>`, so columns, periods, constraints and a `<like clause>` share the element list.

- **Trade-off:** `pTimePeriodSpecification` / `pTablePeriodDefinition` moved above `pCreateTableStatement` so `<table period definition>` can be a table element; `pAddSystemTimePeriodColumnList` (11.27) stayed put because it only depends on `pColumnDefinition`.
- **Trade-off:** `WITH SYSTEM VERSIONING` and `ON COMMIT ... ROWS` are `attempt`ed suffixes, because `WITH` also starts `<with or without data>` and `ON` is a join keyword.
- **Trade-off:** The `<like option>` keywords are `INCLUDING` / `EXCLUDING` + `IDENTITY` / `DEFAULTS` / `GENERATED`. Only `IDENTITY` is reserved; `LIKE` is reserved, so `pColumnDefinition` can never swallow a `<like clause>`.
- **Not modelled:** `<typed table element list>` (`OF <UDT> ( <table element>... )`) and `<view element list>` (11.32) remain unsupported.

## `<with or without data>` is mandatory (11.3)

`<as subquery clause> ::= [ ( <column name list> ) ] AS <table subquery> <with or without data>` — the grammar's `<with or without data>` has no brackets, so `pAsSubquery` now requires `WITH DATA` or `WITH NO DATA`, and `CreateTableStatement.WithData` is always `Some` when `AsQuery` is `Some`.

- **Trade-off:** Most dialects allow the clause to be omitted, so this is deliberate grammar-faithful strictness (the same choice as the 11.23/11.26 `<drop behavior>`); `CREATE TABLE t AS SELECT 1` no longer parses.

## `CREATE [ RECURSIVE ] VIEW` and the `<subview clause>` (11.32)

`CreateViewStatement` gained `IsRecursive` and `Under`, and `pViewSpecification`'s referenceable branch is `OF <path-resolved user-defined type name> [ UNDER <table name> ]`.

- **Trade-off:** `RECURSIVE` is a reserved word, so `opt (pKeyword "RECURSIVE")` cannot collide with a view named `recursive`.

## `<parameter type>` and `<returns type>` (11.60)

`ParameterDeclaration.DataType` became `ParameterType = DataTypeParameter of DataType * bool | GenericTableParameter of PassThroughOption option * GenericTableSemantics option | DescriptorParameter`, and `CreateRoutine.Returns` became `ReturnsType option` (`ReturnsData of ReturnsDataType | ReturnsTable of TableFunctionColumn list option | ReturnsOnlyPassThrough`), so `<returns table type>`, `<result cast>` and `<locator indication>` are representable. `pParameterDeclaration` tries `<parameter mode> <name> <parameter type>` first and backtracks, which is what makes `IN mytype` (an anonymous parameter whose type is a UDT) parse.

- **Trade-off:** `<generic table parameter type>` / `<descriptor parameter type>` are tried **before** `<data type>` even though the grammar lists `<data type>` first, because `DESCRIPTOR` is a non-reserved word and `pDataType`'s user-defined-type branch would otherwise consume `d DESCRIPTOR` as a parameter named `d` of UDT type `DESCRIPTOR`. `TABLE` is reserved, so only `DESCRIPTOR` is affected.
- **Trade-off:** `<returns data type> [ <result cast> ]` is one record because `<result cast>` is a suffix of the data type rather than an alternative of `<returns type>`; `CastFrom` is `(DataType * bool) option`, where the bool is the `<result cast from type>`'s `AS LOCATOR`.
- **Not modelled:** `<method specification designator>` (`CREATE METHOD ...`), `<dispatch clause>` (`STATIC DISPATCH`), `<rights clause>` (`SQL SECURITY INVOKER | DEFINER`), the `<external body reference>` extras (`<parameter style clause>`, `<transform group specification>`, `<external security clause>`), `<polymorphic table function body>` and `<descriptor argument>`.
- **Not tightened:** `<parameter default>` still accepts a general `pExpression` alongside `<descriptor value constructor>`; the grammar's `<contextually typed value specification>` alternative would need the same treatment as `<default option>`.

## The `<target table>` of a positioned `DELETE` / `UPDATE` may be omitted (20.25 / 20.27)

`DELETE [ FROM <target table> ] WHERE CURRENT OF <preparable dynamic cursor name>` and `UPDATE [ <target table> ] SET <set clause list> WHERE CURRENT OF <preparable dynamic cursor name>` allow the target table to be dropped. `DmlTarget = TableTarget of Expression * bool | OmittedTarget` (`bool` = `ONLY ( <table name> )`) makes the two shapes distinguishable, and `DeleteStatement.Target` / `UpdateStatement.Target` hold it instead of the old `Table` + `TableIsOnly` pair.

- **Trade-off:** `MergeStatement.Target` / `MergeStatement.TargetIsOnly` keep the mandatory `Expression * bool` pair — 14.12 has no omitted form, so a `DmlTarget` there would only add an unconstructible `OmittedTarget` case.
- **Trade-off:** `OmittedTarget` is accepted only for a positioned statement with no `<portion of>`, correlation name or search condition (`pOmittedTargetGuard`).
- **Trade-off:** `DELETE` builds the target as `opt (attempt (pKeyword "FROM" >>. pTargetTable))` and maps `None` to `OmittedTarget`, so a `FROM` with no table name is still rejected rather than silently becoming the omitted form.

## `<direct SQL statement>` requires a `<semicolon>` and a directly executable statement (22.1)

`SqlParser.parse` runs `ws >>. pDirectSqlStatement .>> eof`, where `pDirectSqlStatement` accepts only the grammar's `<directly executable statement>` families — `<direct SQL data statement>`, `<SQL schema statement>`, `<SQL transaction statement>`, `<SQL connection statement>`, `<SQL session statement>` — followed by `<semicolon>`. `SqlParser.parseStatement` is the general entry point (`ws >>. pStatement .>> pSemicolon .>> eof`).

- **Trade-off:** Requiring the `<semicolon>` is a breaking change for callers: `SqlParser.parse "SELECT 1"` no longer parses, and every test file's `parse` / `parseFails` / `parseExpr` helper appends it. Requiring it also makes `SELECT 1;;` and a bare `;` fail.
- **Trade-off:** `parse` rejects `DECLARE CURSOR` (14.1), `OPEN`/`FETCH`/`CLOSE` (14.4–14.6), `SELECT ... INTO` (14.7), `FREE`/`HOLD LOCATOR` (14.17/14.18), positioned `DELETE`/`UPDATE` (14.8/14.13, 20.25/20.27), `CALL`/`RETURN` (16.1/16.2), `GET DIAGNOSTICS` (23.1) and every dynamic-SQL statement (20.x) — those are `<SQL procedure statement>`s (13.4), reachable through `parseStatement`. A `<temporary table declaration>` (14.16) **is** part of `<direct SQL data statement>`, so it stays in `parse`.
- **Trade-off:** `pWithStatement` requires the `<with list>` to be followed by a query (`<query expression>`, 7.17), so `WITH ... INSERT/UPDATE/DELETE/MERGE` is rejected by **both** `parse` and `parseStatement` — `<with clause>` is a prefix of `<query expression>`, not a general statement prefix. The earlier `pDml`-based form that accepted those was removed, along with the now-redundant `pDirectWithStatement` duplicate.
- **Not implemented:** 22.1's `<direct implementation-defined statement>` has no parser, so it is omitted from the choice.

## `[ SQL ]` is optional in `<using descriptor>` / `<into descriptor>`

20.10/20.11/20.12 all spell the descriptor form as `[ SQL ] DESCRIPTOR <descriptor name>`, but `pUsingClause` / `pIntoClause` used to require the `SQL` keyword.

- **Trade-off:** The shared `pDescriptorName` helper is `opt (pKeyword "SQL" >>% ()) .>> pKeyword "DESCRIPTOR" >>. pQualifiedNameExpr`, so `USING SQL DESCRIPTOR d` and `USING DESCRIPTOR d` both parse. The descriptor branch is `attempt`ed as a whole, which keeps a plain argument named `DESCRIPTOR` usable (`INTO descriptor` → `UsingArguments [ DESCRIPTOR ]`).
- **Not modelled:** `<into argument>` / `<using argument>` are still parsed as `pQualifiedNameExpr` / `pExpression`, so the `<target specification>` host-parameter forms are not modelled (unchanged from before).

## SQL terminal characters are modelled as they are used (5.1)

`<SQL terminal character>` (5.1) defines more terminals than any production of the grammar uses, so `Lexer.fs` models the terminals that are actually referenced — each as a named parser carrying its own `// 5.1 <...>` citation. The characters that already had a home keep theirs (`pQuote`, `pLeftBracket` / `pRightBracket`, `pQuestionMark`, `pSemicolon`); the ones that had none are collected in one 5.1 block: `<left brace>` / `<right brace>` / `<circumflex>` / `<vertical bar>` / `<dollar sign>`, together with the 5.2 compound tokens `<left brace minus>` / `<right minus brace>`. The 7.9 `<row pattern>` parser (quantifiers, anchors, alternation, exclusion) is the only consumer.

- **Trade-off:** `<percent>` and `<reverse solidus>` are deliberately **not** modelled. `<percent>` never occurs in a SQL production: it appears only inside embedded languages — 8.6's `<regular expression>`, which the grammar defines but no production consumes (the patterns are strings: `<similar pattern> ::= <character value expression>` for `SIMILAR TO`, `<XQuery pattern>` for `LIKE_REGEX`), and the SQL/JSON path language of 9.38/9.39. `<reverse solidus>` is referenced by no production at all. Both are carried as opaque string content, exactly like the JSON path. Adding parsers for them would be unreachable code and would not change the accepted language, so 5.1 counts as Implemented.
- Alternative (modelling every terminal in the 5.1 text, including the unreferenced ones) was rejected: dead code, and it would suggest the parser understands `%`-arithmetic or regex syntax that it deliberately treats as text.
