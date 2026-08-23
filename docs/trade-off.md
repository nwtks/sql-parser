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

`CURRENT_DATE`/`CURRENT_TIME`/`CURRENT_TIMESTAMP`/`LOCALTIME`/`LOCALTIMESTAMP` are reserved words, so they cannot be parsed as identifiers or generic function calls. They get dedicated `ExpressionKind` cases (`CurrentDate`, `CurrentTime of int option`, ...) with optional precision. Likewise `SUBSTRING(x FROM a FOR b)` and `OVERLAY(x PLACING y FROM n)` get dedicated cases, while the comma form (`SUBSTRING(x, a, b)`) still parses as a generic `FunctionCall`.

- **Trade-off:** More AST cases vs. forcing these into `FunctionCall` (which would lose the FROM/FOR structure and require reserved-word identifiers). The dedicated parsers are tried before `pRoutineInvocation` and `attempt`-backtrack to it for the comma form.

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

`CREATE SEQUENCE` / `ALTER SEQUENCE` (11.54/11.73) and the `<identity column specification>` (11.2) share the same `<common sequence generator options>`. Both are parsed by a single `pSequenceOption` producing a `SequenceOption` DU (`DataTypeOption` / `StartWith` / `IncrementBy` / `MaxValue of decimal option` / `MinValue of decimal option` / `Cycle of bool` / `Restart of decimal option`), placed in `DdlParser.fs` *before* `pColumnDefinition` so both parsers can reference it.

- **Trade-off:** One options DU keeps the two features consistent and avoids a second near-identical type. `MaxValue`/`MinValue` use `decimal option` where `None` means `NO MAXVALUE`/`NO MINVALUE`; `Restart` is only valid in `ALTER SEQUENCE` but is not restricted at parse time. `CreateTableStatement.TableScope` (`TableScope option`: `GLOBAL TEMPORARY`/`LOCAL TEMPORARY`) and `ColumnDefinition.Identity` (`IdentitySpec option` = `{ IsAlways; Options }`) are plain option fields, so the absent forms (`CREATE TABLE ...`, `id INT`) are `None`.

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

## `ONLY` / data-change-delta drop the optional correlation name

`ONLY (t)` and `FINAL|NEW|OLD TABLE (dml)` both allow an optional `[ AS <correlation name> ]` / `[ <correlation or recognition> ]` suffix in the grammar, but `TableSourceKind.Only of Expression` and `TableSourceKind.DataChangeDelta of ResultOption * StatementKind` discard it.

- **Trade-off:** The alias has no consumer yet, so keeping the cases minimal wins. `TableFunction`/`PtfTable` (likewise) *do* keep the alias because the grammar's `<table function derived table>` commonly uses it.

## `TABLE (expr)` classified as `TableFunction` vs `PtfTable` by a `FunctionCall` check

`<table function derived table>` and `<PTF derived table>` are syntactically identical (`TABLE ( <value expression> )`), so `pTablePrimary` parses the expression once and classifies it: a `FunctionCall` → `PtfTable`, anything else → `TableFunction`.

- **Trade-off:** A shape-based heuristic. A PTF that is not a plain routine invocation would be misclassified, but in practice PTFs are function calls. A dedicated parser cannot distinguish the two without semantic knowledge.

## `ASYMMETRIC` / `SYMMETRIC` parsed and discarded

`FOR SYSTEM_TIME BETWEEN [ ASYMMETRIC | SYMMETRIC ] p1 AND p2` accepts the optional qualifier but discards it — `SystemTimeSpec.Between` does not carry it.

- **Trade-off:** The qualifier is semantic metadata (which side is inclusive) with no consumer; surfacing it would add a field to `Between` for no current benefit.

## Point-in-time parsed with `pValueExpressionNoBoolean`

`<point in time>` is a `<datetime value expression>`, which must not include boolean `AND`/`OR`. The full `pExpression` would consume `BETWEEN ... AND ...`'s `AND` as a boolean operator. `pValueExpressionNoBoolean = opp.ExpressionParser` (the `OperatorPrecedenceParser` without boolean operators) is used instead.

- **Trade-off:** An approximation — it accepts any non-boolean value expression, not just datetime ones. A dedicated `<datetime value expression>` parser would be stricter but is overkill for the point-in-time slot.

## DML target `ONLY (t)` not implemented

The grammar's `<target table>` allows `ONLY ( <table or query name> )`, but DML statements (`UPDATE`/`DELETE`/`MERGE`) still use `Table: Expression` (a plain qualified name).

- **Trade-off:** `ONLY` in DML targets is deferred; the `ONLY` support added for the FROM-clause covers the table primary only. Implementing the DML-target form would require threading an `Only` flag through the DML target parsers.

## `AllFieldsReference` only for identifier chains

`<all fields reference> ::= <value expression primary> <period> <asterisk> [ AS ( <all fields column name list> ) ]` technically allows any value expression primary (e.g. `(a + b).*`), but `pQualifiedAsterisk` only handles identifier chains (`t.*`, `s.t.*`).

- **Trade-off:** `(a + b).*` is rejected; the common qualified-asterisk form is supported. `pQualifiedAsterisk` yields `QualifiedStar` when the `AS (cols)` suffix is absent and `AllFieldsReference` when present, so the two shapes share one parser.

## `pPartitionBy` uses `pExpression`

`<partitioned join column reference list>` is a list of column references, but `pPartitionBy` parses `sepBy1 pExpression (token ",")`.

- **Trade-off:** Accepts any expression where the grammar wants column references — permissive but consistent with other column-list parsers in the codebase, and simpler than a dedicated column-reference-only parser.

## `pJoinSpecification` returns `(JoinCondition * Expression option)`

`<named columns join> ::= USING ( <join column list> ) [ AS <join correlation name> ]` — the `USING` alias is folded into `JoinSource.UsingAlias: Expression option` via the tuple returned by `pJoinSpecification`.

- **Trade-off:** A tuple return keeps the `ON`/`USING` choice and the optional alias in one parser; the caller (`pJoinedTableSuffix`) destructures it into the `JoinSource` fields.

## Reserved-word forms must precede `pRoutineInvocation`

`pRoutineInvocation` uses `pIdentifierRaw`, which accepts reserved words. So `EXISTS (SELECT ...)`, `UNIQUE (...)`, `JSON_EXISTS(...)`, and `PERIOD (s, e)` would all be greedily parsed as generic function calls (`FunctionCall(EXISTS, [], ...)`) if their dedicated parsers came after it. `pValueExpressionPrimary` therefore lists `pExistsPredicate`, `pUniquePredicate`, `pJsonExistsPredicate`, and `pPeriodValue` BEFORE `pRoutineInvocation`.

- **Trade-off:** This was a latent bug for `EXISTS`/`UNIQUE` (no prior test exercised them as standalone expressions); the `PERIOD`/`JSON_EXISTS` tests exposed it. Ordering the choice is the fix — no AST change needed.

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

## JSON clauses use permissive `pExpression`

`<JSON API common syntax>` context item, `<JSON name and value>` name/value, `<JSON passing argument>`, and the `DEFAULT <value expression>` behavior all use the full `pExpression` rather than grammar-specific value-expression parsers.

- **Trade-off:** Consistent with the codebase's permissive expression slots (e.g. `pPartitionBy`). The JSON path specification is parsed as a string literal wrapped in `Literal(String ...)`.

## `JSON_QUERY` wrapper/quotes/behavior clauses

`<JSON query wrapper behavior>` (`WITHOUT [ARRAY] | WITH [CONDITIONAL|UNCONDITIONAL] [ARRAY]`) is a record `{ WithWrapper; Conditional; Array }`; `<JSON query quotes behavior>` is `Keep | Omit`; `<JSON query empty/error behavior>` is `JsonQueryError | JsonQueryNull | JsonQueryEmptyArray | JsonQueryEmptyObject`. `JSON_VALUE`'s empty/error behavior is `JsonError | JsonNull | JsonDefault of Expression`.

- **Trade-off:** Two distinct behavior DUs (`JsonValueBehavior` vs `JsonQueryBehavior`) because the grammar defines different alternatives for each. `JsonOutput = { Returning: DataType; Format: JsonRepresentation option }` and `JsonRepresentation = JsonEncoding of JsonEncoding option` (None = no `ENCODING` clause).

## Parser names mirror grammar non-terminals

As a readability refactor, parser functions were renamed to match the SQL-2016 non-terminals they implement, and each module gained a banner listing the grammar sections it covers. Examples: `pTableSource`→`pTableReference`, `pColumnExpr`→`pDerivedColumn`, `pSelectCore`→`pQuerySpecification`, `pTerm`→`pValueExpressionPrimary`, `pPredicateSuffix`→`pPredicate`, `pCastExpr`→`pCastSpecification`, `pSubstringExpr`→`pCharacterSubstringFunction`, `pFunctionCallExpr`→`pRoutineInvocation`, `pParameterExpr`→`pGeneralValueSpecification`.

- **Trade-off:** Renames touch every reference (cross-file references like `DmlParser`'s use of `pTableReference` must be updated in lockstep), but the payoff is that a reader can map each parser to the grammar rule it implements. FParsec's define-before-use constraint means files are still ordered leaf-first rather than grammar-top-down; the banners and names bridge that gap.

## Schema elements reuse `pDdl` via a forward reference

`<schema element>` (inside `CREATE SCHEMA`) is "any DDL statement". `pSchemaElementImpl` is a `createParserForwardedToRef` whose `.Value` is wired to `pDdl` in `SqlParser.fs` (after `pDdl` is defined, mirroring `pDataChangeStatementRef`).

- **Trade-off:** Reusing `pDdl` means `DROP`/`ALTER` statements are also accepted as schema elements, which is slightly over-permissive versus the grammar's `<schema element>` list (which is mostly `CREATE` statements). The alternative — a dedicated schema-element parser — would duplicate the whole DDL choice. `CREATE SCHEMA s CREATE TABLE t (id INT)` yields `Elements = [CreateTable _]`.

## `ConstraintCharacteristics` as a permissive record

`<constraint characteristics>` is `[ <constraint check time> ] [ [ NOT ] DEFERRABLE ] [ INITIALLY <check time> ] [ NOT ENFORCED ]`. `ConstraintCharacteristics = { InitiallyDeferred: bool option; Deferrable: bool option; Enforced: bool option }` with `pConstraintCharacteristics` returning a `(bool option * bool option * bool option)` tuple folded from three `attempt`-wrapped alternatives.

- **Trade-off:** Three independent option fields accept any combination (including ones the grammar's ordering forbids), but keep the AST flat and easy to consume. `INITIALLY DEFERRED NOT DEFERRABLE` and `NOT ENFORCED` both parse.

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

`<routine characteristic>` is `LANGUAGE | PARAMETER STYLE | SPECIFIC | DETERMINISTIC | SQL-data access | null-call | DYNAMIC RESULT SETS | savepoint level`. `RoutineCharacteristic` is a flat DU (`Language of string`, `ParameterStyle of string`, `SpecificName of Expression`, `Deterministic of bool`, `SqlDataAccess of SqlDataAccess`, `NullCall of bool`, `DynamicResultSets of uint64`, `SavepointLevel of bool`, `ExternalName of Expression`). `many pRoutineCharacteristic` collects them in order.

- **Trade-off:** A flat list loses the grammar's ordering constraints (e.g. `LANGUAGE` before `DETERMINISTIC`) but keeps the AST simple and lets consumers filter by characteristic. `Deterministic`/`NullCall`/`SavepointLevel` use `bool` (`NOT DETERMINISTIC` → `false`); `DynamicResultSets` uses `uint64` (from `pUnsignedInteger`, which must be followed by `.>> ws` since it does not consume trailing whitespace). `NAME <external routine name>` is included so `ALTER ROUTINE ... NAME f` works.

## Parameter declaration treats a leading identifier as the name

`<SQL parameter declaration> ::= [ <parameter mode> ] [ <SQL parameter name> ] <parameter type> [ RESULT ] [ DEFAULT <parameter default> ]` — both the mode and the name are optional, and `<parameter type>` can be a user-defined type name (an identifier). `pParameterDeclaration` parses `opt mode .>>. opt pIdentifierExpr .>>. pDataType`, so a leading identifier is always the name.

- **Trade-off:** `IN mytype` (a nameless parameter whose type is a UDT) is ambiguous and fails — the identifier is consumed as the name. This is a documented simplification; real-world routines virtually always name their parameters. `<parameter type>` is limited to `<data type>` (no `TABLE`/`DESCRIPTOR` parameters, no `AS LOCATOR`).

## Trigger transitions reuse `Expression` for names

`<transition table or variable>` is `OLD [ROW] [AS] <name> | NEW [ROW] [AS] <name> | OLD TABLE [AS] <name> | NEW TABLE [AS] <name>`. `TransitionTableOrVariable` is `OldRow | NewRow | OldTable | NewTable of Expression`, with the `TABLE` forms tried before the `[ROW]` forms (so `OLD TABLE` isn't consumed as `OLD` + name).

- **Trade-off:** Four cases mirror the grammar. The `AS` keyword is optional and discarded. `TriggerEvent.Update of Expression list option` uses `None` for `UPDATE` without `OF <column list>`.

## `SELECT ( <privilege method list> )` must precede `SELECT [ <privilege column list> ]`

12.3 `<action>` includes `SELECT ( <privilege method list> )` alongside `SELECT [ <privilege column list> ]`. If the plain `SELECT [ column list ]` alternative came first, its `opt pPrivilegeColumnList` would succeed with `None` (leaving the `(` unconsumed) and the grant would fail on the following `ON`. The method-list alternative is therefore tried *before* the column-list one.

- **Trade-off:** For `SELECT (col1, col2)` the method-list parser wins and yields `Select (Some [col1; col2])` — the same AST shape as the column-list form, so the distinction is lost. Both forms produce `PrivilegeAction.Select (Expression list option)`; the method-list form is only distinguishable when it contains a routine designator (`SELECT (SPECIFIC FUNCTION f)`).

## `pWhereClause` returns `(Expression option * Expression option)`

Positioned `DELETE`/`UPDATE` (`WHERE CURRENT OF <cursor>`) and the searched form (`WHERE <search condition>`) share one parser. `pWhereClause` (defined locally in `DmlParser.fs`) returns a tuple `(cursor, search condition)` — exactly one element is `Some`. Both `pUpdateStatement`/`pDeleteStatement` destructure it into separate `Cursor: Expression option` and `Where: Expression option` fields.

- **Trade-off:** A tuple return keeps one parser for both forms, but the two DML statements must unpack it. The `Cursor`/`Where` split at the AST level keeps each field's meaning explicit and lets `UPDATE`/`DELETE` share `pWhereClause` unchanged. The `CURRENT`/`OF` branch is `attempt`-wrapped and tried *before* the search-condition branch because `CURRENT` could otherwise parse as a column reference.

## `pOverride` hoisted and shared by `INSERT` and `MERGE`

`<override clause> ::= OVERRIDING USER VALUE | OVERRIDING SYSTEM VALUE` appears in both the `<insert statement>` (14.5) and MERGE's `<merge when not matched clause>` (14.12). `pOverride` is defined once at module level and reused, returning `bool option` (`Some true` = USER, `Some false` = SYSTEM, `None` = absent). `InsertStatement.Override` and `MergeInsert`'s second field both hold it.

- **Trade-off:** One definition avoids two near-identical parsers, but the `bool option` encoding requires callers to remember the USER=true/SYSTEM=false convention (matching how the drop-behavior booleans are used elsewhere). A dedicated DU would be self-documenting but adds a type for a two-way choice.

## `FOR PORTION OF` as a dedicated `PortionOfSpec`

14.9/14.14 `<application time period specification> ::= FOR PORTION OF <period name> FROM <point in time 1> TO <point in time 2>` is modeled as `PortionOfSpec = { PeriodName; From; To }` (all `Expression`), stored in `UpdateStatement.PortionOf`/`DeleteStatement.PortionOf` as an option.

- **Trade-off:** A record keeps the three components named. The point-in-time operands use `pValueExpressionNoBoolean` (not `pExpression`) so a `BETWEEN ... AND ...` inside the value isn't consumed as a boolean — the same decision as the `<point in time>` for `FOR SYSTEM_TIME`. `pPortionOf` is `attempt`-wrapped at the call sites so a plain `UPDATE t SET ...` isn't broken when `FOR` fails.

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

`ExpressionKind` gained `MethodInvocation of Expression * Expression * Expression list` (receiver, method name, args), `StaticMethodInvocation of Expression * Expression * Expression list` (type name, method name, args), `NewSpecification of Expression * Expression list` (type, args), and `FieldReference of Expression * Expression` (receiver, field name). `pValueExpressionPrimary` parses a base expression then applies `many pMethodOrFieldReference` (`.name(args)` → `MethodInvocation`, `.name` → `FieldReference`), folding left.

- **Trade-off:** `FieldReference` keeps the field name as an `Expression` (not a `string`) for consistency with the rest of the AST. `pStaticMethodInvocation` (`type::method(args)`) and `pNewSpecification` (`NEW type(args)`) are tried *before* `pRoutineInvocation` and `pColumnReferenceExpr` in `pValueExpressionPrimary` so `NEW`/`::` are not consumed as identifiers. The generalized invocation `(expr AS type).method()` and the `->` dereference operator are **not** implemented.

## `pColumnReferenceExpr` rewritten to avoid `sepBy1` double-parse

The original `pColumnReferenceExpr` used `sepBy1 pIdentifier (attempt (token "." >>. pIdentifier .>>? notFollowedBy "("))`. Because `sepBy1 p sep = p .>>. many (sep >>. p)`, the separator already parsed `pIdentifier`, causing a *double* identifier parse that broke `SELECT t.id FROM t`, `JOIN ... ON a.id = b.id`, and DML aliases. It is now `pIdentifier .>>. many (attempt (token "." >>. pIdentifier .>>? notFollowedBy "("))`.

- **Trade-off:** The rewrite is the correct `sepBy1`-equivalent (separator parses only the `.`), and the `notFollowedBy "("` keeps `a.b.c(x)` from being misread as a column reference (it becomes a method invocation). This was a latent bug exposed by the method-invocation work.

## Typed tables / views keep only `OfType`

`<typed table clause> ::= OF <UDT name> [ UNDER <supertable> ]` and `<referenceable view specification> ::= OF <UDT name> [ UNDER <supertable> ]`. `CreateTableStatement.OfType` and `CreateViewStatement.OfType` are `Expression option`; the `UNDER <supertable>` part is parsed and discarded.

- **Trade-off:** The supertable is not surfaced in the AST (no consumer yet). `pCreateTableStatement`'s content-source alternatives now produce a 4-tuple `(elems, asCols, asQuery, ofType)` with the typed-table clause as the fourth alternative; `pCreateViewStatement`'s `pViewSpecification` is `Choice1Of2 (column list) | Choice2Of2 (OF type)`.

## `REF(type)` data type

`<reference type> ::= REF ( <data type> ) [ SCOPE <table name> ]`. `DataType` gained `ReferenceType of DataType * Expression option` (the `SCOPE` name). It is added to `pDataTypeElementRef.Value` *before* `pIdentifierExpr |>> UserDefinedType`, so `REF(...)` is not consumed as a user-defined type name.

- **Trade-off:** `REF` is a reserved word, so `pKeyword "REF"` disambiguates cleanly. The `SCOPE` name is an `Expression option` (None = no scope).

## `SELECT INTO` reuses `QueryParser`'s clause parsers chained with `>>=`

`pSelectIntoStatement` chains `pSetQuantifier` / `pSelectSublist` / `pFromClause` / `pWhereClause` / `pGroupByClause` / `pHavingClause` / `pWindowClause` with `>>=` (bind) to build the `SelectIntoStatement` record. Optional clauses are normalized to empty values with `Option.defaultValue`.

- **Trade-off:** `SelectIntoStatement` holds `IsDistinct` / `Columns` / `Into` / `From` / `Where` / `GroupBy` / `GroupByDistinct` / `Having` / `Window`, mirroring the `SELECT` clause structure. Because `pWhereClause` exists with different signatures in both `QueryParser.fs` and `DmlParser.fs`, `CursorParser.fs` opens only `QueryParser`.

## Shorthand keywords are normalized to `None`

`CONNECT TO DEFAULT` / `SET TIME ZONE LOCAL` / `SET NO COLLATION` are normalized to `ConnectStatement.Server = None` / `SetTimeZone None` / `SetSessionCollation(None, _)` respectively.

- **Trade-off:** `ConnectStatement` uses `Server: Expression option` (None = DEFAULT), `SetTimeZone` uses `Expression option` (None = LOCAL), and `SetSessionCollation` uses `Expression option` (None = NO COLLATION), unifying the shorthand forms to `None` to reduce branching on the consumer side.

## `ExecuteUsing` models argument lists and descriptors with a DU

`EXECUTE`'s `INTO` / `USING` clauses can take either a `<result target list>` (argument list) or an `<SQL descriptor>`.

- **Trade-off:** An `ExecuteUsing = UsingArguments of Expression list | UsingDescriptor of Expression` DU models both forms, and `Execute` holds `ExecuteUsing option * ExecuteUsing option` (INTO, USING).

## `DescribeStatement` distinguishes its three forms with bool flags

The three forms `DESCRIBE INPUT` / `DESCRIBE OUTPUT CURSOR ... STRUCTURE` / `DESCRIBE <name>` are distinguished by the `IsInput` / `IsCursor` bool flags.

- **Trade-off:** `DescribeStatement` holds `{ IsInput; IsCursor; Name; Descriptor; Nesting }`, where `Nesting` is `bool option` (`WITH/WITHOUT NESTING`). `pDescribeStatement` assembles the branches with `>>=`.

## `GetDiagnosticsStatement` models its three forms with a DU

`GET DIAGNOSTICS` has three forms: statement information / condition information / all information.

- **Trade-off:** Modeled as a `StatementInfo of (Expression * string) list | ConditionInfo of Expression * (Expression * string) list | AllInfo of Expression * AllQualifier option` DU. The information item names (`NUMBER` / `ROW_COUNT` / `MESSAGE_TEXT` etc.) include reserved words, so they are parsed with `pIdentifierRaw` (kept as `string`).

## `pJsonTableColumn` tries the NESTED branch before the regular-column branch

`JSON_TABLE`'s `<JSON table column>` has several forms: NESTED / ORDINALITY / CHAINING / regular and formatted columns. Because `pDataType` accepts any identifier as a user-defined type (`UserDefinedType`), `NESTED PATH '$.items' ...` would be misread as a regular column (name `NESTED`, type `PATH`) and then fail on the following string literal.

- **Trade-off:** The NESTED branch is placed first in the `choice`, followed by ORDINALITY → CHAINING → regular/formatted-column branches. Given `pDataType`'s permissiveness (any identifier accepted as a UDT), trying the keyword-like column-name forms first resolves the ambiguity.

## `<JSON query wrapper behavior>` is `pJsonQueryWrapper .>> pKeyword "WRAPPER"` in that order

The grammar is `WITHOUT [ ARRAY ] WRAPPER | WITH [ UNCONDITIONAL | CONDITIONAL ] [ ARRAY ] WRAPPER` — the behavior (WITH/WITHOUT etc.) comes first and `WRAPPER` closes the clause. It was initially written in the reverse order (`pKeyword "WRAPPER" >>. pJsonQueryWrapper`), which made `WITH WRAPPER` fail.

- **Trade-off:** The behavior parser comes first and the `WRAPPER` keyword last. `pJsonQueryWrapper` returns `{ WithWrapper; Conditional; Array }` and `WRAPPER` is discarded with `>>.`.

## `pExistingWindowName` rejects `MEASURES` as a window name

In the `<window name or specification>` `( <existing window name> ... )` branch, using `opt pIdentifierExpr` would let the `<row pattern measures>` inside a window frame (`MEASURES y AS m`) be swallowed as an existing window name. `pExistingWindowName` rejects with `notFollowedBy` the case where `MEASURES` is followed by `<expr> AS`.

- **Trade-off:** When the inner parser succeeds, `notFollowedBy` marks the failure as **fatal**, so inside `opt` it cannot be caught and `opt pWindowFrame` is never reached. Wrapping it in `attempt` converts the fatal error back into an ordinary failure so `opt` can return `None`.

## `OUT` is a reserved word — cannot be used as a MATCH_RECOGNIZE output name

The output correlation name of `MATCH_RECOGNIZE (...) AS out` is parsed with `pIdentifierExpr`, so the reserved word `OUT` is rejected (fails at Col 72).

- **Trade-off:** Tests use non-reserved output names (e.g. `out_t`). `OUT` is in `reservedWords` (Lexer.fs line 237).

## `VALUE_OF(x)` without `AT` is parsed as a regular function call

`VALUE_OF ( <expr> AT <row marker expr> [ , <expr> ] )` — when `AT` is omitted, `VALUE_OF(x)` is accepted by `pRoutineInvocation` as a regular function call, so it does not fail as an invalid-syntax test.

- **Trade-off:** Invalid-syntax tests use a form that cannot be consumed as a regular function call either, e.g. `VALUE_OF(x AT 5)`. `VALUE_OF` is a reserved word (Lexer.fs line 359), but `pRoutineInvocation` uses `pIdentifierRaw`, so it accepts reserved words too.

## JSON_TABLE / JSON_TABLE_PRIMITIVE share `pJsonApiCommon`

Both statements share the `<JSON API common syntax>` (context expression / PATH / AS name / PASSING).

- **Trade-off:** `pJsonApiCommon` is extracted as a shared parser and reused by both `pJsonTableStatement` and `pJsonTablePrimitiveStatement`. They differ only in that `JSON_TABLE` has an optional PLAN clause and optional `ON ERROR`, while `JSON_TABLE_PRIMITIVE` requires `ON ERROR`.

## MATCH_RECOGNIZE shares the row-pattern common parser between QueryParser / ExpressionParser

`MATCH_RECOGNIZE` (QueryParser) and the `<row pattern measures>` / `<row pattern common syntax>` inside window frames (ExpressionParser) share the same row-pattern syntax.

- **Trade-off:** `pRowPatternMeasures` / `pRowPatternCommon` are defined as forward references with `createParserForwardedToRef` and referenced from both modules. `WindowFrame` gained `Measures: RowPatternMeasure list option` and `RowPattern: RowPatternCommon option` fields to hold the window row pattern.
