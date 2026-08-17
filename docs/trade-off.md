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

## Schema-qualified names via `pQualifiedName`

`app.users` (and deeper `a.b.c`) now parse in table-name positions (`FROM`, `INSERT INTO`, `UPDATE`, `DELETE FROM`, `MERGE INTO`, `CREATE TABLE/INDEX/VIEW`, `DROP`, `TRUNCATE`, `ALTER TABLE`, `GRANT`/`REVOKE` objects). `pQualifiedName` returns a single `Expression` — `Identifier` for one part, `ColumnReference parts` for two or more — reusing the same shape as column references.

- **Trade-off:** Reusing `ColumnReference` avoids a new AST case but means a table name and a column reference are indistinguishable by `Kind` alone (consumers must rely on position). A dedicated `QualifiedName` case would be clearer but adds a case for a shape already modeled.

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

## Parser names mirror grammar non-terminals

As a readability refactor, parser functions were renamed to match the SQL-2016 non-terminals they implement, and each module gained a banner listing the grammar sections it covers. Examples: `pTableSource`→`pTableReference`, `pColumnExpr`→`pDerivedColumn`, `pSelectCore`→`pQuerySpecification`, `pTerm`→`pValueExpressionPrimary`, `pPredicateSuffix`→`pPredicate`, `pCastExpr`→`pCastSpecification`, `pSubstringExpr`→`pCharacterSubstringFunction`, `pFunctionCallExpr`→`pRoutineInvocation`, `pParameterExpr`→`pGeneralValueSpecification`.

- **Trade-off:** Renames touch every reference (cross-file references like `DmlParser`'s use of `pTableReference` must be updated in lockstep), but the payoff is that a reader can map each parser to the grammar rule it implements. FParsec's define-before-use constraint means files are still ordered leaf-first rather than grammar-top-down; the banners and names bridge that gap.
