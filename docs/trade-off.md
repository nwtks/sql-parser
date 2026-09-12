# Design Trade-offs

Significant design decisions and the alternatives rejected. Organised by theme;
the architecture itself is described in [architecture.md](architecture.md) and
recurring pitfalls in [gotchas.md](gotchas.md).

## Module organisation

### One module per grammar section

Statements are grouped by the grammar section they belong to rather than by
"DDL/DML/other": `TransactionParser` (§17), `ControlParser` (§16),
`SessionParser` (§19), `TypeParser` (UDTs), `RoutineParser` (routines and
triggers), `CursorParser` (§14 cursors), `ConnectionParser` (§18),
`DiagnosticsParser` (§23), `DynamicParser` (§20).

- **Trade-off:** More files and `Compile Include` entries, but each module stays
  conceptually cohesive and mirrors the standard's section names. Folding these
  into `DmlParser`/`DdlParser` would blur responsibility.

### Compile order is part of the design

F# requires definition before use, so `SqlParser.fsproj` lists modules leaf-first.
`RoutineParser.fs` compiles after `DdlParser.fs` (it needs
`pSpecificRoutineDesignator`); `TypeParser.fs` after `RoutineParser.fs` (it reuses
`pMethodKind` / `pLanguageClause` / `pParameterStyleClause`);
`CursorParser.fs` before `DynamicParser.fs` (it owns `pUsingClause` /
`pIntoClause`, which `pOpenStatement`/`pFetchStatement` need).

- **Trade-off:** Moving a parser to a "more logical" module can force a compile-
  order change or a forward reference. Shared sub-parsers are hoisted to the
  earliest module all callers can reach — e.g. `<scope clause>` and `<method
  kind>` live in `Types.fs` (not `DdlParser.fs`), and `pConstraintEnforcement` /
  `pDropBehavior` / `pGrantor` / `pOverride` are module-level in `DdlParser.fs`.

### Forward references carry cross-module recursion

Mutually recursive rules defined in different modules are wired with
`createParserForwardedToRef` and assigned in `SqlParser.fs` (`pStatement`,
`pDataChangeStatementRef`, `pSchemaElementImpl`, `pRoutineBodyStatementRefImpl`,
`pExpression`, `pDataType`, `pQuery`, the row-pattern refs, …).

- **Trade-off:** A small amount of indirection (and a dummy parser until
  assignment), in exchange for keeping grammar rules in their natural module.
  Reading `.Value` at module-initialisation time captures the dummy parser and is
  forbidden.

## Entry points and statement dispatch

### Two entry points: `parse` (22.1) and `parseStatement` (13.4)

`parse` accepts only 22.1 `<direct SQL statement>` (the directly executable
families plus the mandatory `<semicolon>`); `parseStatement` is a superset that
also accepts `<SQL procedure statement>` forms (cursors, positioned DML, `CALL`,
diagnostics, dynamic SQL).

- **Trade-off:** Requiring the `<semicolon>` and restricting `parse` is a breaking
  change for callers (every test helper appends `;`). In exchange, `parse` is a
  faithful, standard-conforming entry point and the positioned `UPDATE`/`DELETE`
  forms are excluded via explicit guards.
- 22.1's `<direct implementation-defined statement>` is deliberately not
  implemented.

### Dispatch order is load-bearing for non-reserved words

`pKeyword` matches reserved *and* non-reserved words, so a statement `choice` must
order alternatives to keep prefixes apart: `ALTER TYPE` before `ALTER ROUTINE`,
`DROP TYPE` after `DROP ROUTINE`, `EXECUTE IMMEDIATE` before `EXECUTE <name>`,
`DECLARE LOCAL TEMPORARY TABLE` before `DECLARE <cursor>`, `SELECT ... INTO`
before `pQuery`, the PTF `DESCRIBE WITH` body before the generic statement branch,
and 20.17 `ALLOCATE ... FOR <stmt>` before 20.18 `ALLOCATE ... FOR PROCEDURE`.

- **Trade-off:** Order carries meaning and is easy to break; the ordering rules
  are recorded in [gotchas.md](gotchas.md) and covered by tests. A `choice` cannot
  be read independently of the grammar's keyword reservedness.

### `<with clause>` is a query prefix, not a statement prefix

7.17 makes `<with clause>` a prefix of `<query expression>`, so
`WITH ... INSERT/UPDATE/DELETE/MERGE` is rejected by both entry points. The
earlier `pDml`-based form that accepted those was removed as non-standard.

### Schema elements are CREATE-family statements and `GRANT` only

`CREATE SCHEMA`'s `<schema element>` list is an explicit `choice` of the
CREATE-family parsers plus `pGrantStatement`; `DROP` / `ALTER` / `TRUNCATE` /
`REVOKE` are not schema elements.

- **Trade-off:** The list duplicates the CREATE alternatives instead of reusing
  `pDdl`, but it closes the "any DDL is a schema element" gap.

## Grammar-faithful strictness

The parser rejects constructs the standard does not permit, even when they are
common in vendor dialects. Each of these is a deliberate, documented break:

- **`<drop behavior>` is required** where the grammar requires it, so bare
  `DROP TABLE t` / `DROP VIEW v` / `DROP SEQUENCE s` / `DROP TYPE t` /
  `REVOKE ... FROM u` / `ALTER TABLE t DROP COLUMN c` are rejected. It is modelled
  as a `bool` (`true` = `CASCADE`, `false` = `RESTRICT`).
- **Non-standard syntax was removed:** `CREATE INDEX` / `DROP INDEX`,
  `ALTER TABLE ... RENAME TO|COLUMN`, and `FOR SHARE` do not exist in the
  grammar (indexes are implementation-defined; the `<updatability clause>` is only
  `FOR READ ONLY | FOR UPDATE`).
- **`<with or without data>` is mandatory** (11.3), so
  `CREATE TABLE t AS SELECT 1` is rejected.
- **`NATURAL CROSS JOIN` is rejected** (7.10 `<join type>` has no `CROSS`), and
  `<sample method>` is restricted to `BERNOULLI | SYSTEM`.
- **`OFFSET` requires `ROW`/`ROWS`; `FETCH` quantity is optional** (defaults to 1),
  matching the asymmetric grammar.
- **`<partitioned join column reference list>` is column references only**, so
  `PARTITION BY (a + b)` is rejected.
- **`<default option>` is a closed set** (11.5): `DEFAULT (1 + 2)`, `DEFAULT a + b`
  and `DEFAULT ?` are rejected because `<default option>` has no general value
  expression.
- **`pRoutineInvocation` accepts only reserved *function* keywords.** Reserved
  words that start dedicated constructs (`EXISTS`, `UNIQUE`, `JSON_EXISTS`,
  `PERIOD`, `VALUE_OF`, …) are excluded from the whitelist, so they cannot degrade
  to a generic `FunctionCall`. Malformed built-in calls (e.g. `ABS(a, b)`) are
  rejected for the same reason. Adding a reserved-name built-in requires extending
  `functionKeywords`.

- **Trade-off (all of the above):** Real-world SQL that omits standard-mandated
  clauses no longer parses. Consumer-side extensions would have to layer on top.
  The payoff is that an accepted string is much more likely to be valid SQL-2016.

## Expressions and data types

### Types avoid left recursion by construction, not by lookahead

6.1's collection types are mutually recursive with `<data type>` and therefore
left-recursive as written. `pDataTypeElement` parses every non-collection type and
`pCollectionType` folds the `ARRAY`/`MULTISET` suffixes left-to-right.

- **Trade-off:** Unknown/typo'd type names fail cleanly instead of overflowing the
  stack, and nested collections (`INT ARRAY ARRAY`, `INT MULTISET ARRAY[3]`) are
  supported. `<collection type>` may only appear as a postfix; the element parser
  must stay collection-free.

### Quantified comparison uses an intermediate AST node

`x = ANY (SELECT ...)` cannot be parsed as an infix operator because FParsec's
operator-precedence parser consumes `=` and does not backtrack. `ANY/SOME/ALL
(subquery)` is parsed as a term (`QuantifiedSubquery`) and rewritten to
`QuantifiedComparison` by the comparison-operator mapping; a recursive check in
`pExpression` rejects any surviving standalone `QuantifiedSubquery`.

- **Trade-off:** An intermediate case leaks into the AST, but it avoids 18
  per-operator×quantifier infix operators. `Quantifier` uses `SomeQuantifier`
  because `Some` would shadow F#'s option constructor.

### Reserved built-ins get dedicated AST cases

`CURRENT_DATE`/`CURRENT_TIME`/`CURRENT_TIMESTAMP`/`LOCALTIME`/`LOCALTIMESTAMP`
are reserved and cannot be identifiers or generic calls, so they have dedicated
`ExpressionKind` cases; likewise `SUBSTRING(x FROM a FOR b)` and
`OVERLAY(x PLACING y FROM n)`, the numeric/string/regex/collection function
catalogue, row-pattern navigation (`PREV`/`NEXT`/`FIRST`/`LAST`),
`RUNNING`/`FINAL`, and `GROUPING`.

- **Trade-off:** More AST cases versus losing the FROM/FOR structure in a generic
  `FunctionCall`. Zero/one-argument built-ins share `NumericValueFunction` with an
  `Expression list`, so arity is enforced by the parser rather than the type.
  `SetFunction` wraps the aggregate `FunctionCall` instead of adding a seventh
  field that every existing pattern match would have to handle.

### Multiple operand shapes for the same grammar rule

Where the grammar admits several forms with identical syntax, one AST case is
shared and the distinction is documented: `ABS` covers 6.30 and 6.38 (numeric and
interval); `Dereference` covers 6.20–6.22; `FieldReference` keeps the field name
as an `Expression` (not `string`) for consistency; `GeneralizedInvocation`
(6.17) parses its operand with the full `pExpression`, which is slightly more
permissive than `<arithmetic expression>`; `<empty specification>` (6.5) needs no
case because `sepBy` already yields `ArrayConstructor []` / `MultisetConstructor []`.

### Postfix constructs reuse existing layers

`COLLATE` is a predicate suffix, `MULTISET UNION|INTERSECT|EXCEPT` is a left-folded
postfix (so its left operand may be any value expression, while the right operand
is a `<multiset term>`), and `<time zone specifier>` reuses `<interval primary>`.

- **Trade-off:** The parent layer is slightly more permissive than the rule, but no
  new precedence level is needed. 6.45's `<table value constructor by query>`
  (`TABLE ( <query> )`) is a distinct case reachable only from an expression.

### Interval and point-in-time parsing

`<point in time>` uses a dedicated 6.35 `<datetime value expression>` parser
(left-folded over `+`/`-`, with `AT TIME ZONE`), so `a * b` / `a || b` / `a = b`
are rejected where a point in time is expected. `<interval primary>` is
transparent when no qualifier follows (so `INTERVAL '1' DAY` keeps its literal
shape); 6.37's fourth alternative produces `DatetimeDifference` and keeps the
qualifier.

- **Trade-off:** `<interval term>`'s `*`/`/` right operand is the grammar's
  `<factor>` (approximated by `<value expression primary>`), so the
  `[ <interval qualifier> ]` suffix is rejected there
  (`INTERVAL '1' DAY * ? DAY` fails). INTERVAL literals are validated by shape
  against their qualifier at parse time — coarse (no per-month day check), but it
  rejects `INTERVAL 'abc' YEAR`.

### Row value constructors and JSON

- **7.1 `<explicit row value constructor>`** is an expression case so
  `<row value predicand>` positions work: `(1, 2) = (3, 4)`,
  `(1, 2) IN ((1, 2), (3, 4))`. The parenthesized form needs ≥ 2 elements, so
  `(a)` still falls through to the plain parenthesized expression.
- **JSON paths are plain `string`s**, not synthetic `Literal` nodes, and the JSON
  argument slots use the boolean-free `pValueExpressionNoBoolean` — deliberately
  stricter than the grammar, because a comma-separated argument list must stay
  unambiguous.
- **JSON behaviours are separate DUs** (`JsonValueBehavior` vs
  `JsonQueryBehavior`) because the grammar defines different alternatives for
  each; the `<JSON predicate type constraint>` is prefixed `JsonType*` to avoid
  clashing with `ExpressionKind.JsonValue`/`JsonArray`/`JsonObject`.
- **`pJsonKeyUniqueness` returns `bool`**, not `bool option`, so callers can wrap
  it in `opt` without double-wrapping. Likewise `pPadCharacteristic` returns
  `bool`.

## Query AST shape

### `FROM` is a list, not a join tree

7.6 `<from clause>` is `FROM <table reference list>`, so `SelectStatement.From` is
`TableSource list` (empty = no `FROM`). `FROM a, b, c` is `[a; b; c]`, keeping the
comma distinct from `CROSS JOIN`.

### Set-operation `ORDER BY` scope via a `QueryExpression` wrapper

`SELECT a UNION SELECT b ORDER BY c` applies `ORDER BY` to the whole set operation.
`Query` gained a `QueryExpression` case carrying the top-level
`ORDER BY`/`OFFSET`/`FETCH`/`LOCKING` when the body is a set operation or a `WITH`
query.

- **Trade-off:** A new union case, but it keeps scope correct without attaching
  `ORDER BY` to the last operand or bloating `WithQuery`. Plain
  `SELECT ... ORDER BY` still folds into `SelectStatement`, so common AST shapes
  are unchanged. `INTERSECT` binds tighter than `UNION`/`EXCEPT` (separate
  `pQueryTerm`).

### `GROUP BY` as grouping elements

`SelectStatement.GroupBy` is `GroupingElement list` (`GroupingSet | Rollup | Cube |
GroupingSets | EmptyGroupingSet`) plus `GroupByDistinct: bool`.

- **Trade-off:** A parenthesized `(a, b)` is one grouping set, not two columns,
  which a flat `Expression list` would conflate.

### `<simple table>` alternatives as `Query` cases

`ExplicitTable of Expression` (`TABLE t`) and
`TableValueConstructor of Expression list list` (`VALUES (1, 'a'), (2, 'b')`) are
valid `<query primary>`s, so they can be set-operation operands.

- **Trade-off:** Two new cases, wrapped in `QueryExpression` when trailing clauses
  are present. In `FROM`, the parenthesized `<table value constructor>` branch is
  tried before the subquery branch so `FROM (VALUES ...)` keeps yielding the
  dedicated `ValuesTable` node.

### Table sources hold their correlation names

`TableSourceKind.Only` and `DataChangeDelta` keep the optional correlation name and
derived column list; `TableSample` wraps a `TableSource` (it is a suffix on
`<table primary>`); `Lateral`/`Unnest` require an alias, matching `Subquery`;
`SystemTime`'s first argument is a full `TableSource` (preserving its position).

- **Trade-off:** `Only`/`DataChangeDelta` hold one more field than `Table`. The
  `<row pattern recognition clause>` half of `<correlation or recognition>` is
  still not accepted in that slot.

### Window modelling

`WindowFrameUnit` gained `Groups` (ROWS | RANGE | GROUPS). `FunctionCall` gained
`filter` and `withinGroup` fields (6.10 `FILTER`, ordered-set `WITHIN GROUP`) —
a 6-field tuple, but one case for all function calls. `WindowFrame` gained
`Measures`/`RowPattern` fields, shared with `MATCH_RECOGNIZE` via forward refs.

### Ambiguity resolved by shape heuristics

`TABLE (expr)` is classified as `PtfTable` when the expression is a `FunctionCall`
and `TableFunction` otherwise, because `<table function derived table>` and
`<PTF derived table>` are syntactically identical.

- **Trade-off:** A PTF that is not a plain routine invocation would be
  misclassified; no parser can distinguish them without semantic knowledge.

## DDL and DML AST shape

### Column and table constraints

`ColumnDefinition` gained `Constraints` and also keeps `IsNullable` /
`IsPrimaryKey` / `IsUnique` / `References` / `Check` as convenience accessors
derived from it (redundant by design, so existing consumers keep compiling).
`ColumnConstraint { Name; Kind; Characteristics }` and
`TableConstraintDefinition { Constraint; Characteristics }` model
`[ <constraint name definition> ] … [ <constraint characteristics> ]`.

- **Trade-off:** A bare `NULL` was dropped from `ColumnConstraintKind` (11.4's
  `<column constraint>` has no such alternative); that also removed a union-case
  clash with `Literal.Null`. 11.4's optional clause slot is a single `opt` over a
  `Choice`, so `GENERATED ALWAYS AS IDENTITY DEFAULT 5` is rejected (the grammar
  makes `<default clause>` and `<identity column specification>` alternatives).
- `ConstraintCharacteristics` is a flat record of three `bool option`s, tried as
  the grammar's three alternatives in order; it must not swallow a following
  `COLLATE`.

### `CREATE TABLE` contents source

`CreateTableStatement` carries `Under`, `Like`, `Periods`, `WithSystemVersioning`,
`OnCommit`, `AsQuery`, `AsColumns`, `WithData` and `TypedElements`.
`<table element>` is a four-way `Choice` (column / period / constraint / like).

- **Trade-off:** Optional fields keep the common `CREATE TABLE (col defs)` shape
  unchanged, at the cost of a 6-tuple in `pCreateTableStatement`'s content-source
  alternatives.

### Typed tables and views surface their element lists

`CreateTableStatement` / `CreateViewStatement` carry `OfType`, `Under`, and
`TypedElements` / `ViewElements` (DUs, so element order is preserved, unlike the
separate-list style used for `<table element>`). `<column options>` has no data
type (the UDT supplies it), so it is its own record; `<view column option>`'s
`<scope clause>` is mandatory, so `ViewColumnOptions.Scope` is not an option.

- **Trade-off:** `pReferenceGeneration` / `pSelfReferencingColumn` had to be
  hoisted to module level so 11.32 can reuse them.

### Sequence options are shared between sequences and identity columns

`SequenceOption` is one DU used by `CREATE`/`ALTER SEQUENCE` and
`<identity column specification>`, composed from per-rule parsers
(`pBasicSequenceGeneratorOption`, `pSequenceGeneratorStartWithOption`,
`pAlterSequenceGeneratorRestartOption`) so each caller takes exactly what its
clause allows.

- **Trade-off:** One options type avoids near-identical types; `MaxValue` /
  `MinValue` use `decimal option` where `None` means `NO MAXVALUE` /
  `NO MINVALUE`. `CREATE TABLE`'s identity spec and `ALTER SEQUENCE` reuse the
  permissive `pSequenceOption`, while 11.20 `<alter identity column
  specification>` uses the narrowed parsers.

### `ALTER TABLE` covers every 11.10 action

`AlterTableAction` covers `ADD [COLUMN]`, `ALTER [COLUMN]`,
`DROP [COLUMN] … <drop behavior>`, `ADD`/`ALTER`/`DROP CONSTRAINT`,
`ADD`/`DROP <period>`, and `ADD`/`DROP SYSTEM VERSIONING`;
`ColumnAlteration` gained `AddColumnScope`, `DropColumnScope`,
`AlterIdentityColumn`, `DropIdentity`, `DropExpression`.

- **Trade-off:** `<drop behavior>` is a bare `bool`, matching
  `DropTable`/`DropView`/`DropSequence`. `AlterConstraint` carries `[NOT]
  ENFORCED` as a `bool` rather than a `ConstraintCharacteristics`, because 11.25
  only permits `<constraint enforcement>`. `AddTablePeriod`'s optional column list
  holds exactly 0 or 2 entries (the grammar fixes the arity).

### DML modelling choices

- **`SetClause` is a DU** (`SingleSet | MultipleSet | MutatedSet`), tried
  MultipleSet → MutatedSet → SingleSet so `SET (a,b)=(1,2)`, `SET a.b = 1` and
  `SET a = 1` each hit the right branch. `MutatedSet` folds the dotted target into
  a `FieldReference` chain instead of adding a new node.
- **`InsertSource.DefaultValues`** plus `ExpressionKind.Default` cover
  `INSERT INTO t DEFAULT VALUES` and `VALUES (DEFAULT)` / `SET col = DEFAULT`.
  `DEFAULT` is deliberately not a general expression (`SELECT DEFAULT` is
  rejected).
- **`pOverride` is hoisted** and shared by `INSERT` and `MERGE`, returning
  `bool option` (`Some true` = `USER`, `Some false` = `SYSTEM`).
- **`MergeInsert` keeps a single VALUES row** (`Expression list`), unlike
  top-level `INSERT` (`Expression list list`).
- **`FOR PORTION OF` is a `PortionOfSpec` record** using the 6.35 datetime parser
  and `attempt`-wrapped at its call sites.
- **`DmlTarget = TableTarget | OmittedTarget`** lets positioned 20.25/20.27
  statements omit the target; the omitted form is guarded so it cannot combine
  with `<portion of>`, an alias or a search condition. `MERGE` keeps
  `Expression * bool` because 14.12 has no omitted form.
- **`ONLY ( <table> )`** is recorded as `TableIsOnly` / `TargetIsOnly` flags rather
  than a DU, so existing record patterns keep compiling. `INSERT` deliberately
  keeps a plain name (14.11 `<insertion target>` has no `ONLY` form).

### `DROP`, `GRANT` and `REVOKE`

- **One drop parser.** `DROP SCHEMA/DOMAIN/COLLATION/CHARACTER SET/
  TRANSLITERATION/ASSERTION/CAST/ORDERING/TRANSFORM/TYPE/ROUTINE/TRIGGER` were
  added as alternatives of the existing `pDropStatement`. `DropStatement` is
  uniformly `Expression * bool` except `DropRole` (no behavior) and
  `DropAssertion` (`bool option`, since the grammar's behavior is optional).
- **`GRANT`/`REVOKE` are DUs distinguishing privileges from roles**
  (`GrantPrivileges | GrantRoles`), because the two forms differ in the `ON`
  clause and in `WITH GRANT OPTION` vs `WITH ADMIN OPTION`. `GRANTED BY` is parsed
  and discarded (authorization metadata with no consumer);
  `PrivilegeSelectTarget` distinguishes `SELECT (…methods…)` from
  `SELECT (…columns…)`.

### Schema-object details

- `DropCast`'s operands are `DataType`s, so `DROP CAST (INT AS BIGINT) CASCADE`
  works. `DropTransform` reuses `pTransformsToBeDropped` (`ALL | <group name>`).
- **Transform groups are whitespace-separated** (`many1`, no separator), matching
  the grammar; `ALTER TRANSFORM`'s drop behavior sits *inside* the parentheses per
  11.70.
- **Schema charset-or-path accepts either order** (`many (pCharset <|> pPath)`),
  slightly over-permissive but robust.
- **Transliteration source is a name**, not a full routine designator — a
  simplification consistent with dropping the `<specific routine designator>`'s
  `FOR <udt>` suffix.
- `CreateTypeStatement` is one record with optional fields (plus a
  `TypeRepresentation` DU); `AlterTypeAction` is a DU of the four actions;
  `MethodSpecification` reuses `ParameterDeclaration`/`RoutineCharacteristic`,
  which means a method cannot carry `DYNAMIC RESULT SETS`.

## Routines, triggers and types

### One `CreateRoutine` record for procedures and functions

`CreateRoutine = { Name; Parameters; Returns: ReturnsType option;
Characteristics; Dispatch: bool; Body }` is shared;
`Returns = None` ⇔ procedure and only a function reports `Dispatch = true`.

- **Trade-off:** One record instead of two near-identical types.
  `CREATE METHOD` is *not* folded in — `<method specification designator>` has no
  `<routine characteristics>` slot — so it gets `MethodSpecificationDesignator`
  and `StatementKind.CreateMethod`.

### 11.60 and 11.61 use different characteristic sets

`<routine characteristic>` (11.60) and `<alter routine characteristic>` (11.61)
are distinct sets sharing one duplicate check (`rejectDuplicateCharacteristics`):
`NAME <external routine name>` belongs to 11.61 only, while `ALTER ROUTINE` does
not accept `SPECIFIC`, `<deterministic characteristic>` or `<savepoint level
indication>`. `RoutineCharacteristic` is a flat DU (`Language`, `ParameterStyle`,
`SpecificName`, `Deterministic`, `SqlDataAccess`, `NullCall`,
`DynamicResultSets`, `SavepointLevel`, `ExternalName`); the BNF permits any order
but the syntax rules allow each at most once.

- **Trade-off:** A flat DU plus a duplicate check keeps all characteristics in one
  shape; `<language name>` and `<parameter style>` are closed keyword sets, and
  `pLanguageClause` / `pParameterStyleClause` live in `RoutineParser.fs` so
  `TypeParser.fs` can reuse them.

### `RoutineBody` models the body variants

`RoutineBody` is `SqlRoutine of RightsClause option * StatementKind` (the
`<rights clause>` belongs to `<SQL routine spec>`, not to the routine),
`ExternalRoutine of ExternalBodyReference` (name / parameter style / transform
group / external security), or `PolymorphicTableFunction`.

- **Trade-off:** `BEGIN ATOMIC` with `sepEndBy1` matches the grammar (each
  statement, including the last, is semicolon-terminated), but it is an extension
  of this parser — 13.4 has no `<compound statement>`. The PTF branch must be
  tried first because `DESCRIBE` also starts `<describe statement>`.
  `validateRoutine` rejects a `PARAMETER STYLE` written twice.

### `<specific routine designator>` is a record

`SpecificRoutineDesignator { IsSpecific; RoutineType; Name; DataTypeList;
ForType }` replaced a bare `Expression`, so the designator is round-trippable
across its seven AST positions (`PrivilegeSelectTarget.PrivilegeMethods`,
`CreateCast`, `CreateTransliteration`, `AlterRoutineStatement`, …).

- **Trade-off:** A wide retyping, and `RoutineType` stays an option because a bare
  routine name is still valid. `pRoutineDesignatorWithType` (an `Expression`) is
  still used for `GRANT ... ON FUNCTION f`, because 12.3's `<object name>` carries
  a plain name.

### Parameter and return types

`ParameterType = DataTypeParameter | GenericTableParameter | DescriptorParameter`
and `ReturnsType = ReturnsData | ReturnsTable | ReturnsOnlyPassThrough` make
`<returns table type>`, `<result cast>` and `<locator indication>` representable.

- **Trade-off:** `<generic table parameter type>` / `<descriptor parameter type>`
  are tried before `<data type>` even though the grammar lists `<data type>`
  first, because `DESCRIPTOR` is non-reserved and would otherwise be consumed as a
  user-defined type name. `ParameterDeclaration` treats a leading identifier as
  the parameter name, so `IN mytype` (a nameless UDT parameter) is rejected.

## Cursors, dynamic SQL, diagnostics and sessions

- **`UsingClause = UsingArguments | UsingDescriptor`** models the argument-list vs
  `[SQL] DESCRIPTOR` alternatives shared by `EXECUTE`, `OPEN` and `FETCH`.
- **`DescribeStatement`** distinguishes its three forms with `IsInput`/`IsCursor`
  flags; **`GetDiagnosticsStatement`** uses a DU
  (`StatementInfo | ConditionInfo | AllInfo`), and the item names are closed
  `pKeyword` enumerations kept as `string` (`pIdentifierRaw` would accept `ALL`).
- **`<cursor properties>` is four separate DUs** (sensitivity, scrollability,
  holdability, returnability), so an absent attribute is `None` and the AST
  preserves exactly what was written. `<cursor attributes>` (20.8) is implemented
  as a reusable parser but deliberately not reused by `pCursorProperties`, because
  the latter fixes the attribute order.
- **Dynamic cursors reuse `CursorProperties`**; `<extended statement name>` and
  `<extended cursor name>` share one `ExtendedName { Scope; SimpleValue }` record.
- **`<locator reference>` is limited to `:name` and `?`** (the only forms with
  standalone syntax); an embedded `<embedded variable name>` is treated as a host
  parameter.
- **`LockingClause.ForUpdate` carries `Expression list option`** for
  `FOR UPDATE OF …`; the type lives inside the recursive `and` group because it
  references `Expression`.
- **`<descriptor value constructor>` is wired into `<parameter default>` only**,
  not into `<value expression primary>`, matching the grammar.
- **Shorthand keywords normalise to `None`**: `CONNECT TO DEFAULT`,
  `SET TIME ZONE LOCAL`, `SET NO COLLATION` all become `None`.

## Lexical and JSON modelling

### SQL terminal characters are modelled as they are used (5.1)

Only terminals referenced by a production get a parser: the characters already
modelled keep theirs, and `<left brace>` / `<right brace>` / `<circumflex>` /
`<vertical bar>` / `<dollar sign>` plus the 5.2 compound tokens
`{- ` / `-}` were collected in one 5.1 block (used by the 7.9 row pattern parser).

- **Trade-off:** `<percent>` and `<reverse solidus>` are deliberately *not*
  modelled: they occur only inside the embedded XQuery-regex (8.6) and SQL/JSON
  path (9.38/9.39) languages, whose text is kept opaque, or in no production at
  all. Modelling them would be unreachable code that suggests the parser
  understands syntax it deliberately treats as text.
