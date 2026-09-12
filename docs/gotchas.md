# Gotchas & Common Mistakes

## F# union-case name clashes

Many SQL keywords map to natural F# union-case names that already exist elsewhere in `Ast.fs`. Adding a new DU case with a clashing name silently resolves to the wrong type (usually the most recently defined). Known clashes:

| Case name | Clashes between |
|-----------|-----------------|
| `Select`, `Insert`, `Update`, `Delete` | `StatementKind` vs `PrivilegeAction` |
| `Unique` | `ExpressionKind` (subquery predicate) vs `TableConstraint` |
| `DropRole` | `StatementKind` vs `DropStatement` |
| `SetDefault` | `ReferentialAction` vs `ColumnAlteration` |

**Fix:** qualify with the type name at the use site, e.g. `PrivilegeAction.Select`, `TableConstraint.Unique`, `DropStatement.DropRole`, `ReferentialAction.SetDefault`. Always qualify new case names that could clash with existing ones.

## `pKeyword` returns `Parser<string, unit>`, not `Parser<unit, unit>`

`pKeyword` keeps the matched string as its result. This is normally harmless because it is combined with `>>.`/`.>>`/`>>%`. But in an `<|>` (or other places where both branch result types must agree), mix it with a `Parser<unit, _>` and you get a type error. Coerce branches with `|>> ignore`.

## Nested `Some <record>` patterns in tests

F#'s pattern parser is finicky about `Some <record-pattern>` (e.g. `Some { Kind = ... }`) when it appears inside a union-case tuple pattern. It can fail with "Unexpected start of structured construct in pattern". Prefer binding to a variable and asserting on fields, e.g. match `Some name` then check `name.Kind`, instead of inlining the record pattern.

## `char list` to `string` conversion

`List.toArray |> string` on a `char list` yields `"System.Char[]"`, not the character contents. Use `System.String.Concat` (see `hexToInt32` in `Lexer.fs`). The old form made `Convert.ToInt32(..., 16)` throw `FormatException`, crashing every Unicode escape (`U&'\0041'`, `U&"\0041"`).

## Data type left recursion

6.1 `<data type>` — `pCollectionType` must NOT be inside the same `choice` as the base type parser it calls, or unknown types (`MyType`, typos) trigger infinite recursion → `StackOverflowException`. Split into `pDataTypeElement` (all non-collection types) + `pDataType` = `pCollectionType <|> pDataTypeElement` (`Types.fs`).

## `decimal` overflow in approximate literals

5.3 `<approximate numeric literal>` — `1E400` / `1.23E400` overflowed `decimal` with `OverflowException`. Clamp the exponent to ~28 (decimal max) before computing (`pApproximateNumericLiteral` in `Lexer.fs`).

## Time seconds must not accept repeated dots

5.3 `<time literal>` — `TIME '12:00:00.5.5'` used to throw `FormatException` from `decimal`. Parse seconds as digits + optional `.fraction` only, so invalid input fails cleanly (`pTimeValue` in `Lexer.fs`).

## Record patterns: no `; _`

F# record patterns ignore omitted fields automatically — do NOT write `{ Kind = X; _ }` (syntax error: "Unexpected symbol '_' in pattern"). Use `{ Kind = X }`.

## FParsec combinator precedence

`<|>` binds TIGHTER than `.>>`/`.>>.`/`>>.` in FParsec (all `>>.`-family operators are left-associative at equal precedence). Forgetting this produces wrong tuple arities (e.g. `pKeyword "X" >>. p >>. q` gives `p * q`, not a 3-tuple).

## Record type inference: `{ Kind = ...; Pos = ... }` may resolve to `Statement`

Both `Expression` and `Statement` have `Kind` + `Pos` fields. When a record literal like `{ Kind = Literal(Number 1m); Pos = ... }` is bound with `let` and its expected type isn't known at the binding site, F# can infer `Statement` (whose `Kind` is `StatementKind`) and report "expected StatementKind but here has type ExpressionKind". Fix: annotate explicitly, e.g. `let defaultCount: Expression = { ... }` (see `pFetch` in `QueryParser.fs`).

## `sepBy1` does not backtrack a consumed separator

7.6 `<derived column>` — `sepBy1 p sep` fails if `sep` succeeds but the following `p` fails — the consumed separator is not rolled back. For `t.*` (identifier chain ending in `*`), `sepBy1 pIdentifier (token ".")` consumes `t.` then fails on `*`. Use `p .>>. many (attempt (sep >>. p))` instead so a trailing separator is optional (see the qualified-star branch of `pDerivedColumn` in `QueryParser.fs`).

## `override` is a reserved F# keyword

14.8 `<insert statement>` / `<override clause>` — `override` cannot be used as a lambda parameter or local binding name (used for `OVERRIDING` in `INSERT`). F# reports "Unexpected keyword 'override' in lambda expression". Rename the variable (e.g. `ovr`) — see `pInsertStatement` in `DmlParser.fs`.

## Record type inference ambiguity with identical field shapes

When two record types have the same field names/types, an unannotated record literal resolves to the *most recently defined* type. Adding `Columns: Expression list option` to `CreateViewStatement` made it identical to `Cte` (`{ Name: Expression; Columns: Expression list option; Query: Query }`), so `pCte`'s record literal silently became `CreateViewStatement` and broke `WithQuery`. Fix: qualify the first field, e.g. `{ Cte.Name = name; ... }` (see `pCte` in `ExpressionParser.fs`).

## `base` is a reserved F# keyword

7.6 `<table primary>` — `base` cannot be used as a lambda parameter or local binding (used for the table primary in `TABLESAMPLE`). F# reports "Unexpected keyword 'base' in pattern". Rename the variable (e.g. `tbl`) — see `pTablePrimary` in `QueryParser.fs`.

## `TableSource` record inference may resolve to `Statement`

7.6 `<table primary>` — like `Expression`, `TableSource` has `Kind` + `Pos` fields, so an unannotated `{ Kind = TableSample(...); Pos = ... }` literal can be inferred as `Statement`. Qualify the first field: `{ TableSource.Kind = ...; Pos = ... }` (see the `TABLESAMPLE` suffix in `pTablePrimary`).

## `Range` clashes between `IntervalQualifier` and `WindowFrameUnit`

6.1 `<interval qualifier>` / 7.15 `<window frame unit>` — `Range` is a case of both `IntervalQualifier` (with arguments) and `WindowFrameUnit` (no arguments). In `Lexer.fs`, an unqualified `Range(Year, Month)` pattern resolves to `WindowFrameUnit.Range` and fails with "This union case does not take arguments". Qualify as `IntervalQualifier.Range(...)` (see `isValidIntervalValue` in `Lexer.fs`).

## `pint32` accepts a leading `-` in date components

5.3 `<date literal>` — `pDateValue` previously used `pint32`, so `DATE '2023--1-5'` parsed with a negative month. Use `pUnsignedInteger` (which rejects `-`) and validate month/day ranges explicitly (see `pDateValue` in `Lexer.fs`).

## Value restriction on parsers built with `withExprPosition`

A `let`-bound parser whose type isn't pinned to `Parser<_, unit>` can hit F#'s value restriction ("The value 'p' has an inferred generic function type"). `withExprPosition` is generic over the user-state type, so `let pDefaultValue = pKeyword "DEFAULT" >>% Default |> withExprPosition` fails. Fix: annotate explicitly, e.g. `let pDefaultValue: Parser<Expression, unit> = ...` (see `ExpressionParser.fs`).

## FParsec `opt` does not backtrack on partial consumption

12.2 `<grant privilege statement>` / 12.7 `<revoke statement>` — `opt p` is `(p |>> Some) <|> preturn None`. If `p` consumes input and then fails, the `<|>` cannot recover because input was consumed — the whole `opt` fails. This bites with multi-keyword optional clauses: `opt (pKeyword "WITH" >>. pKeyword "HIERARCHY" >>. pKeyword "OPTION")` consumes `WITH` then fails on `GRANT` (for `WITH GRANT OPTION`), failing the entire statement. Fix: wrap the optional body in `attempt`, e.g. `opt (attempt (pKeyword "WITH" >>. pKeyword "HIERARCHY" >>. pKeyword "OPTION"))` (see `pGrantStatement`/`pRevokeStatement` in `DdlParser.fs`).

## Adding a `<simple table>` case makes `pQuery` greedily match `(VALUES ...)` in FROM

7.6 `<table primary>` / 7.17 `<simple table>` / `<table value constructor>` — once `TableValueConstructor` was added to `pSimpleTable`, `pQuery` (a full query expression) could parse a bare `VALUES ...`. In `FROM (VALUES ...) AS t(...)`, the `pTablePrimary` subquery branch (`( <pQuery> )`) then matched first and produced `Subquery(TableValueConstructor ...)` instead of the dedicated `ValuesTable` node. Fix: in `pTablePrimary`, try the parenthesized `<table value constructor>` branch *before* the subquery branch so `FROM (VALUES ...)` keeps yielding `ValuesTable` (see `pTablePrimary` in `QueryParser.fs`).

## FParsec has no `pipe6`

7.16 `<query specification>` — FParsec's `pipe` combinator only goes up to `pipe5`. A 6-way `pipe6` fails to compile with "The value or constructor 'pipe6' is not defined". Fix: split into two stages — `pipe5` for the first five parsers returning an intermediate tuple, then `pipe2` with the sixth (see `pSelectBase` + `pQuerySpecification` in `QueryParser.fs`).

## `>>.` discards earlier results (CREATE TABLE scope)

11.1 `<table definition>` — in `pCreateTableStatement`, `pKeyword "CREATE" >>. opt pTableScope .>> pKeyword "TABLE" >>. pQualifiedName` discards the scope: `>>. pQualifiedName` throws away the accumulated `TableScope option` (all `>>.`-family combinators are left-associative at equal precedence, so only the last result survives). The compiler only flags the mismatch later, in the final lambda ("expected Expression but is a tuple"). Fix: use `.>>. pQualifiedName` to keep the scope in the tuple (see `pCreateTableStatement` in `DdlParser.fs`).

## Optional `Identity`/`CHECK OPTION` need `opt (attempt ...)`

11.2 `<column definition>` / 11.32 `<view definition>` — `opt` does not backtrack on partial consumption, so a bare `opt pIdentitySpec` would consume `GENERATED` and then fail on a plain column, taking the whole column down. Wrap in `attempt`: `opt (attempt pIdentitySpec)` and `opt (attempt pCheckOption)` (see `pColumnDefinition`/`pCreateViewStatement` in `DdlParser.fs`).

## New parsers must be defined before their users (forward references)

F# requires definitions before use. Adding `Identity` to `pColumnDefinition` means `pIdentitySpec` (and its dependency `pSequenceOption`) must be defined *before* `pColumnDefinition`; defining it after fails with FS0727/FS0001 "value not defined" (see the ordering at the top of `DdlParser.fs`). The same applies to `pCreateSequenceStatement`/`pAlterSequenceStatement`, which must precede any `pDdl` wiring that references them.

## `>>%` and `<|>` have the same F# precedence (left-associative)

7.6 `<table reference>` — `>>%` starts with `>` and `<|>` starts with `<` — both are in the same F# operator-precedence group, so `pKeyword "A" >>% x <|> pKeyword "B" >>% y` parses as `((pKeyword "A" >>% x) <|> pKeyword "B") >>% y`, not as two alternatives. Parenthesize each alternative: `(pKeyword "A" >>% x) <|> (pKeyword "B" >>% y)` (see the `FINAL|NEW|OLD` choice in `pTablePrimary`).

## `SystemTime`'s first argument is a full `TableSource`

7.6 `<table reference>` (FOR SYSTEM_TIME) — `TableSourceKind.SystemTime of TableSource * SystemTimeSpec` wraps the base `Table(name, alias)` so the position is preserved. Tests must match `SystemTime({ Kind = Table(...) }, spec)`, and the construction needs `{ TableSource.Kind = Table(name, alias); Pos = pos' }` (record inference would otherwise resolve to `Statement`).

## `FINAL` is not a reserved word

5.2 `<reserved word>` — `FINAL` is not in `reservedWords`, so `pKeyword "FINAL"` works — but the `DataChangeDelta` alternative (`FINAL|NEW|OLD TABLE (...)`) must precede the plain `Table` alternative in `pTablePrimary`, otherwise `FINAL TABLE (...)` would parse `FINAL` as a table name.

## `pExpression` includes boolean ops — use `pValueExpressionNoBoolean`

6.3 `<value expression primary>` — for `<point in time>` in `FOR SYSTEM_TIME`, the full `pExpression` would consume `BETWEEN ... AND ...`'s `AND` as a boolean operator. Use `pValueExpressionNoBoolean = opp.ExpressionParser` (the `OperatorPrecedenceParser` without boolean `AND`/`OR`) for non-boolean value-expression slots.

## F# record patterns with newline-separated fields require same-column alignment

A record pattern whose fields are on separate lines fails with FS0010 ("Unexpected symbol") unless all fields start at the *same column*. Either align the fields or extract the record first and match on it separately (see the `WITH SEARCH`/`WITH CYCLE` tests in `QueryTests.fs`).

## `opt pLockingClause` no-backtrack — `FOR` is now ambiguous

7.6 `<updatability clause>` / FOR SYSTEM_TIME — `FOR` starts both the `<updatability clause>` (`FOR READ ONLY`/`FOR UPDATE`, query level) and `FOR SYSTEM_TIME` (table reference). If a table reference fails to consume `FOR SYSTEM_TIME ...` (e.g. an unsupported point-in-time literal), the query-level `opt pLockingClause` consumes `FOR` and then fails without backtracking, taking the whole query down. Fix: `opt (attempt pLockingClause)` at all three query-level sites in `QueryParser.fs`.

## `pchar '*'` does not consume trailing whitespace

7.6 `<derived column>` — `pKeyword` (and every token parser) does NOT skip leading whitespace — the previous parser must consume its own trailing whitespace. In `pQualifiedAsterisk`, after `pchar '*'` the parser sits at ` AS ...` (leading space), so `opt (attempt (pKeyword "AS" >>. ...))` fails and the `AS (cols)` suffix is silently dropped, leaving `SELECT t.* AS (a, b)` to fail at `AS`. Fix: `pchar '*' .>> ws` so the following `pKeyword "AS"` starts at the token.

## `pRoutineInvocation` only accepts reserved *function* keywords

10.9 `<routine invocation>` — `pRoutineInvocation` derives the routine name from `pReservedFunctionName`, an explicit whitelist of the reserved keywords the grammar spells as functions (`COUNT`, `ROW_NUMBER`, `PERCENTILE_CONT`, `ABS`, `MOD`, `LOWER`, ...), plus non-reserved/delimited identifiers. Two consequences:

- Any reserved word that starts a dedicated construct (`EXISTS (SELECT ...)`, `UNIQUE (...)`, `JSON_EXISTS(...)`, `PERIOD (s, e)`, `VALUE_OF (...)`) must **not** be whitelisted, and its dedicated parser must still be listed BEFORE `pRoutineInvocation` in `pValueExpressionPrimary` (belt and braces).
- **Adding a new built-in or vendor function whose name is reserved requires adding it to `functionKeywords`** — otherwise `SELECT ROW_NUMBER() OVER (...)` / `SELECT ABS(x)` fail with "reserved word." Non-reserved names (`foo(...)`, `app.foo(...)`) are unaffected. See `pReservedFunctionName` in `ExpressionParser.fs`.

## `JSON_ARRAY(NULL ON NULL)` — `NULL` is ambiguous with the null clause

6.33 `<JSON array constructor>` — `sepBy pExpression ","` greedily parses `NULL` as an element, so `JSON_ARRAY(NULL ON NULL)` leaves `ON NULL` unconsumed and fails. Guard each element with `pExpression .>>? notFollowedBy (attempt (pKeyword "ON" >>. pKeyword "NULL"))` so a leading `NULL`/`ABSENT` that starts the null clause is rejected as an element (see `pJsonArrayFunction` in `ExpressionParser.fs`).

## `constraint` is a reserved F# identifier

8.22 `<JSON predicate>` — `constraint` cannot be used as a lambda parameter or local binding ("The identifier 'constraint' is reserved for future use by F#"). Rename (e.g. `typeConstraint`) — see the `IS JSON` predicate in `ExpressionParser.fs`.

## `opt` double-wraps a parser that already returns `option`

6.27 `<JSON key uniqueness>` — `opt p` wraps the result in `Some`, so `opt pJsonKeyUniqueness` where `pJsonKeyUniqueness` returns `bool option` yields `bool option option`. Make the parser return the bare value (`bool`) and let the caller apply `opt` (see `pJsonKeyUniqueness` in `ExpressionParser.fs`).

## `DomainAlteration` case names clash with `ColumnAlteration` / `AlterTableAction`

11.35 `<alter domain statement>` / 11.10 `<alter table statement>` — `DomainAlteration`'s cases `SetDefault`/`DropDefault`/`AddConstraint`/`DropConstraint` clash with `ColumnAlteration.SetDefault`/`DropDefault` and `AlterTableAction.AddConstraint`/`DropConstraint`. Unqualified use in `pAlterDomainStatement` or `pAlterTableStatement` resolves to the wrong type. Qualify at the use site: `DomainAlteration.SetDefault`, `AlterTableAction.AddConstraint`, etc. (see `pAlterDomainStatement`/`pAlterTableStatement` in `DdlParser.fs` and the `ALTER TABLE extended actions` / `ALTER DOMAIN` tests).

## `opt` double-wraps again: `pConstraintCharacteristics` / `pPadCharacteristic`

11.6 `<table constraint definition>` / 11.43 `<collation definition>` — `pCheckTime` already returns `bool option`, so `attempt (pCheckTime |>> fun b -> (Some b, None, None))` produced `bool option option`. Fix: `fun b -> (b, None, None)`.
- `pPadCharacteristic` returned `bool option`, then `opt pPadCharacteristic` gave `bool option option` but `CreateCollation`'s 4th field is `bool option`. Fix: `pPadCharacteristic` returns `bool` (`>>% true`/`>>% false`), so `opt pPadCharacteristic : bool option`.

## Forward-ref wiring must happen after `pDdl` in `SqlParser.fs`

`pSchemaElementImpl` (a `createParserForwardedToRef` in `DdlParser.fs`) is wired with `pSchemaElementImpl.Value <- pDdl` in `SqlParser.fs` *after* `pDdl` is defined — the same pattern as `pDataChangeStatementRef`. Wiring it inside `DdlParser.fs` would fail because `pDdl` lives in `SqlParser.fs` (compiled later).

## `pTransformsToBeDropped` must precede `pDropStatement`

11.71 `<drop transform statement>` — `pDropStatement`'s `DROP TRANSFORM` alternative references `pTransformsToBeDropped`, so that parser must be defined *before* `pDropStatement` (F# define-before-use). Placing it after produces FS0727/FS0001 "value not defined".

## Avoid deeply nested multi-line record patterns in tests

A record pattern nested several levels deep inside a list pattern (e.g. `[ { Constraints = [ { Name = ...; Characteristics = { ... } } ] } ]` spread across many lines) fails with "Unexpected start of structured construct in pattern" / "Incomplete structured construct ... Expected '->'". Fix: bind sub-values to names and assert on their fields separately, e.g. match `Constraints = [ c ]` then `Assert.Equal(None, c.Name)` / `Assert.Equal(None, c.Characteristics.InitiallyDeferred)`, or match `Actions = [ addAction; dropAction ]` then `match addAction with ...` (see the `CREATE DOMAIN` / `ALTER TRANSFORM` tests in `DdlTests.fs`).

## `ALTER TRANSFORM` drop behavior is inside the parens

11.70 `<drop transform element list> ::= DROP ( <transform kind> [ , <transform kind> ] <drop behavior> )` — the `CASCADE`/`RESTRICT` goes *inside* the parentheses: `ALTER TRANSFORM FOR t g (DROP (TO SQL RESTRICT))`. Writing `DROP (TO SQL) RESTRICT` (behavior outside) fails with a confusing top-level error (backtracked to `ALTER TABLE` expecting `TABLE`). The parser is correct per the grammar; the test input was wrong.

## `default` and `when` are reserved F# keywords

11.60 `<SQL-invoked routine>` / 11.49 `<trigger definition>` — `default` and `when` cannot be used as lambda parameter names (F# reports "Unexpected keyword 'default'/'when' in lambda expression"). `ParameterDeclaration.Default` and `TriggeredAction.When` are record fields, so the lambda parameters must be renamed (e.g. `defaultVal`, `whenCond`) — see `pParameterDeclaration`/`pTriggeredAction` in `RoutineParser.fs`.

## `BeginAtomic` is a case of both `RoutineBody` and `TriggeredStatement`

11.60 `<SQL-invoked routine>` / 11.49 `<trigger definition>` — both `RoutineBody` and `TriggeredStatement` have a `BeginAtomic of StatementKind list` case. An unqualified `BeginAtomic` in `RoutineParser.fs` resolves to the wrong type (F# reports "expected RoutineBody but here has type TriggeredStatement"). Qualify: `RoutineBody.BeginAtomic` / `TriggeredStatement.BeginAtomic` — in both the parser and the tests.

## `[` must be on the same line as `=` in a record pattern with a list of records

A record pattern whose value is a list of inline records fails with "Unexpected start of structured construct in pattern" when the `[` is on its own line after `=`:

```fsharp
// FAILS
| CreateProcedure { Parameters =
                        [ { Mode = Some In } ] } -> ()
// WORKS
| CreateProcedure { Parameters = [ { Mode = Some In } ] } -> ()
```

The `[` must start on the same line as the `=` (the `{` may then span lines). This bit the `CREATE PROCEDURE`/`CREATE FUNCTION`/`CREATE TRIGGER` tests (see `DdlTests.fs` — 11.60 `<SQL-invoked routine>` / 11.49 `<trigger definition>`).

## `LOG` is a reserved word

5.2 `<reserved word>` — `LOG` is in `reservedWords` (Lexer.fs line 193), so `INSERT INTO log (msg) ...` fails to parse — `pQualifiedName` rejects it. Use a non-reserved table name (e.g. `logs`) in tests. The `CREATE TRIGGER BEFORE INSERT` test hit this.

## `pUnsignedInteger` does not consume trailing whitespace

11.60 `<SQL-invoked routine>` — `pUnsignedInteger` (`many1Chars digit |>> uint64`) leaves trailing whitespace unconsumed, so a following `pKeyword` (which does not skip leading whitespace) fails. In `DYNAMIC RESULT SETS <n> <next characteristic>`, follow it with `.>> ws`: `pUnsignedInteger .>> ws |>> DynamicResultSets` (see `pRoutineCharacteristic` in `RoutineParser.fs`).

## `SELECT ( <privilege method list> )` must precede `SELECT [ <privilege column list> ]`

12.3 `<privileges>` — in `pPrivilegeAction`, the method-list alternative must be tried *before* the plain `SELECT [ column list ]` one. Otherwise `opt pPrivilegeColumnList` succeeds with `None` (leaving the `(` unconsumed) and the grant fails on the following `ON` (see `pPrivilegeAction` in `DdlParser.fs`).

## `TYPE` / `UNDER` / `OVERRIDING` / `INSTANCE` / `CONSTRUCTOR` / `INSTANTIABLE` / `FINAL` are not reserved words

5.2 `<reserved word>` — `TYPE`, `UNDER`, `OVERRIDING`, `INSTANCE`, `CONSTRUCTOR`, `INSTANTIABLE`, and `FINAL` are **not** in `reservedWords` (only `REF`, `METHOD`, `NEW`, `SCOPE`, `STATIC`, `SYSTEM`, `LOG` are). Consequences:

- `pAlterTypeStatement` must be tried **before** `pAlterRoutineStatement` in `pDdl` — otherwise `ALTER TYPE my_type ...` is consumed as `ALTER ROUTINE` (which expects `FUNCTION`/`PROCEDURE`/`ROUTINE`/`METHOD`).
- `DROP TYPE` must be placed **after** `DropRoutine` in `pDropStatement` — otherwise `DROP TYPE t` is consumed as a bare routine designator (`DROP t`).
- `pKeyword "TYPE"` still matches the literal string, so `CREATE TYPE` works; the risk is only in ambiguous positions.

## `pKeyword` returns `Parser<string, unit>` — discard with `>>.` / `>>% ()`

11.54 `<alter type action>` — `pKeyword s` keeps the matched string. When chained with `.>>.`/`.>>`, the string lands in the tuple and must be discarded (`|>> ignore` or `>>% ()`). In the four `CAST` type options (`CAST (SOURCE AS REF) WITH a` etc.), `pKeyword "CAST" >>. between "(" ")" (KEYWORDS >>% ()) .>> pKeyword "WITH" >>. pIdentifierExpr` — the `>>.` (not `.>>.`) discards the keyword strings (see `pTypeOption` in `TypeParser.fs`).

## `Final` clashes between `ResultOption` and `TypeOption`

`Final` is a case of both `ResultOption` (7.6) and `TypeOption` (11.40). In `TypeParser.fs`, an unqualified `Final false` resolves to `ResultOption.Final` (which takes no arguments) and fails. Qualify: `TypeOption.Final false` (see `pTypeOption`).

## `sepBy1` parses `p` in the separator — don't put `p` in both

6.3 `<column reference>` — `sepBy1 p sep = p .>>. many (sep >>. p)`. The original `pColumnReferenceExpr` used `sepBy1 pIdentifier (attempt (token "." >>. pIdentifier .>>? notFollowedBy "("))` — the separator already parsed `pIdentifier`, so `t.id` double-parsed and broke `SELECT t.id FROM t`, `JOIN ... ON a.id = b.id`, and DML aliases. Fix: `pIdentifier .>>. many (attempt (token "." >>. pIdentifier .>>? notFollowedBy "("))` (see `ExpressionParser.fs`).

## `Choice1Of2` / `Choice2Of2` type inference back-propagates from record fields

`pViewSpecification` returns `Choice1Of2 (column list) | Choice2Of2 (OF type)`, and the caller destructures it into `CreateViewStatement.Columns`/`OfType`. If the `Choice1Of2`/`Choice2Of2` arguments are not annotated, F# infers them from the record field types — but only after the whole expression type-checks, so a wrong guess surfaces as a confusing error at the `|>>` lambda. Fix: annotate the choice arguments explicitly (`Choice1Of2 c` / `Choice2Of2 t`) and destructure with a `match` (see `pCreateViewStatement` in `DdlParser.fs`).

## `METHOD` and `ref` are reserved words — use non-reserved names in tests

`METHOD` is reserved, so a method name cannot be `METHOD` (`SELECT a.b.method(x)` fails). Use e.g. `prune`. `ref` is reserved, so a column named `ref` fails (`CREATE TABLE t (ref REF(...))`); use `r`. The `CREATE TYPE` tests hit both.

## Inline list-of-record pattern must not be nested inside `Some(...)`

The previous rule ("`[` on the same line as `=`") is necessary but not sufficient: an inline list-of-record pattern *inside* `Some(...)` still fails with FS0010 ("Unexpected identifier in pattern"):

```fsharp
// FAILS — `[` is inside Some(...), not directly after `=`
| CreateType { Representation = Some(TypeRepresentation.MemberList [ { Name = ... } ]) } -> ()
// WORKS — bind the list, then match it separately
| CreateType { Representation = Some(TypeRepresentation.MemberList attrs) } ->
    match attrs with
    | [ { Name = ... } ] -> ()
```

The `[` must be directly after the `=` of the field it is assigned to. A single-level `Methods = [ { ... } ]` (with `[` after `=`) and a nested `Methods = [ { ... Parameters = [ { ... } ] ... } ]` both work; only the `Some(...)`-wrapped form fails (see the `CREATE TYPE` tests in `DdlTests.fs`).

## `Expression` constructions need `Pos` — prefer patterns over `Assert.Equal`

`Expression = { Kind; Pos }` is a record, so *constructing* an expected `Expression` (e.g. inside `Assert.Equal`) requires `Pos = { Line = ...; Column = ... }`, and the parsed value's real `Pos` will never equal a hand-written one. Prefer **pattern matching** (which ignores `Pos`) over `Assert.Equal` on values containing `Expression`s. These tests bind lists to variables and match them separately rather than comparing whole records.

## Record type inference ambiguity again: the `MutatedSet` `List.fold`

Building the mutated target in `pSetClause` (`DmlParser.fs`) with `List.fold (fun acc name -> { Kind = FieldReference(acc, name); Pos = acc.Pos }) first rest` hits the same `Expression`-vs-`Statement` ambiguity as `pFetch` — both records have `Kind`/`Pos` fields, and bidirectional inference pins the fold's `'State` to `Statement`, producing "expected StatementKind but here has ExpressionKind" / "expected Statement but here has Expression". Fix: annotate both the result and the lambda params:

```fsharp
let target: Expression =
    List.fold
        (fun (acc: Expression) (name: Expression) ->
            { Kind = FieldReference(acc, name); Pos = acc.Pos })
        first
        (List.take (rest.Length - 1) rest)
```

## `MergeInsert` values is a single `Expression list`, not list-of-lists

`MergeAction.MergeInsert`'s third field is one `VALUES (...)` row — a single `Expression list`. A test pattern `[ [ { Kind = Default } ] ]` (list-of-lists, copied from the top-level `INSERT` VALUES shape) fails with "expected Expression but here has ''a list". Use `[ { Kind = Default } ]` and `[ { Kind = Literal(Number 1m) } ]` (see the MERGE OVERRIDING tests in `DmlTests.fs`).

## Order the `UPDATE` set-clause branches: MultipleSet → MutatedSet → SingleSet

All three branches of `pSetClause` could match the start of the input (`(` for MultipleSet; an identifier for both MutatedSet and SingleSet). MutatedSet must be tried *before* SingleSet: SingleSet uses `pIdentifierExpr` (a single identifier), so `SET a.b = c` would consume `a` and fail at `.`. `<|>` backtracks on failure, but if SingleSet is last there is nothing after it — the whole UPDATE fails. Correct order: MultipleSet, MutatedSet, SingleSet (see `pSetClause` in `DmlParser.fs`).

## `pWhereClause` is defined in two modules

Both `QueryParser.fs` (line 467) and `DmlParser.fs` define a module-level `pWhereClause` with different shapes. `QueryParser`'s is `WHERE <expression>`; `DmlParser`'s returns `(cursor option, search condition option)` to support `WHERE CURRENT OF`. They do not clash (separate modules), but when reading code, know which one you're looking at.

## `pOverride` must be defined before both `INSERT` and `MERGE`

`pOverride` is hoisted to module level in `DmlParser.fs` and used by both `pInsertStatement` and `pMergeStatement`, so it must appear at the top of the module (F# define-before-use). The USER/SYSTEM alternative parenthesizes the second branch: `(pKeyword "USER" >>% true <|> (pKeyword "SYSTEM" >>% false))` — `>>%` and `<|>` share precedence, so without the inner parens the `<|>` would bind differently (see the earlier `>>%`/`<|>` gotcha).

## `EXECUTE IMMEDIATE` must be tried before `EXECUTE <name>`

Because `IMMEDIATE` is not a reserved word, `pExecuteStatement` (`EXECUTE <name>`) would greedily consume `IMMEDIATE` as the statement name in `EXECUTE IMMEDIATE ...`. Place `pExecuteImmediateStatement` at the head of the `pDynamic` `choice`.

## `GET DIAGNOSTICS x = ALL` is ambiguous with the statement-information form

`pIdentifierRaw` accepts reserved words too (including `ALL`), so `x = ALL` could also be parsed as the statement-information item name `ALL`. In `pGetDiagnosticsStatement`, the order must be CONDITION form → ALL form → statement-information form.

## `SELECT INTO` must be tried before `pQuery`

`SELECT ... INTO ...` also matches `pQuery` (a normal `SELECT`), leaving `INTO` unconsumed and failing at eof. In the `pStatementRef.Value` `choice`, place `pCursor` before `pDml`.

## `pWhereClause` exists in two modules

`QueryParser.fs` and `DmlParser.fs` define a module-level `pWhereClause` with different signatures. The cursor parsers (`CursorParser.fs`) must open only `QueryParser` and must NOT open `DmlParser`.

## `pIdentifierRaw` must not be used for closed-enumeration item names

Diagnostics / descriptor item names (`NUMBER` / `ROW_COUNT` / `COUNT` / `DATA` / `MESSAGE_TEXT` etc.) are reserved words, so they cannot be parsed with `pIdentifier` — but `pIdentifierRaw` is too permissive (it also accepts `ALL`, `SELECT`, ...). Use an explicit `choice [ pKeyword "..." ... ]` enumeration instead, as `pStatementInfoItemName` / `pConditionInfoItemName` (DiagnosticsParser) and `pHeaderItemName` / `pDescriptorItemName` / `pCopyDescriptorOptions` (DynamicParser) now do. Note the grammar distinguishes `<header item name>` (`COUNT`, `KEY_TYPE`, ...) from `<descriptor item name>` (`DATA`, `INDICATOR`, ...): the `<get/set header information>` forms use the former, the `VALUE` item forms the latter.

## `notFollowedBy` makes the failure fatal when the inner parser succeeds — wrap in `attempt` inside `opt`

`notFollowedBy p` marks the failure as **fatal** when `p` succeeds. `opt` does not catch fatal errors, so `opt (pIdentifierExpr >>= ... notFollowedBy ...)` fails the entire `opt` and never reaches the following `opt pWindowFrame`. Wrap with `attempt` to convert the fatal error into an ordinary failure (see `pExistingWindowName`).

## `pDataType` accepts any identifier as a user-defined type

`pDataTypeElementRef.Value` includes a `pSchemaQualifiedName` branch that maps to `UserDefinedType`, so any identifier is a valid UDT name. Without a guard, `NESTED PATH '$.items'` is misread as a regular column (name `NESTED`, type `PATH`). The UDT branch now uses `pSchemaQualifiedName .>>? notFollowedBy pIdentifier`, so a name immediately followed by another identifier is rejected; `pJsonTableColumn` still tries the NESTED branch before the regular-column branch (`QueryParser.fs`).

## `OUT` is a reserved word — cannot be used as a MATCH_RECOGNIZE output name

`MATCH_RECOGNIZE (...) AS out` fails because `pIdentifierExpr` rejects the reserved word `OUT`. Tests must use non-reserved output names (e.g. `out_t`).

## `VALUE_OF(x)` without `AT` is rejected

`VALUE_OF` is a reserved word that is deliberately not in `pRoutineInvocation`'s `functionKeywords` whitelist, so `VALUE_OF(x)` (no `AT`) fails instead of falling through to a generic function call. Invalid-syntax tests can use `VALUE_OF(x AT 5)` for the error path.

## `RowPatternDefinition.Condition` is an `Expression` record

The `Condition` field of `RowPatternDefinition` is an `Expression` record (`{ Kind; Pos }`), not an `ExpressionKind`. Test patterns must wrap it as `Condition = { Kind = ... }` (see the `DEFINE A AS a > 0` validation tests).

## `pValueExpressionNoBoolean` must be a forward ref when JSON parsers use it

6.28 `<value expression>` — the JSON parsers (`pJsonApiCommon`, `pJsonArgument`, `pJsonNameAndValue`, `pJsonValueBehavior`) must reject boolean operators, so they use `pValueExpressionNoBoolean` rather than `pExpression`. That parser is `opp.ExpressionParser`, but the JSON parsers are defined *before* the `OperatorPrecedenceParser` (`opp`) is built. Fix: declare `pValueExpressionNoBoolean` at the top of `ExpressionParser` with `createParserForwardedToRef` and wire `pValueExpressionNoBooleanRef.Value <- opp.ExpressionParser` only after `opp` has all its operators (the same forward-ref pattern as `pExpression`). Reverting it to `let pValueExpressionNoBoolean = opp.ExpressionParser` fails with "value not defined" because `opp` does not exist yet at that point.

## `<constraint characteristics>` must not swallow a following `COLLATE`

10.8 — a domain constraint is `[ <constraint name definition> ] CHECK ( ... ) [ <constraint characteristics> ]` and the enclosing `<domain definition>` ends with `[ <collate clause> ]`. The parser must leave `COLLATE` unconsumed so the domain can parse it. `pConstraintCharacteristics`'s second alternative requires `[ NOT ] DEFERRABLE` before the optional `<constraint check time>`, so a bare `NOT DEFERRABLE COLLATE en_us` parses the deferrability and stops cleanly. Do **not** guard the branch with `notFollowedBy (pKeyword "COLLATE")`: that makes a valid `NOT DEFERRABLE` fail fatally (via `attempt` backtracking) and rejects `CREATE DOMAIN ... NOT DEFERRABLE COLLATE ...`. The clause is optional overall, so the parser ends with a `preturn` empty result.
