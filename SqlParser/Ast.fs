namespace SqlParser

type Position = { Line: int64; Column: int64 }
type ParseError = ParseError of string * Position

type BinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Equal
    | NotEqual
    | LessThan
    | LessThanOrEqual
    | GreaterThan
    | GreaterThanOrEqual
    | And
    | Or
    | Concatenate

type UnaryOperator =
    | Not
    | Plus
    | Minus

type Quantifier =
    | Any
    | SomeQuantifier
    | All

type DateValue = { Year: int; Month: int; Day: int }

type TimeZoneOffset = { Sign: int; Hours: int; Minutes: int }

type TimeValue =
    { Hour: int
      Minute: int
      Second: decimal
      TzOffset: TimeZoneOffset option }

type TimestampValue = { Date: DateValue; Time: TimeValue }

type DateTimeField =
    | Year
    | Month
    | Day
    | Hour
    | Minute
    | Second

type IntervalQualifier =
    | SingleField of DateTimeField
    | Range of DateTimeField * DateTimeField

type IntervalValue =
    { IsNegative: bool
      ValueString: string
      Qualifier: IntervalQualifier }

type Literal =
    | String of string
    | NationalString of string
    | UnicodeString of string
    | Number of decimal
    | Bool of bool option
    | Date of DateValue
    | Time of TimeValue
    | Timestamp of TimestampValue
    | Interval of IntervalValue
    | Binary of byte[]
    | Null

type JoinType =
    | InnerJoin
    | LeftJoin
    | RightJoin
    | FullJoin
    | CrossJoin

type SetOperatorKind =
    | Union
    | Intersect
    | Except

type NullsOrder =
    | NullsFirst
    | NullsLast

type LockingClause = | ForUpdate

type WindowFrameUnit =
    | Rows
    | Range
    | Groups

type TrimSpecification =
    | Both
    | Leading
    | Trailing

type DataType =
    | Character of int option
    | Varchar of int option
    | CharacterLargeObject of int option
    | NationalCharacter of int option
    | NationalVarchar of int option
    | NationalCharacterLargeObject of int option
    | Binary of int option
    | VarBinary of int option
    | BinaryLargeObject of int option
    | Numeric of int option * int option
    | Decimal of int option * int option
    | DecFloat of int option
    | SmallInt
    | Integer
    | BigInt
    | Float of int option
    | Real
    | DoublePrecision
    | Boolean
    | DateType
    | TimeType of int option * bool
    | TimestampType of int option * bool
    | IntervalType of string
    | RowType of (Expression * DataType) list
    | ArrayType of DataType * int option
    | MultisetType of DataType
    | UserDefinedType of Expression

and ExpressionKind =
    | Literal of Literal
    | Identifier of string
    | BinaryOp of BinaryOperator * Expression * Expression
    | UnaryOp of UnaryOperator * Expression
    | FunctionCall of
        Expression *
        bool *
        Expression list *
        WindowDefinition option *
        Expression option *
        (Expression * bool * NullsOrder option) list option
    | Cast of Expression * DataType
    | Case of Expression option * (Expression * Expression) list * Expression option
    | SubqueryExpression of Query
    | Star
    | QualifiedStar of string list
    | Parameter of string
    | WindowFunction of WindowFunction
    | ColumnReference of string list
    | Between of Expression * bool * bool * Expression * Expression
    | InList of Expression * bool * Expression list
    | InSubquery of Expression * bool * Query
    | IsNull of Expression * bool
    | IsBoolean of Expression * bool * bool option
    | Exists of Query
    | Unique of Query
    | IsDistinctFrom of Expression * bool * Expression
    | Overlaps of Expression * Expression
    | QuantifiedComparison of BinaryOperator * Quantifier * Expression * Query
    | QuantifiedSubquery of Quantifier * Query
    | Collate of Expression * Expression
    | Like of Expression * bool * Expression * Expression option
    | SimilarTo of Expression * bool * Expression * Expression option
    | Extract of Expression * Expression
    | Position of Expression * Expression * Expression option
    | Trim of TrimSpecification option * Expression option * Expression
    | CurrentDate
    | CurrentTime of int option
    | CurrentTimestamp of int option
    | LocalTime of int option
    | LocalTimestamp of int option
    | Substring of Expression * Expression * Expression option * string option
    | Overlay of Expression * Expression * Expression * Expression option
    | Default

and Expression = { Kind: ExpressionKind; Pos: Position }

and WindowFrameExclusion =
    | ExcludeCurrentRow
    | ExcludeGroup
    | ExcludeTies
    | ExcludeNoOthers

and WindowFrameBound =
    | UnboundedPreceding
    | Preceding of Expression
    | CurrentRow
    | Following of Expression
    | UnboundedFollowing

and WindowFrame =
    { Unit: WindowFrameUnit
      Start: WindowFrameBound
      End: WindowFrameBound option
      Exclusion: WindowFrameExclusion option }

and WindowDefinition =
    { ExistingWindowName: Expression option
      PartitionBy: Expression list
      OrderBy: (Expression * bool * NullsOrder option) list
      Frame: WindowFrame option }

and WindowFunction =
    { Function: Expression
      Args: Expression list
      IsDistinct: bool
      Window: WindowDefinition }

and SetOperator =
    { Kind: SetOperatorKind
      IsAll: bool
      IsDistinct: bool
      Corresponding: Expression list option option }

and Query =
    | SelectQuery of SelectStatement
    | SetOperation of Query * SetOperator * Query
    | WithQuery of bool * Cte list * Query
    | ExplicitTable of Expression
    | TableValueConstructor of Expression list list
    | QueryExpression of
        Query *
        (Expression * bool * NullsOrder option) list *
        (Expression option * FetchClause option) option *
        LockingClause option

and Cte =
    { Name: Expression
      Columns: Expression list option
      Query: Query }

and TableSourceKind =
    | Table of Expression * Expression option
    | Subquery of Query * Expression * Expression list option
    | ValuesTable of Expression list list * Expression * Expression list option
    | JoinedTable of JoinSource
    | Lateral of Query * Expression * Expression list option
    | Unnest of Expression * bool * Expression * Expression list option
    | TableSample of TableSource * string * Expression * Expression option

and TableSource =
    { Kind: TableSourceKind; Pos: Position }

and JoinCondition =
    | On of Expression
    | Using of Expression list

and JoinSource =
    { JoinType: JoinType
      IsNatural: bool
      Left: TableSource
      Right: TableSource
      Condition: JoinCondition option }

and ColumnSource = Column of Expression * Expression option

and GroupingElement =
    | GroupingSet of Expression list
    | Rollup of GroupingElement list
    | Cube of GroupingElement list
    | GroupingSets of GroupingElement list
    | EmptyGroupingSet

and FetchClause =
    { Count: Expression
      IsPercent: bool
      WithTies: bool }

and SelectStatement =
    { IsDistinct: bool
      Columns: ColumnSource list
      From: TableSource list
      Where: Expression option
      GroupBy: GroupingElement list
      GroupByDistinct: bool
      Having: Expression option
      Window: (Expression * WindowDefinition) list
      OrderBy: (Expression * bool * NullsOrder option) list
      Offset: Expression option
      Fetch: FetchClause option
      Locking: LockingClause option }

type InsertSource =
    | Values of Expression list list
    | Query of Query
    | DefaultValues

type InsertStatement =
    { Table: Expression
      Columns: Expression list option
      Source: InsertSource
      Override: bool option }

type SetClause =
    | SingleSet of Expression * Expression
    | MultipleSet of Expression list * Expression list

type UpdateStatement =
    { Table: Expression
      TableAlias: Expression option
      Set: SetClause list
      Where: Expression option }

type DeleteStatement =
    { Table: Expression
      TableAlias: Expression option
      Where: Expression option }

type MergeMatchCondition =
    | Matched
    | NotMatched

type MergeAction =
    | MergeUpdate of (Expression * Expression) list
    | MergeDelete
    | MergeInsert of Expression list option * Expression list

type MergeWhenClause =
    { MatchCondition: MergeMatchCondition
      Condition: Expression option
      Action: MergeAction }

type MergeStatement =
    { Target: Expression
      TargetAlias: Expression option
      Source: TableSource
      On: Expression
      WhenClauses: MergeWhenClause list }

type ReferentialAction =
    | Cascade
    | SetNull
    | SetDefault
    | Restrict
    | NoAction

type ForeignKeyConstraint =
    { Name: Expression option
      Columns: Expression list
      Table: Expression
      RefColumns: Expression list option
      OnUpdate: ReferentialAction option
      OnDelete: ReferentialAction option }

type ColumnDefinition =
    { Name: Expression
      DataType: DataType
      IsNullable: bool option
      IsPrimaryKey: bool
      DefaultValue: Expression option
      IsUnique: bool
      References: ForeignKeyConstraint option
      Check: Expression option }

type TableConstraint =
    | PrimaryKey of Expression option * Expression list
    | Unique of Expression option * Expression list
    | ForeignKey of ForeignKeyConstraint
    | Check of Expression option * Expression

type CreateTableStatement =
    { Table: Expression
      Columns: ColumnDefinition list
      Constraints: TableConstraint list
      AsQuery: Query option
      AsColumns: Expression list option
      WithData: bool option }

type CreateViewStatement =
    { Name: Expression
      Columns: Expression list option
      Query: Query }

type DropStatement =
    | DropTable of Expression * bool
    | DropView of Expression
    | DropRole of Expression

type ColumnAlteration =
    | SetDefault of Expression
    | DropDefault
    | SetNotNull
    | DropNotNull
    | SetDataType of DataType

type AlterTableAction =
    | AddColumn of ColumnDefinition
    | DropColumn of Expression
    | AlterColumn of Expression * ColumnAlteration
    | AddConstraint of TableConstraint
    | DropConstraint of Expression

type AlterTableStatement =
    { Table: Expression
      Action: AlterTableAction }

type PrivilegeAction =
    | Select of Expression list option
    | Insert of Expression list option
    | Update of Expression list option
    | Delete
    | References of Expression list option
    | Usage
    | Trigger
    | Under
    | Execute

type Privileges =
    | AllPrivileges
    | Actions of PrivilegeAction list

// 12.7 <revoke option extension> ::= GRANT OPTION FOR | HIERARCHY OPTION FOR
// (None covers the absent case)
type RevokeOptionExtension =
    | NoOption
    | GrantOptionFor
    | HierarchyOptionFor

type GrantStatement =
    // 12.2: (privileges, object name, grantees, withHierarchyOption, withGrantOption)
    | GrantPrivileges of Privileges * Expression * Expression list * bool * bool
    // 12.5: (roles, grantees, withAdminOption)
    | GrantRoles of Expression list * Expression list * bool

type RevokeStatement =
    // 12.7: (privileges, object name, grantees, option, cascade)
    | RevokePrivileges of Privileges * Expression * Expression list * RevokeOptionExtension * bool
    // 12.7: (roles, grantees, adminOptionFor, cascade)
    | RevokeRoles of Expression list * Expression list * bool * bool

type IsolationLevel =
    | ReadUncommitted
    | ReadCommitted
    | RepeatableRead
    | Serializable

type TransactionAccessMode =
    | ReadOnly
    | ReadWrite

type TransactionMode =
    | Isolation of IsolationLevel
    | AccessMode of TransactionAccessMode

type StatementKind =
    | Select of Query
    | Insert of InsertStatement
    | Update of UpdateStatement
    | Delete of DeleteStatement
    | Merge of MergeStatement
    | CreateTable of CreateTableStatement
    | CreateView of CreateViewStatement
    | Drop of DropStatement
    | AlterTable of AlterTableStatement
    | Truncate of Expression * bool option
    | Grant of GrantStatement
    | Revoke of RevokeStatement
    | CreateRole of Expression
    | DropRole of Expression
    | StartTransaction of TransactionMode list
    | Commit of bool option
    | Rollback of bool option * Expression option
    | Savepoint of Expression
    | ReleaseSavepoint of Expression
    | SetTransaction of bool * TransactionMode list
    | SetConstraints of Expression list option * bool
    | WithStatement of bool * Cte list * StatementKind

type Statement = { Kind: StatementKind; Pos: Position }
