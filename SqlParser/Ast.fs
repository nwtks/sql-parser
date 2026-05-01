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

type Literal =
    | String of string
    | NationalString of string
    | UnicodeString of string
    | Number of decimal
    | Bool of bool option
    | Date of string
    | Time of string
    | Timestamp of string
    | Interval of string * string
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

type LockingClause =
    | ForUpdate
    | ForShare

type WindowFrameUnit =
    | Rows
    | Range

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
    | FunctionCall of Expression * bool * Expression list * WindowDefinition option
    | Cast of Expression * DataType
    | Case of Expression option * (Expression * Expression) list * Expression option
    | SubqueryExpression of Query
    | Star
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
    | Like of Expression * bool * Expression * Expression option
    | SimilarTo of Expression * bool * Expression * Expression option
    | Extract of Expression * Expression
    | Position of Expression * Expression * Expression option
    | Trim of string option * Expression option * Expression

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

and Cte =
    { Name: Expression
      Columns: Expression list option
      Query: Query }

and TableSourceKind =
    | Table of Expression * Expression option
    | Subquery of Query * Expression * Expression list option
    | ValuesTable of Expression list list * Expression * Expression list option
    | JoinedTable of JoinSource

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

and FetchClause =
    { Count: Expression
      IsPercent: bool
      WithTies: bool }

and SelectStatement =
    { IsDistinct: bool
      Columns: ColumnSource list
      From: TableSource option
      Where: Expression option
      GroupBy: Expression list
      Having: Expression option
      Window: (Expression * WindowDefinition) list
      OrderBy: (Expression * bool * NullsOrder option) list
      Offset: Expression option
      Fetch: FetchClause option
      Locking: LockingClause option }

type InsertSource =
    | Values of Expression list list
    | Query of Query

type InsertStatement =
    { Table: Expression
      Columns: Expression list option
      Source: InsertSource }

type UpdateStatement =
    { Table: Expression
      Set: (Expression * Expression) list
      Where: Expression option }

type DeleteStatement =
    { Table: Expression
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

type ColumnDefinition =
    { Name: Expression
      DataType: DataType
      IsNullable: bool option
      IsPrimaryKey: bool
      DefaultValue: Expression option }

type CreateTableStatement =
    { Table: Expression
      Columns: ColumnDefinition list }

type CreateIndexStatement =
    { Name: Expression
      Table: Expression
      Columns: Expression list
      Unique: bool }

type CreateViewStatement = { Name: Expression; Query: Query }

type DropStatement =
    | DropTable of Expression
    | DropIndex of Expression
    | DropView of Expression

type AlterTableAction =
    | AddColumn of ColumnDefinition
    | DropColumn of Expression

type AlterTableStatement =
    { Table: Expression
      Action: AlterTableAction }

type StatementKind =
    | Select of Query
    | Insert of InsertStatement
    | Update of UpdateStatement
    | Delete of DeleteStatement
    | Merge of MergeStatement
    | CreateTable of CreateTableStatement
    | CreateIndex of CreateIndexStatement
    | CreateView of CreateViewStatement
    | Drop of DropStatement
    | AlterTable of AlterTableStatement
    | Truncate of Expression
    | WithStatement of bool * Cte list * StatementKind

type Statement = { Kind: StatementKind; Pos: Position }
