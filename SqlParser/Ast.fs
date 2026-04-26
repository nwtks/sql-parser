namespace SqlParser.Ast

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
    | StringLike
    | SimilarTo

type UnaryOperator =
    | Not
    | Plus
    | Minus

type Literal =
    | String of string
    | NationalString of string
    | UnicodeString of string
    | Number of decimal
    | Bool of bool
    | Date of string
    | Time of string
    | Timestamp of string
    | Interval of string * string
    | Binary of byte[]
    | Null

type DataType =
    | Character of int option
    | Varchar of int option
    | CharacterLargeObject of int option
    | NationalCharacter of int option
    | NationalVarchar of int option
    | NationalCharacterLargeObject of int option
    | BinaryLargeObject of int option
    | Numeric of int option * int option
    | Decimal of int option * int option
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
    | RowType of (string * DataType) list
    | ArrayType of DataType * int option
    | MultisetType of DataType
    | UserDefinedType of string

type JoinType =
    | InnerJoin
    | LeftJoin
    | RightJoin
    | FullJoin
    | CrossJoin

type SetOperator =
    | Union of bool
    | Intersect of bool
    | Except of bool

type NullsOrder =
    | NullsFirst
    | NullsLast

type LockingClause =
    | ForUpdate
    | ForShare

type WindowFrameUnit =
    | Rows
    | Range

type ExpressionKind =
    | Literal of Literal
    | Identifier of string
    | BinaryOp of BinaryOperator * Expression * Expression
    | UnaryOp of UnaryOperator * Expression
    | FunctionCall of string * bool * Expression list * WindowDefinition option
    | Cast of Expression * DataType
    | Case of Expression option * (Expression * Expression) list * Expression option
    | SubqueryExpression of Query
    | Star
    | ColumnReference of string list

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
    { ExistingWindowName: string option
      PartitionBy: Expression list
      OrderBy: (Expression * bool * NullsOrder option) list
      Frame: WindowFrame option }

and Query =
    | SelectQuery of SelectStatement
    | SetOperation of Query * SetOperator * Query

and Cte =
    { Name: string
      Columns: string list option
      Query: Query }

and TableSourceKind =
    | Table of string * string option
    | Subquery of Query * string * string list option
    | ValuesTable of Expression list list * string * string list option
    | JoinedTable of JoinSource

and TableSource =
    { Kind: TableSourceKind; Pos: Position }

and JoinCondition =
    | On of Expression
    | Using of string list

and JoinSource =
    { JoinType: JoinType
      IsNatural: bool
      Left: TableSource
      Right: TableSource
      Condition: JoinCondition option }

and ColumnSource = Column of Expression * string option

and SelectStatement =
    { IsDistinct: bool
      Columns: ColumnSource list
      From: TableSource option
      Where: Expression option
      GroupBy: Expression list
      Having: Expression option
      Window: (string * WindowDefinition) list
      OrderBy: (Expression * bool * NullsOrder option) list
      Offset: Expression option
      Fetch: Expression option
      Locking: LockingClause option }

type InsertSource =
    | Values of Expression list list
    | Query of Query

type InsertStatement =
    { Table: string
      Columns: string list option
      Source: InsertSource }

type UpdateStatement =
    { Table: string
      Set: (string * Expression) list
      Where: Expression option }

type DeleteStatement =
    { Table: string
      Where: Expression option }

type MergeMatchCondition =
    | Matched
    | NotMatched

type MergeAction =
    | MergeUpdate of (string * Expression) list
    | MergeDelete
    | MergeInsert of string list option * Expression list

type MergeWhenClause =
    { MatchCondition: MergeMatchCondition
      Condition: Expression option
      Action: MergeAction }

type MergeStatement =
    { Target: string
      TargetAlias: string option
      Source: TableSource
      On: Expression
      WhenClauses: MergeWhenClause list }

type ColumnDefinition =
    { Name: string
      DataType: DataType
      IsNullable: bool option
      IsPrimaryKey: bool
      DefaultValue: Expression option }

type CreateTableStatement =
    { Table: string
      Columns: ColumnDefinition list }

type CreateIndexStatement =
    { Name: string
      Table: string
      Columns: string list
      Unique: bool }

type CreateViewStatement = { Name: string; Query: Query }

type DropStatement =
    | DropTable of string
    | DropIndex of string
    | DropView of string

type AlterTableAction =
    | AddColumn of ColumnDefinition
    | DropColumn of string

type AlterTableStatement =
    { Table: string
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
    | Truncate of string
    | WithStatement of bool * Cte list * StatementKind

type Statement = { Kind: StatementKind; Pos: Position }
