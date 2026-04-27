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

type SetOperatorKind =
    | Union
    | Intersect
    | Except

type SetOperator =
    { Kind: SetOperatorKind
      IsAll: bool
      IsDistinct: bool
      Corresponding: string list option option }

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
    | Parameter of string
    | WindowFunction of WindowFunction
    | ColumnReference of string list
    | Between of Expression * bool * bool * Expression * Expression // expr, isNot, isSymmetric, start, end
    | InList of Expression * bool * Expression list // expr, isNot, list
    | InSubquery of Expression * bool * Query // expr, isNot, subquery
    | IsNull of Expression * bool // expr, isNot
    | IsBoolean of Expression * bool * bool option // expr, isNot, value (Some true=TRUE, Some false=FALSE, None=UNKNOWN)
    | Exists of Query
    | Unique of Query
    | IsDistinctFrom of Expression * bool * Expression // l, isNot, r
    | Overlaps of Expression * Expression
    | Like of Expression * bool * Expression * Expression option // source, isNot, pattern, escape
    | SimilarTo of Expression * bool * Expression * Expression option // source, isNot, pattern, escape
    | Extract of string * Expression // field, source
    | Position of Expression * Expression * string option // target, source, unit
    | Trim of string option * Expression option * Expression // specification, character, source

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

and WindowFunction =
    { Function: string
      Args: Expression list
      IsDistinct: bool
      Window: WindowDefinition }

and Query =
    | SelectQuery of SelectStatement
    | SetOperation of Query * SetOperator * Query
    | WithQuery of bool * Cte list * Query

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
      Window: (string * WindowDefinition) list
      OrderBy: (Expression * bool * NullsOrder option) list
      Offset: Expression option
      Fetch: FetchClause option
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
