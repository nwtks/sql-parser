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

type Literal =
    | String of string
    | Number of decimal
    | Bool of bool
    | Date of string
    | Time of string
    | Timestamp of string
    | Interval of string
    | Binary of byte[]
    | Null

type ExpressionKind =
    | Literal of Literal
    | Identifier of string
    | BinaryOp of BinaryOperator * Expression * Expression
    | UnaryOp of string * Expression
    | FunctionCall of string * Expression list
    | Star

and Expression = ExpressionKind * Position

type ColumnSource = Column of Expression * string option

type JoinType =
    | InnerJoin
    | LeftJoin
    | RightJoin
    | FullJoin

type SetOperator =
    | Union
    | UnionAll
    | Intersect
    | IntersectAll
    | Except
    | ExceptAll

type Cte =
    { Name: string
      Columns: string list option
      Query: Query }

and Query =
    | SelectQuery of SelectStatement
    | SetOperation of Query * SetOperator * Query

and TableSourceKind =
    | Table of string * string option
    | Subquery of Query * string

and TableSource = TableSourceKind * Position

and JoinSource =
    { JoinType: JoinType
      Table: TableSource
      On: Expression option }

and SelectStatement =
    { Columns: ColumnSource list
      From: TableSource option
      Joins: JoinSource list
      Where: Expression option
      GroupBy: Expression list
      Having: Expression option
      OrderBy: (Expression * bool) list
      Limit: int option }

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

type StatementKind =
    | Select of Query
    | Insert of InsertStatement
    | Update of UpdateStatement
    | Delete of DeleteStatement
    | Merge of MergeStatement
    | WithStatement of bool * Cte list * StatementKind

type Statement = StatementKind * Position
