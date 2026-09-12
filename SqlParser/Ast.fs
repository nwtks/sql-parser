namespace SqlParser

type Position = { Line: int64; Column: int64 }
type ParseError = ParseError of string * Position

// 8.2 <comp op> / 6.29 <term> <factor> / <concatenation> / <boolean value expression>
// <binary operator> ::= + | - | * | / | = | <> | < | <= | > | >= | AND | OR | ||
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

// 6.29 <sign> / 8.x <boolean factor>
// <unary operator> ::= NOT | + | -
type UnaryOperator =
    | Not
    | Plus
    | Minus

// 8.9 <quantifier> ::= ALL | ANY | SOME
type Quantifier =
    | Any
    | SomeQuantifier
    | All

// 5.3 <date literal> / <date string>
type DateValue = { Year: int; Month: int; Day: int }

// 5.3 <time literal> / <time string> — <time zone interval>
type TimeZoneOffset = { Sign: int; Hours: int; Minutes: int }

// 5.3 <time literal> / <time string>
type TimeValue =
    { Hour: int
      Minute: int
      Second: decimal
      TzOffset: TimeZoneOffset option }

// 5.3 <timestamp literal> / <timestamp string>
type TimestampValue = { Date: DateValue; Time: TimeValue }

// 10.1 <interval qualifier> — <datetime field>
type DateTimeField =
    | Year
    | Month
    | Day
    | Hour
    | Minute
    | Second

// 10.1 <interval leading field precision> ::= <unsigned integer>
// 10.1 <interval fractional seconds precision> ::= <unsigned integer>
// Precision attached to an <interval qualifier> field. `Leading` is the <interval leading field
// precision> (applies to the start field / a single non-second field), `FractionalSeconds` is the
// <interval fractional seconds precision> (applies to a SECOND end field).
type IntervalPrecision =
    { Leading: int option
      FractionalSeconds: int option }

// 10.1 <interval qualifier> ::= <start field> TO <end field> | <single datetime field>
type IntervalQualifier =
    | SingleField of DateTimeField * IntervalPrecision option
    | Range of DateTimeField * DateTimeField * IntervalPrecision option

// 5.3 <interval literal> / <interval string>
type IntervalValue =
    { IsNegative: bool
      ValueString: string
      Qualifier: IntervalQualifier }

// 5.3 <literal> / <general literal> — <character string literal> | <national character string literal>
//     | <Unicode character string literal> | <binary string literal> | <exact numeric literal>
//     | <boolean literal> | <date literal> | <time literal> | <timestamp literal> | <interval literal> | <null>
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

// 7.10 <join type> ::= INNER | <outer join type> [ OUTER ]  (+ 7.10 <cross join> ::= CROSS JOIN)
type JoinType =
    | InnerJoin
    | LeftJoin
    | RightJoin
    | FullJoin
    | CrossJoin

// 7.17 <query expression body> — <set operator> ::= UNION | EXCEPT | INTERSECT
type SetOperatorKind =
    | Union
    | Intersect
    | Except

// 10.10 <sort specification> ::= ... [ NULLS { FIRST | LAST } ]
type NullsOrder =
    | NullsFirst
    | NullsLast

// 7.15 <window frame units> ::= ROWS | RANGE | GROUPS
type WindowFrameUnit =
    | Rows
    | Range
    | Groups

// 6.32 <trim specification> ::= LEADING | TRAILING | BOTH
type TrimSpecification =
    | Both
    | Leading
    | Trailing

// 6.1 <data type> ::= <predefined type> | <row type> | <reference type> | <collection type>
// <predefined type> ::= <character string type> | <binary string type> | <numeric type>
//     | <boolean type> | <datetime type> | <interval type>
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
    // 6.1 <row type> ::= ROW <row type body>
    | RowType of (Expression * DataType) list
    // 6.1 <collection type> ::= <array type> | <multiset type>
    | ArrayType of DataType * int option
    | MultisetType of DataType
    | UserDefinedType of Expression
    // 6.1 <reference type> ::= REF ( <referenced type> ) [ SCOPE <table name> ]
    | ReferenceType of DataType * Expression option

// 6.28 <value expression> / 6.29 <numeric value expression> / 6.31 <string value expression>
// 8.x <predicate> — the full expression grammar
and ExpressionKind =
    // 5.3 <literal> / <general literal>
    | Literal of Literal
    // 6.7 <column reference> | <identifier>
    | Identifier of string
    // 6.29 <term> | <factor> | 8.2 <comp op> | 6.31 <concatenation>
    | BinaryOp of BinaryOperator * Expression * Expression
    // 5.3 <signed numeric literal> | 6.39 <boolean factor>
    | UnaryOp of UnaryOperator * Expression
    // 10.9 <aggregate function> / 6.10 <window function> / 10.4 <routine invocation>
    | FunctionCall of
        Expression *
        bool *
        Expression list *
        WindowDefinition option *
        Expression option *
        (Expression * bool * NullsOrder option) list option
    // 6.13 <cast specification> ::= CAST ( <value expression> AS <data type> )
    | Cast of Expression * DataType
    // 6.12 <case expression> ::= CASE ... END
    | Case of Expression option * (Expression * Expression) list * Expression option
    // 7.19 <scalar subquery> ::= <subquery>
    | SubqueryExpression of Query
    // 7.1 <explicit row value constructor> ::= ( <row value constructor element> <comma> <list> ) | ROW ( <list> )
    | RowValueConstructor of Expression list
    // 7.16 <asterisk> ::= *
    | Star
    // 7.16 <qualified asterisk> ::= <identifier chain> . *
    | QualifiedStar of string list
    // 7.16 <all fields reference> ::= <value expression primary> . * [ AS ( <column list> ) ]
    | AllFieldsReference of Expression * Expression list option
    // 6.4 <dynamic parameter specification> / <host parameter specification>
    | Parameter of string
    // 20.16 <descriptor value constructor> ::= DESCRIPTOR ( <descriptor column list> )
    | DescriptorValueConstructor of (Expression * DataType option) list
    // 6.10 <window function> ::= <window function type> OVER <window name or specification>
    | WindowFunction of WindowFunction
    // 6.7 <column reference> ::= [ <table name> . ] <column name>
    | ColumnReference of string list
    // 8.3 <between predicate> ::= <row value predicand> [ NOT ] BETWEEN [ ASYMMETRIC | SYMMETRIC ] <left> AND <right>
    | Between of Expression * bool * bool * Expression * Expression
    // 8.4 <in predicate> — <in predicate value> IN <in predicate value list>
    | InList of Expression * bool * Expression list
    // 8.4 <in predicate> — <row value predicand> [ NOT ] IN <table subquery>
    | InSubquery of Expression * bool * Query
    // 8.8 <null predicate> ::= <row value predicand> IS [ NOT ] NULL
    | IsNull of Expression * bool
    // 6.39 <boolean test> ::= <boolean predicand> IS [ NOT ] [ <truth value> ]
    | IsBoolean of Expression * bool * bool option
    // 8.10 <exists predicate> ::= EXISTS <table subquery>
    | Exists of Query
    // 8.11 <unique predicate> ::= UNIQUE <table subquery>
    | Unique of Query
    // 8.15 <distinct predicate> ::= <row value predicand> IS [ NOT ] DISTINCT FROM <row value predicand>
    | IsDistinctFrom of Expression * bool * Expression
    // 8.14 <overlaps predicate> ::= <row value predicand> OVERLAPS <row value predicand>
    | Overlaps of Expression * Expression
    // 8.9 <quantified comparison predicate> ::= <row value predicand> <comp op> <quantifier> <table subquery>
    | QuantifiedComparison of BinaryOperator * Quantifier * Expression * Query
    // 8.9 <quantified comparison predicate> — row value constructor <quantifier> <table subquery>
    | QuantifiedSubquery of Quantifier * Query
    // 10.7 <collate clause> ::= COLLATE <collation name>
    | Collate of Expression * Expression
    // 8.5 <like predicate> ::= <character string predicand> [ NOT ] LIKE <pattern> [ ESCAPE <escape> ]
    | Like of Expression * bool * Expression * Expression option
    // 8.6 <similar predicate> ::= <character string predicand> [ NOT ] SIMILAR TO <pattern> [ ESCAPE <escape> ]
    | SimilarTo of Expression * bool * Expression * Expression option
    // 6.30 <extract expression> ::= EXTRACT ( <extract field> FROM <extract source> )
    | Extract of Expression * Expression
    // 6.30 <character position expression> ::= POSITION ( ... IN ... [ USING ... ] )
    | Position of Expression * Expression * Expression option
    // 6.32 <trim function> ::= TRIM ( [ [ <trim specification> ] [ <trim character> ] FROM ] <trim source> )
    | Trim of TrimSpecification option * Expression option * Expression
    // 6.36 <current date value function> / <current time/timestamp/localtime/localtimestamp>
    | CurrentDate
    | CurrentTime of int option
    | CurrentTimestamp of int option
    | LocalTime of int option
    | LocalTimestamp of int option
    // 6.32 <character substring function> ::= SUBSTRING ( <character value expression> FROM <start> [ FOR <length> ] [ USING ... ] )
    | Substring of Expression * Expression * Expression option * string option
    // 6.32 <character overlay function> ::= OVERLAY ( <character value expression> PLACING <replacement> FROM <start> [ FOR <length> ] )
    | Overlay of Expression * Expression * Expression * Expression option
    // 6.5 <default specification> ::= DEFAULT
    | Default
    // 6.14 <next value expression> ::= NEXT VALUE FOR <sequence generator name>
    | NextValueFor of Expression
    // 6.4 <general value specification> keyword forms
    | CurrentCatalog
    | CurrentDefaultTransformGroup
    | CurrentPath
    | CurrentRole
    | CurrentSchema
    | CurrentTransformGroupForType of Expression
    | CurrentUser
    | SessionUser
    | SystemUser
    | User
    | Value
    | CollationFor of Expression
    // 6.42 <array value constructor> / 6.45 <multiset value constructor>
    | ArrayConstructor of Expression list
    | ArrayQuery of Query
    | MultisetConstructor of Expression list
    | MultisetQuery of Query
    // 6.45 <multiset value constructor> ::= ... | <table value constructor by query>
    //   <table value constructor by query> ::= TABLE <table subquery>
    | TableQuery of Query
    // 6.24 <array element reference> ::= <array value expression> [ <numeric value expression> ]
    | ArrayElement of Expression * Expression
    // 6.16 <subtype treatment> / 6.23 <reference resolution> / 6.25 <multiset element reference>
    | Treat of Expression * DataType
    | Deref of Expression
    | Element of Expression
    // 6.17 <method invocation> (direct form) / 6.18 <static method invocation>
    // 6.19 <new specification> / 6.15 <field reference>
    | MethodInvocation of Expression * Expression * Expression list
    | StaticMethodInvocation of Expression * Expression * Expression list
    | NewSpecification of Expression * Expression list
    | FieldReference of Expression * Expression
    // 8.12 <normalized predicate> / 8.19 <type predicate> / 8.22 <JSON predicate>
    | IsNormalized of Expression * bool * NormalForm option
    | IsOfType of Expression * bool * TypeSpec list
    | IsJson of Expression * bool * JsonTypeConstraint option * bool option
    // 8.7 <regex like predicate> / 8.13 <match predicate>
    | RegexLike of Expression * bool * Expression * Expression option
    | Match of Expression * bool * MatchOption option * Query
    // 8.16 <member predicate> / 8.17 <submultiset predicate> / 8.18 <set predicate>
    | MemberOf of Expression * bool * Expression
    | SubmultisetOf of Expression * bool * Expression
    | IsSet of Expression * bool
    // 8.20 <period predicate> (OVERLAPS is covered by the existing Overlaps case)
    | PeriodPredicate of PeriodPredicateKind * Expression * Expression
    // 8.20 <period predicand> ::= <period reference> | PERIOD ( <start> , <end> )
    | PeriodValue of Expression * Expression
    // 8.23 <JSON exists predicate>
    | JsonExists of JsonApiCommon * JsonExistsErrorBehavior option
    // 6.27 <JSON value function> / 6.34 <JSON query> / 6.33 <JSON value constructor>
    //     / 10.11 <JSON aggregate function>
    | JsonValue of JsonApiCommon * JsonReturning option * JsonValueBehavior option * JsonValueBehavior option
    | JsonQuery of
        JsonApiCommon *
        JsonOutput option *
        JsonQueryWrapper option *
        JsonQueryQuotes option *
        JsonQueryBehavior option *
        JsonQueryBehavior option
    | JsonObject of JsonNameValue list * JsonConstructorNull option * bool option * JsonOutput option
    | JsonArray of Expression list * JsonConstructorNull option * JsonOutput option
    | JsonObjectAgg of JsonNameValue * JsonConstructorNull option * bool option * JsonOutput option
    | JsonArrayAgg of
        Expression *
        (Expression * bool * NullsOrder option) list option *
        JsonConstructorNull option *
        JsonOutput option
    // 6.11 <nested window function> ::= <nested row number function> | <value_of expression at row>
    | NestedRowNumber of RowMarker
    | ValueOf of Expression * RowMarkerExpression * Expression option
    // 6.9 <set function specification> ::= [ <running or final> ] <aggregate function> | <grouping operation>
    | SetFunction of RunningOrFinal option * Expression
    | Grouping of Expression list
    // 6.17 <generalized invocation> ::= ( <value expression primary> AS <data type> ) <period> <method name> [ <SQL argument list> ]
    | GeneralizedInvocation of Expression * DataType * Expression * Expression list option
    // 6.20 <attribute or method reference> / 6.21 <dereference operation> / 6.22 <method reference>
    //   <value expression primary> <dereference operator> <qualified identifier> [ <SQL argument list> ]
    //   (no argument list = 6.21 attribute access, argument list = 6.22 method reference)
    | Dereference of Expression * Expression * Expression list option
    // 6.26 <row pattern navigation operation>
    | RowPatternNavigation of RowPatternNavigation
    // 6.30 <length expression> ::= <char length expression> | <octet length expression>
    | LengthExpression of LengthFunction * Expression * string option
    // 6.30 <numeric value function> — the built-ins of the shape <name> ( <args> )
    | NumericValueFunction of NumericFunction * Expression list
    // 6.30 <regex occurrences function> / <regex position expression>
    | RegexOccurrences of RegexArgument
    | RegexPosition of RegexStart option * RegexArgument
    // 6.32 <regular expression substring function> ::= SUBSTRING ( <src> SIMILAR <pattern> ESCAPE <escape> )
    | SubstringSimilar of Expression * Expression * Expression
    // 6.32 <fold> ::= { UPPER | LOWER } ( <character value expression> )
    | Fold of FoldFunction * Expression
    // 6.32 <transcoding> ::= CONVERT ( <character value expression> USING <transcoding name> )
    | Transcoding of Expression * Expression
    // 6.32 <character transliteration> ::= TRANSLATE ( <character value expression> USING <transliteration name> )
    | CharacterTransliteration of Expression * Expression
    // 6.32 <regex substring function> / <regex transliteration>
    | RegexSubstring of RegexArgument
    | RegexTransliterate of RegexArgument
    // 6.32 <normalize function> ::= NORMALIZE ( <character value expression> [ , <normal form> [ , <result length> ] ] )
    | NormalizeFunction of Expression * NormalForm option * Expression option
    // 6.32 <specific type method> ::= <user-defined type value expression> <period> SPECIFICTYPE [ ( ) ]
    | SpecificTypeMethod of Expression * bool
    // 6.32 <classifier function> ::= CLASSIFIER ( [ <row pattern variable name> ] )
    | Classifier of Expression option
    // 6.35 <time zone> ::= AT <time zone specifier>
    | AtTimeZone of Expression * TimeZoneSpecifier
    // 6.37 <interval value expression> ::= ... | ( <datetime value expression> <minus sign> <datetime term> ) <interval qualifier>
    | DatetimeDifference of Expression * Expression * IntervalQualifier
    // 6.37 <interval primary> ::= <value expression primary> [ <interval qualifier> ]
    // Only built when an <interval qualifier> is present; a qualifier-less
    // <interval primary> is represented by its <value expression primary>.
    | IntervalPrimary of Expression * IntervalQualifier
    // 6.41 <trim array function> ::= TRIM_ARRAY ( <array value expression> , <numeric value expression> )
    | TrimArray of Expression * Expression
    // 6.43 <multiset value expression> ::= ... MULTISET { UNION | INTERSECT | EXCEPT } [ ALL | DISTINCT ] ...
    | MultisetSetOperation of MultisetSetOperator * bool option * Expression * Expression
    // 6.44 <multiset set function> ::= SET ( <multiset value expression> )
    | MultisetSetFunction of Expression

// 6.28 <value expression> — wrapper carrying source position
and Expression = { Kind: ExpressionKind; Pos: Position }

// 6.9 / 6.26 <running or final> ::= RUNNING | FINAL
// (qualify as RunningOrFinal.Running / RunningOrFinal.Final — the bare case names
//  clash with ResultOption.Final and TypeOption.Final)
and RunningOrFinal =
    | Running
    | Final

// 6.30 <length expression> ::= <char length expression> | <octet length expression>
and LengthFunction =
    | CharLength
    | CharacterLength
    | OctetLength

// 6.30 <numeric value function> — the built-ins of the shape <name> ( <args> )
and NumericFunction =
    | Cardinality
    | ArrayMaxCardinality
    | AbsoluteValue
    | Modulus
    | Sin
    | Cos
    | Tan
    | Sinh
    | Cosh
    | Tanh
    | Asin
    | Acos
    | Atan
    | GeneralLogarithm
    | CommonLogarithm
    | NaturalLogarithm
    | Exponential
    | Power
    | SquareRoot
    | Floor
    | Ceiling
    | WidthBucket
    | MatchNumber

// 6.30 <regex occurrences function> / <regex position expression>
// 6.32 <regex substring function> / <regex transliteration>
// — all four share the shape
//   <pattern> [ FLAG <flag> ] IN <subject> [ WITH <replacement> ] [ FROM <start> ]
//   [ USING <char length units> ] [ OCCURRENCE <occurrence> ] [ GROUP <capture group> ]
// `<WITH>` and `<GROUP>`/`<OCCURRENCE>` are not part of every one of the four rules;
// the shared parser accepts the superset (see docs/trade-off.md).
and RegexArgument =
    { Pattern: Expression
      Flag: Expression option
      Subject: Expression
      Replacement: Expression option
      From: Expression option
      Using: string option
      Occurrence: RegexOccurrence option
      CaptureGroup: Expression option }

// 6.30 <regex occurrence> ::= <numeric value expression>
// 6.32 <regex transliteration occurrence> ::= <regex occurrence> | ALL
and RegexOccurrence =
    | RegexOccurrenceNumber of Expression
    | RegexOccurrenceAll

// 6.30 <regex position start or after> ::= START | AFTER
and RegexStart =
    | RegexStartOfString
    | RegexAfterMatch

// 6.32 <fold> ::= { UPPER | LOWER } ( <character value expression> )
and FoldFunction =
    | FoldUpper
    | FoldLower

// 6.35 <time zone specifier> ::= LOCAL | TIME ZONE <interval primary>
and TimeZoneSpecifier =
    | TimeZoneLocal
    | TimeZoneOffset of Expression

// 6.26 <row pattern navigation operation> ::= <logical> | <physical> | <compound>
and RowPatternNavigation =
    | Logical of RunningOrFinal option * FirstOrLast * Expression * Expression option
    | Physical of PrevOrNext * Expression * Expression option
    | Compound of PrevOrNext * RunningOrFinal option * FirstOrLast * Expression * Expression option * Expression option

// 6.26 <first or last> ::= FIRST | LAST
// (qualify as FirstOrLast.First / FirstOrLast.Last — Direction already owns those names)
and FirstOrLast =
    | First
    | Last

// 6.26 <prev or next> ::= PREV | NEXT
and PrevOrNext =
    | Prev
    | Next

// 6.43 <multiset value expression> — <set operator> of the multiset form
and MultisetSetOperator =
    | MultisetUnion
    | MultisetIntersect
    | MultisetExcept

// 8.12 <normal form> ::= NFC | NFD | NFKC | NFKD
and NormalForm =
    | Nfc
    | Nfd
    | Nfkc
    | Nfkd

// 8.19 <user-defined type specification> — inclusive (plain name) or exclusive (ONLY name)
and TypeSpec =
    | Inclusive of Expression
    | Exclusive of Expression

// 8.22 <JSON predicate type constraint> ::= VALUE | ARRAY | OBJECT | SCALAR
and JsonTypeConstraint =
    | JsonTypeValue
    | JsonTypeArray
    | JsonTypeObject
    | JsonTypeScalar

// 8.13 <match option> ::= SIMPLE | PARTIAL | FULL
and MatchOption =
    | Simple
    | Partial
    | Full

// 8.20 <period predicate> operators (OVERLAPS is covered by the existing Overlaps case)
and PeriodPredicateKind =
    | PeriodEquals
    | PeriodContains
    | PeriodPrecedes
    | PeriodSucceeds
    | PeriodImmediatelyPrecedes
    | PeriodImmediatelySucceeds

// 10.14 <JSON API common syntax> ::= <JSON context item> , <JSON path specification>
//     [ AS <JSON table path name> ] [ <JSON passing clause> ]
// <JSON path specification> is a <character string literal>, so Path is a plain string.
and JsonApiCommon =
    { Context: Expression
      Path: string
      PathName: Expression option
      Passing: (Expression * Expression) list }

// 6.27 <JSON returning clause> ::= RETURNING <data type>
and JsonReturning = DataType

// 6.27 <JSON value empty/error behavior> ::= ERROR | NULL | DEFAULT <value expression>
and JsonValueBehavior =
    | JsonError
    | JsonNull
    | JsonDefault of Expression

// 10.13 <JSON output clause> ::= RETURNING <data type> [ FORMAT <JSON representation> ]
and JsonOutput =
    { Returning: DataType
      Format: JsonRepresentation option }

// 10.12 <JSON representation> ::= JSON [ ENCODING { UTF8 | UTF16 | UTF32 } ]
and JsonRepresentation = JsonEncoding of JsonEncoding option

and JsonEncoding =
    | Utf8
    | Utf16
    | Utf32

// 6.34 <JSON query wrapper behavior> ::= WITHOUT [ ARRAY ] | WITH [ CONDITIONAL | UNCONDITIONAL ] [ ARRAY ]
and JsonQueryWrapper =
    { WithWrapper: bool
      Conditional: bool option
      Array: bool }

// 6.34 <JSON query quotes behavior> ::= KEEP | OMIT
and JsonQueryQuotes =
    | Keep
    | Omit

// 6.34 <JSON query empty/error behavior> ::= ERROR | NULL | EMPTY ARRAY | EMPTY OBJECT
and JsonQueryBehavior =
    | JsonQueryError
    | JsonQueryNull
    | JsonQueryEmptyArray
    | JsonQueryEmptyObject

// 6.33 <JSON name and value> ::= [ KEY ] <JSON name> VALUE <JSON value expression>
//                              | <JSON name> : <JSON value expression>
and JsonNameValue =
    { Name: Expression
      Value: Expression
      Key: bool }

// 6.33 <JSON constructor null clause> ::= NULL ON NULL | ABSENT ON NULL
and JsonConstructorNull =
    | JsonNullOnNull
    | JsonAbsentOnNull

// 8.23 <JSON exists error behavior> ::= TRUE | FALSE | UNKNOWN | ERROR
and JsonExistsErrorBehavior =
    | JsonExistsTrue
    | JsonExistsFalse
    | JsonExistsUnknown
    | JsonExistsError

// 6.11 <row marker> ::= BEGIN_PARTITION | BEGIN_FRAME | CURRENT_ROW | FRAME_ROW
//     | END_FRAME | END_PARTITION
and RowMarker =
    | BeginPartition
    | BeginFrame
    | CurrentRow
    | FrameRow
    | EndFrame
    | EndPartition

// 6.11 <row marker expression> ::= <row marker> [ <row marker delta> ]
// 6.11 <row marker delta> ::= + <row marker offset> | - <row marker offset>
and RowMarkerExpression =
    { Marker: RowMarker
      // Some(true, n) = +n, Some(false, n) = -n
      Delta: (bool * Expression) option }

// 7.8 <row pattern measure definition> ::= <row pattern measure expression> AS <measure name>
and RowPatternMeasure =
    { Expression: Expression
      Name: Expression }

// 7.7 <row pattern rows per match> ::= ONE ROW PER MATCH
//     | ALL ROWS PER MATCH [ <row pattern empty match handling> ]
and RowPatternRowsPerMatch =
    | OneRowPerMatch
    | AllRowsPerMatch of RowPatternEmptyMatchHandling option

// 7.7 <row pattern empty match handling> ::= SHOW EMPTY MATCHES | OMIT EMPTY MATCHES
//     | WITH UNMATCHED ROWS
and RowPatternEmptyMatchHandling =
    | ShowEmptyMatches
    | OmitEmptyMatches
    | WithUnmatchedRows

// 7.9 <row pattern skip to> ::= SKIP TO NEXT ROW | SKIP PAST LAST ROW
//     | SKIP TO FIRST <var> | SKIP TO LAST <var> | SKIP TO <var>
and RowPatternSkipTo =
    | SkipToNextRow
    | SkipPastLastRow
    | SkipToFirst of Expression
    | SkipToLast of Expression
    | SkipTo of Expression

// 7.9 <row pattern> ::= <row pattern term> | <row pattern alternation>
// Represented as a list of terms separated by | (alternation).
and RowPattern = { Terms: RowPatternTerm list }

// 7.9 <row pattern term> ::= <row pattern factor> | <row pattern term> <row pattern factor>
// Represented as a list of factors (concatenation).
and RowPatternTerm = { Factors: RowPatternFactor list }

// 7.9 <row pattern factor> ::= <row pattern primary> [ <row pattern quantifier> ]
and RowPatternFactor =
    { Primary: RowPatternPrimary
      Quantifier: RowPatternQuantifier option }

// 7.9 <row pattern quantifier>
and RowPatternQuantifier =
    | Star of bool // * [ ? ]
    | Plus of bool // + [ ? ]
    | Question of bool // ? [ ? ]
    | Brace of Expression option * Expression option * bool // { [ n ] , [ m ] } [ ? ]
    | BraceExact of Expression // { n }

// 7.9 <row pattern primary>
and RowPatternPrimary =
    | RowPatternVariable of Expression
    | RowPatternAnchorStart // ^
    | RowPatternAnchorEnd // $
    | RowPatternGroup of RowPattern option // ( [ <row pattern> ] )
    | RowPatternExclude of RowPattern // {- <row pattern> -}
    | RowPatternPermute of RowPattern list // PERMUTE ( <row pattern> [ , ... ] )

// 7.9 <row pattern subset item> ::= <var> = ( <var> [ , <var> ]... )
and RowPatternSubset =
    { Name: Expression
      Variables: Expression list }

// 7.9 <row pattern definition> ::= <var> AS <search condition>
and RowPatternDefinition =
    { Name: Expression
      Condition: Expression }

// 7.9 <row pattern common syntax> ::= [ AFTER MATCH <skip to> ] [ INITIAL | SEEK ]
//     PATTERN ( <row pattern> ) [ <subset clause> ] DEFINE <definition list>
and RowPatternCommon =
    { AfterMatch: RowPatternSkipTo option
      // Some true = INITIAL, Some false = SEEK
      InitialOrSeek: bool option
      Pattern: RowPattern
      Subset: RowPatternSubset list
      Define: RowPatternDefinition list }

// 7.7 <row pattern recognition clause> ::= MATCH_RECOGNIZE ( [ <partition by> ]
//     [ <order by> ] [ <measures> ] [ <rows per match> ] <common syntax> )
and RowPatternRecognition =
    { PartitionBy: Expression list
      OrderBy: (Expression * bool * NullsOrder option) list
      Measures: RowPatternMeasure list
      RowsPerMatch: RowPatternRowsPerMatch option
      Common: RowPatternCommon }

// 7.11 <JSON table column definition>
and JsonTableColumn =
    | JsonOrdinality of Expression // <column name> FOR ORDINALITY
    | JsonRegular of JsonRegularColumn
    | JsonFormatted of JsonFormattedColumn
    | JsonNested of JsonNestedColumns // NESTED [ PATH ] <path> [ AS <name> ] COLUMNS (...)
    | JsonChaining of Expression // <column name> FOR CHAINING (primitive only)

// 7.11 <JSON table regular column definition> ::= <column name> <data type>
//     [ PATH <path> ] [ <empty behavior> ON EMPTY ] [ <error behavior> ON ERROR ]
// <JSON table column path specification> is a <JSON path specification> (character string literal),
// so Path is a plain string.
and JsonRegularColumn =
    { Name: Expression
      DataType: DataType
      Path: string option
      OnEmpty: JsonColumnBehavior option
      OnError: JsonColumnBehavior option }

// 7.11 <JSON table column empty/error behavior> ::= ERROR | NULL | DEFAULT <value expression>
//     (formatted columns additionally allow EMPTY ARRAY | EMPTY OBJECT)
and JsonColumnBehavior =
    | JsonColumnError
    | JsonColumnNull
    | JsonColumnDefault of Expression
    | JsonColumnEmptyArray
    | JsonColumnEmptyObject

// 7.11 <JSON table formatted column definition> ::= <column name> <data type>
//     FORMAT <JSON representation> [ PATH <path> ] [ <wrapper> WRAPPER ]
//     [ <quotes> QUOTES [ ON SCALAR STRING ] ] [ <empty> ON EMPTY ] [ <error> ON ERROR ]
and JsonFormattedColumn =
    { Name: Expression
      DataType: DataType
      Format: JsonRepresentation
      Path: string option
      Wrapper: JsonQueryWrapper option
      Quotes: JsonQueryQuotes option
      OnEmpty: JsonColumnBehavior option
      OnError: JsonColumnBehavior option }

// 7.11 <JSON table nested columns> ::= NESTED [ PATH ] <path> [ AS <name> ] <columns clause>
// <JSON table nested path specification> is a <JSON path specification> (character string literal).
and JsonNestedColumns =
    { Path: string
      Name: Expression option
      Columns: JsonTableColumn list }

// 7.11 <JSON table plan>
and JsonTablePlan =
    | JsonPlanName of Expression
    | JsonPlanOuter of Expression * JsonTablePlanPrimary
    | JsonPlanInner of Expression * JsonTablePlanPrimary
    | JsonPlanUnion of JsonTablePlanPrimary list
    | JsonPlanCross of JsonTablePlanPrimary list
    | JsonPlanDefault of JsonDefaultPlanChoices

// 7.11 <JSON table plan primary> ::= <path name> | ( <plan> )
and JsonTablePlanPrimary =
    | JsonPlanPrimaryName of Expression
    | JsonPlanPrimaryGroup of JsonTablePlan

// 7.11 <JSON table default plan choices>
and JsonDefaultPlanChoices =
    { InnerOuter: string option // INNER | OUTER
      UnionCross: string option } // UNION | CROSS

// 7.11 <JSON table error behavior> ::= ERROR | EMPTY
and JsonTableErrorBehavior =
    | JsonTableError
    | JsonTableEmpty

// 7.11 <JSON table> ::= JSON_TABLE ( <JSON API common syntax> <columns clause>
//     [ <plan clause> ] [ <error behavior> ON ERROR ] )
and JsonTableStatement =
    { Common: JsonApiCommon
      Columns: JsonTableColumn list
      Plan: JsonTablePlan option
      OnError: JsonTableErrorBehavior option }

// 7.15 <window frame exclusion> ::= EXCLUDE CURRENT ROW | EXCLUDE GROUP | EXCLUDE TIES | EXCLUDE NO OTHERS
and WindowFrameExclusion =
    | ExcludeCurrentRow
    | ExcludeGroup
    | ExcludeTies
    | ExcludeNoOthers

// 7.15 <window frame bound> ::= UNBOUNDED PRECEDING | <value> PRECEDING | CURRENT ROW | <value> FOLLOWING | UNBOUNDED FOLLOWING
and WindowFrameBound =
    | UnboundedPreceding
    | Preceding of Expression
    | CurrentRow
    | Following of Expression
    | UnboundedFollowing

// 7.15 <window frame clause> ::= [ <row pattern measures> ] <units> <extent> [ <exclusion> ] [ <row pattern common syntax> ]
and WindowFrame =
    { Unit: WindowFrameUnit
      Start: WindowFrameBound
      End: WindowFrameBound option
      Exclusion: WindowFrameExclusion option
      // 7.15 <window frame clause> ::= [ <row pattern measures> ] <units> <extent>
      //     [ <exclusion> ] [ <row pattern common syntax> ]
      Measures: RowPatternMeasure list option
      RowPattern: RowPatternCommon option }

// 7.15 <window definition> ::= <name> AS <window specification>
// 7.15 <window specification> ::= ( [ <existing window name> ] [ PARTITION BY ... ] [ ORDER BY ... ] [ <window frame clause> ] )
and WindowDefinition =
    { ExistingWindowName: Expression option
      PartitionBy: Expression list
      OrderBy: (Expression * bool * NullsOrder option) list
      Frame: WindowFrame option }

// 6.10 <window function> ::= <window function type> OVER <window name or specification>
and WindowFunction =
    { Function: Expression
      Args: Expression list
      IsDistinct: bool
      Window: WindowDefinition }

// 7.17 <query expression body> — UNION / EXCEPT / INTERSECT [ ALL | DISTINCT ] [ CORRESPONDING [ BY (...) ] ]
and SetOperator =
    { Kind: SetOperatorKind
      IsAll: bool
      IsDistinct: bool
      Corresponding: Expression list option option }

// 7.17 <query expression> ::= [ <with clause> ] <query expression body> [ <order by> ] [ <offset> ] [ <fetch> ]
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

// 7.18 <search clause> ::= SEARCH { DEPTH FIRST | BREADTH FIRST } BY <cols> SET <col>
and SearchClause =
    { IsDepthFirst: bool
      OrderBy: Expression list
      SetColumn: Expression }

// 7.18 <cycle clause> ::= CYCLE <cols> SET <col> TO <mark> DEFAULT <default> USING <path>
and CycleClause =
    { CycleColumns: Expression list
      SetColumn: Expression
      MarkValue: Expression
      DefaultValue: Expression
      PathColumn: Expression }

// 7.17 <with list element> ::= <query name> [ ( <column list> ) ] AS <table subquery> [ <search or cycle clause> ]
and Cte =
    { Name: Expression
      Columns: Expression list option
      Query: Query
      SearchClause: SearchClause option
      CycleClause: CycleClause option }

// 7.6 <query system time period specification>
// <query system time period specification> ::= FOR SYSTEM_TIME BETWEEN [ ASYMMETRIC | SYMMETRIC ] <p1> AND <p2>
and SystemTimeSymmetry =
    | Symmetric
    | Asymmetric

and SystemTimeSpec =
    | AsOf of Expression
    | Between of Expression * Expression * SystemTimeSymmetry option
    | FromTo of Expression * Expression

// 7.6 <result option> ::= FINAL | NEW | OLD
and ResultOption =
    | Final
    | New
    | Old

// 7.6 <table primary> / 7.10 <joined table> / 7.11 <JSON table> — table reference variants
and TableSourceKind =
    | Table of Expression * Expression option
    | Subquery of Query * Expression * Expression list option
    | ValuesTable of Expression list list * Expression * Expression list option
    | JoinedTable of JoinSource
    | Lateral of Query * Expression * Expression list option
    | Unnest of Expression * bool * Expression * Expression list option
    | TableSample of TableSource * string * Expression * Expression option
    | Only of Expression * Expression option * Expression list option
    | SystemTime of TableSource * SystemTimeSpec
    | TableFunction of Expression * Expression option * Expression list option
    | PtfTable of Expression * Expression option * Expression list option
    | DataChangeDelta of ResultOption * StatementKind * Expression option * Expression list option
    // 7.11 <JSON table> <correlation or recognition>
    | JsonTable of JsonTableStatement * (Expression * Expression list option) option
    // 7.11 <JSON table primitive> <correlation name>
    | JsonTablePrimitive of JsonTableStatement * Expression option
    // 7.6 <table or query name> <row pattern recognition clause and name>
    | MatchRecognize of
        Expression *
        (Expression * Expression list option) option *
        RowPatternRecognition *
        (Expression * Expression list option) option

// 7.6 <table primary> — wraps TableSourceKind with source position
and TableSource =
    { Kind: TableSourceKind; Pos: Position }

// 7.10 <join specification> ::= <join condition> | <named columns join>
and JoinCondition =
    | On of Expression
    | Using of Expression list

// 7.10 <joined table> ::= <cross join> | <qualified join> | <natural join>
// 7.10 <qualified join> / <natural join> / <partitioned join table> — join of two table references
and JoinSource =
    { JoinType: JoinType
      IsNatural: bool
      Left: TableSource
      Right: TableSource
      Condition: JoinCondition option
      // 7.10 <named columns join> USING (...) [ AS <join correlation name> ]
      UsingAlias: Expression option
      // 7.10 <partitioned join table> PARTITION BY ( <cols> )
      PartitionBy: Expression list option }

// 7.16 <derived column> ::= <value expression> [ <as clause> ]
and ColumnSource = Column of Expression * Expression option

// 7.13 <grouping element> ::= <ordinary grouping set> | <rollup list> | <cube list> | <grouping sets specification> | <empty grouping set>
and GroupingElement =
    | GroupingSet of Expression list
    | Rollup of GroupingElement list
    | Cube of GroupingElement list
    | GroupingSets of GroupingElement list
    | EmptyGroupingSet

// 7.17 <fetch first clause> ::= FETCH { FIRST | NEXT } [ <fetch quantity> ] { ROW | ROWS } { ONLY | WITH TIES }
and FetchClause =
    { Count: Expression
      IsPercent: bool
      WithTies: bool }

// 7.16 <query specification> ::= SELECT [ <set quantifier> ] <select list> <table expression>
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

// 14.11 <insert statement> — <from constructor> / <from subquery> / DEFAULT VALUES
and InsertSource =
    | Values of Expression list list
    | Query of Query
    | DefaultValues

// 14.11 <insert statement> ::= INSERT INTO <table name> [ <insert column list> ] [ <overriding clause> ] <insert source>
and InsertStatement =
    { Table: Expression
      Columns: Expression list option
      Source: InsertSource
      Override: bool option }

// 14.15 <set clause> ::= <set clause> | <multiple column assignment> | <mutated set clause>
and SetClause =
    | SingleSet of Expression * Expression
    | MultipleSet of Expression list * Expression list
    // 14.15 <mutated set clause> ::= <mutated target> <period> <method name>
    //                      <equals operator> <update source>
    // MutatedSet (mutated target, method name, value)
    | MutatedSet of Expression * Expression * Expression

// 14.9/14.14 <application time period name>
// FOR PORTION OF <period name> FROM <point in time 1> TO <point in time 2>
and PortionOfSpec =
    { PeriodName: Expression
      From: Expression
      To: Expression }

// 14.8/14.9/14.13/14.14 <target table> ::= <table name> | ONLY ( <table name> )
// 20.25/20.27 allow the target table to be omitted when the statement is positioned
// through a preparable dynamic cursor (<preparable dynamic delete/update statement:
// positioned) → OmittedTarget.
and DmlTarget =
    // TableTarget (table name, isOnly)
    | TableTarget of Expression * bool
    | OmittedTarget

// 14.14 <update statement: searched> ::= UPDATE <target table> SET <set clause list> [ WHERE <search condition> ]
and UpdateStatement =
    { Target: DmlTarget
      TableAlias: Expression option
      Set: SetClause list
      Where: Expression option
      PortionOf: PortionOfSpec option
      Cursor: Expression option }

// 14.9 <delete statement: searched> ::= DELETE FROM <target table> [ WHERE <search condition> ]
and DeleteStatement =
    { Target: DmlTarget
      TableAlias: Expression option
      Where: Expression option
      PortionOf: PortionOfSpec option
      Cursor: Expression option }

// 14.12 <merge when clause> — MATCHED | NOT MATCHED
and MergeMatchCondition =
    | Matched
    | NotMatched

// 14.12 <merge update specification> | <merge delete specification> | <merge insert specification>
and MergeAction =
    | MergeUpdate of (Expression * Expression) list
    | MergeDelete
    // MergeInsert (insert column list, override, values)
    | MergeInsert of Expression list option * bool option * Expression list

// 14.12 <merge when clause> ::= WHEN { MATCHED | NOT MATCHED } [ AND <search condition> ] THEN <merge operation>
and MergeWhenClause =
    { MatchCondition: MergeMatchCondition
      Condition: Expression option
      Action: MergeAction }

// 14.12 <merge statement> ::= MERGE INTO <target> [ [ AS ] <alias> ] USING <source> ON <search condition> <merge when clause>...
and MergeStatement =
    { Target: Expression
      // 14.12 <target table> ::= <table name> | ONLY ( <table name> )
      TargetIsOnly: bool
      TargetAlias: Expression option
      Source: TableSource
      On: Expression
      WhenClauses: MergeWhenClause list }

// 11.8 <referential action> ::= CASCADE | SET NULL | SET DEFAULT | RESTRICT | NO ACTION
and ReferentialAction =
    | Cascade
    | SetNull
    | SetDefault
    | Restrict
    | NoAction

// 11.8 <referential constraint definition> ::= FOREIGN KEY ( <column list> ) REFERENCES <table> [ ( <column list> ) ] [ ON UPDATE <referential action> ] [ ON DELETE <referential action> ]
and ForeignKeyConstraint =
    { Name: Expression option
      Columns: Expression list
      Table: Expression
      RefColumns: Expression list option
      OnUpdate: ReferentialAction option
      OnDelete: ReferentialAction option }

// 11.3 <table scope> ::= GLOBAL TEMPORARY | LOCAL TEMPORARY
// (None covers the absent case, i.e. a persistent base table)
and TableScope =
    | Global
    | Local

// 11.72 <sequence generator option> — options shared by CREATE/ALTER SEQUENCE
// and the <identity column specification> (11.2).
// MaxValue/MinValue: None means NO MAXVALUE / NO MINVALUE.
// Cycle: true = CYCLE, false = NO CYCLE.
// Restart: only valid in ALTER SEQUENCE (11.73).
and SequenceOption =
    | DataTypeOption of DataType
    | StartWith of decimal
    | IncrementBy of decimal
    | MaxValue of decimal option
    | MinValue of decimal option
    | Cycle of bool
    | Restart of decimal option

// 11.4 <identity column specification> ::= GENERATED { ALWAYS | BY DEFAULT }
//     AS IDENTITY [ ( <common sequence generator options> ) ]
and IdentitySpec =
    { IsAlways: bool
      Options: SequenceOption list }

// 11.4 <column constraint> ::= NOT NULL | <unique specification>
//     | <references specification> | <check constraint definition>
// (the <default clause> is not a column constraint — it is a separate clause of
//  <column definition>, so it is modelled by ColumnDefinition.DefaultValue instead;
//  a bare NULL is also not a <column constraint> in SQL-2016)
and ColumnConstraintKind =
    | NotNull
    | PrimaryKey
    | Unique
    | References of ForeignKeyConstraint
    | Check of Expression

// 11.4 <column constraint definition> ::=
//     [ <constraint name definition> ] <column constraint> [ <constraint characteristics> ]
and ColumnConstraint =
    { Name: Expression option
      Kind: ColumnConstraintKind
      Characteristics: ConstraintCharacteristics }

// 11.4 <system time period start column specification> ::= GENERATED ALWAYS AS ROW START
// 11.4 <system time period end column specification>   ::= GENERATED ALWAYS AS ROW END
and SystemTimePeriodKind =
    | RowStart
    | RowEnd

// 11.4 the single optional clause that may follow the column's data type:
//     <default clause> | <identity column specification> | <generation clause>
//     | <system time period start column specification> | <system time period end column specification>
and ColumnGeneration =
    | IdentityColumn of IdentitySpec
    // 11.4 <generation clause> ::= GENERATED ALWAYS AS ( <value expression> )
    | GeneratedColumn of Expression
    | SystemTimePeriodColumn of SystemTimePeriodKind

// 11.4 <column definition> ::= <column name> [ <data type or domain name> ]
//       [ <default clause> | <identity column specification> | <generation clause>
//       | <system time period start column specification> | <system time period end column specification> ]
//       [ <column constraint definition>... ] [ <collate clause> ]
// IsNullable / IsPrimaryKey / IsUnique / References / Check are convenience accessors
// derived from Constraints (the <column constraint definition> list).
and ColumnDefinition =
    { Name: Expression
      DataType: DataType
      IsNullable: bool option
      IsPrimaryKey: bool
      DefaultValue: Expression option
      IsUnique: bool
      References: ForeignKeyConstraint option
      Check: Expression option
      Identity: IdentitySpec option
      // 11.4 <generation clause>
      Generation: Expression option
      // 11.4 <system time period start|end column specification>
      SystemTimePeriod: SystemTimePeriodKind option
      // 10.7 <collate clause> ::= COLLATE <collation name>
      Collation: Expression option
      Constraints: ColumnConstraint list }

// 11.6 <table constraint definition> ::= [ <constraint name definition> ] <table constraint>
// 11.6 <table constraint> ::= <unique constraint definition> | <referential constraint definition> | <check constraint definition>
and TableConstraint =
    | PrimaryKey of Expression option * Expression list
    | Unique of Expression option * Expression list
    | ForeignKey of ForeignKeyConstraint
    | Check of Expression option * Expression

// 11.6 <table constraint definition> ::=
//     [ <constraint name definition> ] <table constraint> [ <constraint characteristics> ]
and TableConstraintDefinition =
    { Constraint: TableConstraint
      Characteristics: ConstraintCharacteristics }

// 11.3 <like option> ::= <identity option> | <column default option> | <generation option>
// 11.3 <identity option> ::= INCLUDING IDENTITY | EXCLUDING IDENTITY
// 11.3 <column default option> ::= INCLUDING DEFAULTS | EXCLUDING DEFAULTS
// 11.3 <generation option> ::= INCLUDING GENERATED | EXCLUDING GENERATED
and LikeOption =
    | IncludingIdentity
    | ExcludingIdentity
    | IncludingDefaults
    | ExcludingDefaults
    | IncludingGenerated
    | ExcludingGenerated

// 11.3 <reference generation> ::= SYSTEM GENERATED | USER GENERATED | DERIVED
and ReferenceGeneration =
    | SystemGenerated
    | UserGenerated
    | Derived

// 11.3 <self-referencing column specification> ::=
//     REF IS <self-referencing column name> [ <reference generation> ]
and SelfReferencingColumnSpecification =
    { Name: Expression
      Generation: ReferenceGeneration option }

// 11.3 <column options> ::= <column name> WITH OPTIONS <column option list>
// 11.3 <column option list> ::= [ <scope clause> ] [ <default clause> ] [ <column constraint definition>... ]
// There is no data type: a typed table's columns take their type from the UDT,
// which is why this is not a ColumnDefinition.
and ColumnOptions =
    { Name: Expression
      // 6.1 <scope clause> ::= SCOPE <table name>
      Scope: Expression option
      // 11.5 <default clause> ::= DEFAULT <default option>
      DefaultValue: Expression option
      // 11.4 <column constraint definition>...
      Constraints: ColumnConstraint list }

// 11.3 <typed table element> ::=
//     <column options> | <table constraint definition> | <self-referencing column specification>
and TypedTableElement =
    | TypedColumnOptions of ColumnOptions
    | TypedTableConstraint of TableConstraintDefinition
    | TypedSelfReference of SelfReferencingColumnSpecification

// 11.32 <view column option> ::= <column name> WITH OPTIONS <scope clause>
// (the <scope clause> is mandatory here, unlike in 11.3's <column option list>)
and ViewColumnOptions =
    { Name: Expression
      // 6.1 <scope clause> ::= SCOPE <table name>
      Scope: Expression }

// 11.32 <view element> ::= <self-referencing column specification> | <view column option>
and ViewElement =
    | ViewColumnOption of ViewColumnOptions
    | ViewSelfReference of SelfReferencingColumnSpecification

// 11.3 <table definition> ::= CREATE [ <table scope> ] TABLE <table name> <table contents source>
//       [ WITH <system versioning clause> ] [ ON COMMIT <table commit action> ROWS ]
// 11.3 <table contents source> ::= <table element list> | <typed table clause> | <as subquery clause>
and CreateTableStatement =
    { Table: Expression
      TableScope: TableScope option
      Columns: ColumnDefinition list
      Constraints: TableConstraintDefinition list
      AsQuery: Query option
      AsColumns: Expression list option
      // <with or without data>: None = the <as subquery clause> is absent,
      // Some true = WITH DATA, Some false = WITH NO DATA
      WithData: bool option
      // 11.3 <typed table clause> ::= OF <UDT name> [ <subtable clause> ] [ <typed table element list> ]
      OfType: Expression option
      // 11.3 <subtable clause> ::= UNDER <supertable clause>
      Under: Expression option
      // 11.3 <typed table element list> ::= ( <typed table element> [ , ... ] )
      // (empty when the <typed table clause> has no element list)
      TypedElements: TypedTableElement list
      // 11.3 <table element> also allows <like clause> ::= LIKE <table name> [ <like option>... ]
      Like: (Expression * LikeOption list) option
      // 11.3 <system versioning clause> ::= SYSTEM VERSIONING
      WithSystemVersioning: bool
      // 11.3 ON COMMIT <table commit action> ROWS
      OnCommit: TableCommitAction option
      // 11.3 <table element> also allows <table period definition>
      Periods: TablePeriodDefinition list }

// 11.32 <view definition> ::= CREATE [ RECURSIVE ] VIEW <table name> <view specification>
//       AS <query expression> [ WITH [ <levels clause> ] CHECK OPTION ]
and CreateViewStatement =
    { Name: Expression
      // 11.32 CREATE [ RECURSIVE ] VIEW
      IsRecursive: bool
      Columns: Expression list option
      Query: Query
      // 11.32 WITH [ CASCADED | LOCAL ] CHECK OPTION
      // (Some true = CASCADED, Some false = LOCAL, None = absent)
      CheckOption: bool option
      // 11.32 <referenceable view specification> ::=
      //     OF <path-resolved user-defined type name> [ <subview clause> ] [ <view element list> ]
      OfType: Expression option
      // 11.32 <subview clause> ::= UNDER <table name>
      Under: Expression option
      // 11.32 <view element list> ::= ( <view element> [ , ... ] )
      // (empty when the <referenceable view specification> has no element list)
      ViewElements: ViewElement list }

// 11.31 <drop table statement> / 11.33 <drop view statement> / 11.50 <drop trigger statement> etc. — unified DROP
and DropStatement =
    | DropTable of Expression * bool
    | DropView of Expression * bool
    | DropRole of Expression
    | DropSequence of Expression * bool
    | DropSchema of Expression * bool
    | DropDomain of Expression * bool
    | DropCollation of Expression * bool
    | DropCharacterSet of Expression
    | DropTransliteration of Expression
    | DropAssertion of Expression * bool option
    | DropCast of DataType * DataType * bool
    | DropOrdering of Expression * bool
    | DropTransform of Expression * TransformDropTarget * bool
    | DropRoutine of Expression * bool
    | DropTrigger of Expression
    | DropType of Expression * bool

// 11.3 <system or application time period specification> — also used by
// <add table period definition> (11.27) and <drop table period definition> (11.28)
and TimePeriodSpecification =
    | SystemTimePeriod
    | ApplicationTimePeriod of Expression

// 11.3 <table period definition> ::= <system or application time period specification>
//     <left paren> <period begin column name> <comma> <period end column name> <right paren>
and TablePeriodDefinition =
    { Specification: TimePeriodSpecification
      BeginColumn: Expression
      EndColumn: Expression }

// 11.20 <alter identity column specification> ::=
//         <set identity column generation clause> [ <alter identity column option>... ]
//       | <alter identity column option>...
// Generation: Some true = SET GENERATED ALWAYS, Some false = SET GENERATED BY DEFAULT,
//             None = the <set identity column generation clause> is absent.
// The options are 11.73 <alter sequence generator restart option> (written `RESTART [WITH n]`)
// or 11.72 <basic sequence generator option> preceded by SET. The parser only yields that subset.
and AlterIdentityColumnSpecification =
    { Generation: bool option
      Options: SequenceOption list }

// 11.12 <alter column action> ::= <set column default clause> | <drop column default clause>
//     | <set column not null clause> | <drop column not null clause>
//     | <add column scope clause> | <drop column scope clause> | <alter column data type clause>
//     | <alter identity column specification> | <drop identity property clause>
//     | <drop column generation expression clause>
and ColumnAlteration =
    | SetDefault of Expression
    | DropDefault
    | SetNotNull
    | DropNotNull
    // 11.17 <add column scope clause> ::= ADD <scope clause> (the scope is a <table name>)
    | AddColumnScope of Expression
    // 11.18 <drop column scope clause> ::= DROP SCOPE <drop behavior> — true = CASCADE, false = RESTRICT
    | DropColumnScope of bool
    | SetDataType of DataType
    | AlterIdentityColumn of AlterIdentityColumnSpecification
    | DropIdentity
    | DropExpression

// 11.10 <alter table action> ::= <add column definition> | <alter column definition>
//     | <drop column definition> | <add table constraint definition>
//     | <alter table constraint definition> | <drop table constraint definition>
//     | <add table period definition> | <drop table period definition>
//     | <add system versioning clause> | <drop system versioning clause>
and AlterTableAction =
    | AddColumn of ColumnDefinition
    // 11.23 <drop column definition> ::= DROP [ COLUMN ] <column name> <drop behavior> — true = CASCADE
    | DropColumn of Expression * bool
    | AlterColumn of Expression * ColumnAlteration
    // 11.24 <add table constraint definition> ::= ADD <table constraint definition>
    | AddConstraint of TableConstraintDefinition
    // 11.25 <alter table constraint definition> ::= ALTER CONSTRAINT <constraint name> <constraint enforcement>
    // true = ENFORCED, false = NOT ENFORCED
    | AlterConstraint of Expression * bool
    // 11.26 <drop table constraint definition> ::= DROP CONSTRAINT <constraint name> <drop behavior>
    // true = CASCADE, false = RESTRICT
    | DropConstraint of Expression * bool
    // 11.27 <add table period definition> ::= ADD <table period definition> [ <add system time period column list> ]
    // the list holds exactly 0 or 2 columns; [] means the optional clause is absent
    | AddTablePeriod of TablePeriodDefinition * ColumnDefinition list
    // 11.28 <drop table period definition> ::= DROP <system or application time period specification> <drop behavior>
    | DropTablePeriod of TimePeriodSpecification * bool
    // 11.29 <add system versioning clause> ::= ADD <system versioning clause>
    | AddSystemVersioning
    // 11.30 <drop system versioning clause> ::= DROP SYSTEM VERSIONING <drop behavior>
    // true = CASCADE, false = RESTRICT
    | DropSystemVersioning of bool

// 11.10 <alter table statement> ::= ALTER TABLE <table name> <alter table action>
and AlterTableStatement =
    { Table: Expression
      Action: AlterTableAction }

// 10.6 <routine type> ::= ROUTINE | FUNCTION | PROCEDURE
//     | [ INSTANCE | STATIC | CONSTRUCTOR ] METHOD
and RoutineType =
    | Routine
    | Function
    | Procedure
    | Method of MethodKind option

// 10.6 <specific routine designator> ::=
//       SPECIFIC <routine type> <specific name>
//     | <routine type> <member name> [ FOR <schema-resolved user-defined type name> ]
// 10.6 <member name> ::= <member name alternatives> [ <data type list> ]
// 10.6 <data type list> ::= ( [ <data type> [ { <comma> <data type> }... ] ] )
// IsSpecific: true for the SPECIFIC alternative.
// RoutineType: None when the <routine type> is absent — the implementation also
//   accepts a bare <schema qualified routine name> (e.g. `ALTER ROUTINE add`).
// DataTypeList: None = the optional <data type list> is absent, Some [] = `( )`.
and SpecificRoutineDesignator =
    { IsSpecific: bool
      RoutineType: RoutineType option
      Name: Expression
      DataTypeList: DataType list option
      ForType: Expression option }

// 12.3 <action> SELECT form: bare SELECT | SELECT ( <privilege column list> )
// | SELECT ( <privilege method list> ). The column-list and method-list forms are
// distinguished in the AST (grammar rules <privilege column list> vs <privilege method list>).
and PrivilegeSelectTarget =
    | PrivilegeColumns of Expression list
    | PrivilegeMethods of SpecificRoutineDesignator list

// 12.3 <action> / <privileges> — SELECT | INSERT | UPDATE | DELETE | REFERENCES | USAGE | TRIGGER | UNDER | EXECUTE
and PrivilegeAction =
    | Select of PrivilegeSelectTarget option
    | Insert of Expression list option
    | Update of Expression list option
    | Delete
    | References of Expression list option
    | Usage
    | Trigger
    | Under
    | Execute

// 12.3 <privileges> ::= ALL PRIVILEGES | <action> [ { <comma> <action> }... ]
and Privileges =
    | AllPrivileges
    | Actions of PrivilegeAction list

// 12.7 <revoke option extension> ::= GRANT OPTION FOR | HIERARCHY OPTION FOR
// (None covers the absent case)
and RevokeOptionExtension =
    | NoOption
    | GrantOptionFor
    | HierarchyOptionFor

and GrantStatement =
    // 12.2: (privileges, object name, grantees, withHierarchyOption, withGrantOption)
    | GrantPrivileges of Privileges * Expression * Expression list * bool * bool
    // 12.5: (roles, grantees, withAdminOption)
    | GrantRoles of Expression list * Expression list * bool

and RevokeStatement =
    // 12.7: (privileges, object name, grantees, option, cascade)
    | RevokePrivileges of Privileges * Expression * Expression list * RevokeOptionExtension * bool
    // 12.7: (roles, grantees, adminOptionFor, cascade)
    | RevokeRoles of Expression list * Expression list * bool * bool

// 17.3 <isolation level> ::= READ UNCOMMITTED | READ COMMITTED | REPEATABLE READ | SERIALIZABLE
and IsolationLevel =
    | ReadUncommitted
    | ReadCommitted
    | RepeatableRead
    | Serializable

// 17.3 <transaction access mode> ::= READ ONLY | READ WRITE
and TransactionAccessMode =
    | ReadOnly
    | ReadWrite

// 17.3 <transaction mode> ::= <isolation level> | <transaction access mode> | <diagnostics size>
and TransactionMode =
    | Isolation of IsolationLevel
    | AccessMode of TransactionAccessMode
    | DiagnosticsSize of Expression

// 11.1 <schema definition> — CREATE SCHEMA may contain further <schema element>s.
// Elements are stored as StatementKind values; the parser reuses the full DDL
// parser for elements via a forward reference.
and SchemaDefinition =
    { Name: Expression option
      Authorization: Expression option
      CharacterSet: Expression option
      Path: Expression list option
      Elements: StatementKind list }

// 11.34 <domain constraint> ::= [ <constraint name definition> ] <check constraint definition> [ <constraint characteristics> ]
and DomainConstraint =
    { Name: Expression option
      Check: Expression
      Characteristics: ConstraintCharacteristics }

// 10.8 <constraint characteristics> ::= [ <constraint check time> ] [ [ NOT ] DEFERRABLE ] [ <constraint enforcement> ]
and ConstraintCharacteristics =
    { InitiallyDeferred: bool option
      Deferrable: bool option
      Enforced: bool option }

// 11.34 <domain definition>
and DomainDefinition =
    { Name: Expression
      DataType: DataType
      Default: Expression option
      Constraints: DomainConstraint list
      Collation: Expression option }

// 11.35 <alter domain statement> actions
and DomainAlteration =
    | SetDefault of Expression
    | DropDefault
    | AddConstraint of DomainConstraint
    | DropConstraint of Expression

// 11.65 <ordering category> ::= RELATIVE WITH <relative function specification>
//     | MAP WITH <map function specification> | STATE [ <specific name> ]
// (<relative function specification> / <map function specification> are
//  <specific routine designator>s — see 10.6)
and OrderingCategory =
    | Relative of SpecificRoutineDesignator
    | Map of SpecificRoutineDesignator
    | State of Expression option

// 11.65 <ordering form> ::= EQUALS ONLY BY <ordering category> | ORDER FULL BY <ordering category>
and OrderingForm =
    | EqualsOnlyBy of OrderingCategory
    | OrderFullBy of OrderingCategory

// 11.67 <transform element> ::= TO SQL WITH <specific routine designator>
//     | FROM SQL WITH <specific routine designator>
and TransformElement =
    | ToSql of SpecificRoutineDesignator
    | FromSql of SpecificRoutineDesignator

// 11.67 <transform group> ::= <group name> <transform element> [ <transform element> ]
and TransformGroup =
    { Name: Expression
      Elements: TransformElement list }

// 11.70 <transform kind> ::= TO SQL | FROM SQL
and TransformKind =
    | ToSqlKind
    | FromSqlKind

// 11.68 <alter transform statement> actions
and TransformAlteration =
    | AddTransformElements of TransformElement list
    | DropTransformElements of TransformKind list * bool

// 11.68 <alter transform group> ::= ALTER GROUP <group name> <alter transform action> [ { <comma> <alter transform action> }... ]
and AlterTransformGroup =
    { Name: Expression
      Actions: TransformAlteration list }

// 11.71 <transforms to be dropped> ::= ALL TRANSFORMS | TRANSFORM <group name>
and TransformDropTarget =
    | AllTransforms
    | TransformGroup of Expression

// 11.60 <parameter mode> ::= IN | OUT | INOUT
and ParameterMode =
    | In
    | Out
    | InOut

// 11.60 <pass through option> ::= PASS THROUGH | NO PASS THROUGH
and PassThroughOption =
    | PassThrough
    | NoPassThrough

// 11.60 <generic table pruning> ::= PRUNE ON EMPTY | KEEP ON EMPTY
and GenericTablePruning =
    | PruneOnEmpty
    | KeepOnEmpty

// 11.60 <generic table semantics> ::= WITH ROW SEMANTICS
//     | WITH SET SEMANTICS [ <generic table pruning> ]
and GenericTableSemantics =
    | RowSemantics
    | SetSemantics of GenericTablePruning option

// 11.60 <parameter type> ::= <data type> [ <locator indication> ]
//     | <generic table parameter type>
//     | <descriptor parameter type>
// 11.60 <generic table parameter type> ::= TABLE [ <pass through option> ] [ <generic table semantics> ]
// 11.60 <descriptor parameter type> ::= DESCRIPTOR
// The bool carried by DataTypeParameter is true when <locator indication> (AS LOCATOR) is present.
and ParameterType =
    | DataTypeParameter of DataType * bool
    | GenericTableParameter of PassThroughOption option * GenericTableSemantics option
    | DescriptorParameter

// 11.60 <table function column list element> ::= <column name> <data type>
and TableFunctionColumn =
    { Name: Expression; DataType: DataType }

// 11.60 <returns data type> ::= <data type> [ <locator indication> ]
// 11.60 <result cast> ::= CAST FROM <result cast from type>
and ReturnsDataType =
    { DataType: DataType
      AsLocator: bool
      CastFrom: (DataType * bool) option }

// 11.60 <returns type> ::= <returns data type> [ <result cast> ] | <returns table type>
// 11.60 <returns table type> ::= TABLE [ <table function column list> ] | ONLY PASS THROUGH
// ReturnsTable None = `RETURNS TABLE` without a <table function column list>.
and ReturnsType =
    | ReturnsData of ReturnsDataType
    | ReturnsTable of TableFunctionColumn list option
    | ReturnsOnlyPassThrough

// 11.60 <SQL parameter declaration> ::= [ <parameter mode> ] [ <SQL parameter name> ]
//     <parameter type> [ RESULT ] [ DEFAULT <parameter default> ]
and ParameterDeclaration =
    { Mode: ParameterMode option
      Name: Expression option
      ParameterType: ParameterType
      IsResult: bool
      Default: Expression option }

// 11.60 <SQL-data access indication>
and SqlDataAccess =
    | NoSql
    | ContainsSql
    | ReadsSqlData
    | ModifiesSqlData

// 11.60 <routine characteristic>
and RoutineCharacteristic =
    | Language of string
    | ParameterStyle of string
    | SpecificName of Expression
    | Deterministic of bool
    | SqlDataAccess of SqlDataAccess
    | NullCall of bool
    | DynamicResultSets of uint64
    | SavepointLevel of bool
    | ExternalName of Expression

// 11.60 <routine body>
and RoutineBody =
    | SqlRoutine of StatementKind
    | BeginAtomic of StatementKind list
    | ExternalRoutine of Expression option

// 11.60 <SQL-invoked routine> — shared by CREATE FUNCTION / CREATE PROCEDURE
and CreateRoutine =
    { Name: Expression
      Parameters: ParameterDeclaration list
      // 11.60 <returns clause> — None for a procedure
      Returns: ReturnsType option
      Characteristics: RoutineCharacteristic list
      Body: RoutineBody }

// 11.61 <alter routine statement>
and AlterRoutineStatement =
    { Routine: SpecificRoutineDesignator
      Characteristics: RoutineCharacteristic list }

// 11.49 <trigger action time> ::= BEFORE | AFTER | INSTEAD OF
and TriggerActionTime =
    | Before
    | After
    | InsteadOf

// 11.49 <trigger event>
and TriggerEvent =
    | Insert
    | Delete
    | Update of Expression list option

// 11.49 <transition table or variable>
and TransitionTableOrVariable =
    | OldRow of Expression
    | NewRow of Expression
    | OldTable of Expression
    | NewTable of Expression

// 11.49 <triggered SQL statement>
and TriggeredStatement =
    | SingleStatement of StatementKind
    | BeginAtomic of StatementKind list

// 11.49 <triggered action>
and TriggeredAction =
    { ForEach: bool option
      When: Expression option
      Statement: TriggeredStatement }

// 11.49 <trigger definition>
and CreateTriggerStatement =
    { Name: Expression
      ActionTime: TriggerActionTime
      Event: TriggerEvent
      Table: Expression
      Transitions: TransitionTableOrVariable list
      Action: TriggeredAction }

// 11.51 <method specification> kind — INSTANCE | STATIC | CONSTRUCTOR
and MethodKind =
    | Instance
    | Static
    | Constructor

// 11.52 <attribute definition> ::= <attribute name> <data type>
//     [ <attribute default> ] [ <collate clause> ]
and AttributeDefinition =
    { Name: Expression
      DataType: DataType
      Default: Expression option
      Collate: Expression option }

// 11.51 <representation> ::= <predefined type> | <collection type> | <member list>
and TypeRepresentation =
    | Predefined of DataType
    | MemberList of AttributeDefinition list

// 11.51 <user-defined type option> ::= INSTANTIABLE | NOT INSTANTIABLE | FINAL | NOT FINAL | REF USING ... | REF FROM ... | REF IS SYSTEM GENERATED | CAST (...) etc.
and TypeOption =
    | Instantiable of bool
    | Final of bool
    | RefUsing of DataType
    | RefFrom of Expression list
    | RefIsSystemGenerated
    | CastToRef of Expression
    | CastToType of Expression
    | CastToDistinct of Expression
    | CastToSource of Expression

// 11.51 <method specification> (original form)
and MethodSpecification =
    { Kind: MethodKind option
      Name: Expression
      Parameters: ParameterDeclaration list
      Returns: DataType option
      Specific: Expression option
      SelfAsResult: bool
      SelfAsLocator: bool
      Characteristics: RoutineCharacteristic list }

// 11.51 <user-defined type definition>
and CreateTypeStatement =
    { Name: Expression
      Under: Expression option
      Representation: TypeRepresentation option
      Options: TypeOption list
      Methods: MethodSpecification list }

// 11.53 <alter type action>
and AlterTypeAction =
    | AddAttribute of AttributeDefinition
    | DropAttribute of Expression
    | AddMethod of MethodSpecification * bool
    | DropMethod of MethodKind option * Expression * DataType list

// 11.53 <alter type statement>
and AlterTypeStatement =
    { Name: Expression
      Action: AlterTypeAction }

// 14.2 <cursor sensitivity> ::= SENSITIVE | INSENSITIVE | ASENSITIVE
and CursorSensitivity =
    | Sensitive
    | Insensitive
    | Asensitive

// 14.2 <cursor scrollability> ::= SCROLL | NO SCROLL
and CursorScrollability =
    | Scroll
    | NoScroll

// 14.2 <cursor holdability> ::= WITH HOLD | WITHOUT HOLD
and CursorHoldability =
    | WithHold
    | WithoutHold

// 14.2 <cursor returnability> ::= WITH RETURN | WITHOUT RETURN
and CursorReturnability =
    | WithReturn
    | WithoutReturn

// 20.8 <cursor attribute> ::= <cursor sensitivity> | <cursor scrollability>
//     | <cursor holdability> | <cursor returnability>
and CursorAttribute =
    | SensitivityAttribute of CursorSensitivity
    | ScrollabilityAttribute of CursorScrollability
    | HoldabilityAttribute of CursorHoldability
    | ReturnabilityAttribute of CursorReturnability

// 14.2 <cursor properties> ::= [ <cursor sensitivity> ] [ <cursor scrollability> ] CURSOR
//     [ <cursor holdability> ] [ <cursor returnability> ]
and CursorProperties =
    { Sensitivity: CursorSensitivity option
      Scrollability: CursorScrollability option
      Holdability: CursorHoldability option
      Returnability: CursorReturnability option }

// 14.1 <declare cursor> ::= DECLARE <cursor name> <cursor properties> FOR <cursor specification>
// 14.3 <cursor specification> ::= <query expression> [ <updatability clause> ]
and DeclareCursorStatement =
    { Name: Expression
      Properties: CursorProperties
      Specification: Query }

// 14.3 <updatability clause> ::= FOR { READ ONLY | UPDATE [ OF <column name list> ] }
and LockingClause =
    | ForUpdate of Expression list option
    | ForReadOnly

// 14.16 <table commit action> ::= PRESERVE | DELETE
and TableCommitAction =
    | PreserveOnCommit
    | DeleteOnCommit

// 14.16 <temporary table declaration> ::= DECLARE LOCAL TEMPORARY TABLE <table name> <table element list>
//     [ ON COMMIT <table commit action> ROWS ]
and TemporaryTableDeclarationStatement =
    { Name: Expression
      Columns: ColumnDefinition list
      Constraints: TableConstraintDefinition list
      OnCommit: TableCommitAction option }

// 5.4 <scope option> ::= GLOBAL | LOCAL
and ScopeOption =
    | ScopeGlobal
    | ScopeLocal

// 20.15 <statement name> / <extended statement name> / 20.17 <extended cursor name>
//     ::= [ <scope option> ] <simple value specification>
and ExtendedName =
    { Scope: ScopeOption option
      SimpleValue: Expression }

// 20.15 <dynamic declare cursor> ::= DECLARE <cursor name> <cursor properties> FOR <statement name>
and DynamicDeclareCursorStatement =
    { Name: Expression
      Properties: CursorProperties
      Statement: ExtendedName }

// 20.17 <allocate extended dynamic cursor statement> ::= ALLOCATE <extended cursor name>
//     <cursor properties> FOR <extended statement name>
and AllocateExtendedDynamicCursorStatement =
    { Cursor: ExtendedName
      Properties: CursorProperties
      Statement: ExtendedName }

// 20.18 <allocate received cursor statement> ::= ALLOCATE <cursor name> [ CURSOR ]
//     FOR PROCEDURE <specific routine designator>
and AllocateReceivedCursorStatement =
    { Name: Expression
      Routine: SpecificRoutineDesignator }

// 14.5 <fetch orientation>
and FetchOrientation =
    | Next
    | Prior
    | First
    | Last
    | Absolute of Expression
    | Relative of Expression

// 14.7 <select statement: single row>
// SELECT [ <set quantifier> ] <select list> INTO <select target list> <table expression>
and SelectIntoStatement =
    { IsDistinct: bool
      Columns: ColumnSource list
      Into: Expression list
      From: TableSource list
      Where: Expression option
      GroupBy: GroupingElement list
      GroupByDistinct: bool
      Having: Expression option
      Window: (Expression * WindowDefinition) list }

// 18.1 <connect statement>
// CONNECT TO <SQL-server name> [ AS <connection name> ] [ USER <connection user name> ] | DEFAULT
// Server = None means CONNECT TO DEFAULT.
and ConnectStatement =
    { Server: Expression option
      ConnectionName: Expression option
      User: Expression option }

// 18.3 <disconnect object> ::= <connection object> | ALL | CURRENT
and DisconnectObject =
    | DisconnectDefault
    | DisconnectAll
    | DisconnectCurrent
    | DisconnectName of Expression

// 23.1 <get diagnostics statement>
// <SQL diagnostics information> ::= <statement information> | <condition information> | <all information>
and GetDiagnosticsStatement =
    | StatementInfo of (Expression * string) list
    | ConditionInfo of Expression * (Expression * string) list
    | AllInfo of Expression * AllQualifier option

// 23.1 <SQL diagnostics information> — ALL { STATEMENT | CONDITION <n> }
and AllQualifier =
    | AllStatement
    | AllCondition of Expression option

// 20.10 <describe statement>
// DESCRIBE INPUT <name> <using descriptor> [ <nesting option> ]
// | DESCRIBE [ OUTPUT ] <described object> <using descriptor> [ <nesting option> ]
and DescribeStatement =
    { IsInput: bool
      IsCursor: bool
      Name: Expression
      Descriptor: Expression
      Nesting: bool option }

// 20.4 <get descriptor information> / 20.5 <set descriptor information>
and GetDescriptorInfo =
    | GetHeader of (Expression * string) list
    | GetItem of Expression * (Expression * string) list

and SetDescriptorInfo =
    | SetHeader of (string * Expression) list
    | SetItem of Expression * (string * Expression) list

// 20.6 <copy descriptor statement>
// COPY <source> TO <target> | COPY <source> VALUE <n> ( <options> ) TO <target> VALUE <m>
and CopyDescriptorStatement =
    { Source: Expression
      SourceItem: Expression option
      Options: string list option
      Target: Expression
      TargetItem: Expression option }

// 20.11 <input using clause> / 20.12 <output using clause>
// <input using clause>  ::= USING <args> | USING [ SQL ] DESCRIPTOR <name>
// <output using clause> ::= INTO <args> | INTO [ SQL ] DESCRIPTOR <name>
// Shared by 20.13 <execute statement>, 20.19 <dynamic open statement> and
// 20.20 <dynamic fetch statement>.
and UsingClause =
    | UsingArguments of Expression list
    | UsingDescriptor of Expression

// 7.17 <query expression> / 14.11 <insert statement> / 14.14 <update statement: searched>
// 14.9 <delete statement: searched> / 14.12 <merge statement> / 11.3 <table definition>
// 11.32 <view definition> / 11.10 <alter table statement> / 12.x <grant/revoke>
// 16.x <transaction statement> / 11.5x <sequence/domain> / 19.x <session> / 20.x <dynamic SQL>
// <SQL statement> — top-level statement variants
and StatementKind =
    | Select of Query
    | Insert of InsertStatement
    | Update of UpdateStatement
    | Delete of DeleteStatement
    | Merge of MergeStatement
    | CreateTable of CreateTableStatement
    | CreateView of CreateViewStatement
    | Drop of DropStatement
    | AlterTable of AlterTableStatement
    // 14.10 <truncate table statement> ::= TRUNCATE TABLE <table name>
    | Truncate of Expression * bool option
    | Grant of GrantStatement
    | Revoke of RevokeStatement
    // 12.4 <role definition> ::= CREATE ROLE <role name>
    | CreateRole of Expression
    // 12.6 <drop role statement> ::= DROP ROLE <role name>
    | DropRole of Expression
    // 11.1 <schema definition> ::= CREATE SCHEMA ...
    | CreateSchema of SchemaDefinition
    // 11.34 <domain definition> ::= CREATE DOMAIN ...
    | CreateDomain of DomainDefinition
    // 11.35 <alter domain statement> ::= ALTER DOMAIN ...
    | AlterDomain of Expression * DomainAlteration
    // 11.43 <collation definition> ::= CREATE COLLATION ...
    | CreateCollation of Expression * Expression * Expression * bool option
    // 11.41 <character set definition> ::= CREATE CHARACTER SET ...
    | CreateCharacterSet of Expression * Expression * Expression option
    // 11.45 <transliteration definition> ::= CREATE TRANSLITERATION ...
    | CreateTransliteration of Expression * Expression * Expression * SpecificRoutineDesignator
    // 11.47 <assertion definition> ::= CREATE ASSERTION ...
    | CreateAssertion of Expression * Expression * ConstraintCharacteristics
    // 11.63 <user-defined cast definition> ::= CREATE CAST ...
    | CreateCast of DataType * DataType * SpecificRoutineDesignator * bool
    // 11.65 <user-defined ordering definition> ::= CREATE ORDERING ...
    | CreateOrdering of Expression * OrderingForm
    // 11.67 <transform definition> ::= CREATE TRANSFORM ...
    | CreateTransform of Expression * TransformGroup list
    // 11.68 <alter transform statement> ::= ALTER TRANSFORM ...
    | AlterTransform of Expression * AlterTransformGroup list
    // 11.72 <sequence generator definition> ::= CREATE SEQUENCE ...
    | CreateSequence of Expression * SequenceOption list
    // 11.73 <alter sequence generator statement> ::= ALTER SEQUENCE ...
    | AlterSequence of Expression * SequenceOption list
    // 19.3 <set role statement> ::= SET ROLE ...
    | SetRole of Expression option
    // 16.1 <call statement> ::= CALL <routine invocation>
    | Call of Expression * Expression list
    // 16.2 <return statement> ::= RETURN <return value>
    | Return of Expression
    // 17.1 <start transaction statement> ::= START TRANSACTION ...
    | StartTransaction of TransactionMode list
    // 17.2 <set transaction statement> ::= SET [ LOCAL ] TRANSACTION ...
    | SetTransaction of bool * TransactionMode list
    // 17.4 <set constraints mode statement> ::= SET CONSTRAINTS ...
    | SetConstraints of Expression list option * bool
    // 17.5 <savepoint statement> ::= SAVEPOINT <savepoint specifier>
    | Savepoint of Expression
    // 17.6 <release savepoint statement> ::= RELEASE SAVEPOINT <savepoint specifier>
    | ReleaseSavepoint of Expression
    // 17.7 <commit statement> ::= COMMIT [ AND { CHAIN | NO CHAIN } ]
    | Commit of bool option
    // 17.8 <rollback statement> ::= ROLLBACK [ AND { CHAIN | NO CHAIN } ] [ TO SAVEPOINT <savepoint specifier> ]
    | Rollback of bool option * Expression option
    // 7.17 <with clause> + <SQL statement>
    | WithStatement of bool * Cte list * StatementKind
    // 11.60 <SQL-invoked routine> — CREATE PROCEDURE / FUNCTION
    | CreateProcedure of CreateRoutine
    | CreateFunction of CreateRoutine
    // 11.61 <alter routine statement> ::= ALTER <specific routine designator> ...
    | AlterRoutine of AlterRoutineStatement
    // 11.49 <trigger definition> ::= CREATE TRIGGER ...
    | CreateTrigger of CreateTriggerStatement
    // 11.51 <user-defined type definition> ::= CREATE TYPE ...
    | CreateType of CreateTypeStatement
    // 11.53 <alter type statement> ::= ALTER TYPE ...
    | AlterType of AlterTypeStatement
    // 14.1 <declare cursor> / 14.2 <cursor properties> / 14.3 <cursor specification>
    | DeclareCursor of DeclareCursorStatement
    // 14.4 <open statement> / 20.19 <dynamic open statement>
    | Open of Expression * UsingClause option
    // 14.5 <fetch statement> / 20.20 <dynamic fetch statement>
    | Fetch of FetchOrientation option * Expression * UsingClause
    // 14.6 <close statement> / 20.22 <dynamic close statement>
    | Close of Expression
    | SelectInto of SelectIntoStatement
    // 14.16 <temporary table declaration>
    | DeclareTemporaryTable of TemporaryTableDeclarationStatement
    // 14.17 <free locator statement> / 14.18 <hold locator statement>
    | FreeLocator of Expression list
    | HoldLocator of Expression list
    // 18.1-18.3 connection statements
    | Connect of ConnectStatement
    | SetConnection of Expression option
    | Disconnect of DisconnectObject
    // 19.x session statements
    | SetSessionAuthorization of Expression
    | SetTimeZone of Expression option
    | SetSessionCharacteristics of TransactionMode list
    | SetCatalog of Expression
    | SetSchema of Expression
    | SetNames of Expression
    | SetPath of Expression
    | SetTransformGroup of Expression * Expression option
    | SetSessionCollation of Expression option * Expression list option
    // 23.1 diagnostics
    | GetDiagnostics of GetDiagnosticsStatement
    // 20.x dynamic SQL
    | Prepare of Expression * Expression option * Expression
    | DeallocatePrepare of Expression
    | Describe of DescribeStatement
    | Execute of Expression * UsingClause option * UsingClause option
    | ExecuteImmediate of Expression
    | AllocateDescriptor of Expression * Expression option
    | DeallocateDescriptor of Expression
    | GetDescriptor of Expression * GetDescriptorInfo
    | SetDescriptor of Expression * SetDescriptorInfo
    | CopyDescriptor of CopyDescriptorStatement
    // 20.15 <dynamic declare cursor>
    | DynamicDeclareCursor of DynamicDeclareCursorStatement
    // 20.17 <allocate extended dynamic cursor statement>
    | AllocateExtendedDynamicCursor of AllocateExtendedDynamicCursorStatement
    // 20.18 <allocate received cursor statement>
    | AllocateReceivedCursor of AllocateReceivedCursorStatement
    // 20.28 <pipe row statement> ::= PIPE ROW <PTF descriptor name>
    | PipeRow of Expression

type Statement = { Kind: StatementKind; Pos: Position }
