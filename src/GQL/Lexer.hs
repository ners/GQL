{-# LANGUAGE OverloadedStrings #-}

module GQL.Lexer where

import Data.Text (Text, pack, unpack)
import Data.Void
import Debug.Trace
import GHC.Natural (Natural, naturalToInteger)
import GQL.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol t = do
    result <- L.symbol' sc t
    trace ("  lexing symbol " <> unpack t <> ": " <> unpack result) $ pure result

charLiteral :: Parser Char
charLiteral = do
    c <- lexeme $ between (char '\'') (char '\'') L.charLiteral
    trace ("  lexing char literal: " <> [c]) $ return c

stringLiteral :: Parser String
stringLiteral = do
    s <- lexeme $ char '\"' *> manyTill L.charLiteral (char '\"')
    trace ("  lexing string literal: " <> s) $ return s

natural :: Parser Natural
natural = do
    d <- lexeme L.decimal
    trace ("  lexing natural literal: " <> show d) $ return d

float :: Parser Double
float = do
    f <- lexeme L.float
    trace ("  lexing float literal: " <> show f) $ return f

integer :: Parser Integer
integer = do
    i <- L.signed sc $ naturalToInteger <$> natural
    trace ("  lexing integer literal: " <> show i) $ return i

signedFloat :: Parser Double
signedFloat = do
    f <- L.signed sc float
    trace ("  lexing signed float literal: " <> show f) $ return f

betweenParens = between (symbol "(") (symbol ")")
betweenBraces = between (symbol "{") (symbol "}")
betweenAngles = between (symbol "<") (symbol ">")
betweenBrackets = between (symbol "[") (symbol "]")

symbolAmpersand = symbol "&"
symbolAsterisk = symbol "*"
symbolBracketRightArrow = symbol "]->"
symbolBracketTildeRightArrow = symbol "]~>"
symbolCircumflex = symbol "^"
symbolColon = symbol ":"
symbolComma = symbol ","
symbolConcatenation = symbol "||"
symbolDollarSign = symbol "$"
symbolDoubleColon = symbol "::"
symbolDoubleMinusSign = symbol "--"
symbolDoublePeriod = symbol ".."
symbolDoubleQuote = symbol "\""
symbolDoubleSolidus = symbol "//"
symbolDoubledGraveAccent = symbol "``"
symbolEq = symbol "="
symbolExclamationMark = symbol "!"
symbolGeq = symbol ">="
symbolGraveAccent = symbol "`"
symbolGt = symbol ">"
symbolLeftArrow = symbol "<-"
symbolLeftArrowBracket = symbol "<-["
symbolLeftArrowTilde = symbol "<~"
symbolLeftArrowTildeBracket = symbol "<~["
symbolLeftMinusRight = symbol "<->"
symbolLeftMinusSlash = symbol "<-/"
symbolLeftTildeSlash = symbol "<~/"
symbolLeq = symbol "<="
symbolLt = symbol "<"
symbolMinusLeftBracket = symbol "-["
symbolMinusSign = symbol "-"
symbolMinusSlash = symbol "-/"
symbolMultisetAlternation = symbol "|+|"
symbolNeq = symbol "<>"
symbolPercent = symbol "%"
symbolPeriod = symbol "."
symbolPlusSign = symbol "+"
symbolQuestionMark = symbol "?"
symbolQuote = symbol "\'"
symbolReverseSolidus = symbol "\\"
symbolRightArrow = symbol "->"
symbolRightBracketMinus = symbol "]-"
symbolRightBracketTilde = symbol "]~"
symbolSemicolon = symbol ";"
symbolSlashMinus = symbol "/-"
symbolSlashMinusRight = symbol "/->"
symbolSlashTilde = symbol "/~"
symbolSlashTildeRight = symbol "/~>"
symbolSolidus = symbol "/"
symbolTilde = symbol "~"
symbolTildeLeftBracket = symbol "~["
symbolTildeRightArrow = symbol "~>"
symbolTildeSlash = symbol "~/"
symbolVerticalBar = symbol "|"

symbolAbs = symbol "ABS"
symbolAcos = symbol "ACOS"
symbolAcyclic = symbol "ACYCLIC"
symbolAdd = symbol "ADD"
symbolAggregate = symbol "AGGREGATE"
symbolAlias = symbol "ALIAS"
symbolAll = symbol "ALL"
symbolAnd = symbol "AND"
symbolAny = symbol "ANY"
symbolArray = symbol "ARRAY"
symbolAs = symbol "AS"
symbolAsc = symbol "ASC"
symbolAscending = symbol "ASCENDING"
symbolAsin = symbol "ASIN"
symbolAt = symbol "AT"
symbolAtan = symbol "ATAN"
symbolAvg = symbol "AVG"
symbolBinary = symbol "BINARY"
symbolBinding = symbol "BINDING"
symbolBoolean = symbol "BOOLEAN"
symbolBoth = symbol "BOTH"
symbolBy = symbol "BY"
symbolCall = symbol "CALL"
symbolCase = symbol "CASE"
symbolCatalog = symbol "CATALOG"
symbolCeil = symbol "CEIL"
symbolCeiling = symbol "CEILING"
symbolCharachter = symbol "CHARACHTER"
symbolCharacterLength = symbol "CHARACTER_LENGTH"
symbolClassOrigin = symbol "CLASS_ORIGIN"
symbolClear = symbol "CLEAR"
symbolClone = symbol "CLONE"
symbolClose = symbol "CLOSE"
symbolCollect = symbol "COLLECT"
symbolCommandFunction = symbol "COMMAND_FUNCTION"
symbolCommandFunctionCode = symbol "COMMAND_FUNCTION_CODE"
symbolCommit = symbol "COMMIT"
symbolConditionNumber = symbol "CONDITION_NUMBER"
symbolConnecting = symbol "CONNECTING"
symbolConstant = symbol "CONSTANT"
symbolConstraint = symbol "CONSTRAINT"
symbolConstrcut = symbol "CONSTRCUT"
symbolCopy = symbol "COPY"
symbolCos = symbol "COS"
symbolCosh = symbol "COSH"
symbolCost = symbol "COST"
symbolCot = symbol "COT"
symbolCount = symbol "COUNT"
symbolCreate = symbol "CREATE"
symbolCurrentDate = symbol "CURRENT_DATE"
symbolCurrentGraph = symbol "CURRENT_GRAPH"
symbolCurrentPropertyGraph = symbol "CURRENT_PROPERTY_GRAPH"
symbolCurrentRole = symbol "CURRENT_ROLE"
symbolCurrentSchema = symbol "CURRENT_SCHEMA"
symbolCurrentTime = symbol "CURRENT_TIME"
symbolCurrentTimestamp = symbol "CURRENT_TIMESTAMP"
symbolCurrentUser = symbol "CURRENT_USER"
symbolDate = symbol "DATE"
symbolDatetime = symbol "DATETIME"
symbolDecimal = symbol "DECIMAL"
symbolDefault = symbol "DEFAULT"
symbolDegrees = symbol "DEGREES"
symbolDelete = symbol "DELETE"
symbolDesc = symbol "DESC"
symbolDescending = symbol "DESCENDING"
symbolDestination = symbol "DESTINATION"
symbolDetach = symbol "DETACH"
symbolDirected = symbol "DIRECTED"
symbolDirectories = symbol "DIRECTORIES"
symbolDirectory = symbol "DIRECTORY"
symbolDistinct = symbol "DISTINCT"
symbolDo = symbol "DO"
symbolDot = symbol "."
symbolDrop = symbol "DROP"
symbolDuration = symbol "DURATION"
symbolEdge = symbol "EDGE"
symbolEdges = symbol "EDGES"
symbolElse = symbol "ELSE"
symbolEmptyBindingTable = symbol "EMPTY_BINDING_TABLE"
symbolEmptyGraph = symbol "EMPTY_GRAPH"
symbolEmptyPropertyGraph = symbol "EMPTY_PROPERTY_GRAPH"
symbolEmptyTable = symbol "EMPTY_TABLE"
symbolEnd = symbol "END"
symbolEnds = symbol "ENDS"
symbolExcept = symbol "EXCEPT"
symbolExisting = symbol "EXISTING"
symbolExists = symbol "EXISTS"
symbolExp = symbol "EXP"
symbolExplain = symbol "EXPLAIN"
symbolFalse = symbol "FALSE"
symbolFilter = symbol "FILTER"
symbolFinal = symbol "FINAL"
symbolFloat = symbol "FLOAT"
symbolFloat128 = symbol "FLOAT128"
symbolFloat32 = symbol "FLOAT32"
symbolFloat64 = symbol "FLOAT64"
symbolFloor = symbol "FLOOR"
symbolFor = symbol "FOR"
symbolFrom = symbol "FROM"
symbolFunctions = symbol "FUNCTIONS"
symbolFunctopm = symbol "FUNCTOPM"
symbolGqlstatus = symbol "GQLSTATUS"
symbolGraph = symbol "GRAPH"
symbolGraphs = symbol "GRAPHS"
symbolGroup = symbol "GROUP"
symbolGroups = symbol "GROUPS"
symbolHaving = symbol "HAVING"
symbolHomeGraph = symbol "HOME_GRAPH"
symbolHomePropertyGraph = symbol "HOME_PROPERTY_GRAPH"
symbolHomeSchema = symbol "HOME_SCHEMA"
symbolIf = symbol "IF"
symbolIn = symbol "IN"
symbolIndex = symbol "INDEX"
symbolInsert = symbol "INSERT"
symbolInteger = symbol "INTEGER"
symbolInteger128 = symbol "INTEGER128"
symbolInteger16 = symbol "INTEGER16"
symbolInteger32 = symbol "INTEGER32"
symbolInteger64 = symbol "INTEGER64"
symbolInteger8 = symbol "INTEGER8"
symbolIntersect = symbol "INTERSECT"
symbolIs = symbol "IS"
symbolKeep = symbol "KEEP"
symbolLabel = symbol "LABEL"
symbolLabels = symbol "LABELS"
symbolLeading = symbol "LEADING"
symbolLeft = symbol "LEFT"
symbolLength = symbol "LENGTH"
symbolLet = symbol "LET"
symbolLike = symbol "LIKE"
symbolLimit = symbol "LIMIT"
symbolList = symbol "LIST"
symbolLn = symbol "LN"
symbolLocaldatetime = symbol "LOCALDATETIME"
symbolLocaltime = symbol "LOCALTIME"
symbolLocaltimestamp = symbol "LOCALTIMESTAMP"
symbolLog = symbol "LOG"
symbolLog10 = symbol "LOG10"
symbolLower = symbol "LOWER"
symbolMandatory = symbol "MANDATORY"
symbolMap = symbol "MAP"
symbolMatch = symbol "MATCH"
symbolMax = symbol "MAX"
symbolMerge = symbol "MERGE"
symbolMessageText = symbol "MESSAGE_TEXT"
symbolMin = symbol "MIN"
symbolMod = symbol "MOD"
symbolMulti = symbol "MULTI"
symbolMultiple = symbol "MULTIPLE"
symbolMultiset = symbol "MULTISET"
symbolMutable = symbol "MUTABLE"
symbolNew = symbol "NEW"
symbolNfc = symbol "NFC"
symbolNfd = symbol "NFD"
symbolNfkc = symbol "NFKC"
symbolNfkd = symbol "NFKD"
symbolNode = symbol "NODE"
symbolNodes = symbol "NODES"
symbolNormalize = symbol "NORMALIZE"
symbolNormalized = symbol "NORMALIZED"
symbolNot = symbol "NOT"
symbolNothing = symbol "NOTHING"
symbolNull = symbol "NULL"
symbolNumber = symbol "NUMBER"
symbolOctetLength = symbol "OCTET_LENGTH"
symbolOf = symbol "OF"
symbolOffset = symbol "OFFSET"
symbolOn = symbol "ON"
symbolOnly = symbol "ONLY"
symbolOptional = symbol "OPTIONAL"
symbolOr = symbol "OR"
symbolOrder = symbol "ORDER"
symbolOrdered = symbol "ORDERED"
symbolOrdinality = symbol "ORDINALITY"
symbolOtherwise = symbol "OTHERWISE"
symbolParameter = symbol "PARAMETER"
symbolPartition = symbol "PARTITION"
symbolPath = symbol "PATH"
symbolPaths = symbol "PATHS"
symbolPattern = symbol "PATTERN"
symbolPatterns = symbol "PATTERNS"
symbolPower = symbol "POWER"
symbolProcedure = symbol "PROCEDURE"
symbolProcedures = symbol "PROCEDURES"
symbolProduct = symbol "PRODUCT"
symbolProfile = symbol "PROFILE"
symbolProject = symbol "PROJECT"
symbolProperties = symbol "PROPERTIES"
symbolProperty = symbol "PROPERTY"
symbolQueries = symbol "QUERIES"
symbolQuery = symbol "QUERY"
symbolRadians = symbol "RADIANS"
symbolRcord = symbol "RCORD"
symbolRead = symbol "READ"
symbolRecords = symbol "RECORDS"
symbolReference = symbol "REFERENCE"
symbolRelationship = symbol "RELATIONSHIP"
symbolRelationships = symbol "RELATIONSHIPS"
symbolRemove = symbol "REMOVE"
symbolRename = symbol "RENAME"
symbolReplace = symbol "REPLACE"
symbolRequire = symbol "REQUIRE"
symbolReset = symbol "RESET"
symbolResult = symbol "RESULT"
symbolReturn = symbol "RETURN"
symbolReturnedGqlstatus = symbol "RETURNED_GQLSTATUS"
symbolRight = symbol "RIGHT"
symbolRollback = symbol "ROLLBACK"
symbolScalar = symbol "SCALAR"
symbolSchema = symbol "SCHEMA"
symbolSchemas = symbol "SCHEMAS"
symbolSchemata = symbol "SCHEMATA"
symbolSelect = symbol "SELECT"
symbolSession = symbol "SESSION"
symbolSet = symbol "SET"
symbolShortest = symbol "SHORTEST"
symbolSimple = symbol "SIMPLE"
symbolSin = symbol "SIN"
symbolSingle = symbol "SINGLE"
symbolSinh = symbol "SINH"
symbolSqrt = symbol "SQRT"
symbolStart = symbol "START"
symbolStarts = symbol "STARTS"
symbolString = symbol "STRING"
symbolSubclassOrigin = symbol "SUBCLASS_ORIGIN"
symbolSubstring = symbol "SUBSTRING"
symbolSum = symbol "SUM"
symbolTable = symbol "TABLE"
symbolTables = symbol "TABLES"
symbolTan = symbol "TAN"
symbolTanh = symbol "TANH"
symbolThen = symbol "THEN"
symbolTies = symbol "TIES"
symbolTime = symbol "TIME"
symbolTimestamp = symbol "TIMESTAMP"
symbolTo = symbol "TO"
symbolTrail = symbol "TRAIL"
symbolTrailing = symbol "TRAILING"
symbolTransaction = symbol "TRANSACTION"
symbolTrim = symbol "TRIM"
symbolTrue = symbol "TRUE"
symbolTruncate = symbol "TRUNCATE"
symbolType = symbol "TYPE"
symbolTypes = symbol "TYPES"
symbolUndirected = symbol "UNDIRECTED"
symbolUnion = symbol "UNION"
symbolUnique = symbol "UNIQUE"
symbolUnit = symbol "UNIT"
symbolUnitBindingTable = symbol "UNIT_BINDING_TABLE"
symbolUnitTable = symbol "UNIT_TABLE"
symbolUnknown = symbol "UNKNOWN"
symbolUnnest = symbol "UNNEST"
symbolUnwind = symbol "UNWIND"
symbolUpper = symbol "UPPER"
symbolUse = symbol "USE"
symbolValue = symbol "VALUE"
symbolValues = symbol "VALUES"
symbolVertex = symbol "VERTEX"
symbolVertices = symbol "VERTICES"
symbolWalk = symbol "WALK"
symbolWhen = symbol "WHEN"
symbolWhere = symbol "WHERE"
symbolWith = symbol "WITH"
symbolWorkingGraph = symbol "WORKING_GRAPH"
symbolWrite = symbol "WRITE"
symbolXor = symbol "XOR"
symbolYield = symbol "YIELD"
symbolZero = symbol "ZERO"

keyword =
    choice
        [ symbolAbs
        , symbolAcos
        , symbolAcyclic
        , symbolAdd
        , symbolAggregate
        , symbolAlias
        , symbolAll
        , symbolAnd
        , symbolAny
        , symbolArray
        , symbolAs
        , symbolAsc
        , symbolAscending
        , symbolAsin
        , symbolAt
        , symbolAtan
        , symbolAvg
        , symbolBinary
        , symbolBinding
        , symbolBoolean
        , symbolBoth
        , symbolBy
        , symbolCall
        , symbolCase
        , symbolCatalog
        , symbolCeil
        , symbolCeiling
        , symbolCharachter
        , symbolCharacterLength
        , symbolClassOrigin
        , symbolClear
        , symbolClone
        , symbolClose
        , symbolCollect
        , symbolCommandFunction
        , symbolCommandFunctionCode
        , symbolCommit
        , symbolConditionNumber
        , symbolConnecting
        , symbolConstant
        , symbolConstraint
        , symbolConstrcut
        , symbolCopy
        , symbolCos
        , symbolCosh
        , symbolCost
        , symbolCot
        , symbolCount
        , symbolCreate
        , symbolCurrentDate
        , symbolCurrentGraph
        , symbolCurrentPropertyGraph
        , symbolCurrentRole
        , symbolCurrentSchema
        , symbolCurrentTime
        , symbolCurrentTimestamp
        , symbolCurrentUser
        , symbolDate
        , symbolDatetime
        , symbolDecimal
        , symbolDefault
        , symbolDegrees
        , symbolDelete
        , symbolDesc
        , symbolDescending
        , symbolDestination
        , symbolDetach
        , symbolDirected
        , symbolDirectories
        , symbolDirectory
        , symbolDistinct
        , symbolDo
        , symbolDot
        , symbolDrop
        , symbolDuration
        , symbolEdge
        , symbolEdges
        , symbolElse
        , symbolEmptyBindingTable
        , symbolEmptyGraph
        , symbolEmptyPropertyGraph
        , symbolEmptyTable
        , symbolEnd
        , symbolEnds
        , symbolExcept
        , symbolExisting
        , symbolExists
        , symbolExp
        , symbolExplain
        , symbolFalse
        , symbolFilter
        , symbolFinal
        , symbolFloat
        , symbolFloat128
        , symbolFloat32
        , symbolFloat64
        , symbolFloor
        , symbolFor
        , symbolFrom
        , symbolFunctions
        , symbolFunctopm
        , symbolGqlstatus
        , symbolGraph
        , symbolGraphs
        , symbolGroup
        , symbolGroups
        , symbolHaving
        , symbolHomeGraph
        , symbolHomePropertyGraph
        , symbolHomeSchema
        , symbolIf
        , symbolIn
        , symbolIndex
        , symbolInsert
        , symbolInteger
        , symbolInteger128
        , symbolInteger16
        , symbolInteger32
        , symbolInteger64
        , symbolInteger8
        , symbolIntersect
        , symbolIs
        , symbolKeep
        , symbolLabel
        , symbolLabels
        , symbolLeading
        , symbolLeft
        , symbolLength
        , symbolLet
        , symbolLike
        , symbolLimit
        , symbolList
        , symbolLn
        , symbolLocaldatetime
        , symbolLocaltime
        , symbolLocaltimestamp
        , symbolLog
        , symbolLog10
        , symbolLower
        , symbolMandatory
        , symbolMap
        , symbolMatch
        , symbolMax
        , symbolMerge
        , symbolMessageText
        , symbolMin
        , symbolMod
        , symbolMulti
        , symbolMultiple
        , symbolMultiset
        , symbolMutable
        , symbolNew
        , symbolNfc
        , symbolNfd
        , symbolNfkc
        , symbolNfkd
        , symbolNode
        , symbolNodes
        , symbolNormalize
        , symbolNormalized
        , symbolNot
        , symbolNothing
        , symbolNull
        , symbolNumber
        , symbolOctetLength
        , symbolOf
        , symbolOffset
        , symbolOn
        , symbolOnly
        , symbolOptional
        , symbolOr
        , symbolOrder
        , symbolOrdered
        , symbolOrdinality
        , symbolOtherwise
        , symbolParameter
        , symbolPartition
        , symbolPath
        , symbolPaths
        , symbolPattern
        , symbolPatterns
        , symbolPower
        , symbolProcedure
        , symbolProcedures
        , symbolProduct
        , symbolProfile
        , symbolProject
        , symbolProperties
        , symbolProperty
        , symbolQueries
        , symbolQuery
        , symbolRadians
        , symbolRcord
        , symbolRead
        , symbolRecords
        , symbolReference
        , symbolRelationship
        , symbolRelationships
        , symbolRemove
        , symbolRename
        , symbolReplace
        , symbolRequire
        , symbolReset
        , symbolResult
        , symbolReturn
        , symbolReturnedGqlstatus
        , symbolRight
        , symbolRollback
        , symbolScalar
        , symbolSchema
        , symbolSchemas
        , symbolSchemata
        , symbolSelect
        , symbolSession
        , symbolSet
        , symbolShortest
        , symbolSimple
        , symbolSin
        , symbolSingle
        , symbolSinh
        , symbolSqrt
        , symbolStart
        , symbolStarts
        , symbolString
        , symbolSubclassOrigin
        , symbolSubstring
        , symbolSum
        , symbolTable
        , symbolTables
        , symbolTan
        , symbolTanh
        , symbolThen
        , symbolTies
        , symbolTime
        , symbolTimestamp
        , symbolTo
        , symbolTrail
        , symbolTrailing
        , symbolTransaction
        , symbolTrim
        , symbolTrue
        , symbolTruncate
        , symbolType
        , symbolTypes
        , symbolUndirected
        , symbolUnion
        , symbolUnique
        , symbolUnit
        , symbolUnitBindingTable
        , symbolUnitTable
        , symbolUnknown
        , symbolUnnest
        , symbolUnwind
        , symbolUpper
        , symbolUse
        , symbolValue
        , symbolValues
        , symbolVertex
        , symbolVertices
        , symbolWalk
        , symbolWhen
        , symbolWhere
        , symbolWith
        , symbolWorkingGraph
        , symbolWrite
        , symbolXor
        , symbolYield
        , symbolZero
        ]

identifierFirstChars :: String
identifierFirstChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> "_"

identifierChars :: String
identifierChars = identifierFirstChars <> ['0' .. '9']
