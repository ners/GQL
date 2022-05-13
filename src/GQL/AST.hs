module GQL.AST where

import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty, toList)
import Numeric.Natural (Natural)

-- Source of truth=
-- https=//github.com/OlofMorra/GQL-parser/blob/main/src/main/java/antlr/GqlParser.g4

-- query
--     : queryExpression (queryConjunction queryExpression)* EOF
--     ;
-- queryConjunction
--     : setOperator
--     | OTHERWISE
data Query
    = Query QueryExpression
    | Conjunction QueryExpression SetOperator Query
    | Otherwise QueryExpression QueryExpression
    deriving (Show, Eq)

-- setOperator
--     : unionOperator
--     | otherSetOperator
-- unionOperator
--     : UNION (setQuantifier | MAX)?
-- otherSetOperator
--     : (EXCEPT | INTERSECT) setQuantifier?
data SetOperator
    = Union
    | UnionQuantifier SetQuantifier
    | UnionMax
    | Except (Maybe SetQuantifier)
    | Intersect (Maybe SetQuantifier)
    deriving (Show, Eq)

data SetQuantifier
    = Distinct
    | All
    deriving (Show, Eq)

-- queryExpression
--     : focusedQueryExpression
--     | ambientQueryExpression
-- focusedQueryExpression
--     : focusedMatchClause+ returnStatement
-- ambientQueryExpression
--     : matchClause+ returnStatement
data QueryExpression
    = FocusedQueryExpression (NonEmpty FocusedMatchClause) ReturnStatement
    | AmbientQueryExpression (NonEmpty MatchClause) ReturnStatement
    deriving (Show, Eq)

-- focusedMatchClause
--     : FROM graphName matchClause+
data FocusedMatchClause = From GraphName (NonEmpty MatchClause)
    deriving (Show, Eq)

-- matchClause
--     : (OPTIONAL | MANDATORY)? MATCH pathPatternList whereClause?
-- pathPatternList
--     : pathPattern (COMMA pathPattern)*
data MatchClause = Match (Maybe OptionalOrMandatory) (NonEmpty PathPattern) (Maybe WhereClause)
    deriving (Show, Eq)

data OptionalOrMandatory = Optional | Mandatory
    deriving (Show, Eq)

-- whereClause
--     : WHERE expr
newtype WhereClause = Where Expr
    deriving (Show, Eq)

-- returnStatement
--     : RETURN setQuantifier? (ASTERISK | returnList)
data ReturnStatement = Return (Maybe SetQuantifier) ReturnList
    deriving (Show, Eq)

-- returnList
--     : returnItem (COMMA returnItem)*
data ReturnList = Asterisk | ReturnList (NonEmpty ReturnItem)
    deriving (Show, Eq)

-- pathPattern
--     : (pathVariable EQ)? pathPatternPrefix? pathPatternExpression
data PathPattern = PathPattern (Maybe PathVariable) (Maybe PathPatternPrefix) PathPatternExpression
    deriving (Show, Eq)

-- pathPatternPrefix
--     : WALK
--     | TRAIL
--     | ACYCLIC
--     | SIMPLE
data PathPatternPrefix = Walk | Trail | Acyclic | Simple
    deriving (Show, Eq)

-- pathPatternExpression
--     : pathTerm //(VERTICAL_BAR pathTerm)*
newtype PathPatternExpression = PathPatternExpression (NonEmpty PathTerm)
    deriving (Show, Eq)

-- pathTerm
--     : path
-- //    | LEFT_PAREN pathPattern whereClause? RIGHT_PAREN len?
data PathTerm
    = PathTermPath Path
    | PathTermPattern PathPattern (Maybe WhereClause) (Maybe Len)
    deriving (Show, Eq)

-- path
--     : nodePattern (edgePattern nodePattern)*
-- nodePattern
--     : LEFT_PAREN elementPatternFiller RIGHT_PAREN
data Path
    = PathNode ElementPatternFiller
    | PathEdge ElementPatternFiller EdgePattern Path
    deriving (Show, Eq)

-- edgePattern
--     : (fullEdgePointingLeft | fullEdgeUndirected | fullEdgePointingRight) len?
-- fullEdgePointingLeft
--     : LEFT_ARROW_BRACKET elementPatternFiller RIGHT_BRACKET_MINUS
-- fullEdgeUndirected
--     : TILDE_LEFT_BRACKET elementPatternFiller RIGHT_BRACKET_TILDE
-- fullEdgePointingRight
--     : MINUS_LEFT_BRACKET elementPatternFiller BRACKET_RIGHT_ARROW
data EdgePattern = EdgePattern EdgePatternInner (Maybe Len)
    deriving (Show, Eq)

data EdgePatternInner
    = FullEdgePointingLeft ElementPatternFiller
    | FullEdgeUndirected ElementPatternFiller
    | FullEdgePointingRight ElementPatternFiller
    deriving (Show, Eq)

-- // TODO: change to syntax in report
-- elementPatternFiller
--     : elementVariable? isLabelExpr? (LEFT_BRACE propertyList RIGHT_BRACE)?
data ElementPatternFiller = ElementPatternFiller (Maybe ElementVariable) (Maybe IsLabelExpr) (Maybe PropertyList)
    deriving (Show, Eq)

-- propertyList
--     : key COLON expr (COMMA key COLON expr)*
newtype PropertyList = PropertyList (NonEmpty KeyExprPair)
    deriving (Show, Eq)

data KeyExprPair = KeyExprPair Key Expr
    deriving (Show, Eq)

-- returnItem
--     : expr (AS name)?
data ReturnItem = ReturnItem Expr (Maybe Name)
    deriving (Show, Eq)

-- len
--     : LEFT_BRACE quantifier RIGHT_BRACE
newtype Len = Len Quantifier
    deriving (Show, Eq)

-- quantifier
--     : UNSIGNED_INTEGER COMMA UNSIGNED_INTEGER
--     | UNSIGNED_INTEGER
data Quantifier
    = Quantifier Natural
    | CommaQuantifier Natural Natural
    deriving (Show, Eq)

data Expr
    = Value Value
    | Name Name
    | PropertyReference Name Key
    | Not Expr
    | Eq Expr Expr
    | Neq Expr Expr
    | Lt Expr Expr
    | Gt Expr Expr
    | Leq Expr Expr
    | Geq Expr Expr
    | Or Expr Expr
    | And Expr Expr
    | Xor Expr Expr
    | Is Expr TruthValue
    | IsNot Expr TruthValue
    deriving (Show, Eq)

data TruthValue = True | False | Unknown | Null
    deriving (Show, Eq)

-- isLabelExpr
--     : (IS | COLON) labelExpression
data IsLabelExpr
    = IsLabelExpr LabelExpression
    | ColonLabelExpr LabelExpression
    deriving (Show, Eq)

-- labelExpression
--     : labelTerm (VERTICAL_BAR labelTerm)*
newtype LabelExpression = LabelExpression (NonEmpty LabelTerm) deriving (Show, Eq)

-- labelTerm
--     : labelFactor (AMPERSAND labelFactor)*
newtype LabelTerm = LabelTerm (NonEmpty LabelFactor) deriving (Show, Eq)

-- labelFactor
--     : labelPrimary
--     | labelNegation
--
-- labelNegation
--     : EXCLAMATION_MARK labelPrimary
data LabelFactor
    = LabelPrimary LabelPrimary
    | LabelNegation LabelPrimary
    deriving (Show, Eq)

-- labelPrimary
--     : label
--     | labelWildcard
--     | parenthesizedLabelExpression
-- labelWildcard
--     : PERCENT
--parenthesizedLabelExpression
--    : LEFT_PAREN labelExpression RIGHT_PAREN
--    | LEFT_BRACKET labelExpression RIGHT_BRACKET
data LabelPrimary
    = Label Identifier
    | Wildcard
    | ParenthesizedLabelExpression LabelExpression
    | BracketedLabelExpression LabelExpression
    deriving (Show, Eq)

data Value
    = TruthValue TruthValue
    | Int Integer
    | Float Double
    | StringLiteral String
    deriving (Show, Eq)

newtype Identifier = Identifier String
    deriving (Show, Eq)

type GraphName = Name
type Name = Identifier
type PathVariable = Identifier
type ElementVariable = Identifier
type Key = Identifier
