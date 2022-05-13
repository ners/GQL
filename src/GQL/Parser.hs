module GQL.Parser where

import Control.Monad (mplus)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Control.Monad.Combinators.NonEmpty as NonEmpty
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text, pack, unpack)
import Debug.Trace
import GHC.Read (choose)
import GQL.AST
import GQL.Lexer
import Text.Megaparsec
import Text.Megaparsec.Char

fullQuery :: Parser Query
fullQuery = query <* eof

query :: Parser Query
query = do
    trace "parsing query" $ do
        qe <- queryExpression
        choice
            [ Conjunction qe <$> setOperator <*> query
            , symbolOtherwise >> Otherwise qe <$> queryExpression
            , pure $ Query qe
            ]

queryExpression :: Parser QueryExpression
queryExpression =
    trace "parsing queryExpression" $
        choice
            [ FocusedQueryExpression <$> NonEmpty.some focusedMatchClause <*> returnStatement
            , AmbientQueryExpression <$> NonEmpty.some matchClause <*> returnStatement
            ]

focusedMatchClause :: Parser FocusedMatchClause
focusedMatchClause =
    trace "parsing focusedMatchClause" $
        symbolFrom >> From <$> graphName <*> NonEmpty.some matchClause

matchClause :: Parser MatchClause
matchClause =
    trace "parsing matchClause" $
        Match <$> optional optionalOrMandatory <*> (symbolMatch >> NonEmpty.sepBy1 pathPattern symbolComma) <*> optional whereClause

optionalOrMandatory :: Parser OptionalOrMandatory
optionalOrMandatory =
    trace "parsing optionalOrMandatory" $
        choice
            [ Optional <$ symbolOptional
            , Mandatory <$ symbolMandatory
            ]

whereClause :: Parser WhereClause
whereClause = trace "parsing whereClause" $ symbolWhere >> Where <$> expr

returnStatement :: Parser ReturnStatement
returnStatement = trace "parsing returnStatement" $ symbolReturn >> Return <$> optional setQuantifier <*> returnList

returnList :: Parser ReturnList
returnList =
    trace "parsing returnList" $
        choice
            [ Asterisk <$ symbolAsterisk
            , ReturnList <$> NonEmpty.sepBy1 returnItem symbolComma
            ]

pathPattern :: Parser PathPattern
pathPattern =
    trace "parsing pathPattern" $
        PathPattern
            <$> optional (try $ pathVariable <* symbolEq)
            <*> optional pathPatternPrefix
            <*> pathPatternExpression

pathPatternPrefix :: Parser PathPatternPrefix
pathPatternPrefix =
    trace "parsing pathPatternPrefix" $
        choice
            [ Walk <$ symbolWalk
            , Trail <$ symbolTrail
            , Acyclic <$ symbolAcyclic
            , Simple <$ symbolSimple
            ]

pathPatternExpression :: Parser PathPatternExpression
pathPatternExpression =
    trace "parsing pathPatternExpression" $
        PathPatternExpression <$> NonEmpty.sepBy1 pathTerm symbolVerticalBar

pathTerm :: Parser PathTerm
pathTerm =
    trace "parsing pathTerm" $
        choice
            [ try pathTermPattern
            , pathTermPath
            ]
  where
    pathTermPattern = trace "parsing pathTermPattern" $ do
        (pathPattern, whereClause) <- betweenParens $ (,) <$> pathPattern <*> optional whereClause
        len <- optional len
        return $ PathTermPattern pathPattern whereClause len
    pathTermPath = trace "parsing pathTermPath" $ PathTermPath <$> path

path :: Parser Path
path =
    trace "parsing path" $ do
        filler <- betweenParens elementPatternFiller
        choice
            [ PathEdge filler <$> edgePattern <*> path
            , pure $ PathNode filler
            ]

edgePattern :: Parser EdgePattern
edgePattern = trace "parsing edgePattern" $ EdgePattern <$> edgePatternInner <*> optional len

edgePatternInner :: Parser EdgePatternInner
edgePatternInner =
    trace "parsing edgePatternInner" $
        choice
            [ FullEdgePointingLeft <$> between symbolLeftArrowBracket symbolRightBracketMinus elementPatternFiller
            , FullEdgeUndirected <$> between symbolTildeLeftBracket symbolRightBracketTilde elementPatternFiller
            , FullEdgePointingRight <$> between symbolMinusLeftBracket symbolBracketRightArrow elementPatternFiller
            ]

elementPatternFiller :: Parser ElementPatternFiller
elementPatternFiller =
    trace "parsing elementPatternFiller" $
        ElementPatternFiller <$> optional elementVariable <*> optional isLabelExpr <*> optional (betweenBraces propertyList)

propertyList :: Parser PropertyList
propertyList = trace "parsing propertyList" $ PropertyList <$> NonEmpty.sepBy1 keyExprPair symbolComma

keyExprPair :: Parser KeyExprPair
keyExprPair = trace "parsing keyExprPair" $ KeyExprPair <$> key <*> (symbolColon >> expr)

returnItem :: Parser ReturnItem
returnItem = trace "parsing returnItem" $ ReturnItem <$> expr <*> optional (symbolAs >> name)

setOperator :: Parser SetOperator
setOperator =
    trace "parsing setOperator" $
        choice
            [ symbolUnion
                >> choice
                    [ UnionQuantifier <$> setQuantifier
                    , UnionMax <$ symbolMax
                    , pure Union
                    ]
            , symbolExcept >> Except <$> optional setQuantifier
            , symbolIntersect >> Intersect <$> optional setQuantifier
            ]

setQuantifier :: Parser SetQuantifier
setQuantifier = trace "parsing setQuantifier" $ choice [Distinct <$ symbolDistinct, All <$ symbolAll]

truthValue :: Parser TruthValue
truthValue =
    trace "parsing truthValue" $
        choice
            [ GQL.AST.True <$ symbolTrue
            , GQL.AST.False <$ symbolFalse
            , Unknown <$ symbolUnknown
            , Null <$ symbolNull
            ]

expr :: Parser Expr
expr =
    trace "parsing expr" $
        makeExprParser
            term
            [ [Prefix (Not <$ symbolNot)]
            ,
                [ InfixL (Or <$ symbolOr)
                , InfixL (And <$ symbolAnd)
                , InfixL (Xor <$ symbolXor)
                ]
            ,
                [ InfixL (Eq <$ symbolEq)
                , InfixL (Neq <$ symbolNeq)
                , InfixL (Geq <$ symbolGeq)
                , InfixL (Leq <$ symbolLeq)
                , InfixL (Gt <$ symbolGt)
                , InfixL (Lt <$ symbolLt)
                ]
            , [Postfix $ symbolIs >> isOrNot]
            ]
  where
    term =
        asum
            [ try $ PropertyReference <$> key <*> (symbolPeriod >> name)
            , Value <$> value
            , Name <$> name
            ]
    isOrNot :: Parser (Expr -> Expr)
    isOrNot = do
        f <- choice [symbolNot >> pure IsNot, pure Is]
        flip f <$> truthValue

isLabelExpr :: Parser IsLabelExpr
isLabelExpr =
    trace "parsing isLabelExpr" $
        choice
            [ symbolIs >> IsLabelExpr <$> labelExpression
            , symbolColon >> ColonLabelExpr <$> labelExpression
            ]

labelExpression :: Parser LabelExpression
labelExpression = trace "parsing labelExpression" $ LabelExpression <$> NonEmpty.sepBy1 labelTerm symbolVerticalBar

labelTerm :: Parser LabelTerm
labelTerm = trace "parsing labelTerm" $ LabelTerm <$> NonEmpty.sepBy1 labelFactor symbolAmpersand

labelFactor :: Parser LabelFactor
labelFactor =
    trace "parsing labelFactor" $
        choice
            [ symbolExclamationMark >> LabelNegation <$> labelPrimary
            , LabelPrimary <$> labelPrimary
            ]

labelPrimary :: Parser LabelPrimary
labelPrimary =
    trace "parsing labelPrimary" $
        choice
            [ ParenthesizedLabelExpression <$> betweenParens labelExpression
            , BracketedLabelExpression <$> betweenBrackets labelExpression
            , Wildcard <$ symbolPercent
            , GQL.AST.Label <$> identifier
            ]

len :: Parser Len
len = trace "parsing len" $ Len <$> quantifier

quantifier :: Parser Quantifier
quantifier =
    trace "parsing quantifier" $
        choice
            [ Quantifier <$> natural
            , commaQuantifier
            ]
  where
    commaQuantifier = CommaQuantifier <$> natural <*> (symbolComma >> natural)

value :: Parser Value
value =
    trace "parsing value" $
        choice
            [ TruthValue <$> truthValue
            , try $ Float <$> signedFloat
            , Int <$> integer
            , StringLiteral <$> stringLiteral
            ]

identifier :: Parser Identifier
identifier =
    try $ do
        ident <- lexeme $ (:) <$> oneOf identifierFirstChars <*> many (oneOf identifierChars)
        case parseMaybe keyword $ pack ident of
            Just keyword -> fail $ "keyword " <> unpack keyword <> " cannot be an identifier"
            Nothing -> trace ("parsing identifier: " <> ident) $ return $ Identifier ident

key :: Parser Key
key = trace "parsing key" identifier

name :: Parser Name
name = trace "parsing name" identifier

graphName :: Parser GraphName
graphName = trace "parsing graphName" identifier

pathVariable :: Parser PathVariable
pathVariable = trace "parsing pathVariable" identifier

elementVariable :: Parser ElementVariable
elementVariable = trace "parsing elementVariable" identifier
