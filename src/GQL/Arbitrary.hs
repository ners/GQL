module GQL.Arbitrary where

import Control.Monad (replicateM)
import Data.List.NonEmpty (NonEmpty, fromList, singleton)
import Data.Ratio (Rational)
import Data.Word (Word)
import Debug.Trace (trace)
import GHC.Generics
import GHC.OverloadedLabels (IsLabel)
import GQL.AST
import Numeric.Natural (Natural)
import Test.QuickCheck

letters = ['a' .. 'z'] <> ['A' .. 'Z']
numbers = ['0' .. '9']

randomWord = sized $ \n -> do
    k <- chooseInt (1, n)
    replicateM k $ elements $ letters <> numbers

instance Arbitrary Query where
    arbitrary = sized $ \n ->
        oneof $
            if n < 2
                then one
                else
                    if n < 4
                        then map (resize $ n `div` 2) two
                        else map (resize $ n `div` 4) four
      where
        one = [Query <$> arbitrary]
        two = one <> [Otherwise <$> arbitrary <*> arbitrary]
        four = two <> [Conjunction <$> arbitrary <*> arbitrary <*> arbitrary]

instance Arbitrary SetOperator where
    arbitrary = sized $ \n ->
        oneof $
            if n < 2
                then
                    [ pure Union
                    , pure UnionMax
                    , pure $ Except Nothing
                    , pure $ Intersect Nothing
                    ]
                else
                    map
                        (resize $ n `div` 2)
                        [ pure Union
                        , UnionQuantifier <$> arbitrary
                        , pure UnionMax
                        , Except <$> arbitrary
                        , Intersect <$> arbitrary
                        ]

instance Arbitrary SetQuantifier where
    arbitrary = elements [Distinct, All]

instance Arbitrary QueryExpression where
    arbitrary = sized $ \n ->
        oneof $
            map
                (resize $ n `div` 2)
                [ FocusedQueryExpression <$> arbitrary <*> arbitrary
                , AmbientQueryExpression <$> arbitrary <*> arbitrary
                ]

instance Arbitrary FocusedMatchClause where
    arbitrary = From <$> arbitrary <*> arbitrary

instance Arbitrary MatchClause where
    arbitrary = Match <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary OptionalOrMandatory where
    arbitrary = elements [Optional, Mandatory]

instance Arbitrary WhereClause where
    arbitrary = Where <$> arbitrary

instance Arbitrary PathPattern where
    arbitrary = PathPattern <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PathPatternExpression where
    arbitrary = PathPatternExpression <$> arbitrary

instance Arbitrary PathPatternPrefix where
    arbitrary = elements [Walk, Trail, Acyclic, Simple]

instance Arbitrary PathTerm where
    arbitrary = sized $ \n ->
        oneof $
            if n < 2
                then one
                else map (resize $ n `div` 2) two
      where
        one = [PathTermPath <$> arbitrary]
        two = one <> [PathTermPattern <$> arbitrary <*> arbitrary <*> arbitrary]

instance Arbitrary Path where
    arbitrary = sized $ \n ->
            oneof $
                if n < 2
                    then one
                    else map (resize $ n `div` 2) two
      where
        one = [PathNode <$> arbitrary]
        two = one <> [PathEdge <$> arbitrary <*> arbitrary <*> arbitrary]

instance Arbitrary Len where
    arbitrary = Len <$> arbitrary

instance Arbitrary EdgePattern where
    arbitrary = EdgePattern <$> arbitrary <*> arbitrary

instance Arbitrary EdgePatternInner where
    arbitrary =
        oneof
            [ FullEdgePointingLeft <$> arbitrary
            , FullEdgeUndirected <$> arbitrary
            , FullEdgePointingRight <$> arbitrary
            ]

instance Arbitrary ElementPatternFiller where
    arbitrary = ElementPatternFiller <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PropertyList where
    arbitrary = PropertyList <$> arbitrary

instance Arbitrary KeyExprPair where
    arbitrary = KeyExprPair <$> arbitrary <*> arbitrary

instance Arbitrary ReturnStatement where
    arbitrary = Return <$> arbitrary <*> arbitrary

instance Arbitrary ReturnList where
    arbitrary = sized $ \n ->
        oneof $ if n < 3 then one else map (resize $ n `div` 3) three
      where
        one = [pure Asterisk]
        three = one <> [ReturnList <$> arbitrary]

instance Arbitrary ReturnItem where
    arbitrary = ReturnItem <$> arbitrary <*> arbitrary

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural

instance Arbitrary Quantifier where
    arbitrary = oneof [Quantifier <$> arbitrary, CommaQuantifier <$> arbitrary <*> arbitrary]

instance Arbitrary Expr where
    arbitrary = sized $ \n ->
        oneof $
            if n < 2
                then one
                else
                    if n < 4
                        then map (resize $ n `div` 2) two
                        else map (resize $ n `div` 4) four
      where
        one = [Value <$> arbitrary, Name <$> arbitrary]
        two =
            one
                <> [ PropertyReference <$> arbitrary <*> arbitrary
                   , Not <$> arbitrary
                   , Is <$> arbitrary <*> arbitrary
                   , IsNot <$> arbitrary <*> arbitrary
                   ]
        four =
            two
                <> [ Eq <$> arbitrary <*> arbitrary
                   , Neq <$> arbitrary <*> arbitrary
                   , Lt <$> arbitrary <*> arbitrary
                   , Gt <$> arbitrary <*> arbitrary
                   , Leq <$> arbitrary <*> arbitrary
                   , Geq <$> arbitrary <*> arbitrary
                   , Or <$> arbitrary <*> arbitrary
                   , And <$> arbitrary <*> arbitrary
                   , Xor <$> arbitrary <*> arbitrary
                   ]

instance Arbitrary TruthValue where
    arbitrary = elements [GQL.AST.True, GQL.AST.False, Unknown, Null]

instance Arbitrary IsLabelExpr where
    arbitrary = oneof [IsLabelExpr <$> arbitrary, ColonLabelExpr <$> arbitrary]

instance Arbitrary LabelFactor where
    arbitrary = oneof [LabelPrimary <$> arbitrary, LabelNegation <$> arbitrary]

instance Arbitrary LabelPrimary where
    arbitrary = sized $ \n ->
        oneof $
            if n < 2
                then one
                else map (resize $ n `div` 2) two
      where
        one =
            [ Label <$> arbitrary
            , pure Wildcard
            ]
        two =
            one
                <> [ ParenthesizedLabelExpression <$> arbitrary
                   , BracketedLabelExpression <$> arbitrary
                   ]

instance Arbitrary Value where
    arbitrary =
        oneof
            [ TruthValue <$> arbitrary
            , Int <$> arbitrary
            , Float <$> arbitrary
            , StringLiteral <$> randomWord
            ]

instance Arbitrary Identifier where
    arbitrary = Identifier <$> ((:) <$> elements letters <*> randomWord)

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = sized $ \n ->
        resize (n `div` 2) $
            fromList <$> ((:) <$> arbitrary <*> arbitrary)

instance Arbitrary LabelExpression where
    arbitrary = LabelExpression <$> arbitrary

instance Arbitrary LabelTerm where
    arbitrary = LabelTerm <$> arbitrary
