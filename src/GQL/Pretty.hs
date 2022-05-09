{-# LANGUAGE OverloadedStrings #-}

module GQL.Pretty where

import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import GQL.AST
import Prettyprinter

instance Pretty TruthValue where
    pretty = pretty . show

instance Pretty Value where
    pretty (TruthValue v) = pretty v
    pretty (Int n) = pretty n
    pretty (Float f) = pretty f
    pretty (StringLiteral s) = "\"" <> pretty s <> "\""

instance Pretty Identifier where
    pretty (Identifier id) = pretty id

instance Pretty Expr where
    pretty (Value v) = pretty v
    pretty (Name n) = pretty n
    pretty (PropertyReference n k) = pretty n <> "." <> pretty k
    pretty (Not e) = "NOT " <> pretty e
    pretty (Eq e1 e2) = pretty e1 <> " = " <> pretty e2
    pretty (Neq e1 e2) = pretty e1 <> " <> " <> pretty e2
    pretty (Lt e1 e2) = pretty e1 <> " < " <> pretty e2
    pretty (Gt e1 e2) = pretty e1 <> " > " <> pretty e2
    pretty (Leq e1 e2) = pretty e1 <> " <= " <> pretty e2
    pretty (Geq e1 e2) = pretty e1 <> " >= " <> pretty e2
    pretty (Or e1 e2) = pretty e1 <> " OR " <> pretty e2
    pretty (And e1 e2) = pretty e1 <> " AND " <> pretty e2
    pretty (Xor e1 e2) = pretty e1 <> " XOR " <> pretty e2
    pretty (Is e1 e2) = pretty e1 <> " IS " <> pretty e2
    pretty (IsNot e1 e2) = pretty e1 <> " IS NOT " <> pretty e2

instance Pretty Query where
    pretty (Query q) = pretty q
    pretty (Conjunction q1 op q2) = pretty q1 <> " " <> pretty op <> " " <> pretty q2
    pretty (Otherwise q1 q2) = pretty q1 <> "\nOTHERWISE " <> pretty q2

prettyPrintList :: Pretty a => Doc b -> [a] -> Doc b
prettyPrintList sep xs = mconcat $ intersperse sep (pretty <$> xs)

prettyPrintNEList :: Pretty a => Doc b -> NonEmpty a -> Doc b
prettyPrintNEList sep xs = prettyPrintList sep $ toList xs

between :: Pretty a => Doc b -> Doc b -> a -> Doc b
between l r p = l <> pretty p <> r

instance Pretty QueryExpression where
    pretty (FocusedQueryExpression m r) = prettyPrintNEList " " m <> " " <> pretty r
    pretty (AmbientQueryExpression m r) = prettyPrintNEList " " m <> " " <> pretty r

maybeAppend :: Pretty a => Doc b -> Maybe a -> Doc b
maybeAppend _ Nothing = mempty
maybeAppend sep (Just v) = sep <> pretty v

maybePrepend :: Pretty a => Maybe a -> Doc b -> Doc b
maybePrepend Nothing _ = mempty
maybePrepend (Just v) sep = pretty v <> sep

prettyWords :: [Maybe (Doc a)] -> Doc a
prettyWords = mconcat . intersperse " " . catMaybes

instance Pretty SetOperator where
    pretty Union = "\nUNION"
    pretty (UnionQuantifier q) = "\nUNION " <> pretty q
    pretty UnionMax = "\nUNION MAX"
    pretty (Except q) = "EXCEPT" <> maybeAppend " " q
    pretty (Intersect q) = "INTERSECT" <> maybeAppend " " q

instance Pretty SetQuantifier where
    pretty Distinct = "DISTINCT"
    pretty All = "ALL"

instance Pretty ReturnStatement where
    pretty (Return q l) = "\n  RETURN" <> maybeAppend " " q <> " " <> pretty l

instance Pretty ReturnList where
    pretty Asterisk = "*"
    pretty (ReturnList rl) = prettyPrintNEList "1," rl

instance Pretty ReturnItem where
    pretty (ReturnItem e n) = pretty e <> maybeAppend " AS " n

instance Pretty FocusedMatchClause where
    pretty (From g c) = "FROM " <> pretty g <> " " <> prettyPrintNEList " " c

instance Pretty MatchClause where
    pretty (Match om pp wc) = maybePrepend om " " <> "MATCH " <> prettyPrintNEList "2," pp <> maybeAppend " " wc

instance Pretty OptionalOrMandatory where
    pretty Optional = "OPTIONAL"
    pretty Mandatory = "MANDATORY"

instance Pretty PathPattern where
    pretty (PathPattern pv ppp ppe) = maybePrepend pv " = " <> maybePrepend ppp " " <> pretty ppe

instance Pretty PathPatternPrefix where
    pretty Walk = "WALK"
    pretty Trail = "TRAIL"
    pretty Acyclic = "ACYCLIC"
    pretty Simple = "SIMPLE"

instance Pretty PathPatternExpression where
    pretty (PathPatternExpression pts) = prettyPrintNEList "|" pts

instance Pretty PathTerm where
    pretty (PathTermPath p) = pretty p
    pretty (PathTermPattern pp wc l) = parens (pretty pp <> maybeAppend " " wc) <> maybeAppend " " l

instance Pretty Path where
    pretty (PathNode epf) = parens $ pretty epf
    pretty (PathEdge epf ep p) = pretty (PathNode epf) <> pretty ep <> pretty p

instance Pretty ElementPatternFiller where
    pretty (ElementPatternFiller ev ile pl) = prettyWords [pretty <$> ev, pretty <$> ile, braces <$> (pretty <$> pl)]

instance Pretty PropertyList where
    pretty (PropertyList l) = prettyPrintNEList "," l

instance Pretty KeyExprPair where
    pretty (KeyExprPair k e) = pretty k <> ":" <> pretty e

instance Pretty IsLabelExpr where
    pretty (IsLabelExpr le) = "IS " <> pretty le
    pretty (ColonLabelExpr le) = ": " <> pretty le

instance Pretty LabelExpression where
    pretty (LabelExpression lt) = prettyPrintNEList "|" lt

instance Pretty LabelTerm where
    pretty (LabelTerm lf) = prettyPrintNEList "&" lf

instance Pretty LabelFactor where
    pretty (LabelPrimary lp) = pretty lp
    pretty (LabelNegation lp) = "!" <> pretty lp

instance Pretty LabelPrimary where
    pretty (Label id) = pretty id
    pretty Wildcard = "%"
    pretty (ParenthesizedLabelExpression le) = parens $ pretty le
    pretty (BracketedLabelExpression le) = brackets $ pretty le

instance Pretty EdgePattern where
    pretty (EdgePattern epi l) = pretty epi <> maybeAppend " " l

instance Pretty EdgePatternInner where
    pretty (FullEdgePointingLeft epf) = between "<-[" "]-" epf
    pretty (FullEdgeUndirected epf) = between "~[" "]~" epf
    pretty (FullEdgePointingRight epf) = between "-[" "]->" epf

instance Pretty Len where
    pretty (Len q) = braces $ pretty q

instance Pretty Quantifier where
    pretty (Quantifier n) = pretty n
    pretty (CommaQuantifier n1 n2) = pretty n1 <> "4," <> pretty n2

instance Pretty WhereClause where
    pretty (Where e) = "\n  WHERE " <> pretty e
