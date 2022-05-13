{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..), fromList)
import Data.Text (pack, unpack)
import qualified Data.Text.Lazy as TL
import Debug.Trace
import GQL.AST
import GQL.Arbitrary
import GQL.Lexer
import GQL.Parser
import GQL.Pretty
import Numeric.Natural (Natural)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Property (Result, failed, reason, succeeded)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Pretty.Simple (pShow, pShowNoColor)

pshow :: Pretty a => a -> String
pshow = unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty

pshow' :: Show a => a -> String
pshow' = TL.unpack . pShowNoColor

parsePrettyQuery :: Query -> Result
parsePrettyQuery q = do
    let qStr = pack $ pshow q
    case parse (sc >> query <* eof) "" qStr of
        Right q' ->
            if q' == q
                then succeeded
                else failed{reason = unlines ["Could not equate", pshow q, "with", pshow q', show q, show q']}
        Left err -> failed{reason = unlines [errorBundlePretty err, unpack qStr]}

main :: IO ()
main = quickCheck parsePrettyQuery
