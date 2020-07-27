module Platinum.Parser.LiteralSpec where

import Data.Text (pack)

import Test.Hspec
import Test.Hspec.Megaparsec

import Text.Megaparsec

import Platinum
import Platinum.AST
import Platinum.Parser

spec :: Spec
spec = do
  let parser = runParser (literal <* eof) "code"

  describe "integer parser" $ do

    it "parses integer correctly" $ do
      parser (pack "1") `parseSatisfies` \(Literal (Integer x) _) -> x == 1

    it "parses signed integer correctly" $ do
      parser (pack "-1") `parseSatisfies` \(Literal (Integer x) _) -> x == (-1)

    it "fails on incorrect integer" $ do
      parser `shouldFailOn` pack "1a"
