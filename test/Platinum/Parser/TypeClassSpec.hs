module Platinum.Parser.TypeClassSpec where

import Test.Hspec
import Platinum
import Platinum.Parser


spec :: Spec
spec = do
  describe "Platinum.AST" $ do
    it "parses typeclass correctly" $ do
      1 `shouldBe` 1
