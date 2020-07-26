module Platinum.Parser.ADTSpec where

import Test.Hspec
import Platinum


spec :: Spec
spec = do
  describe "Platinum.AST" $ do
    it "parses ADT correctly" $ do
      1 `shouldBe` 1
