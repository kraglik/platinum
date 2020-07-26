module ParserSpec where

import Test.Hspec
import Platinum


spec :: Spec
spec = do
  describe "Platinum.AST" $ do
    it "1 is equal to 1" $ do
      1 `shouldBe` 1


