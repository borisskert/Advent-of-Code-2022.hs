module Day11.OperationSpec (spec) where

import Day11.Operation
import Test.Hspec

spec :: Spec
spec = do
  describe "When reading Monkey's Operation from input" $ do
    it "Should read first example" $ do
      read
        "  Operation: new = old * 19"
        `shouldBe` multiplication 19

    it "Should read second example" $ do
      read
        "  Operation: new = old + 6"
        `shouldBe` addition 6

    it "Should read third example" $ do
      read
        "  Operation: new = old * old"
        `shouldBe` square
