module Day11.TestSpec (spec) where

import qualified Day11.MonkeyId as MonkeyId
import Day11.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "When reading Tests" $ do
    it "Should read first example" $ do
      read
        "  Test: divisible by 23\n  If true: throw to monkey 2\n  If false: throw to monkey 3"
        `shouldBe` from 23 (MonkeyId.from 2) (MonkeyId.from 3)
