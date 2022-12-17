module Day11.TestSpec (spec) where

import Test.Hspec
import Day11.MonkeyId
import Day11.Test

spec :: Spec
spec = do
  describe "When reading Tests" $ do
    it "Should read first example" $ do
      read "  Test: divisible by 23\n  If true: throw to monkey 2\n  If false: throw to monkey 3" `shouldBe` (test 23 (monkeyId 2) (monkeyId 3))
