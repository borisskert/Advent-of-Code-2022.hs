module Day11.MonkeySpec (spec) where

import qualified Day11.Item as Item
import Day11.Items
import Day11.Monkey
import qualified Day11.MonkeyId as MonkeyId
import Day11.Operation
import qualified Day11.Test as Test
import Test.Hspec

spec :: Spec
spec = do
  describe "When reading Monkeys from input" $ do
    it "Should read first monkey from example" $ do
      read "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3"
        `shouldBe` from (MonkeyId.from 0) (fromList [Item.from 79, Item.from 98]) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3))
