module Day11.MonkeysSpec (spec) where

import qualified Day11.Item as Item
import qualified Day11.Items as Items
import qualified Day11.Monkey as Monkey
import qualified Day11.MonkeyId as MonkeyId
import Day11.Monkeys
import Day11.Operation
import qualified Day11.Test as Test
import Test.Hspec

exampleInput :: String
exampleInput = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"

spec :: Spec
spec = do
  describe "When reading Monkeys from input" $ do
    it "Should read example input" $ do
      read exampleInput
        `shouldBe` fromList
          [ Monkey.from (MonkeyId.from 0) (Items.fromList [Item.from 79, Item.from 98]) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3)),
            Monkey.from (MonkeyId.from 1) (Items.fromList [Item.from 54, Item.from 65, Item.from 75, Item.from 74]) (addition 6) (Test.from 19 (MonkeyId.from 2) (MonkeyId.from 0)),
            Monkey.from (MonkeyId.from 2) (Items.fromList [Item.from 79, Item.from 60, Item.from 97]) square (Test.from 13 (MonkeyId.from 1) (MonkeyId.from 3)),
            Monkey.from (MonkeyId.from 3) (Items.fromList [Item.from 74]) (addition 3) (Test.from 17 (MonkeyId.from 0) (MonkeyId.from 1))
          ]
