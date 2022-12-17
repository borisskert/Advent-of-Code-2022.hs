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

monkeys :: Monkeys
monkeys =
  fromList
    [ Monkey.from (MonkeyId.from 0) (Items.fromList [Item.from 79, Item.from 98]) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3)) 0,
      Monkey.from (MonkeyId.from 1) (Items.fromList [Item.from 54, Item.from 65, Item.from 75, Item.from 74]) (addition 6) (Test.from 19 (MonkeyId.from 2) (MonkeyId.from 0)) 0,
      Monkey.from (MonkeyId.from 2) (Items.fromList [Item.from 79, Item.from 60, Item.from 97]) square (Test.from 13 (MonkeyId.from 1) (MonkeyId.from 3)) 0,
      Monkey.from (MonkeyId.from 3) (Items.fromList [Item.from 74]) (addition 3) (Test.from 17 (MonkeyId.from 0) (MonkeyId.from 1)) 0
    ]

spec :: Spec
spec = do
  describe "When reading Monkeys from input" $ do
    it "Should read example input" $ do
      read exampleInput
        `shouldBe` monkeys

  describe "When playing rounds divided by worry=3" $ do
    it "Should play one round" $ do
      playRounds 3 1 monkeys
        `shouldBe` fromList
          [ Monkey.from (MonkeyId.from 0) (Items.fromList [Item.from 20, Item.from 23, Item.from 27, Item.from 26]) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3)) 2,
            Monkey.from (MonkeyId.from 1) (Items.fromList [Item.from 2080, Item.from 25, Item.from 167, Item.from 207, Item.from 401, Item.from 1046]) (addition 6) (Test.from 19 (MonkeyId.from 2) (MonkeyId.from 0)) 4,
            Monkey.from (MonkeyId.from 2) (Items.fromList []) square (Test.from 13 (MonkeyId.from 1) (MonkeyId.from 3)) 3,
            Monkey.from (MonkeyId.from 3) (Items.fromList []) (addition 3) (Test.from 17 (MonkeyId.from 0) (MonkeyId.from 1)) 5
          ]

    it "Should play 20 rounds" $ do
      playRounds 3 20 monkeys
        `shouldBe` fromList
          [ Monkey.from (MonkeyId.from 0) (Items.fromList [Item.from 10, Item.from 12, Item.from 14, Item.from 26, Item.from 34]) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3)) 101,
            Monkey.from (MonkeyId.from 1) (Items.fromList [Item.from 245, Item.from 93, Item.from 53, Item.from 199, Item.from 115]) (addition 6) (Test.from 19 (MonkeyId.from 2) (MonkeyId.from 0)) 95,
            Monkey.from (MonkeyId.from 2) (Items.fromList []) square (Test.from 13 (MonkeyId.from 1) (MonkeyId.from 3)) 7,
            Monkey.from (MonkeyId.from 3) (Items.fromList []) (addition 3) (Test.from 17 (MonkeyId.from 0) (MonkeyId.from 1)) 105
          ]
