module Day11.MonkeySpec (spec) where

import qualified Day11.Item as Item
import qualified Day11.ItemThrow as ItemThrow (from)
import Day11.Items
import Day11.Monkey
import qualified Day11.MonkeyId as MonkeyId
import Day11.Operation
import qualified Day11.Test as Test
import Test.Hspec

monkey0 :: Monkey
monkey0 = from (MonkeyId.from 0) (fromList [Item.from 79, Item.from 98]) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3)) 0

spec :: Spec
spec = do
  describe "When reading Monkeys from input" $ do
    it "Should read first monkey from example" $ do
      read "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3"
        `shouldBe` monkey0

  describe "When inspecting items" $ do
    it "Should inspect first item" $ do
      inspectItem 3 monkey0
        `shouldBe` Just
          ( ItemThrow.from (Item.from 500) (MonkeyId.from 3),
            from (MonkeyId.from 0) (fromList [Item.from 98]) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3)) 1
          )

    it "Should inspect all items" $ do
      inspectItems 3 monkey0
        `shouldBe` ( [ItemThrow.from (Item.from 500) (MonkeyId.from 3), ItemThrow.from (Item.from 620) (MonkeyId.from 3)],
                     from (MonkeyId.from 0) (fromList []) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3)) 2
                   )

  describe "When catching items" $ do
    it "Should catch item" $ do
      catch (Item.from 500) monkey0 `shouldBe` from (MonkeyId.from 0) (fromList [Item.from 79, Item.from 98, Item.from 500]) (multiplication 19) (Test.from 23 (MonkeyId.from 2) (MonkeyId.from 3)) 0
