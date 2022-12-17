module Day11.MonkeyInTheMiddleSpec (spec) where

import Day11.MonkeyInTheMiddle (monkeyLevel)
import Test.Hspec

exampleInput :: String
exampleInput = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"

spec :: Spec
spec = do
  describe "Figure out which monkeys to chase by counting how many items they inspect over 20 rounds. What is the level of monkey business after 20 rounds of stuff-slinging simian shenanigans?" $ do
    it "In this example, the two most active monkeys inspected items 101 and 105 times. The level of monkey business in this situation can be found by multiplying these together: 10605" $ do
      monkeyLevel 3 20 exampleInput `shouldBe` 10605

  describe "Worry levels are no longer divided by three after each item is inspected; you'll need to find another way to keep your worry levels manageable. Starting again from the initial state in your puzzle input, what is the level of monkey business after 10000 rounds?" $ do
    it "After 10000 rounds, the two most active monkeys inspected items 52166 and 52013 times. Multiplying these together, the level of monkey business in this situation is now 2713310158" $ do
      monkeyLevel 1 10000 exampleInput `shouldBe` 2713310158

  describe "With these new rules, you can still figure out the monkey business after 10000 rounds. Using the same example above:" $ do
    it "== After round 1 ==" $ do
      monkeyLevel 1 1 exampleInput `shouldBe` (4 * 6)

    it "== After round 20 ==" $ do
      monkeyLevel 1 20 exampleInput `shouldBe` (103 * 99)

    it "== After round 1000 ==" $ do
      monkeyLevel 1 1000 exampleInput `shouldBe` (5204 * 5192)
