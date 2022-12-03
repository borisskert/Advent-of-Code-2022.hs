module Day03.RucksackReorganizationSpec (spec) where

import Day03.RucksackReorganization
import Test.Hspec

exampleInput :: String
exampleInput = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

spec :: Spec
spec = do
  describe "Find the item type that appears in both compartments of each rucksack. What is the sum of the priorities of those item types?" $ do
    it "In the above example, the priority of the item type that appears in both compartments of each rucksack is 16 (p), 38 (L), 42 (P), 22 (v), 20 (t), and 19 (s); the sum of these is 157" $ do
      prioritySum exampleInput `shouldBe` 157
