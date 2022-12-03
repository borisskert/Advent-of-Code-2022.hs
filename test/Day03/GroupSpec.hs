module Day03.GroupSpec (spec) where

import Day03.Group (Group (Group), badge, readMany)
import Day03.Item (from)
import Day03.Rucksack (readOne)
import Test.Hspec

exampleInput :: String
exampleInput = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

spec :: Spec
spec = do
  describe "Should read Group of Rucksacks" $ do
    it "Should read example" $ do
      readMany exampleInput
        `shouldBe` [ Group
                       (readOne "vJrwpWtwJgWrhcsFMMfFFhFp")
                       (readOne "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
                       (readOne "PmmdzqPrVvPwwTWBwg"),
                     Group
                       (readOne "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")
                       (readOne "ttgJtRGJQctTZtZT")
                       (readOne "CrZsJsPPZsGzwwsLwLmpwMDw")
                   ]

  describe "Should find the badge of Group" $ do
    it "Should find 'r' in first example Group" $ do
      badge
        ( Group
            (readOne "vJrwpWtwJgWrhcsFMMfFFhFp")
            (readOne "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
            (readOne "PmmdzqPrVvPwwTWBwg")
        )
        `shouldBe` from 'r'
