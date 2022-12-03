module Day03.RucksackSpec (spec) where

import Day03.Item
import Day03.Rucksack
import Test.Hspec

spec :: Spec
spec = do
  describe "Should read Rucksacks" $ do
    it "Should read 'vJrwpWtwJgWrhcsFMMfFFhFp'" $ do
      readOne "vJrwpWtwJgWrhcsFMMfFFhFp" `shouldBe` Rucksack (fromMany "vJrwpWtwJgWr") (fromMany "hcsFMMfFFhFp")
      
  describe "Find items which apear in both compartments" $ do
    it "The only item type that appears in both compartments is lowercase p" $ do
      itemInBoth (Rucksack (fromMany "vJrwpWtwJgWr") (fromMany "hcsFMMfFFhFp")) `shouldBe` Just (from 'p')
