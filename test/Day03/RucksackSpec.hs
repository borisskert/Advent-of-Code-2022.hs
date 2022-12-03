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

  describe "Find containing items" $ do
    it "Should determine if a Rucksack contains an Item" $ do
      (Rucksack (fromMany "vJrwpWtwJgWr") (fromMany "hcsFMMfFFhFp") `contains` from 'r') `shouldBe` True 
    it "Should determine if a Rucksack does not contain an Item" $ do
      (Rucksack (fromMany "vJrwpWtwJgWr") (fromMany "hcsFMMfFFhFp") `contains` from 'a') `shouldBe` False
