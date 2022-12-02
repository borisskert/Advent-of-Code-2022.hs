module Day02.HandShapeSpec (spec) where

import Day02.HandShape
import Test.Hspec

spec :: Spec
spec = do
  describe "Should readLeft" $ do
    it "'A' for Rock" $ do
      readLeft 'A' `shouldBe` Rock
    it "'B' for Paper" $ do
      readLeft 'B' `shouldBe` Paper
    it "'C'for Scissors" $ do
      readLeft 'C' `shouldBe` Scissors

  describe "Should readRight" $ do
    it "'X' for Rock" $ do
      readRight 'X' `shouldBe` Rock
    it "'Y' for Paper" $ do
      readRight 'Y' `shouldBe` Paper
    it "'Z'for Scissors" $ do
      readRight 'Z' `shouldBe` Scissors

  describe "Should score" $ do
    it "1 because you chose Rock" $ do
      score Rock `shouldBe` 1
    it "2 because you chose Paper" $ do
      score Paper `shouldBe` 2
    it "3 because you chose Scissors" $ do
      score Scissors `shouldBe` 3
