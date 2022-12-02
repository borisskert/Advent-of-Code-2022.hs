module Day02.MatchSpec (spec) where

import Day02.HandShape (HandShape (Paper, Rock, Scissors))
import Day02.Match (Match (Match), readOne, score)
import Test.Hspec

spec :: Spec
spec = do
  describe "Should readOne" $ do
    it "'A Y' -> Rock vs Paper" $ do
      readOne "A Y" `shouldBe` Match Rock Paper
    it "'B X' -> Paper vs Rock" $ do
      readOne "B X" `shouldBe` Match Paper Rock
    it "'C Z' -> Scissors vs Scissors" $ do
      readOne "C Z" `shouldBe` Match Scissors Scissors

  describe "Should score" $ do
    it "A X -> 4" $ do
      score (Match Rock Rock) `shouldBe` 3 + 1
    it "A Y -> 8" $ do
      score (Match Rock Paper) `shouldBe` 6 + 2
    it "A Z -> 3 (1 vs 3 -> 3 + 6)" $ do
      score (Match Rock Scissors) `shouldBe` 0 + 3
    
    it "B X -> 1" $ do
      score (Match Paper Rock) `shouldBe` 0 + 1
    it "B Y -> 5" $ do
      score (Match Paper Paper) `shouldBe` 3 + 2
    it "B Z -> 9" $ do
      score (Match Paper Scissors) `shouldBe` 6 + 3

    it "C X -> 7" $ do
      score (Match Scissors Rock) `shouldBe` 6 + 1
    it "C Y -> 2" $ do
      score (Match Scissors Paper) `shouldBe` 0 + 2
    it "C Z -> 6" $ do
       score (Match Scissors Scissors) `shouldBe` 3 + 3
