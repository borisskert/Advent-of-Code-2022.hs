module Day02.RockPaperScissorsSpec (spec) where

import Day02.RockPaperScissors (totalScore, totalScoreDecrypted)
import Test.Hspec

exampleInput :: String
exampleInput = "A Y\nB X\nC Z\n"

exampleInputReversed :: String
exampleInputReversed = "C Z\nB X\nA Y\n"

spec :: Spec
spec = do
  describe "What would your total score be if everything goes exactly according to your strategy guide?" $ do
    it "In this example, if you were to follow the strategy guide, you would get a total score of 15 (8 + 1 + 6)" $ do
      totalScore exampleInput `shouldBe` 15
      totalScore exampleInputReversed `shouldBe` 15

  describe "What would your total score be if everything goes exactly according to your strategy guide? (adjusted)" $ do
    it "Now that you're correctly decrypting the ultra top secret strategy guide, you would get a total score of 12" $ do
      totalScoreDecrypted exampleInput `shouldBe` 12
