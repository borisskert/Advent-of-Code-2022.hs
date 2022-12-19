module Day12.HillClimbingAlgorithmSpec (spec) where

import Day12.HillClimbingAlgorithm
import Test.Hspec

exampleInput :: String
exampleInput = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

spec :: Spec
spec = do
  describe "What is the fewest steps required to move from your current position to the location that should get the best signal?" $ do
    it "This path reaches the goal in 31 steps, the fewest possible." $ do
      howManyPathStepsStartingFromS exampleInput `shouldBe` 31

  describe "What is the fewest steps required to move starting from any square with elevation a to the location that should get the best signal?" $ do
    it "This path reaches the goal in only 29 steps, the fewest possible." $ do
      howManyPathStepsStartingFromAnyA exampleInput `shouldBe` 29
