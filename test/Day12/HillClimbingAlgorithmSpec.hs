module Day12.HillClimbingAlgorithmSpec (spec) where

import Test.Hspec
import Day12.HillClimbingAlgorithm

exampleInput :: String
exampleInput = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

spec :: Spec
spec = do
  describe "What is the fewest steps required to move from your current position to the location that should get the best signal?" $ do
    it "This path reaches the goal in 31 steps, the fewest possible." $ do
      howManyPathSteps exampleInput `shouldBe` 31
