module Day09.RopeBridgeSpec (spec) where

import Day09.RopeBridge
import Test.Hspec

exampleInput :: String
exampleInput = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

spec :: Spec
spec = do
  describe "How many positions does the tail of the rope visit at least once?" $ do
    it "So, there are 13 positions the tail visited at least once." $ do
      howManyTailPositions exampleInput `shouldBe` 13
