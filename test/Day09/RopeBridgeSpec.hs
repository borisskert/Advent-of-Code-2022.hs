module Day09.RopeBridgeSpec (spec) where

import Day09.RopeBridge
import Test.Hspec

exampleInput :: String
exampleInput = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

largerExampleInput :: String
largerExampleInput = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"

spec :: Spec
spec = do
  describe "How many positions does the tail of the rope visit at least once?" $ do
    it "So, there are 13 positions the tail visited at least once." $ do
      howManyTailPositions 1 exampleInput `shouldBe` 13

  describe "How many positions does the tail of the rope visit at least once?" $ do
    it "In this example, the tail never moves, and so it only visits 1 position." $ do
      howManyTailPositions 6 exampleInput `shouldBe` 1

  describe "Simulate your complete series of motions on a larger rope with ten knots. How many positions does the tail of the rope visit at least once?" $ do
    it "Now, the tail (9) visits 36 positions (including s) at least once:" $ do
      howManyTailPositions 9 largerExampleInput `shouldBe` 36
