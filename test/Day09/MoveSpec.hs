module Day09.MoveSpec (spec) where

import Test.Hspec
import Day09.Motion

exampleInput :: String
exampleInput = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

spec :: Spec
spec = do
  describe "When readFrom String" $ do
    it "Should readFrom exampleInput" $ do
      readMany exampleInput `shouldBe` [right 4, up 4, left 3, down 1, right 4, down 1, left 5, right 2]
