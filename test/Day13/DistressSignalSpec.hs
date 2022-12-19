module Day13.DistressSignalSpec (spec) where

import Day13.DistressSignal
import Test.Hspec

exampleInput :: String
exampleInput = "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"

spec :: Spec
spec = do
  describe "Determine which pairs of packets are already in the right order. What is the sum of the indices of those pairs?" $ do
    it "In the above example, the pairs in the right order are 1, 2, 4, and 6; the sum of these indices is 13." $ do
      indicesSum exampleInput `shouldBe` 13
