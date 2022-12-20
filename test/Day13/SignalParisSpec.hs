module Day13.SignalParisSpec (spec) where

import Day13.Signal (group, value)
import Day13.SignalPair (pair)
import Day13.SignalPairs
import Test.Hspec

exampleInput :: String
exampleInput = "[1,1,3,1,1]\n[1,1,5,1,1]\n\n[[1],[2,3,4]]\n[[1],4]\n\n[9]\n[[8,7,6]]\n\n[[4,4],4,4]\n[[4,4],4,4,4]\n\n[7,7,7,7]\n[7,7,7]\n\n[]\n[3]\n\n[[[]]]\n[[]]\n\n[1,[2,[3,[4,[5,6,7]]]],8,9]\n[1,[2,[3,[4,[5,6,0]]]],8,9]"

spec :: Spec
spec = do
  describe "When reading input to SignalPairs" $ do
    it "Should read example input" $ do
      read exampleInput
        `shouldBe` fromList
          [ pair (group [value 1, value 1, value 3, value 1, value 1]) (group [value 1, value 1, value 5, value 1, value 1]),
            pair (group [group [value 1], group [value 2, value 3, value 4]]) (group [group [value 1], value 4]),
            pair (group [value 9]) (group [group [value 8, value 7, value 6]]),
            pair (group [group [value 4, value 4], value 4, value 4]) (group [group [value 4, value 4], value 4, value 4, value 4]),
            pair (group [value 7, value 7, value 7, value 7]) (group [value 7, value 7, value 7]),
            pair (group []) (group [value 3]),
            pair (group [group [group []]]) (group [group []]),
            pair
              ( group [value 1, group [value 2, group [value 3, group [value 4, group [value 5, value 6, value 7]]]], value 8, value 9]
              )
              (group [value 1, group [value 2, group [value 3, group [value 4, group [value 5, value 6, value 0]]]], value 8, value 9])
          ]
