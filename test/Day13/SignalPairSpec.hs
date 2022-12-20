module Day13.SignalPairSpec (spec) where

import Day13.Signal (group, value)
import Day13.SignalPair
import Test.Hspec

spec :: Spec
spec = do
  describe "When reading input to SignalPair" $ do
    it "Should read two empty groups" $ do
      read "[]\n[]" `shouldBe` pair (group []) (group [])

    it "Should read one group containing single-digit and an empty group" $ do
      read "[4]\n[]" `shouldBe` pair (group [value 4]) (group [])

    it "Should read first example" $ do
      read "[1,1,3,1,1]\n[1,1,5,1,1]"
        `shouldBe` pair
          (group [value 1, value 1, value 3, value 1, value 1])
          (group [value 1, value 1, value 5, value 1, value 1])

    it "Should read second example" $ do
      read "[[1],[2,3,4]]\n[[1],4]"
        `shouldBe` pair
          (group [group [value 1], group [value 2, value 3, value 4]])
          (group [group [value 1], value 4])
