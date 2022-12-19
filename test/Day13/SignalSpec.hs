module Day13.SignalSpec (spec) where

import Day13.Signal
import Test.Hspec

spec :: Spec
spec = do
  describe "When parsing first signal from example" $ do
    it "Should read empty group" $ do
      read "[]" `shouldBe` group []

    it "Should read group containing an empty group" $ do
      read "[[]]" `shouldBe` group [group []]

    it "Should read group containing a group containing an empty group" $ do
      read "[[[]]]" `shouldBe` group [group [group []]]

    it "Should read group with single-digit value" $ do
      read "[3]" `shouldBe` group [value 3]

    it "Should read group with two-digit value" $ do
      read "[12]" `shouldBe` group [value 12]

    it "Should read group with two single-digit values" $ do
      read "[3,4]" `shouldBe` group [value 3, value 4]

    it "Should read group with two single-digit values and an aditional empty group" $ do
      read "[3,4,[]]" `shouldBe` group [value 3, value 4, group []]

    it "Should read group with two single-digit values and an aditional group containing a single-digit value" $ do
      read "[3,4,[9]]" `shouldBe` group [value 3, value 4, group [value 9]]

    it "Should read group with two single-digit values and an aditional group containing three values" $ do
      read "[3,4,[9,12,23]]" `shouldBe` group [value 3, value 4, group [value 9, value 12, value 23]]

    it "Should read group with two single-digit values and an aditional group containing three values" $ do
      read "[3,4,[9,12,23]]" `shouldBe` group [value 3, value 4, group [value 9, value 12, value 23]]

    it "Should read group with two single-digit values and an aditional group containing three values" $ do
      read "[3,4,[9,12,[[876]],23]]" `shouldBe` group [value 3, value 4, group [value 9, value 12, group [group [value 876]], value 23]]
