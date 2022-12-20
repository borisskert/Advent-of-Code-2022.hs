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

  describe "When comparing Signals" $ do
    it "Should compare first example" $ do
      compare ((read :: String -> Signal) "[1,1,3,1,1]") (read "[1,1,5,1,1]") `shouldBe` LT
      compare ((read :: String -> Signal) "[1,1,5,1,1]") (read "[1,1,3,1,1]") `shouldBe` GT

    it "Should compare second example" $ do
      compare ((read :: String -> Signal) "[[1],[2,3,4]]") (read "[[1],4]") `shouldBe` LT
      compare ((read :: String -> Signal) "[[1],4]") (read "[[1],[2,3,4]]") `shouldBe` GT

    it "Should compare third example" $ do
      compare ((read :: String -> Signal) "[9]") (read "[[8,7,6]]") `shouldBe` GT
      compare ((read :: String -> Signal) "[[8,7,6]]") (read "[9]") `shouldBe` LT

    it "Should compare fourth example" $ do
      compare ((read :: String -> Signal) "[[4,4],4,4]") (read "[[4,4],4,4,4]") `shouldBe` LT
      compare ((read :: String -> Signal) "[[4,4],4,4,4]") (read "[[4,4],4,4]") `shouldBe` GT
