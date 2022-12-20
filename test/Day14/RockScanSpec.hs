module Day14.RockScanSpec (spec) where

import Day14.RockScan
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6"

spec :: Spec
spec = do
  describe "When parsing example input" $ do
    it "Should read first line" $ do
      read exampleInput `shouldBe` fromList [(498, 4), (498, 6), (496, 6)]
