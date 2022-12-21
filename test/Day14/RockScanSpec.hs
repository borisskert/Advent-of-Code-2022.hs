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

  describe "When deflating RockScans" $ do
    it "Should deflate first example" $ do
      (deflate . fromList $ [(498, 4), (498, 6), (496, 6)]) `shouldBe` [(498, 4), (498, 5), (498, 6), (497, 6), (496, 6)]

    it "Should deflate second example" $ do
      (deflate . fromList $ [(503, 4), (502, 4), (502, 9), (494, 9)])
        `shouldBe` [ (503, 4),
                     (502, 4),
                     (502, 5),
                     (502, 6),
                     (502, 7),
                     (502, 8),
                     (502, 9),
                     (501, 9),
                     (500, 9),
                     (499, 9),
                     (498, 9),
                     (497, 9),
                     (496, 9),
                     (495, 9),
                     (494, 9)
                   ]
