module Day14.ReservoirSpec (spec) where

import Common.Grid (fromTuple)
import Day14.Material
import Day14.Reservoir (empty, insertScans, toList)
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"

spec :: Spec
spec = do
  describe "When inserting RockScans" $ do
    it "Should insert example" $ do
      (toList . insertScans (read exampleInput) $ empty)
        `shouldBe` [ (fromTuple (494, 9), rock),
                     (fromTuple (495, 9), rock),
                     (fromTuple (496, 6), rock),
                     (fromTuple (496, 9), rock),
                     (fromTuple (497, 6), rock),
                     (fromTuple (497, 9), rock),
                     (fromTuple (498, 4), rock),
                     (fromTuple (498, 5), rock),
                     (fromTuple (498, 6), rock),
                     (fromTuple (498, 9), rock),
                     (fromTuple (499, 9), rock),
                     (fromTuple (500, 0), sandSource),
                     (fromTuple (500, 9), rock),
                     (fromTuple (501, 9), rock),
                     (fromTuple (502, 4), rock),
                     (fromTuple (502, 5), rock),
                     (fromTuple (502, 6), rock),
                     (fromTuple (502, 7), rock),
                     (fromTuple (502, 8), rock),
                     (fromTuple (502, 9), rock),
                     (fromTuple (503, 4), rock)
                   ]
