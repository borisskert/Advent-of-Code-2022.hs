module Day14.ReservoirSpec (spec) where

import Common.Grid (fromTuple)
import Day14.Material
import Day14.Reservoir (empty, heightAt, insertScans, sandUnits, toList)
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"

spec :: Spec
spec = do
  describe "When inserting RockScans" $ do
    let reservoir = insertScans (read exampleInput) empty

    it "Should insert example" $ do
      toList reservoir
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

    it "Should have no sand units" $ do
      sandUnits reservoir `shouldBe` []

    it "Should have height 9 at column 500" $ do
      heightAt 500 reservoir `shouldBe` 9

    it "Should have height 9 at column 499" $ do
      heightAt 499 reservoir `shouldBe` 9

    it "Should have height 9 at column 501" $ do
      heightAt 501 reservoir `shouldBe` 9

    it "Should have height 4 at column 498" $ do
      heightAt 498 reservoir `shouldBe` 4

    it "Should have height 4 at column 502" $ do
      heightAt 502 reservoir `shouldBe` 4
