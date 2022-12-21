module Day14.SimulationSpec (spec) where

import Common.Grid (fromTuple)
import Day14.Reservoir (empty, insertScans)
import Day14.Simulation
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"

spec :: Spec
spec = do
  describe "When start simulation" $ do
    let simulation = from . (`insertScans` empty) . read $ exampleInput

    it "Should have no sand" $ do
      (length . sandUnits $ simulation) `shouldBe` 0

    it "Should determine drop point at column 500" $ do
      dropPoint simulation `shouldBe` fromTuple (500, 8)

    describe "When simulating sand unit's drop" $ do
      let one = oneDrop simulation
      let sandUnitsOne = sandUnits one

      it "Should have one more sand unit" $ do
        length sandUnitsOne `shouldBe` 1

      it "Should drop oneUnit" $ do
        sandUnitsOne `shouldBe` [fromTuple (500, 8)]
