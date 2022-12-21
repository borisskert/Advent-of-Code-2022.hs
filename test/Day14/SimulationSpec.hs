module Day14.SimulationSpec (spec) where

import Common.Grid (fromTuple)
import Day14.Reservoir (empty, insertScans)
import Day14.Simulation
import Test.Hspec

exampleInput :: String
exampleInput = "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504\n"

spec :: Spec
spec = do
  describe "When start simulation" $ do
    let simulation = from . (`insertScans` empty) . read $ exampleInput

    it "Should have no sand" $ do
      (length . sandUnits $ simulation) `shouldBe` 0

    describe "When simulating sand unit's drop" $ do
      let one = oneDrop simulation
      let sandUnitsOne = sandUnits one

      it "Should have one more sand unit" $ do
        length sandUnitsOne `shouldBe` 1

      it "Should drop oneUnit" $ do
        sandUnitsOne `shouldBe` [fromTuple (500, 8)]
