module Day14.RockScansSpec (spec) where

import qualified Day14.RockScan as RockScan
import Day14.RockScans
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"

exampleInputWithAdditionalNewline :: String
exampleInputWithAdditionalNewline = "498,4 -> 498,6 ->\n 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9\n"

spec :: Spec
spec = do
  describe "When parse" $ do
    it "Should read exampleInput" $ do
      read exampleInput
        `shouldBe` fromList
          [ RockScan.fromList [(498, 4), (498, 6), (496, 6)],
            RockScan.fromList [(503, 4), (502, 4), (502, 9), (494, 9)]
          ]
