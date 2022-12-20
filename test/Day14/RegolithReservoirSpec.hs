module Day14.RegolithReservoirSpec (spec) where

import Day14.RegolithReservoir
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"

spec :: Spec
spec = do
  describe "How many units of sand come to rest before sand starts flowing into the abyss below?" $ do
    it "Once all 24 units of sand shown above have come to rest, all further sand flows out the bottom, falling into the endless void." $ do
      sandUnits exampleInput `shouldBe` 24
