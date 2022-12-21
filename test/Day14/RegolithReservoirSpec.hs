module Day14.RegolithReservoirSpec (spec) where

import Day14.RegolithReservoir
import Test.Hspec

exampleInput :: String
exampleInput = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"

spec :: Spec
spec = do
  describe "How many units of sand come to rest before sand starts flowing into the abyss below?" $ do
    it "Once all 24 units of sand shown above have come to rest, all further sand flows out the bottom, falling into the endless void." $ do
      sandUnitsUntilFall exampleInput `shouldBe` 24

  describe "Using your scan, simulate the falling sand until the source of the sand becomes blocked. How many units of sand come to rest?" $ do
    it "To find somewhere safe to stand, you'll need to simulate falling sand until a unit of sand comes to rest at 500,0, blocking the source entirely and stopping the flow of sand into the cave. In the example above, the situation finally looks like this after 93 units of sand come to rest" $ do
      sandUnitsUntilRest exampleInput `shouldBe` 93
