module Day08.TreetopTreeHouseSpec (spec) where

import Day08.TreetopTreeHouse
import Test.Hspec

exampleInput :: String
exampleInput = "30373\n25512\n65332\n33549\n35390"

spec :: Spec
spec = do
  describe "Consider your map; how many trees are visible from outside the grid?" $ do
    it "With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this arrangement." $ do
      howManyTrees exampleInput `shouldBe` 21
