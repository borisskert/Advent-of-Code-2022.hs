module Day12.HeightmapSpec (spec) where

import qualified Common.CrossGridPosition as Position
import Day12.Heightmap
import Test.Hspec

exampleInput :: String
exampleInput = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

spec :: Spec
spec = do
  describe "When read from String" $ do
    let heightmap = read exampleInput

    it "Should provide start" $ do
      start heightmap `shouldBe` Position.from 0 0
