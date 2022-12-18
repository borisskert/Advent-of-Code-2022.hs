module Day12.HeightmapSpec (spec) where

import Day12.Heightmap
import qualified Common.CrossGridPosition as Position
import Test.Hspec

exampleInput :: String
exampleInput = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

spec :: Spec
spec = do
  describe "When read from String" $ do
    it "Should provide start" $ do
      (start . read $ exampleInput) `shouldBe` Position.from 0 0
