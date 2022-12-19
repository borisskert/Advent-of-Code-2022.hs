module Day12.HeightmapSpec (spec) where

import qualified Common.CrossGridPosition as Position
import qualified Day12.Height as Height
import Day12.Heightmap
import Test.Hspec

exampleInput :: String
exampleInput = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

spec :: Spec
spec = do
  describe "When read from String" $ do
    let heightmap = read $ exampleInput

    it "Should provide start" $ do
      (start heightmap) `shouldBe` Position.from 0 0

    it "Should find adjacent of 0/0" $ do
      adjacent (Position.from 0 0) heightmap
        `shouldBe` [ (Position.from 1 0, Height.from 0),
                     (Position.from 0 1, Height.from 0)
                   ]

    it "Should find adjacent of 0/1" $ do
      adjacent (Position.from 0 1) heightmap
        `shouldBe` [ (Position.from 0 0, Height.start),
                     (Position.from 1 1, Height.from 1),
                     (Position.from 0 2, Height.from 0)
                   ]

    it "Should find adjacent of 0/2" $ do
      adjacent (Position.from 0 2) heightmap
        `shouldBe` [ (Position.from 0 1, Height.from 0),
                     (Position.from 1 2, Height.from 2),
                     (Position.from 0 3, Height.from 0)
                   ]

    it "Should find adjacent of 0/3" $ do
      adjacent (Position.from 0 3) heightmap
        `shouldBe` [ (Position.from 0 2, Height.from 0),
                     (Position.from 1 3, Height.from 2),
                     (Position.from 0 4, Height.from 0)
                   ]

    it "Should find adjacent of 0/4" $ do
      adjacent (Position.from 0 4) heightmap
        `shouldBe` [ (Position.from 0 3, Height.from 0),
                     (Position.from 1 4, Height.from 1)
                   ]
