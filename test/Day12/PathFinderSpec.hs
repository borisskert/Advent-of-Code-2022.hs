module Day12.PathFinderSpec (spec) where

import qualified Common.CrossGridPosition as Position (from)
import Day12.Path
import Day12.PathFinder
import Test.Hspec

exampleInput :: String
exampleInput = "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"

pathFinder :: PathFinder
pathFinder = from . read $ exampleInput

spec :: Spec
spec = do
  describe "When find paths" $ do
    it "Should find path from start to end" $ do
      find pathFinder
        `shouldBe` fromList
          [ Position.from 0 0,
            Position.from 1 0,
            Position.from 2 0,
            Position.from 2 1,
            Position.from 2 2,
            Position.from 2 3,
            Position.from 2 4,
            Position.from 3 4,
            Position.from 4 4,
            Position.from 5 4,
            Position.from 6 4,
            Position.from 7 4,
            Position.from 7 3,
            Position.from 7 2,
            Position.from 7 1,
            Position.from 7 0,
            Position.from 6 0,
            Position.from 5 0,
            Position.from 4 0,
            Position.from 3 0,
            Position.from 3 1,
            Position.from 3 2,
            Position.from 3 3,
            Position.from 4 3,
            Position.from 5 3,
            Position.from 6 3,
            Position.from 6 2,
            Position.from 6 1,
            Position.from 5 1,
            Position.from 4 1,
            Position.from 4 2,
            Position.from 5 2
          ]
