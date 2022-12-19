module Day12.PathFinderSpec (spec) where

import qualified Common.CrossGridPosition as Position (from)
import Common.Path (toList)
import Data.Maybe (fromJust)
import qualified Day12.Height as Height (end, fromChar, start)
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
      (toList . fromJust . findPathToEnd $ pathFinder)
        `shouldBe` [ (Position.from 0 0, Height.start),
                     (Position.from 1 0, Height.fromChar 'a'),
                     (Position.from 2 0, Height.fromChar 'b'),
                     (Position.from 2 1, Height.fromChar 'c'),
                     (Position.from 2 2, Height.fromChar 'c'),
                     (Position.from 2 3, Height.fromChar 'c'),
                     (Position.from 2 4, Height.fromChar 'd'),
                     (Position.from 3 4, Height.fromChar 'e'),
                     (Position.from 4 4, Height.fromChar 'f'),
                     (Position.from 5 4, Height.fromChar 'g'),
                     (Position.from 6 4, Height.fromChar 'h'),
                     (Position.from 7 4, Height.fromChar 'i'),
                     (Position.from 7 3, Height.fromChar 'j'),
                     (Position.from 7 2, Height.fromChar 'k'),
                     (Position.from 7 1, Height.fromChar 'l'),
                     (Position.from 7 0, Height.fromChar 'm'),
                     (Position.from 6 0, Height.fromChar 'n'),
                     (Position.from 5 0, Height.fromChar 'o'),
                     (Position.from 4 0, Height.fromChar 'p'),
                     (Position.from 3 0, Height.fromChar 'q'),
                     (Position.from 3 1, Height.fromChar 'r'),
                     (Position.from 3 2, Height.fromChar 's'),
                     (Position.from 3 3, Height.fromChar 't'),
                     (Position.from 4 3, Height.fromChar 'u'),
                     (Position.from 5 3, Height.fromChar 'v'),
                     (Position.from 6 3, Height.fromChar 'w'),
                     (Position.from 6 2, Height.fromChar 'x'),
                     (Position.from 6 1, Height.fromChar 'x'),
                     (Position.from 5 1, Height.fromChar 'x'),
                     (Position.from 4 1, Height.fromChar 'y'),
                     (Position.from 4 2, Height.fromChar 'z'),
                     (Position.from 5 2, Height.end)
                   ]
