module Day12.PathFinder (PathFinder, from, findPathToEnd, findPathFromEnd) where

import Common.CrossGridPosition (Position)
import Common.Grid (Path)
import Day12.Height (Height)
import qualified Day12.Height as Height (arePassable, end, fromChar, start)
import Day12.Heightmap (Heightmap)
import qualified Day12.Heightmap as Heightmap (end, findPath, start)

newtype PathFinder = PathFinder Heightmap deriving (Eq, Show)

from :: Heightmap -> PathFinder
from = PathFinder

findPathToEnd :: PathFinder -> Maybe (Path Position Height)
findPathToEnd (PathFinder myMap) = Heightmap.findPath ourStart Height.end isPassable myMap
  where
    ourStart = (Heightmap.start myMap, Height.start)

    isPassable :: (Position, Height) -> (Position, Height) -> Bool
    isPassable (_, heightA) (_, heightB) = Height.arePassable heightA heightB

findPathFromEnd :: PathFinder -> Maybe (Path Position Height)
findPathFromEnd (PathFinder myMap) = Heightmap.findPath ourStart (Height.fromChar 'a') isPassable myMap
  where
    ourStart = (Heightmap.end myMap, Height.end)

    isPassable :: (Position, Height) -> (Position, Height) -> Bool
    isPassable (_, heightA) (_, heightB) = Height.arePassable heightB heightA
