module Day12.PathFinder (PathFinder, from, findPathToEnd, findPathFromEnd) where

import Common.CrossGridPosition (Position)
import Common.ShortestPathFast (findRoute)
import Day12.Height (Height)
import qualified Day12.Height as Height (arePassable, end, fromChar, start)
import Day12.Heightmap (Heightmap)
import qualified Day12.Heightmap as Heightmap (adjacent, end, start)
import Day12.Path (Path)
import qualified Day12.Path as Path (append, singleton)

newtype PathFinder = PathFinder Heightmap deriving (Eq, Show)

from :: Heightmap -> PathFinder
from = PathFinder

findPathToEnd :: PathFinder -> Maybe Path
findPathToEnd pathFinder@(PathFinder myMap) = findPath ourStart Height.end isPassable pathFinder
  where
    ourStart = (Heightmap.start myMap, Height.start)

    isPassable :: (Position, Height) -> (Position, Height) -> Bool
    isPassable (_, heightA) (_, heightB) = Height.arePassable heightA heightB

findPathFromEnd :: PathFinder -> Maybe Path
findPathFromEnd pathFinder@(PathFinder myMap) = findPath ourStart (Height.fromChar 'a') isPassable pathFinder
  where
    ourStart = (Heightmap.end myMap, Height.end)

    isPassable :: (Position, Height) -> (Position, Height) -> Bool
    isPassable (_, heightA) (_, heightB) = Height.arePassable heightB heightA

findPath :: (Position, Height) -> Height -> ((Position, Height) -> (Position, Height) -> Bool) -> PathFinder -> Maybe Path
findPath myStart targetHeight isPassable (PathFinder myMap) = fmap fst . findRoute next isTarget $ source
  where
    source = (Path.singleton . Heightmap.start $ myMap, myStart)

    isTarget :: (Position, Height) -> Bool
    isTarget (_, myHeight) = myHeight == targetHeight

    next :: ((Path, (Position, Height)) -> [(Path, (Position, Height))])
    next (path, (myPos, myHeight)) = map (\(otherPos, otherHeight) -> (Path.append otherPos path, (otherPos, otherHeight))) nextPositions
      where
        adjacentPositions = Heightmap.adjacent myPos myMap
        nextPositions = filter (isPassable (myPos, myHeight)) adjacentPositions
