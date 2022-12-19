module Day12.PathFinder (PathFinder, from, findPathToEnd) where

import Common.CrossGridPosition (Position)
import Common.ShortestPathFast (findRoute)
import Data.Maybe (fromJust)
import qualified Day12.Height as Height (arePassable)
import Day12.Heightmap (Heightmap)
import qualified Day12.Heightmap as Heightmap (adjacent, end, lookup, start)
import Day12.Path (Path)
import qualified Day12.Path as Path (append, singleton)

newtype PathFinder = PathFinder Heightmap deriving (Eq, Show)

from :: Heightmap -> PathFinder
from = PathFinder

findPathToEnd :: PathFinder -> Maybe Path
findPathToEnd (PathFinder myMap) = fmap fst . findRoute next target $ starting
  where
    target = Heightmap.end myMap
    starting = (Path.singleton . Heightmap.start $ myMap, Heightmap.start myMap)

    next :: ((Path, Position) -> [(Path, Position)])
    next (path, pos) = map ((\p -> (Path.append p path, p)) . fst) nextPositions
      where
        myHeight = fromJust . Heightmap.lookup pos $ myMap
        adjacentPositions = Heightmap.adjacent pos myMap
        nextPositions = filter (Height.arePassable myHeight . snd) adjacentPositions
