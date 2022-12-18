module Day12.PathFinder (PathFinder, from, find) where

import Common.CrossGridPosition (Position)
import Common.List
import Data.Maybe (fromJust)
import qualified Day12.Height as Height (arePassable, end)
import Day12.Heightmap (Heightmap)
import qualified Day12.Heightmap as Heightmap (adjacent, lookup, start)
import Day12.Path
import Debug.Trace (traceShow)

newtype PathFinder = PathFinder Heightmap deriving (Eq, Show)

from :: Heightmap -> PathFinder
from = PathFinder

find :: PathFinder -> Path
find pathFinder@(PathFinder myMap) = findFrom (Heightmap.start myMap) pathFinder empty

findFrom :: Position -> PathFinder -> Path -> Path
findFrom myPos pathFinder@(PathFinder myMap) myPath
  | traceShow (myPos, nextPositions, nextPaths) $ null nextPositions = empty
  | not . null $ foundEnd = fromList [myPos, fst . head $ foundEnd]
  | null nextPaths = empty
  | otherwise = prepend myPos . minimumOn steps $ nextPaths
  where
    myHeight = fromJust . Heightmap.lookup myPos $ myMap
    adjacentPositions = Heightmap.adjacent myPos myMap
    nextPositions = filter (not . (`contains` myPath) . fst) . filter (Height.arePassable myHeight . snd) $ adjacentPositions
    nextPaths = filter (not . (myPos `contains`)) . filter (not . isEmpty) . map (createPath . fst) $ nextPositions
    foundEnd = filter ((== Height.end) . snd) nextPositions

    createPath :: Position -> Path
    createPath pos = findFrom pos pathFinder (prepend pos myPath)
