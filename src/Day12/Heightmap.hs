module Day12.Heightmap (Heightmap, empty, start, end, findPath) where

import Common.CrossGridPosition (Position)
import Common.Grid (Grid, Path)
import qualified Common.Grid as Grid (empty, findPath, lookupPositions)
import Day12.Height (Height)
import qualified Day12.Height as Height (end, start)
import Prelude hiding (lookup)

newtype Heightmap = Heightmap (Grid Position Height) deriving (Eq, Show)

instance Read Heightmap where
  readsPrec _ input = [(parseFrom input, [])]

parseFrom :: String -> Heightmap
parseFrom = Heightmap . read

empty :: Heightmap
empty = Heightmap Grid.empty

start :: Heightmap -> Position
start (Heightmap grid) = head . Grid.lookupPositions Height.start $ grid

end :: Heightmap -> Position
end (Heightmap grid) = head . Grid.lookupPositions Height.end $ grid

findPath :: (Position, Height) -> Height -> ((Position, Height) -> (Position, Height) -> Bool) -> Heightmap -> Maybe (Path Position Height)
findPath myStart targetHeight isPassable (Heightmap myMap) = Grid.findPath myStart isTarget isPassable myMap
  where
    isTarget :: (Position, Height) -> Bool
    isTarget (_, myHeight) = myHeight == targetHeight
