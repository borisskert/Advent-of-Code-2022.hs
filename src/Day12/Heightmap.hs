module Day12.Heightmap (Heightmap, empty, start, end) where

import Common.CrossGridPosition (Position)
import Common.Grid (Grid)
import qualified Common.Grid as Grid (empty, lookupPositions)
import Day12.Height (Height)
import qualified Day12.Height as Height (end, start)

newtype Heightmap = Heightmap (Grid Position Height)

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
