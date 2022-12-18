{-# LANGUAGE TupleSections #-}

module Day12.Heightmap (Heightmap, empty, start, end, lookup, adjacent) where

import Common.CrossGridPosition (Position)
import qualified Common.CrossGridPosition as Position (adjacent)
import Common.Grid (Grid)
import qualified Common.Grid as Grid (empty, lookup, lookupPositions)
import Data.Maybe (mapMaybe)
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

lookup :: Position -> Heightmap -> Maybe Height
lookup pos (Heightmap grid) = Grid.lookup pos grid

adjacent :: Position -> Heightmap -> [(Position, Height)]
adjacent myPos myMap = mapMaybe lookupPair . Position.adjacent $ myPos
  where
    lookupPair :: Position -> Maybe (Position, Height)
    lookupPair pos = fmap (pos,) found
      where
        found = lookup pos myMap
