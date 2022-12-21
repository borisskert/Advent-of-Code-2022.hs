module Day14.Reservoir (Reservoir, toList, insertScans, empty, rock, sandSource) where

import Common.Grid (Grid, Value)
import qualified Common.Grid as Grid (empty, fromTuple, fromValue, insert, toList, toValue)
import Common.OctaGridPosition (Position)
import Day14.RockScan (RockScan, deflate)
import Day14.RockScans (RockScans)
import qualified Day14.RockScans as RockScans (toList)

data Material = Sand | Rock | SandSource deriving (Eq, Show, Ord)

instance Value Material where
  toValue = undefined
  fromValue (Just Sand) = 'o'
  fromValue (Just Rock) = '#'
  fromValue (Just SandSource) = '+'
  fromValue Nothing = '.'

rock :: Material
rock = Rock

sandSource :: Material
sandSource = SandSource

newtype Reservoir = Reservoir (Grid Position Material) deriving (Show, Eq)

empty :: Reservoir
empty = Reservoir . Grid.insert (Grid.fromTuple (500, 0)) SandSource $ Grid.empty

toGrid :: Reservoir -> Grid Position Material
toGrid (Reservoir myGrid) = myGrid

insertScans :: RockScans -> Reservoir -> Reservoir
insertScans rs reservoir = foldl (flip insertScan) reservoir . RockScans.toList $ rs

insertScan :: RockScan -> Reservoir -> Reservoir
insertScan rs reservoir = foldl (flip insertScanAt) reservoir . map Grid.fromTuple $ deflate rs

insertScanAt :: Position -> Reservoir -> Reservoir
insertScanAt position = Reservoir . Grid.insert position rock . toGrid

toList :: Reservoir -> [(Position, Material)]
toList = Grid.toList . toGrid
