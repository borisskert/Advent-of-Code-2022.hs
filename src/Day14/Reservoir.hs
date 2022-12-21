module Day14.Reservoir (Reservoir, toList, insertScans, empty, rock, sandSource, sandUnits) where

import Common.Grid (Grid)
import qualified Common.Grid as Grid (empty, fromTuple, insert, lookupPositions, toList)
import Common.OctaGridPosition (Position)
import Day14.Material
import Day14.RockScan (RockScan, deflate)
import Day14.RockScans (RockScans)
import qualified Day14.RockScans as RockScans (toList)

-- | -------------------------------------------------------------------------------------------------------------------
-- | Reservoir data structure
-- | -------------------------------------------------------------------------------------------------------------------
newtype Reservoir = Reservoir (Grid Position Material) deriving (Eq)

empty :: Reservoir
empty = Reservoir . Grid.insert (Grid.fromTuple (500, 0)) sandSource $ Grid.empty

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

sandUnits :: Reservoir -> [Position]
sandUnits = Grid.lookupPositions sand . toGrid

-- | -------------------------------------------------------------------------------------------------------------------
-- | Instance Show
-- | -------------------------------------------------------------------------------------------------------------------
instance Show Reservoir where
  show = show . toGrid
