module Day14.Reservoir
  ( Reservoir,
    toList,
    insertScans,
    empty,
    rock,
    sandSource,
    sandUnits,
    findSandSource,
    toGrid,
    insertSandAt,
    withGround,
    isFull,
  )
where

import Common.Grid (Grid)
import qualified Common.Grid as Grid
  ( empty,
    fromTuple,
    height,
    insert,
    lookup,
    lookupPositions,
    toList,
    x,
  )
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

findSandSource :: Reservoir -> Position
findSandSource = head . Grid.lookupPositions sandSource . toGrid

insertSandAt :: Position -> Reservoir -> Reservoir
insertSandAt position = Reservoir . Grid.insert position sand . toGrid

insertRockAt :: Position -> Reservoir -> Reservoir
insertRockAt position = Reservoir . Grid.insert position rock . toGrid

withGround :: Reservoir -> Reservoir
withGround reservoir = foldl (flip insertRockAt) reservoir ground
  where
    grid = toGrid reservoir
    height = Grid.height grid + 1
    source = findSandSource reservoir
    startX = subtract (height + 0) . Grid.x $ source
    endX = (+ (height + 0)) . Grid.x $ source
    ground = map Grid.fromTuple $ [(x, height) | x <- [startX .. endX]] :: [Position]

isFull :: Reservoir -> Bool
isFull reservoir = (Just sand ==) . Grid.lookup source . toGrid $ reservoir
  where
    source = findSandSource reservoir

-- | -------------------------------------------------------------------------------------------------------------------
-- | Instance Show
-- | -------------------------------------------------------------------------------------------------------------------
instance Show Reservoir where
  show = show . toGrid
