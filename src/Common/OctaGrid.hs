module Common.OctaGrid
  ( OctaGrid,
    Position,
    empty,
    fromList,
    lookup,
    insert,
    insertWith,
    adjust,
    update,
    alter,
    each,
    toList,
    mapGrid,
    keys,
    elems,
    adjacent,
    areAdjacent,
  )
where

import Common.List
import Data.Map (Map)
import qualified Data.Map as Map (adjust, alter, elems, empty, fromList, insert, insertWith, keys, lookup, toList, update)
import Prelude hiding (lookup)

-- (width, height)
type Size = (Int, Int)

type Position = (Int, Int)

data OctaGrid a = OctaGrid Size (Map Position a) deriving (Eq, Show)

empty :: OctaGrid a
empty = OctaGrid (0, 0) Map.empty

lookup :: Position -> OctaGrid a -> Maybe a
lookup position (OctaGrid _ gridMap) = Map.lookup position gridMap

insert :: Position -> a -> OctaGrid a -> OctaGrid a
insert pos v (OctaGrid size gridMap) = OctaGrid size . Map.insert pos v $ gridMap

insertWith :: (a -> a -> a) -> Position -> a -> OctaGrid a -> OctaGrid a
insertWith fn pos value (OctaGrid size gridMap) = OctaGrid size . Map.insertWith fn pos value $ gridMap

adjust :: (a -> a) -> Position -> OctaGrid a -> OctaGrid a
adjust fn pos (OctaGrid size gridMap) = OctaGrid size . Map.adjust fn pos $ gridMap

update :: (a -> Maybe a) -> Position -> OctaGrid a -> OctaGrid a
update fn pos (OctaGrid size gridMap) = OctaGrid size . Map.update fn pos $ gridMap

alter :: (Maybe a -> Maybe a) -> Position -> OctaGrid a -> OctaGrid a
alter fn pos (OctaGrid size gridMap) = OctaGrid size . Map.alter fn pos $ gridMap

each :: ((Position, a) -> (Position, a)) -> OctaGrid a -> OctaGrid a
each mapper (OctaGrid size gridMap) = OctaGrid size newGridMap
  where
    newGridMap = Map.fromList . map mapper . Map.toList $ gridMap

fromList :: [[a]] -> OctaGrid a
fromList list = OctaGrid size gridMap
  where
    size = (maybe 0 length . safeHead $ list, length list)
    gridMap =
      Map.fromList
        . concatMap (uncurry toLine)
        . zip [0 ..]
        $ list

    toLine :: Int -> [a] -> [(Position, a)]
    toLine y line = zipWith (\x c -> ((x, y), c)) [0 ..] line

toList :: OctaGrid a -> [(Position, a)]
toList (OctaGrid _ gridMap) = Map.toList gridMap

mapGrid :: (Position -> a -> b) -> OctaGrid a -> OctaGrid b
mapGrid fn (OctaGrid size gridMap) = OctaGrid size newGridMap
  where
    newGridMap = Map.fromList . map (\(pos, x) -> (pos, fn pos x)) . Map.toList $ gridMap

keys :: OctaGrid a -> [Position]
keys (OctaGrid _ gridMap) = Map.keys gridMap

elems :: OctaGrid a -> [a]
elems (OctaGrid _ gridMap) = Map.elems gridMap

adjacent :: Position -> OctaGrid a -> [Position]
adjacent pos (OctaGrid (width, height) _) =
  filter (\(x, y) -> x >= 0 && x < width && y >= 0 && y < height)
    . adjacentPositions
    $ pos

adjacentPositions :: Position -> [Position]
adjacentPositions (x, y) =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x -1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

areAdjacent :: Position -> Position -> Bool
areAdjacent pos other = other `elem` adjacentPositions'
  where
    adjacentPositions' = adjacentPositions pos
