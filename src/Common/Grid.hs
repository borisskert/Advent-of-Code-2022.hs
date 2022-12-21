{-# LANGUAGE TupleSections #-}

module Common.Grid
  ( Grid,
    Position,
    x,
    y,
    fromTuple,
    toTuple,
    adjacent,
    Value,
    fromValue,
    toValue,
    empty,
    fromList,
    toList,
    lookup,
    lookupPositions,
    find,
    member,
    each,
    mapGrid,
    columns,
    rows,
    columnAt,
    rowAt,
    keys,
    elems,
    allNorthOf,
    allSouthOf,
    allWestOf,
    allEastOf,
    northOf,
    southOf,
    westOf,
    eastOf,
    northWestOf,
    northEastOf,
    southWestOf,
    southEastOf,
    insert,
    width,
    height,
    subgrid,
    adjacentOf,
    findPath,
    Path,
    minX,
    maxX,
    minY,
    maxY,
  )
where

import Common.BidirectionalMap (BidirectionalMap)
import qualified Common.BidirectionalMap as BidirectionalMap
  ( elems,
    empty,
    find,
    fromList,
    insert,
    keys,
    lookup,
    lookupKey,
    member,
    toList,
  )
import qualified Common.List as List
import Common.Path (Path)
import qualified Common.Path as Path (append, singleton)
import Common.ShortestPathFast (findRoute)
import Data.Bifunctor (second)
import Data.List (intercalate)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Prelude hiding (lookup)

type Range = (Int, Int)

-- (width, height)
type Size = (Range, Range)

data Grid p a = Grid Size (BidirectionalMap p a) deriving (Eq)

empty :: Grid p a
empty = Grid ((maxBound, minBound), (maxBound, minBound)) BidirectionalMap.empty

class Position a where
  x :: a -> Int
  y :: a -> Int
  fromTuple :: (Int, Int) -> a
  adjacent :: a -> [a]

toTuple :: (Position a) => a -> (Int, Int)
toTuple position = (x position, y position)

class Value a where
  toValue :: (Position p) => (p, Char) -> Maybe a
  fromValue :: Maybe a -> Char

instance (Position p, Ord p, Value a, Ord a) => Read (Grid p a) where
  readsPrec _ "" = [(empty, [])]
  readsPrec _ input = [(fromLines input, [])]

fromLines :: (Position p, Ord p, Value a, Ord a) => String -> Grid p a
fromLines "" = empty
fromLines input = Grid size gridMap
  where
    inputLines = lines input
    gridMap = fromLinesIntoMap inputLines
    maxX' = maybe 0 (subtract 1 . length) . List.safeHead $ inputLines
    maxY' = length inputLines - 1
    size = ((0, maxX'), (0, maxY'))

fromLinesIntoMap :: (Position p, Ord p, Value a, Ord a) => [[Char]] -> BidirectionalMap p a
fromLinesIntoMap =
  BidirectionalMap.fromList
    . map (second fromJust)
    . filter (isJust . snd)
    . map (\(p, c) -> (p, toValue (p, c)))
    . concatMap (uncurry toLine)
    . zip [0 ..]
  where
    toLine y' line = zipWith (\x' c -> (fromTuple (x', y'), c)) [0 ..] line

lookup :: (Ord p) => p -> Grid p a -> Maybe a
lookup p (Grid _ gridMap) = BidirectionalMap.lookup p gridMap

find :: (Ord p) => p -> Grid p a -> a
find p (Grid _ gridMap) = BidirectionalMap.find p gridMap

lookupPositions :: (Ord a) => a -> Grid p a -> [p]
lookupPositions value (Grid _ gridMap) = BidirectionalMap.lookupKey value gridMap

lookupPair :: (Ord p) => p -> Grid p a -> Maybe (p, a)
lookupPair p (Grid _ gridMap) = (p,) <$> BidirectionalMap.lookup p gridMap

member :: (Ord p) => p -> Grid p a -> Bool
member p (Grid _ gridMap) = BidirectionalMap.member p gridMap

each :: (Ord p, Ord a) => ((p, a) -> (p, a)) -> Grid p a -> Grid p a
each mapper (Grid size gridMap) = Grid size newGridMap
  where
    newGridMap = BidirectionalMap.fromList . map mapper . BidirectionalMap.toList $ gridMap

instance (Position p, Ord p, Value a) => Show (Grid p a) where
  show grid = toLines grid

toLines :: (Position p, Ord p, Value a) => Grid p a -> String
toLines (Grid size gridMap) =
  intercalate "\n"
    . map (\y' -> map (fromValue . (`BidirectionalMap.lookup` gridMap) . fromTuple . (,y')) xs)
    $ ys
  where
    ((minX', maxX'), (minY', maxY')) = size
    xs = [minX', (minX' + 1) .. maxX']
    ys = [minY', (minY' + 1) .. maxY']

fromList :: (Position p, Ord p, Ord a) => [(p, a)] -> Grid p a
fromList = foldl (flip insertPair) empty

toList :: Grid p a -> [(p, a)]
toList (Grid _ gridMap) = BidirectionalMap.toList gridMap

mapGrid :: (Ord p, Ord b) => (p -> a -> b) -> Grid p a -> Grid p b
mapGrid fn (Grid size gridMap) = Grid size newGridMap
  where
    newGridMap = BidirectionalMap.fromList . map (\(pos, x') -> (pos, fn pos x')) . BidirectionalMap.toList $ gridMap

rows :: (Position p) => Grid p a -> [[p]]
rows grid@(Grid (_, _) _) = map (\y' -> map (fromTuple . (,y')) [0 .. gridWidth - 1]) [0 .. gridHeight - 1]
  where
    gridHeight = height grid
    gridWidth = width grid

columns :: (Position p) => Grid p a -> [[p]]
columns grid@(Grid (_, _) _) = map (\x' -> map (fromTuple . (x',)) [0 .. gridHeight - 1]) [0 .. gridWidth - 1]
  where
    gridHeight = height grid
    gridWidth = width grid

rowAt :: (Position p, Ord p) => Int -> Grid p a -> [(p, a)]
rowAt y' grid = mapMaybe ((`lookupPair` grid) . fromTuple . (,y')) [0 .. gridWidth - 1]
  where
    gridWidth = width grid

columnAt :: (Position p, Ord p) => Int -> Grid p a -> [(p, a)]
columnAt x' grid = mapMaybe ((`lookupPair` grid) . fromTuple . (x',)) [0 .. gridHeight - 1]
  where
    gridHeight = height grid

keys :: Grid p a -> [p]
keys (Grid _ gridMap) = BidirectionalMap.keys gridMap

elems :: Grid p a -> [a]
elems (Grid _ gridMap) = BidirectionalMap.elems gridMap

allNorthOf :: (Position p) => p -> Grid p a -> [p]
allNorthOf pos _ = map (fromTuple . (x pos,)) [(y pos - 1), (y pos - 2) .. 0]

allSouthOf :: (Position p) => p -> Grid p a -> [p]
allSouthOf pos (Grid (_, (_, maxY')) _) = map (fromTuple . (x pos,)) [y pos + 1 .. maxY']

allWestOf :: (Position p) => p -> Grid p a -> [p]
allWestOf pos _ = map (fromTuple . (,y pos)) [(x pos - 1), (x pos - 2) .. 0]

allEastOf :: (Position p) => p -> Grid p a -> [p]
allEastOf pos (Grid ((_, maxX'), _) _) = map (fromTuple . (,y pos)) [x pos + 1 .. maxX']

northOf :: (Ord p, Position p) => p -> Grid p a -> (p, Maybe a)
northOf pos grid = (northPos, value)
  where
    northPos = fromTuple (x pos, y pos - 1)
    value = (`lookup` grid) northPos

southOf :: (Ord p, Position p) => p -> Grid p a -> (p, Maybe a)
southOf pos grid = (southPos, value)
  where
    southPos = fromTuple (x pos, y pos + 1)
    value = (`lookup` grid) southPos

westOf :: (Ord p, Position p) => p -> Grid p a -> (p, Maybe a)
westOf pos grid = (westPos, value)
  where
    westPos = fromTuple (x pos - 1, y pos)
    value = (`lookup` grid) westPos

eastOf :: (Ord p, Position p) => p -> Grid p a -> (p, Maybe a)
eastOf pos grid = (eastPos, value)
  where
    eastPos = fromTuple (x pos + 1, y pos)
    value = (`lookup` grid) eastPos

northWestOf :: (Ord p, Position p) => p -> Grid p a -> (p, Maybe a)
northWestOf pos grid = (northWestPos, value)
  where
    northWestPos = fromTuple (x pos - 1, y pos - 1)
    value = (`lookup` grid) northWestPos

northEastOf :: (Ord p, Position p) => p -> Grid p a -> (p, Maybe a)
northEastOf pos grid = (northEastPos, value)
  where
    northEastPos = fromTuple (x pos + 1, y pos - 1)
    value = (`lookup` grid) northEastPos

southWestOf :: (Ord p, Position p) => p -> Grid p a -> (p, Maybe a)
southWestOf pos grid = (southWestPos, value)
  where
    southWestPos = fromTuple (x pos - 1, y pos + 1)
    value = (`lookup` grid) southWestPos

southEastOf :: (Ord p, Position p) => p -> Grid p a -> (p, Maybe a)
southEastOf pos grid = (southEastPos, value)
  where
    southEastPos = fromTuple (x pos + 1, y pos + 1)
    value = (`lookup` grid) southEastPos

insert :: (Position p, Ord p, Ord a) => p -> a -> Grid p a -> Grid p a
insert pos value (Grid ((minX', maxX'), (minY', maxY')) gridMap) = Grid (newWidth, newHeight) newGridMap
  where
    (posX, posY) = toTuple pos
    newWidth = (min minX' posX, max maxX' posX)
    newHeight = (min minY' posY, max maxY' posY)
    newGridMap = BidirectionalMap.insert pos value gridMap

insertPair :: (Position p, Ord p, Ord a) => (p, a) -> Grid p a -> Grid p a
insertPair (p, a) = insert p a

width :: Grid p a -> Int
width (Grid ((minX', maxX'), _) _) = maxX' - minX' + 1

height :: Grid p a -> Int
height (Grid (_, (minY', maxY')) _) = maxY' - minY' + 1

subgrid :: (Position p, Ord p, Ord a) => p -> (Int, Int) -> Grid p a -> Grid p a
subgrid pos (width', height') grid = newGrid
  where
    (posX, posY) = toTuple pos
    xs = [posX .. (posX + width' - 1)]
    ys = [posY .. (posY + height' - 1)]
    positions = [fromTuple (x', y') | x' <- xs, y' <- ys]
    lookupValue position = fmap (position,) . lookup position $ grid
    pairs = mapMaybe lookupValue positions
    newGrid = foldl (\g (k, v) -> insert k v g) empty pairs

adjacentOf :: (Position p, Ord p) => p -> Grid p a -> [(p, a)]
adjacentOf myPos myMap = mapMaybe (`lookupPair` myMap) . adjacent $ myPos

-- Searches a path by Shortest Path Faster Algorithm (SPFA)
-- https://en.wikipedia.org/wiki/Shortest_Path_Faster_Algorithm
findPath ::
  (Position p, Ord p, Ord a) =>
  (p, a) -> -- Where you start from
  ((p, a) -> Bool) -> -- Predicate to define the final target
  ((p, a) -> (p, a) -> Bool) -> -- Can you pass from a position to other?
  Grid p a -> -- Your Grid
  Maybe (Path p a) -- Maybe there is a Path, maybe not?
findPath myStart isTarget canPassTo myMap = fmap fst . findRoute next isTarget $ source
  where
    source = (Path.singleton myStart, myStart)

    next (path, (myPos, myHeight)) = map (\other@(otherPos, otherHeight) -> (Path.append other path, (otherPos, otherHeight))) nextPositions
      where
        myAdjacentPositions = adjacentOf myPos myMap
        nextPositions = filter canPass myAdjacentPositions

        canPass (p, a) = canPassTo (myPos, myHeight) (p, a)

minX :: Grid p a -> Int
minX (Grid ((minX', _), _) _) = minX'

maxX :: Grid p a -> Int
maxX (Grid ((_, maxX'), _) _) = maxX'

minY :: Grid p a -> Int
minY (Grid (_, (minY', _)) _) = minY'

maxY :: Grid p a -> Int
maxY (Grid (_, (_, maxY')) _) = maxY'
