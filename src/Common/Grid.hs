{-# LANGUAGE TupleSections #-}

module Common.Grid
  ( Grid,
    Position,
    x,
    y,
    fromTuple,
    toTuple,
    Value,
    fromValue,
    toValue,
    empty,
    fromList,
    toList,
    lookup,
    lookupPositions,
    each,
    mapGrid,
    columns,
    rows,
    keys,
    elems,
    allNorthOf,
    allSouthOf,
    allWestOf,
    allEastOf,
    insert,
    width,
    height,
    subgrid,
  )
where

import Common.BidirectionalMap (BidirectionalMap)
import qualified Common.BidirectionalMap as BidirectionalMap (elems, empty, fromList, insert, keys, lookup, lookupKey, toList)
import Common.List
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
    maxX = maybe 0 (subtract 1 . length) . safeHead $ inputLines
    maxY = length inputLines - 1
    size = ((0, maxX), (0, maxY))

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

lookupPositions :: (Ord a) => a -> Grid p a -> [p]
lookupPositions value (Grid _ gridMap) = BidirectionalMap.lookupKey value gridMap

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
    ((minX, maxX), (minY, maxY)) = size
    xs = [minX, (minX + 1) .. maxX]
    ys = [minY, (minY + 1) .. maxY]

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

keys :: Grid p a -> [p]
keys (Grid _ gridMap) = BidirectionalMap.keys gridMap

elems :: Grid p a -> [a]
elems (Grid _ gridMap) = BidirectionalMap.elems gridMap

allNorthOf :: (Position p) => p -> Grid p a -> [p]
allNorthOf pos _ = map (fromTuple . (x pos,)) [(y pos - 1), (y pos - 2) .. 0]

allSouthOf :: (Position p) => p -> Grid p a -> [p]
allSouthOf pos (Grid (_, (_, maxY)) _) = map (fromTuple . (x pos,)) [y pos + 1 .. maxY]

allWestOf :: (Position p) => p -> Grid p a -> [p]
allWestOf pos _ = map (fromTuple . (,y pos)) [(x pos - 1), (x pos - 2) .. 0]

allEastOf :: (Position p) => p -> Grid p a -> [p]
allEastOf pos (Grid ((_, maxX), _) _) = map (fromTuple . (,y pos)) [x pos + 1 .. maxX]

insert :: (Position p, Ord p, Ord a) => p -> a -> Grid p a -> Grid p a
insert pos value (Grid ((minX, maxX), (minY, maxY)) gridMap) = Grid (newWidth, newHeight) newGridMap
  where
    (posX, posY) = toTuple pos
    newWidth = (min minX posX, max maxX posX)
    newHeight = (min minY posY, max maxY posY)
    newGridMap = BidirectionalMap.insert pos value gridMap

insertPair :: (Position p, Ord p, Ord a) => (p, a) -> Grid p a -> Grid p a
insertPair (p, a) = insert p a

width :: Grid p a -> Int
width (Grid ((minX, maxX), _) _) = maxX - minX + 1

height :: Grid p a -> Int
height (Grid (_, (minY, maxY)) _) = maxY - minY + 1

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
