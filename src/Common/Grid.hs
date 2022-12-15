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
    fromLines,
    lookup,
    each,
    toLines,
    toList,
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

import Common.List
import Data.Bifunctor (second)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map (elems, empty, fromList, insert, keys, lookup, toList)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Debug.Trace (traceShow)
import Prelude hiding (lookup)

type Range = (Int, Int)

-- (width, height)
type Size = (Range, Range)

data Grid p a = Grid Size (Map p a) deriving (Eq, Show)

empty :: Grid p a
empty = Grid ((maxBound, minBound), (maxBound, minBound)) Map.empty

class Position a where
  x :: a -> Int
  y :: a -> Int
  toTuple :: a -> (Int, Int)
  fromTuple :: (Int, Int) -> a

class Value a where
  toValue :: (Position p) => (p, Char) -> Maybe a
  fromValue :: Maybe a -> Char

fromLines :: (Position p, Ord p, Value a) => String -> Grid p a
fromLines "" = empty
fromLines input = Grid size gridMap
  where
    inputLines = lines input
    gridMap = fromLinesIntoMap inputLines
    maxX = maybe 0 (subtract 1 . length) . safeHead $ inputLines
    maxY = length inputLines - 1
    size = ((0, maxX), (0, maxY))

fromLinesIntoMap :: (Position p, Ord p, Value a) => [[Char]] -> Map p a
fromLinesIntoMap =
  Map.fromList
    . map (second fromJust)
    . filter (isJust . snd)
    . map (\(p, c) -> (p, toValue (p, c)))
    . concatMap (uncurry toLine)
    . zip [0 ..]
  where
    toLine y' line = zipWith (\x' c -> (fromTuple (x', y'), c)) [0 ..] line

lookup :: (Ord p) => p -> Grid p a -> Maybe a
lookup p (Grid _ gridMap) = Map.lookup p gridMap

each :: (Ord p) => ((p, a) -> (p, a)) -> Grid p a -> Grid p a
each mapper (Grid size gridMap) = Grid size newGridMap
  where
    newGridMap = Map.fromList . map mapper . Map.toList $ gridMap

toLines :: (Position p, Ord p, Value a) => Grid p a -> String
toLines (Grid size gridMap) =
  intercalate "\n"
    . map (\y' -> map (fromValue . (`Map.lookup` gridMap) . fromTuple . (,y')) xs)
    $ ys
  where
    ((minX, maxX), (minY, maxY)) = size
    xs = [minX, (minX + 1) .. maxX]
    ys = [minY, (minY + 1) .. maxY]

fromList :: (Position p, Ord p) => [[a]] -> Grid p a
fromList list = Grid size gridMap
  where
    size = ((0, maybe 0 (subtract 1 . length) . safeHead $ list), (0, length list - 1))
    gridMap =
      Map.fromList
        . concatMap (uncurry toLine)
        . zip [0 ..]
        $ list

    toLine y' line = zipWith (\x' c -> (fromTuple (x', y'), c)) [0 ..] line

toList :: (Position p, Ord p, Value a) => Grid p a -> [[Char]]
toList grid@(Grid _ gridMap) = map (map (fromValue . (`Map.lookup` gridMap))) positions
  where
    positions = generatePositions grid

generatePositions :: (Position p) => Grid p a -> [[p]]
generatePositions (Grid ((minX, maxX), (minY, maxY)) _) = map (\y' -> map (fromTuple . (,y')) xs) ys
  where
    xs = [minX .. maxX]
    ys = [minY .. maxY]

mapGrid :: (Ord p) => (p -> a -> b) -> Grid p a -> Grid p b
mapGrid fn (Grid size gridMap) = Grid size newGridMap
  where
    newGridMap = Map.fromList . map (\(pos, x') -> (pos, fn pos x')) . Map.toList $ gridMap

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
keys (Grid _ gridMap) = Map.keys gridMap

elems :: Grid p a -> [a]
elems (Grid _ gridMap) = Map.elems gridMap

allNorthOf :: (Position p) => p -> Grid p a -> [p]
allNorthOf pos _ = map (fromTuple . (x pos,)) [(y pos - 1), (y pos - 2) .. 0]

allSouthOf :: (Position p) => p -> Grid p a -> [p]
allSouthOf pos (Grid (_, (_, maxY)) _) = map (fromTuple . (x pos,)) [y pos + 1 .. maxY]

allWestOf :: (Position p) => p -> Grid p a -> [p]
allWestOf pos _ = map (fromTuple . (,y pos)) [(x pos - 1), (x pos - 2) .. 0]

allEastOf :: (Position p) => p -> Grid p a -> [p]
allEastOf pos (Grid ((_, maxX), _) _) = map (fromTuple . (,y pos)) [x pos + 1 .. maxX]

insert :: (Position p, Ord p) => p -> a -> Grid p a -> Grid p a
insert pos value (Grid ((minX, maxX), (minY, maxY)) gridMap) = Grid (newWidth, newHeight) newGridMap
  where
    (posX, posY) = toTuple pos
    newWidth = (min minX posX, max maxX posX)
    newHeight = (min minY posY, max maxY posY)
    newGridMap = Map.insert pos value gridMap

width :: Grid p a -> Int
width (Grid ((minX, maxX), _) _) = maxX - minX + 1

height :: Grid p a -> Int
height (Grid (_, (minY, maxY)) _) = maxY - minY + 1

subgrid :: (Position p, Ord p) => p -> (Int, Int) -> Grid p a -> Grid p a
subgrid pos (width', height') grid = newGrid
  where
    (posX, posY) = toTuple pos
    xs = [posX .. (posX + width' - 1)]
    ys = [posY .. (posY + height' - 1)]
    positions = [fromTuple (x', y') | x' <- xs, y' <- ys]
    lookupValue position = fmap (position,) . lookup position $ grid
    pairs = mapMaybe lookupValue positions
    newGrid = foldl (\g (k, v) -> insert k v g) empty pairs
