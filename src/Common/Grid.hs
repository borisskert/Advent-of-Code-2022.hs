{-# LANGUAGE TupleSections #-}

module Common.Grid
  ( Grid,
    GridValue,
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
  )
where

import Common.List
import Common.Vector2D
import Data.Bifunctor (second)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map (elems, empty, fromList, keys, lookup, toList)
import Data.Maybe (fromJust, isJust)
import Prelude hiding (lookup)

-- (width, height)
type Size = (Int, Int)

data Grid p a = Grid Size (Map p a) deriving (Eq, Show)

empty :: Grid p a
empty = Grid (0, 0) Map.empty

class GridValue a where
  toValue :: (Vector2D p) => (p, Char) -> Maybe a
  fromValue :: Maybe a -> Char

fromLines :: (Vector2D p, Ord p, GridValue a) => String -> Grid p a
fromLines input = Grid size gridMap
  where
    inputLines = lines input
    gridMap = fromLinesIntoMap inputLines
    size = (maybe 0 length . safeHead $ inputLines, length inputLines)

fromLinesIntoMap :: (Vector2D p, Ord p, GridValue a) => [[Char]] -> Map p a
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

toLines :: (Vector2D p, Ord p, GridValue a) => Grid p a -> String
toLines (Grid size gridMap) =
  intercalate "\n"
    . map (\y' -> map (fromValue . (`Map.lookup` gridMap) . fromTuple . (,y')) xs)
    $ ys
  where
    (width, height) = size
    minX = 0
    minY = 0
    maxX = width - 1
    maxY = height - 1

    xs = [minX, (minX + 1) .. maxX]
    ys = [minY, (minY + 1) .. maxY]

fromList :: (Vector2D p, Ord p) => [[a]] -> Grid p a
fromList list = Grid size gridMap
  where
    size = (maybe 0 length . safeHead $ list, length list)
    gridMap =
      Map.fromList
        . concatMap (uncurry toLine)
        . zip [0 ..]
        $ list

    toLine y' line = zipWith (\x' c -> (fromTuple (x', y'), c)) [0 ..] line

toList :: (Vector2D p, Ord p, GridValue a) => Grid p a -> [[Char]]
toList (Grid size gridMap) = map (map (fromValue . (`Map.lookup` gridMap))) positions
  where
    positions = generatePositions size

generatePositions :: (Vector2D p) => Size -> [[p]]
generatePositions (width, height) = map (\y' -> map (fromTuple . (,y')) xs) ys
  where
    xs = [0 .. (width - 1)]
    ys = [0 .. (height - 1)]

mapGrid :: (Ord p) => (p -> a -> b) -> Grid p a -> Grid p b
mapGrid fn (Grid size gridMap) = Grid size newGridMap
  where
    newGridMap = Map.fromList . map (\(pos, x') -> (pos, fn pos x')) . Map.toList $ gridMap

rows :: (Vector2D p) => Grid p a -> [[p]]
rows (Grid (width, height) _) = map (\y' -> map (fromTuple . (,y')) [0 .. width - 1]) [0 .. height - 1]

columns :: (Vector2D p) => Grid p a -> [[p]]
columns (Grid (width, height) _) = map (\x' -> map (fromTuple . (x',)) [0 .. height - 1]) [0 .. width - 1]

keys :: Grid p a -> [p]
keys (Grid _ gridMap) = Map.keys gridMap

elems :: Grid p a -> [a]
elems (Grid _ gridMap) = Map.elems gridMap

allNorthOf :: (Vector2D p) => p -> Grid p a -> [p]
allNorthOf pos _ = map (fromTuple . (x pos,)) [(y pos - 1), (y pos - 2) .. 0]

allSouthOf :: (Vector2D p) => p -> Grid p a -> [p]
allSouthOf pos (Grid (_, height) _) = map (fromTuple . (x pos,)) [y pos + 1 .. (height - 1)]

allWestOf :: (Vector2D p) => p -> Grid p a -> [p]
allWestOf pos _ = map (fromTuple . (,y pos)) [(x pos - 1), (x pos - 2) .. 0]

allEastOf :: (Vector2D p) => p -> Grid p a -> [p]
allEastOf pos (Grid (width, _) _) = map (fromTuple . (,y pos)) [x pos + 1 .. (width - 1)]
