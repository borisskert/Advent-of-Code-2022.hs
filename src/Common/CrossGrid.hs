{-# LANGUAGE TupleSections #-}

module Common.CrossGrid
  ( CrossGrid,
    empty,
    fromList,
    fromLines,
    lookup,
    each,
    adjacent,
    filter,
    toLines,
    isWithin,
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
import Data.Bifunctor (second)
import Data.List (intercalate, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, keys, lookup, toList, elems)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Ord (Down)
import Prelude hiding (filter, lookup)
import qualified Prelude (filter)

type Position = (Int, Int)

-- (width, height)
type Size = (Int, Int)

data CrossGrid a = CrossGrid Size (Map Position a) deriving (Eq, Show)

empty :: CrossGrid a
empty = CrossGrid (0, 0) Map.empty

fromLines :: ((Position, Char) -> Maybe a) -> String -> CrossGrid a
fromLines toValue input = CrossGrid size gridMap
  where
    inputLines = lines input
    gridMap = fromLinesIntoMap inputLines toValue
    size = (maybe 0 length . safeHead $ inputLines, length inputLines)

fromLinesIntoMap :: [[Char]] -> ((Position, Char) -> Maybe a) -> Map Position a
fromLinesIntoMap inputLines toValue =
  Map.fromList
    . map (second fromJust)
    . Prelude.filter (isJust . snd)
    . map (\(p, c) -> (p, toValue (p, c)))
    . concatMap (uncurry toLine)
    . zip [0 ..]
    $ inputLines
  where
    toLine :: Int -> [Char] -> [(Position, Char)]
    toLine y line = zipWith (\x c -> ((x, y), c)) [0 ..] line

lookup :: Position -> CrossGrid a -> Maybe a
lookup position (CrossGrid _ gridMap) = Map.lookup position gridMap

each :: ((Position, a) -> (Position, a)) -> CrossGrid a -> CrossGrid a
each mapper (CrossGrid size gridMap) = CrossGrid size newGridMap
  where
    newGridMap = Map.fromList . map mapper . Map.toList $ gridMap

adjacentPositions :: Position -> [Position]
adjacentPositions (x, y) =
  [ (x, y - 1),
    (x -1, y),
    (x + 1, y),
    (x, y + 1)
  ]

adjacent :: Position -> CrossGrid a -> [(Position, a)]
adjacent position grid =
  map (second fromJust)
    . Prelude.filter (isJust . snd)
    . map (\p -> (p, lookup p grid))
    $ positions
  where
    positions = adjacentPositions position :: [Position]

filter :: ((Position, a) -> Bool) -> CrossGrid a -> [(Position, a)]
filter filterFunction (CrossGrid _ gridMap) = Prelude.filter filterFunction . Map.toList $ gridMap

toLines :: (Maybe a -> Char) -> CrossGrid a -> String
toLines toChar (CrossGrid size gridMap) =
  intercalate "\n"
    . map (\y -> map (toChar . (`Map.lookup` gridMap) . (,y)) xs)
    $ ys
  where
    (width, height) = size
    minX = 0
    minY = 0
    maxX = width - 1
    maxY = height - 1

    xs = [minX, (minX + 1) .. maxX]
    ys = [minY, (minY + 1) .. maxY]

isWithin :: Position -> Size -> Bool
isWithin (x, y) (width, height) = x >= 0 && x < width && y >= 0 && y < height

fromList :: [[a]] -> CrossGrid a
fromList list = CrossGrid size gridMap
  where
    size = (maybe 0 length . safeHead $ list, length list)
    gridMap =
      Map.fromList
        . concatMap (uncurry toLine)
        . zip [0 ..]
        $ list

    toLine :: Int -> [a] -> [(Position, a)]
    toLine y line = zipWith (\x c -> ((x, y), c)) [0 ..] line

toList :: a -> CrossGrid a -> [[a]]
toList nil (CrossGrid size gridMap) = map (map (fromMaybe nil . (`Map.lookup` gridMap))) positions
  where
    positions = positionsFrom size

positionsFrom :: Size -> [[Position]]
positionsFrom (width, height) = map (\y -> map (,y) xs) ys
  where
    xs = [0 .. (width - 1)]
    ys = [0 .. (height - 1)]

mapGrid :: (Position -> a -> b) -> CrossGrid a -> CrossGrid b
mapGrid fn (CrossGrid size gridMap) = CrossGrid size newGridMap
  where
    newGridMap = Map.fromList . map (\(pos, x) -> (pos, fn pos x)) . Map.toList $ gridMap

rows :: CrossGrid a -> [[Position]]
rows (CrossGrid (width, height) _) = map (\y -> map (,y) [0 .. width - 1]) [0 .. height - 1]

columns :: CrossGrid a -> [[Position]]
columns (CrossGrid (width, height) _) = map (\x -> map (x,) [0 .. height - 1]) [0 .. width - 1]

keys :: CrossGrid a -> [Position]
keys (CrossGrid _ gridMap) = Map.keys gridMap

elems :: CrossGrid a -> [a]
elems (CrossGrid _ gridMap) = Map.elems gridMap

allNorthOf :: Position -> CrossGrid a -> [Position]
allNorthOf (x, y) _ = map (x,) [(y - 1), (y - 2) .. 0]

allSouthOf :: Position -> CrossGrid a -> [Position]
allSouthOf (x, y) (CrossGrid (_, height) _) = map (x,) [y + 1 .. (height - 1)]

allWestOf :: Position -> CrossGrid a -> [Position]
allWestOf (x, y) _ = map (,y) [(x - 1), (x - 2) .. 0]

allEastOf :: Position -> CrossGrid a -> [Position]
allEastOf (x, y) (CrossGrid (width, _) _) = map (,y) [x + 1 .. (width - 1)]
