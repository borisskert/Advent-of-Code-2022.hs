{-# LANGUAGE TupleSections #-}

module Common.CrossGrid
  ( fromLines,
    lookup,
    each,
    adjacent,
    filter,
    toLines,
    isWithin,
  )
where

import Data.Bifunctor (second)
import Data.List (intercalate)
import qualified Data.Map (Map, fromList, lookup, toList)
import Data.Maybe (fromJust, isJust)
import Prelude hiding (filter, lookup)
import qualified Prelude (filter)

type Position = (Int, Int)

type Size = (Int, Int)

data CrossGrid a = CrossGrid Size (Data.Map.Map Position a)

fromLines :: ((Position, Char) -> Maybe a) -> String -> CrossGrid a
fromLines toValue input = CrossGrid size gridMap
  where
    inputLines = lines input
    gridMap = fromLinesIntoMap inputLines toValue
    size = (length . head $ inputLines, length inputLines)

fromLinesIntoMap :: [[Char]] -> ((Position, Char) -> Maybe a) -> Data.Map.Map Position a
fromLinesIntoMap inputLines toValue =
  Data.Map.fromList
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
lookup position (CrossGrid _ gridMap) = Data.Map.lookup position gridMap

each :: ((Position, a) -> (Position, a)) -> CrossGrid a -> CrossGrid a
each mapper (CrossGrid size gridMap) = CrossGrid size newGridMap
  where
    newGridMap = Data.Map.fromList . map mapper . Data.Map.toList $ gridMap

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
filter filterFunction (CrossGrid _ gridMap) = Prelude.filter filterFunction . Data.Map.toList $ gridMap

toLines :: (Maybe a -> Char) -> CrossGrid a -> String
toLines toChar (CrossGrid size gridMap) =
  intercalate "\n"
    . map (\y -> map (toChar . (`Data.Map.lookup` gridMap) . (,y)) xs)
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
