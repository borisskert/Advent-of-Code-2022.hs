module Day15.SignalRow (SignalRow, from, size, toList, hole, fromList) where

import Common.Grid (x, y)
import Common.List (minimumMaximum)
import Common.Math (gaussBetween)
import Common.OctaGridPosition (Position)
import qualified Common.OctaGridPosition as Position (from)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, toList)

newtype SignalRow = SignalRow (Set Position) deriving (Eq, Show)

from :: Set Position -> SignalRow
from = SignalRow

size :: SignalRow -> Int
size (SignalRow mySet) = length mySet

fromList :: [Position] -> SignalRow
fromList = SignalRow . Set.fromList

toList :: SignalRow -> [Position]
toList (SignalRow mySet) = Set.toList mySet

hole :: SignalRow -> Maybe Position
hole (SignalRow mySet)
  | sumX == expectedSumX = Nothing
  | otherwise = Just . Position.from beaconX $ beaconY
  where
    (minX, maxX) = minimumMaximum . map x . Set.toList $ mySet
    sumX = sum . map x . Set.toList $ mySet :: Int
    expectedSumX = gaussBetween minX maxX :: Int
    beaconX = gaussBetween minX maxX - sumX
    beaconY = head . map y . Set.toList $ mySet
