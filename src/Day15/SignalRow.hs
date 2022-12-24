module Day15.SignalRow (SignalRow, from, size, hole, union, withBeacon) where

import Common.Grid (x, y)
import Common.List (minimumMaximum)
import Common.Math (gaussBetween)
import Common.OctaGridPosition (Position)
import qualified Common.OctaGridPosition as Position (from)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, insert, toList, union)
import Common.Range (Range)
import qualified Common.Range as Range (from, size, union, gap, start)

data SignalRow = SignalRow {range:: Range Int, rowY :: Int, beacons :: Set Int} deriving (Eq, Show)

from :: Int -> Int -> Int -> SignalRow
from minimumX maximumX row = SignalRow (Range.from minimumX maximumX) row Set.empty

withBeacon :: Int -> SignalRow -> SignalRow
withBeacon beaconX SignalRow {range = myRange, rowY = myRowY, beacons = myBeacons} =
  SignalRow myRange myRowY (Set.insert beaconX myBeacons)

size :: SignalRow -> Int
size (SignalRow range _ beacons) = Range.size range - (length beacons)

union :: SignalRow -> SignalRow -> SignalRow
union (SignalRow rangeA rowA beaconsA) (SignalRow rangeB _ beaconsB ) = SignalRow (rangeA `Range.union` rangeB) rowA (Set.union beaconsA beaconsB)

hole :: SignalRow -> Maybe Position
hole (SignalRow myRange row _) = fmap ((`Position.from` row). Range.start) . Range.gap $ myRange

--hole :: SignalRow -> Maybe Position
--hole (SignalRow mySet)
--  | sumX == expectedSumX = Nothing
--  | otherwise = Just . Position.from beaconX $ beaconY
--  where
--    (minX, maxX) = minimumMaximum . map x . Set.toList $ mySet
--    sumX = sum . map x . Set.toList $ mySet :: Int
--    expectedSumX = gaussBetween minX maxX :: Int
--    beaconX = gaussBetween minX maxX - sumX
--    beaconY = head . map y . Set.toList $ mySet
