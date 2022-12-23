module Day15.SignalRow (SignalRow, from, size, hole, union, withBeacon) where

import Common.Grid (x, y)
import Common.List (minimumMaximum)
import Common.Math (gaussBetween)
import Common.OctaGridPosition (Position)
import qualified Common.OctaGridPosition as Position (from)
import Data.Set (Set)
import qualified Data.Set as Set (empty, fromList, insert, toList, union)
import Debug.Trace

data SignalRow = SignalRow {minX :: Int, maxX :: Int, rowY :: Int, beacons :: Set Int, gap :: Maybe Int} deriving (Eq, Show)

from :: Int -> Int -> Int -> SignalRow
from minimumX maximumX row = SignalRow minimumX maximumX row Set.empty Nothing

withBeacon :: Int -> SignalRow -> SignalRow
withBeacon beaconX SignalRow {minX = myMinX, maxX = myMaxX, rowY = myRowY, beacons = myBeacons, gap = myGap} =
  SignalRow myMinX myMaxX myRowY (Set.insert beaconX myBeacons) myGap

size :: SignalRow -> Int
size row = maxX row - minX row + 1 - (length . beacons $ row)

union :: SignalRow -> SignalRow -> SignalRow
union (SignalRow minA maxA rowA beaconsA _) (SignalRow minB maxB _ beaconsB _) = traceShow(minA, maxA, minB, maxB) . SignalRow (min minA minB) (max maxA maxB) rowA (Set.union beaconsA beaconsB) $ myGap
  where
    maxBeaconsA = maximum . Set.toList $ beaconsA
    maxA'
      | not . null $ beaconsA = max maxA maxBeaconsA
      | otherwise = maxA
    
    minBeaconsA = minimum . Set.toList $ beaconsA
    minA'
      | not . null $ beaconsA = min minA minBeaconsA
      | otherwise = minA
    
    maxBeaconsB = maximum . Set.toList $ beaconsB
    maxB'
      | not . null $ beaconsB = max maxB maxBeaconsB
      | otherwise = maxB
    
    minBeaconsB = minimum . Set.toList $ beaconsB
    minB'
      | not . null $ beaconsB = min minB minBeaconsB
      | otherwise = minB
    
    myGap
      | minA' > maxB' + 1 = Just (minA' - 1)
      | maxA' < minB' - 1 = Just (maxA' + 1)
      | otherwise = Nothing

hole :: SignalRow -> Maybe Position
hole row = fmap (`Position.from` rowY row) . gap. traceShow(row) $ row

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
