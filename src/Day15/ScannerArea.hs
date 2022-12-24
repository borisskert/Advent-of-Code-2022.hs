module Day15.ScannerArea (ScannerArea, from, intersectionRow, isIntersectsRow, fullIntersectionRow) where

import Common.Grid (x, y)
import Common.OctaGridPosition (Position, manhattanDistance)
import qualified Common.OctaGridPosition as Position (from)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)
import Day15.SignalRow (SignalRow, hole)
import qualified Day15.SignalRow as SignalRow (from, withBeacon)

data ScannerArea = ScannerArea {scanner :: Position, beacon :: Position} deriving (Eq, Ord, Show)

from :: Position -> Position -> ScannerArea
from = ScannerArea

isIntersectsRow :: Int -> ScannerArea -> Bool
isIntersectsRow row ScannerArea {scanner = myScanner, beacon = myBeacon} = minY <= row && row <= maxY
  where
    scannerY = y myScanner
    size = manhattanDistance myScanner myBeacon
    minY = scannerY - size
    maxY = scannerY + size

intersectionRow :: Int -> ScannerArea -> SignalRow
intersectionRow row ScannerArea {scanner = myScanner, beacon = myBeacon}
  | beaconY == row = SignalRow.withBeacon beaconX signalRow
  | otherwise = signalRow
  where
    beaconX = x myBeacon
    beaconY = y myBeacon
    size = manhattanDistance myScanner myBeacon
    distance = abs $ y myScanner - row
    startX = x myScanner - size + distance
    endX = x myScanner + size - distance
    signalRow = SignalRow.from startX endX row

fullIntersectionRow :: Int -> ScannerArea -> SignalRow
fullIntersectionRow row ScannerArea {scanner = myScanner, beacon = myBeacon} = SignalRow.from startX endX row
  where
    size = manhattanDistance myScanner myBeacon
    distance = abs $ y myScanner - row
    startX = x myScanner - size + distance
    endX = x myScanner + size - distance
