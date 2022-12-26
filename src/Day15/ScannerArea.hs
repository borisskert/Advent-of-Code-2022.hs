module Day15.ScannerArea (ScannerArea, from, rowAt) where

import Common.Grid (x, y)
import Common.OctaGridPosition (Position, manhattanDistance)
import Day15.SignalRow (SignalRow)
import qualified Day15.SignalRow as SignalRow (from, withBeacon)

-- | -------------------------------------------------------------------------------------------------------------------
-- | ScannerArea is a rectangle that is defined by two positions. The first position is the sensor position and the
-- | second the beacon position.
-- | -------------------------------------------------------------------------------------------------------------------
data ScannerArea = ScannerArea {scanner :: Position, beacon :: Position} deriving (Eq, Ord, Show)

from :: Position -> Position -> ScannerArea
from = ScannerArea

rowAt :: Int -> ScannerArea -> Maybe SignalRow
rowAt row ScannerArea {scanner = myScanner, beacon = myBeacon}
  | minY > row || row > maxY = Nothing
  | beaconY == row = Just $ SignalRow.withBeacon beaconX signalRow
  | otherwise = Just signalRow
  where
    size = manhattanDistance myScanner myBeacon
    scannerY = y myScanner
    minY = scannerY - size
    maxY = scannerY + size
    distance = abs $ scannerY - row
    startX = x myScanner - size + distance
    endX = x myScanner + size - distance
    signalRow = SignalRow.from startX endX row
    beaconX = x myBeacon
    beaconY = y myBeacon
