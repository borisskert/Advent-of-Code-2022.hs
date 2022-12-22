module Day15.ScannerArea (ScannerArea, from, intersectionRow, intersectsRow) where

import Common.Grid (x, y)
import Common.OctaGridPosition (Position, manhattanDistance)
import qualified Common.OctaGridPosition as Position (from)

data ScannerArea = ScannerArea {scanner :: Position, beacon :: Position} deriving (Eq, Ord, Show)

from :: Position -> Position -> ScannerArea
from = ScannerArea

intersectsRow :: Int -> ScannerArea -> Bool
intersectsRow row ScannerArea {scanner = myScanner, beacon = myBeacon} = minY <= row && row <= maxY
  where
    scannerY = y myScanner
    size = manhattanDistance myScanner myBeacon
    minY = scannerY - size
    maxY = scannerY + size

intersectionRow :: Int -> ScannerArea -> [Position]
intersectionRow row ScannerArea {scanner = myScanner, beacon = myBeacon} = filter (/= myBeacon) [Position.from column row | column <- [startX .. endX]]
  where
    size = manhattanDistance myScanner myBeacon
    distance = abs $ y myScanner - row
    startX = x myScanner - size + distance
    endX = x myScanner + size - distance
