module Day15.Detector (Detector, fromReports, rowAt, distressSignal) where

import Common.List (zigzag)
import Common.OctaGridPosition (Position)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, toList, unions)
import Day15.ScannerArea
import Day15.SensorAndBeaconReport (BeaconAndSensorReport)
import qualified Day15.SensorAndBeaconReport as BeaconAndSensorReport (beacon, sensor)
import Day15.SignalRow (SignalRow)
import qualified Day15.SignalRow as SignalRow (from, hole, size)

newtype Detector = Detector (Set ScannerArea) deriving (Eq, Show)

empty :: Detector
empty = Detector Set.empty

toList :: Detector -> [ScannerArea]
toList (Detector mySet) = Set.toList mySet

fromReports :: [BeaconAndSensorReport] -> Detector
fromReports = foldl importReport empty

importReport :: Detector -> BeaconAndSensorReport -> Detector
importReport (Detector mySet) report =
  Detector . Set.insert scannerArea $ mySet
  where
    beaconPos = BeaconAndSensorReport.beacon report
    sensorPos = BeaconAndSensorReport.sensor report
    scannerArea = from sensorPos beaconPos

rowAt :: Int -> Detector -> SignalRow
rowAt y = SignalRow.from . Set.unions . map (intersectionRow y) . filter (intersectsRow y) . toList

distressSignal :: Int -> Detector -> Position
distressSignal row detector = head . mapMaybe (SignalRow.hole . (`rowAt` detector)) . tail . zigzag $ row
