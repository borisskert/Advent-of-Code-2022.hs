module Day15.ComprehensiveMap (ComprehensiveMap, fromReports, toList, rowAt) where

import Common.List
import Common.OctaGridPosition (Position)
import Data.Set (Set)
import qualified Data.Set as Set (empty, insert, toList)
import Day15.ScannerArea
import Day15.SensorAndBeaconReport (BeaconAndSensorReport)
import qualified Day15.SensorAndBeaconReport as BeaconAndSensorReport (beacon, sensor)

newtype ComprehensiveMap = ComprehensiveMap (Set ScannerArea) deriving (Eq, Show)

empty :: ComprehensiveMap
empty = ComprehensiveMap Set.empty

toList :: ComprehensiveMap -> [ScannerArea]
toList (ComprehensiveMap mySet) = Set.toList mySet

fromReports :: [BeaconAndSensorReport] -> ComprehensiveMap
fromReports = foldl importReport empty

importReport :: ComprehensiveMap -> BeaconAndSensorReport -> ComprehensiveMap
importReport (ComprehensiveMap mySet) report =
  ComprehensiveMap . Set.insert scannerArea $ mySet
  where
    beaconPos = BeaconAndSensorReport.beacon report
    sensorPos = BeaconAndSensorReport.sensor report
    scannerArea = from sensorPos beaconPos

rowAt :: Int -> ComprehensiveMap -> [Position]
rowAt y = distinct . concatMap (intersectionRow y) . filter (intersectsRow y) . toList
