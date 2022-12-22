module Day15.ComprehensiveMap (ComprehensiveMap, fromReports, toList, beacons, clearArea, noDevices, sensors, rowAt) where

import Common.Grid (Grid)
import qualified Common.Grid as Grid (empty, insert, insertMissing, lookupPositions, rowAt, toList)
import Common.OctaGridPosition (Position)
import Day15.Device (Device, beacon, noDevice, sensor)
import Day15.SensorAndBeaconReport (BeaconAndSensorReport)
import qualified Day15.SensorAndBeaconReport as BeaconAndSensorReport (beacon, sensor)

newtype ComprehensiveMap = ComprehensiveMap (Grid Position Device) deriving (Eq, Show)

empty :: ComprehensiveMap
empty = ComprehensiveMap Grid.empty

toList :: ComprehensiveMap -> [(Position, Device)]
toList (ComprehensiveMap grid) = Grid.toList grid

fromReports :: [BeaconAndSensorReport] -> ComprehensiveMap
fromReports = foldl importReport empty

importReport :: ComprehensiveMap -> BeaconAndSensorReport -> ComprehensiveMap
importReport (ComprehensiveMap grid) report =
  ComprehensiveMap . Grid.insert beaconPos beacon . Grid.insert sensorPos sensor $ grid
  where
    beaconPos = BeaconAndSensorReport.beacon report
    sensorPos = BeaconAndSensorReport.sensor report

beacons :: ComprehensiveMap -> [Position]
beacons (ComprehensiveMap grid) = Grid.lookupPositions beacon grid

sensors :: ComprehensiveMap -> [Position]
sensors (ComprehensiveMap grid) = Grid.lookupPositions sensor grid

noDevices :: ComprehensiveMap -> [Position]
noDevices (ComprehensiveMap grid) = Grid.lookupPositions noDevice grid

clearArea :: Position -> ComprehensiveMap -> ComprehensiveMap
clearArea pos (ComprehensiveMap grid) = ComprehensiveMap . Grid.insertMissing pos noDevice $ grid

rowAt :: Int -> ComprehensiveMap -> [(Position, Device)]
rowAt y (ComprehensiveMap grid) = Grid.rowAt y grid
