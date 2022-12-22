module Day15.BeaconExclusionZone (howManyPositions) where

import Day15.ComprehensiveMap (fromReports, rowAt)
import Day15.Detector (from, scanAll, toMap)
import Day15.Device (noDevice)
import Day15.SensorAndBeaconReport (readMany)

-- In the row where y=2000000, how many positions cannot contain a beacon?
howManyPositions :: Int -> String -> Int
howManyPositions row = length . filter ((== noDevice) . snd) . rowAt row . toMap . scanAll . from . fromReports . readMany
