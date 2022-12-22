module Day15.BeaconExclusionZone (howManyPositions) where

import Day15.Detector (fromReports, rowAt)
import Day15.SensorAndBeaconReport (readMany)
import Day15.SignalRow (size)

-- In the row where y=2000000, how many positions cannot contain a beacon?
howManyPositions :: Int -> String -> Int
howManyPositions row = size . rowAt row . fromReports . readMany
