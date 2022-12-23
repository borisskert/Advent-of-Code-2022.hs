{-# LANGUAGE NumericUnderscores #-}

module Day15.BeaconExclusionZone (howManyPositions, tuningFrequency) where

import Common.Grid (toTuple)
import Day15.Detector (distressSignal, fromReports, rowAt)
import Day15.SensorAndBeaconReport (readMany)
import Day15.SignalRow (size)
import Debug.Trace (traceShow)

-- In the row where y=2000000, how many positions cannot contain a beacon?
howManyPositions :: Int -> String -> Int
howManyPositions row = size . rowAt row . fromReports . readMany

-- Find the only possible position for the distress beacon. What is its tuning frequency?
tuningFrequency :: Int -> String -> Integer
tuningFrequency row input = (+ toInteger positionY) . (* 4_000_000) . toInteger $ positionX
  where
    (positionX, positionY) = (\x -> traceShow(x) x) . toTuple . distressSignal row . fromReports . readMany $ input
