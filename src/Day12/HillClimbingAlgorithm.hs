module Day12.HillClimbingAlgorithm (howManyPathStepsStartingFromS, howManyPathStepsStartingFromAnyA) where

import Common.Path (steps)
import Day12.PathFinder

howManyPathStepsStartingFromS :: String -> Int
howManyPathStepsStartingFromS = maybe 0 (subtract 1 . steps) . findPathToEnd . from . read

howManyPathStepsStartingFromAnyA :: String -> Int
howManyPathStepsStartingFromAnyA = maybe 0 (subtract 1 . steps) . findPathFromEnd . from . read
