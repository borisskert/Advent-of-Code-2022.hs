module Day12.HillClimbingAlgorithm (howManyPathSteps) where

import Day12.Path
import Day12.PathFinder

howManyPathSteps :: String -> Int
howManyPathSteps = maybe 0 (subtract 1 . steps) . findPathToEnd . from . read
