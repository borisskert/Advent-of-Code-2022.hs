module Day12.HillClimbingAlgorithm (howManyPathSteps) where

import Day12.Path
import Day12.PathFinder

howManyPathSteps :: String -> Int
howManyPathSteps = subtract 1 . steps . find . from . read
