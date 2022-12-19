module Day12.Day (day12part1) where

-- https://adventofcode.com/2022/day/12

import Common.Day
import Common.TextFile (readFileContents)
import Day12.HillClimbingAlgorithm

day12part1 :: Day
day12part1 =
  Day
    { name = "day12part1",
      friendlyName = "Day 12 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day12.txt"
        let result = howManyPathSteps input
        print result
    }
