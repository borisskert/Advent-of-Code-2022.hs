module Day12.Day (day12part1, day12part2) where

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
        let result = howManyPathStepsStartingFromS input
        print result
    }

day12part2 :: Day
day12part2 =
  Day
    { name = "day12part2",
      friendlyName = "Day 12 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day12.txt"
        let result = howManyPathStepsStartingFromAnyA input
        print result
    }
