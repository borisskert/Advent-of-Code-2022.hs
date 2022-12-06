module Day06.Day (day06part1, day06part2) where

-- https://adventofcode.com/2022/day/6

import Common.Day
import Common.TextFile (readFileContents)
import Day06.TuningTrouble

day06part1 :: Day
day06part1 =
  Day
    { name = "day06part1",
      friendlyName = "Day 06 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day06.txt"
        let result = findFirstMarker input
        print result
    }

day06part2 :: Day
day06part2 =
  Day
    { name = "day06part2",
      friendlyName = "Day 06 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day06.txt"
        let result = findCorrectMarker input
        print result
    }
