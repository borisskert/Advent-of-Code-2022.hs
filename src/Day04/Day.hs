module Day04.Day (day04part1, day04part2) where

-- https://adventofcode.com/2022/day/3

import Common.Day
import Common.TextFile (readFileContents)
import Day04.CampCleanup

day04part1 :: Day
day04part1 =
  Day
    { name = "day04part1",
      friendlyName = "Day 04 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day04.txt"
        let result = howManyPairsFullyContainAnother input
        print result
    }

day04part2 :: Day
day04part2 =
  Day
    { name = "day04part2",
      friendlyName = "Day 04 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day04.txt"
        let result = howManyPairsOverlap input
        print result
    }
