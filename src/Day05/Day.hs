module Day05.Day (day05part1, day05part2) where

-- https://adventofcode.com/2022/day/5

import Common.Day
import Common.TextFile (readFileContents)
import Day05.SupplyStacks (topOfEachStack, topOfEachStackCrateMover9001)

day05part1 :: Day
day05part1 =
  Day
    { name = "day05part1",
      friendlyName = "Day 05 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day05.txt"
        let result = topOfEachStack input
        print result
    }

day05part2 :: Day
day05part2 =
  Day
    { name = "day05part2",
      friendlyName = "Day 05 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day05.txt"
        let result = topOfEachStackCrateMover9001 input
        print result
    }
