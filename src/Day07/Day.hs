module Day07.Day (day07part1, day07part2) where

-- https://adventofcode.com/2022/day/7

import Common.Day
import Common.TextFile (readFileContents)
import Day07.NoSpaceLeftOnDevice

day07part1 :: Day
day07part1 =
  Day
    { name = "day07part1",
      friendlyName = "Day 07 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day07.txt"
        let result = totalSize input
        print result
    }

day07part2 :: Day
day07part2 =
  Day
    { name = "day07part2",
      friendlyName = "Day 07 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day07.txt"
        let result = totalSizeOfDirectoryToDelete input
        print result
    }
