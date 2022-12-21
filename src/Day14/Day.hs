module Day14.Day (day14part1, day14part2) where

-- https://adventofcode.com/2022/day/14

import Common.Day
import Common.TextFile (readFileContents)
import Day14.RegolithReservoir

day14part1 :: Day
day14part1 =
  Day
    { name = "day14part1",
      friendlyName = "Day 14 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day14.txt"
        let result = sandUnitsUntilFall input
        print result
    }

day14part2 :: Day
day14part2 =
  Day
    { name = "day14part2",
      friendlyName = "Day 14 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day14.txt"
        let result = sandUnitsUntilRest input
        print result
    }
