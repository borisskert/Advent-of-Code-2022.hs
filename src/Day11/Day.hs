module Day11.Day (day11part1, day11part2) where

-- https://adventofcode.com/2022/day/11

import Common.Day
import Common.TextFile (readFileContents)
import Day11.MonkeyInTheMiddle

day11part1 :: Day
day11part1 =
  Day
    { name = "day11part1",
      friendlyName = "Day 11 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day11.txt"
        let result = monkeyLevel 3 20 input
        print result
    }

day11part2 :: Day
day11part2 =
  Day
    { name = "day11part2",
      friendlyName = "Day 11 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day11.txt"
        let result = monkeyLevel 1 10000 input
        print result
    }
