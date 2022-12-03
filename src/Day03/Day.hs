module Day03.Day (day03part1, day03part2) where

-- https://adventofcode.com/2022/day/3

import Common.Day
import Common.TextFile (readFileContents)
import Day03.RucksackReorganization

day03part1 :: Day
day03part1 =
  Day
    { name = "day03part1",
      friendlyName = "Day 03 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day03.txt"
        let result = prioritySum input
        print result
    }

day03part2 :: Day
day03part2 =
  Day
    { name = "day03part2",
      friendlyName = "Day 03 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day03.txt"
        let result = badgePrioritySum input
        print result
    }
