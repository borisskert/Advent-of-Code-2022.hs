module Day09.Day (day09part1, day09part2) where

-- https://adventofcode.com/2022/day/9

import Common.Day
import Common.TextFile (readFileContents)
import Day09.RopeBridge

day09part1 :: Day
day09part1 =
  Day
    { name = "day09part1",
      friendlyName = "Day 09 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day09.txt"
        let result = howManyTailPositions 1 input
        print result
    }

day09part2 :: Day
day09part2 =
  Day
    { name = "day09part2",
      friendlyName = "Day 09 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day09.txt"
        let result = howManyTailPositions 9 input
        print result
    }
