module Day08.Day (day08part1) where

-- https://adventofcode.com/2022/day/8

import Common.Day
import Common.TextFile (readFileContents)
import Day08.TreetopTreeHouse

day08part1 :: Day
day08part1 =
  Day
    { name = "day08part1",
      friendlyName = "Day 08 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day08.txt"
        let result = howManyTrees input
        print result
    }
