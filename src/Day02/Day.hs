module Day02.Day (day02part1, day02part2) where

-- https://adventofcode.com/2022/day/2

import Common.Day
import Common.TextFile (readFileContents)
import Day02.RockPaperScissors (totalScore, totalScoreDecrypted)

day02part1 :: Day
day02part1 =
  Day
    { name = "day02part1",
      friendlyName = "Day 02 Part 1",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day02.txt"
        let result = totalScore input
        print result
    }

day02part2 :: Day
day02part2 =
  Day
    { name = "day02part2",
      friendlyName = "Day 02 Part 2",
      isDefault = True,
      run = do
        input <- readFileContents "puzzleinput/day02.txt"
        let result = totalScoreDecrypted input
        print result
    }
