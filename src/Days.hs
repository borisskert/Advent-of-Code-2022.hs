module Days (runDays) where

import Common.Day (Day)
import qualified Common.Day.IO
import Day01.Day (day01part1, day01part2)
import Day02.Day (day02part1, day02part2)
import Day03.Day (day03part1, day03part2)
import Day04.Day (day04part1, day04part2)
import Day05.Day (day05part1, day05part2)
import Day06.Day (day06part1, day06part2)
import Day07.Day (day07part1, day07part2)
import Day08.Day (day08part1, day08part2)
import Day09.Day (day09part1, day09part2)
import Day10.Day (day10part1, day10part2)
import Day11.Day (day11part1, day11part2)
import Day12.Day (day12part1, day12part2)
import Day13.Day (day13part1, day13part2)
import Day14.Day (day14part1, day14part2)

allDays :: [Day]
allDays =
  [ day01part1,
    day01part2,
    day02part1,
    day02part2,
    day03part1,
    day03part2,
    day04part1,
    day04part2,
    day05part1,
    day05part2,
    day06part1,
    day06part2,
    day07part1,
    day07part2,
    day08part1,
    day08part2,
    day09part1,
    day09part2,
    day10part1,
    day10part2,
    day11part1,
    day11part2,
    day12part1,
    day12part2,
    day13part1,
    day13part2,
    day14part1,
    day14part2
  ]

runDays :: [String] -> IO ()
runDays = Common.Day.IO.runDays allDays
