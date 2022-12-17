module Day11.MonkeyInTheMiddle (monkeyLevel) where

import Data.List (sortOn)
import Data.Ord (Down (Down))
import Day11.Monkey
import Day11.Monkeys

monkeyLevel :: String -> Int
monkeyLevel = product . take 2 . sortOn Down . map businessLevel . toList . playRounds 20 . read
