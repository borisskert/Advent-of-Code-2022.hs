module Day11.MonkeyInTheMiddle (monkeyLevel) where

import Data.List (sortOn)
import Data.Ord (Down (Down))
import Day11.Monkey
import Day11.Monkeys

monkeyLevel :: Integer -> Int  -> String -> Int
monkeyLevel worry rounds = product . take 2 . sortOn Down . map businessLevel . toList . playRounds worry rounds . read
