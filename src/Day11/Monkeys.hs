module Day11.Monkeys (Monkeys, fromList) where

import Data.List.Split (splitOn)
import Day11.Monkey

newtype Monkeys = Monkeys [Monkey] deriving (Eq, Show)

instance Read Monkeys where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = parseFromInput input

parseFromInput :: String -> Monkeys
parseFromInput = Monkeys . map (read . unlines) . splitOn [""] . lines

fromList :: [Monkey] -> Monkeys
fromList = Monkeys
