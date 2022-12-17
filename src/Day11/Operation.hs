{-# LANGUAGE QuasiQuotes #-}

module Day11.Operation (Operation, addition, multiplication, square, operate) where

import Common.Regex
import Day11.Item (Item, from, worryLevel)

data Operation = Addition Int | Multiplication Int | Square deriving (Eq, Show)

instance Read Operation where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = parseInputLine input

parseInputLine :: String -> Operation
parseInputLine input
  | operator == "*" && operand == "old" = Square
  | operator == "*" = Multiplication . read $ operand
  | otherwise = Addition . read $ operand
  where
    parsedGroups = parseGroups [re|[ ]+Operation: new = old ([*+]) ([0-9]+|old)|] input
    operator = head parsedGroups
    operand = last parsedGroups

addition :: Int -> Operation
addition = Addition

multiplication :: Int -> Operation
multiplication = Multiplication

square :: Operation
square = Square

type WorryLevel = Int

operate :: Item -> Operation -> Item
operate item operation = from . (`div` 3) . (`execute` operation) . worryLevel $ item

execute :: WorryLevel -> Operation -> WorryLevel
execute w (Addition i) = w + i
execute w (Multiplication i) = w * i
execute w Square = w * w
