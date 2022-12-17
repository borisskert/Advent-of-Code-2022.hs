{-# LANGUAGE QuasiQuotes #-}

module Day11.Test (Test, from) where

import Common.Regex
import Day11.MonkeyId (MonkeyId)

data Test = Test {divisibleBy :: Int, ifTrue :: MonkeyId, ifFalse :: MonkeyId} deriving (Eq, Show)

instance Read Test where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = parseLines . lines $input

from :: Int -> MonkeyId -> MonkeyId -> Test
from d t f = Test {divisibleBy = d, ifTrue = t, ifFalse = f}

parseLines :: [String] -> Test
parseLines [header, true, false] =
  Test
    { divisibleBy = parseDivisibleBy header,
      ifTrue = parseTrueLine true,
      ifFalse = parseFalseLine false
    }
parseLines xs = error ("Day11.Test.parseLines: Illegal input '" ++ show xs ++ "'")

parseDivisibleBy :: String -> Int
parseDivisibleBy = read . head . parseGroups [re|[ ]*Test: divisible by ([0-9]+)|]

parseTrueLine :: String -> MonkeyId
parseTrueLine = read . head . parseGroups [re|[ ]*If true: (.+)|]

parseFalseLine :: String -> MonkeyId
parseFalseLine = read . head . parseGroups [re|[ ]*If false: (.+)|]
