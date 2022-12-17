{-# LANGUAGE QuasiQuotes #-}

module Day11.Test
  ( Test,
    from,
    decide,
    divisibleBy,
  )
where

import Common.Prelude (isMultipleOf)
import Common.Regex
import Day11.Item (Item, worryLevel)
import Day11.MonkeyId (MonkeyId)
import Day11.WorryLevel (WorryLevel)

data Test = Test {divisibleBy :: WorryLevel, ifTrue :: MonkeyId, ifFalse :: MonkeyId} deriving (Eq, Show)

instance Read Test where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = parseLines . lines $input

from :: WorryLevel -> MonkeyId -> MonkeyId -> Test
from d t f = Test {divisibleBy = d, ifTrue = t, ifFalse = f}

parseLines :: [String] -> Test
parseLines [header, true, false] =
  Test
    { divisibleBy = parseDivisibleBy header,
      ifTrue = parseTrueLine true,
      ifFalse = parseFalseLine false
    }
parseLines xs = error ("Day11.Test.parseLines: Illegal input '" ++ show xs ++ "'")

parseDivisibleBy :: String -> WorryLevel
parseDivisibleBy = read . head . parseGroups [re|[ ]*Test: divisible by ([0-9]+)|]

parseTrueLine :: String -> MonkeyId
parseTrueLine = read . head . parseGroups [re|[ ]*If true: (.+)|]

parseFalseLine :: String -> MonkeyId
parseFalseLine = read . head . parseGroups [re|[ ]*If false: (.+)|]

decide :: Item -> Test -> MonkeyId
decide item test
  | w `isMultipleOf` d = ifTrue test
  | otherwise = ifFalse test
  where
    d = divisibleBy test
    w = worryLevel item
