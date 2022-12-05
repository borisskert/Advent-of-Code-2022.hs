{-# LANGUAGE QuasiQuotes #-}

module Day05.Move (Move, count, from, to, moveOf, readOne) where

import Text.RE.PCRE.String

type Index = Char

data Move = Move {count :: Int, from :: Index, to :: Index} deriving (Eq, Show)

moveOf :: Int -> Index -> Index -> Move
moveOf count' from' to' = Move {count = count', from = from', to = to'}

readOne :: String -> Move
readOne s = Move {count = c, from = f, to = t}
  where
    (c, f, t) = parseMoveNumbers s

parseMoveNumbers :: String -> (Int, Char, Char)
parseMoveNumbers s = (count', from', to')
  where
    numbers = matches $ s *=~ [re|[0-9]+|] :: [String]
    count' = read . head $ numbers
    from' = head . head . tail $ numbers
    to' = head . last $ numbers
