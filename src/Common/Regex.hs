module Common.Regex (parseGroups, parseMatches, isMatch, re, Regex) where

import Text.Regex.PCRE.Heavy

parseGroups :: Regex -> String -> [String]
parseGroups pattern input
  | null parsed = error ("Nothing parsed in '" ++ input ++ "'")
  | otherwise = snd . head . scan pattern $ input
  where
    parsed = scan pattern $ input

parseMatches :: Regex -> String -> [String]
parseMatches pattern input = map fst . scan pattern $ input

isMatch :: Regex -> String -> Bool
isMatch pattern input = not . null . scan pattern $ input
