module Common.Regex (parseGroups, parseMatches, isMatch, re) where

import Text.Regex.PCRE.Heavy

parseGroups :: Regex -> String -> [String]
parseGroups pattern input = snd . head . scan pattern $ input

parseMatches :: Regex -> String -> [String]
parseMatches pattern input = map fst . scan pattern $ input

isMatch :: Regex -> String -> Bool
isMatch pattern input = not . null . scan pattern $ input
