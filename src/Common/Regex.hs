module Common.Regex (parseGroups, tryParseGroups, parseGroupsFirstMatch, parseMatches, isMatch, re, Regex) where

import Data.Maybe (mapMaybe)
import Text.Regex.PCRE.Heavy

parseGroups :: Regex -> String -> [String]
parseGroups pattern input
  | null parsed = error ("Nothing parsed in '" ++ input ++ "'")
  | otherwise = snd . head $ parsed
  where
    parsed = scan pattern $ input

tryParseGroups :: Regex -> String -> Maybe [String]
tryParseGroups pattern input
  | null parsed = Nothing
  | otherwise = Just . snd . head $ parsed
  where
    parsed = scan pattern $ input

parseGroupsFirstMatch :: [Regex] -> String -> [String]
parseGroupsFirstMatch patterns input
  | null parsed = error ("Nothing parsed in '" ++ input ++ "'")
  | otherwise = head parsed
  where
    parsed = filter (not . null) . mapMaybe (`tryParseGroups` input) $ patterns

parseMatches :: Regex -> String -> [String]
parseMatches pattern input = map fst . scan pattern $ input

isMatch :: Regex -> String -> Bool
isMatch pattern input = not . null . scan pattern $ input
