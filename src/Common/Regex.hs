module Common.Regex (parseGroups, re) where

import Text.Regex.PCRE.Heavy

parseGroups :: Regex -> String -> [String]
parseGroups pattern input = snd . head . scan pattern $ input
