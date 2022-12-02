module Day02.RockPaperScissors (totalScore) where

import Day02.Match (readMany, score)

totalScore :: String -> Int
totalScore = sum . map score . readMany
