module Day02.RockPaperScissors (totalScore, totalScoreDecrypted) where

import Day02.Match (readMany, readManyDecrypted, score)

totalScore :: String -> Int
totalScore = sum . map score . readMany

totalScoreDecrypted :: String -> Int
totalScoreDecrypted = sum . map score . readManyDecrypted
