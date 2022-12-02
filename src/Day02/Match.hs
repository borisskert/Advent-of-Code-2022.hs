module Day02.Match (Match (Match), score, readOne, readMany, readManyDecrypted) where

import Day02.HandShape (HandShape (Paper, Rock, Scissors))
import qualified Day02.HandShape as HandShape (readLeft, readRight, score, with)
import qualified Day02.RoundResult as RoundResult (readOne)

data Match = Match HandShape HandShape deriving (Show, Eq)

readOne :: String -> Match
readOne s = Match leftHand rightHand
  where
    leftHand = HandShape.readLeft . head $ s
    rightHand = HandShape.readRight . last $ s

readMany :: String -> [Match]
readMany = map readOne . lines

score :: Match -> Int
score (Match Rock right@Paper) = 6 + HandShape.score right
score (Match Paper right@Rock) = 0 + HandShape.score right
score (Match Rock right@Scissors) = 0 + HandShape.score right
score (Match Scissors right@Rock) = 6 + HandShape.score right
score (Match Paper right@Scissors) = 6 + HandShape.score right
score (Match Scissors right@Paper) = 0 + HandShape.score right
score (Match _ right) = 3 + HandShape.score right

readOneDecrypted :: String -> Match
readOneDecrypted s = Match leftHand rightHand
  where
    leftHand = HandShape.readLeft . head $ s
    rightHand = (leftHand `HandShape.with`) . RoundResult.readOne . last $ s

readManyDecrypted :: String -> [Match]
readManyDecrypted = map readOneDecrypted . lines
