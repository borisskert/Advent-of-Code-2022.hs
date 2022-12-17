{-# LANGUAGE QuasiQuotes #-}

module Day11.MonkeyId (MonkeyId, from) where

import Common.Regex

newtype MonkeyId = MonkeyId Int deriving (Eq, Show)

from :: Int -> MonkeyId
from = MonkeyId

instance Read MonkeyId where
  readsPrec _ input = [(readFrom input, [])]

headLinePattern :: Regex
headLinePattern = [re|Monkey ([0-9]+):|]

throwPattern :: Regex
throwPattern = [re|throw to monkey ([0-9]+)|]

readFrom :: String -> MonkeyId
readFrom input = MonkeyId . read . head . (`parseGroupsFirstMatch` input) $ [headLinePattern, throwPattern]
