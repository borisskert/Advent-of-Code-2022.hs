module Day15.SensorAndBeacon (BeaconAndSensor, from, readMany) where

import Common.OctaGridPosition (Position)
import qualified Common.OctaGridPosition as Position (from)
import Common.Read (readBy)
import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
  )
import qualified Text.ParserCombinators.Parsec as Parsec
  ( char,
    digit,
    many1,
    option,
    parse,
    sepBy1,
    string,
  )

data BeaconAndSensor = BeaconAndSensor Position Position deriving (Eq, Show)

from :: Position -> Position -> BeaconAndSensor
from = BeaconAndSensor

-- | -------------------------------------------------------------------------------------------------------------------
-- | Read instance
-- | -------------------------------------------------------------------------------------------------------------------

instance Read BeaconAndSensor where
  readsPrec _ = readBy parse

readMany :: String -> [BeaconAndSensor]
readMany = map read . lines

-- | -------------------------------------------------------------------------------------------------------------------
-- | BeaconAndSensor Parser
-- | -------------------------------------------------------------------------------------------------------------------
parse :: String -> Either ParseError BeaconAndSensor
parse = Parsec.parse parseBeaconAndSensor "(ParseError while parsing BeaconAndSensor)"

parseBeaconAndSensor :: GenParser Char st BeaconAndSensor
parseBeaconAndSensor = do
  _ <- Parsec.string "Sensor at x="
  x <- signedInt
  _ <- Parsec.string ", y="
  y <- signedInt
  _ <- Parsec.string ": closest beacon is at x="
  x' <- signedInt
  _ <- Parsec.string ", y="
  BeaconAndSensor (Position.from x y) . Position.from x' <$> signedInt

signedInt :: GenParser Char st Int
signedInt = do
  signum' <- Parsec.option 1 sign
  digits <- Parsec.many1 Parsec.digit
  return (read digits * signum')

sign :: GenParser Char st Int
sign = do
  _ <- Parsec.char '-'
  return (-1)
