module Day15.SensorAndBeacon (BeaconAndSensorReport, from, readMany) where

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

data BeaconAndSensorReport = BeaconAndSensorReport Position Position deriving (Eq, Show)

from :: Position -> Position -> BeaconAndSensorReport
from = BeaconAndSensorReport

-- | -------------------------------------------------------------------------------------------------------------------
-- | Read instance
-- | -------------------------------------------------------------------------------------------------------------------

instance Read BeaconAndSensorReport where
  readsPrec _ = readBy parse

readMany :: String -> [BeaconAndSensorReport]
readMany = map read . lines

-- | -------------------------------------------------------------------------------------------------------------------
-- | BeaconAndSensor Parser
-- | -------------------------------------------------------------------------------------------------------------------
parse :: String -> Either ParseError BeaconAndSensorReport
parse = Parsec.parse parseBeaconAndSensor "(ParseError while parsing BeaconAndSensor)"

parseBeaconAndSensor :: GenParser Char st BeaconAndSensorReport
parseBeaconAndSensor = do
  _ <- Parsec.string "Sensor at x="
  x <- signedInt
  _ <- Parsec.string ", y="
  y <- signedInt
  _ <- Parsec.string ": closest beacon is at x="
  x' <- signedInt
  _ <- Parsec.string ", y="
  BeaconAndSensorReport (Position.from x y) . Position.from x' <$> signedInt

signedInt :: GenParser Char st Int
signedInt = do
  signum' <- Parsec.option 1 sign
  digits <- Parsec.many1 Parsec.digit
  return (read digits * signum')

sign :: GenParser Char st Int
sign = do
  _ <- Parsec.char '-'
  return (-1)
