module Day13.SignalPair (SignalPair, pair, parse, parseSignalPair, hasCorrectOrder) where

import Day13.Signal
import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
  )
import qualified Text.ParserCombinators.Parsec as Parsec
  ( char,
    parse,
  )

data SignalPair = SignalPair Signal Signal deriving (Eq, Show)

pair :: Signal -> Signal -> SignalPair
pair = SignalPair

hasCorrectOrder :: SignalPair -> Bool
hasCorrectOrder (SignalPair leftHand rightHand) = leftHand <= rightHand

-- ---------------------------------------------------------------------------------------------------------------------
-- Read instance
-- ---------------------------------------------------------------------------------------------------------------------

instance Read SignalPair where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = toSignalPair . parse $ input

      toSignalPair :: Either ParseError SignalPair -> SignalPair
      toSignalPair (Right signalPair) = signalPair
      toSignalPair (Left parseError) = error . show $ parseError

-- ---------------------------------------------------------------------------------------------------------------------
-- Parsec Signal Parser
-- ---------------------------------------------------------------------------------------------------------------------

parseSignalPair :: GenParser Char st SignalPair
parseSignalPair = do
  x <- parseGroup
  _ <- Parsec.char '\n'
  SignalPair x <$> parseGroup

parse :: String -> Either ParseError SignalPair
parse = Parsec.parse parseSignalPair "(unknown)"
