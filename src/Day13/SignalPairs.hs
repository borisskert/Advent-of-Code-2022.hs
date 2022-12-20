module Day13.SignalPairs (SignalPairs, fromList, toList) where

import Day13.SignalPair (SignalPair, parseSignalPair)
import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
  )
import qualified Text.ParserCombinators.Parsec as Parsec
  ( parse,
    sepBy,
    string,
  )

newtype SignalPairs = SignalPairs [SignalPair] deriving (Eq, Show)

fromList :: [SignalPair] -> SignalPairs
fromList = SignalPairs

toList :: SignalPairs -> [SignalPair]
toList (SignalPairs pairs) = pairs

-- ---------------------------------------------------------------------------------------------------------------------
-- Read instance
-- ---------------------------------------------------------------------------------------------------------------------

instance Read SignalPairs where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = toSignalPairs . parse $ input

      toSignalPairs :: Either ParseError SignalPairs -> SignalPairs
      toSignalPairs (Right signalPairs) = signalPairs
      toSignalPairs (Left parseError) = error . show $ parseError

-- ---------------------------------------------------------------------------------------------------------------------
-- Parsec SignalPairs Parser
-- ---------------------------------------------------------------------------------------------------------------------

parseSignalPairs :: GenParser Char st SignalPairs
parseSignalPairs = SignalPairs <$> Parsec.sepBy parseSignalPair (Parsec.string "\n\n")

parse :: String -> Either ParseError SignalPairs
parse = Parsec.parse parseSignalPairs "(unknown)"
