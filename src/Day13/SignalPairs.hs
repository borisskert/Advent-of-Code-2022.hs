module Day13.SignalPairs (SignalPairs, fromList, toList) where

import Day13.SignalPair (SignalPair, parseSignalPair)
import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
    (<|>),
  )
import qualified Text.ParserCombinators.Parsec as Parsec
  ( char,
    choice,
    eof,
    many,
    optional,
    parse,
    string,
    try,
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

parseEnd :: GenParser Char st ()
parseEnd = do
  _ <- Parsec.optional $ Parsec.char '\n'
  _ <- Parsec.eof
  return ()

parseSeparator :: GenParser Char st ()
parseSeparator = do
  _ <- Parsec.string "\n\n"
  return ()

parseSignalPairList :: GenParser Char st [SignalPair]
parseSignalPairList = Parsec.many $ do
  x <- parseSignalPair
  Parsec.choice [Parsec.try parseSeparator, parseEnd]
  return x

parseSignalPairs :: GenParser Char st SignalPairs
parseSignalPairs = SignalPairs <$> parseSignalPairList

parseSignalPairsOrEmpty :: GenParser Char st SignalPairs
parseSignalPairsOrEmpty = parseSignalPairs <|> return (SignalPairs [])

parse :: String -> Either ParseError SignalPairs
parse = Parsec.parse parseSignalPairsOrEmpty "(ParseError while parsing SignalPairs)"
