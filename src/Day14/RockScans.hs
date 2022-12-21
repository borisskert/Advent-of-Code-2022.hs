module Day14.RockScans (RockScans, fromList, toList) where

import Common.Read (readBy)
import Control.Monad (void)
import Day14.RockScan (RockScan)
import qualified Day14.RockScan as RockScan (parseOne)
import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
  )
import qualified Text.ParserCombinators.Parsec as Parsec
  ( char,
    parse,
    sepEndBy,
  )

newtype RockScans = RockScans [RockScan] deriving (Show, Eq)

fromList :: [RockScan] -> RockScans
fromList = RockScans

toList :: RockScans -> [RockScan]
toList (RockScans scans) = scans

-- | -------------------------------------------------------------------------------------------------------------------
-- | Read instance
-- | -------------------------------------------------------------------------------------------------------------------
instance Read RockScans where
  readsPrec _ = readBy parse

-- | -------------------------------------------------------------------------------------------------------------------
-- | Parsec RockScans Parser
-- | -------------------------------------------------------------------------------------------------------------------
parse :: String -> Either ParseError RockScans
parse = Parsec.parse parseRockScans "(ParseError while parsing RockScan)"

separator :: GenParser Char st ()
separator = void (Parsec.char '\n')

manyRockScans :: GenParser Char st [RockScan]
manyRockScans = Parsec.sepEndBy RockScan.parseOne separator

parseRockScans :: GenParser Char st RockScans
parseRockScans = RockScans <$> manyRockScans
