module Day13.Signal (Signal, group, value) where

import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
    between,
    char,
    digit,
    many1,
    parse,
    sepBy,
    (<|>),
  )

-- ---------------------------------------------------------------------------------------------------------------------
-- Signal data structure
-- ---------------------------------------------------------------------------------------------------------------------

data Signal = Value Int | Group [Signal] deriving (Eq, Show)

instance Read Signal where
  readsPrec _ input = [(parsed, [])]
    where
      parsed = toSignal . parseSignal $ input

      toSignal :: Either ParseError Signal -> Signal
      toSignal (Right signal) = signal
      toSignal (Left parseError) = error . show $ parseError

group :: [Signal] -> Signal
group = Group

value :: Int -> Signal
value = Value

-- ---------------------------------------------------------------------------------------------------------------------
-- Parsec Signal Parser
-- ---------------------------------------------------------------------------------------------------------------------

parseInt :: GenParser Char st Int
parseInt = read <$> many1 digit

parseValue :: GenParser Char st Signal
parseValue = Value <$> parseInt

parseSignals :: GenParser Char st [Signal]
parseSignals = sepBy parseGroupOrValue (char ',')

parseGroupOrValue :: GenParser Char st Signal
parseGroupOrValue = parseValue <|> parseGroup

parseGroupContent :: GenParser Char st [Signal]
parseGroupContent = parseSignals <|> return []

parseGroup :: GenParser Char st Signal
parseGroup = Group <$> between (char '[') (char ']') parseGroupContent

parseSignal :: String -> Either ParseError Signal
parseSignal = parse parseGroup "(unknown)"
