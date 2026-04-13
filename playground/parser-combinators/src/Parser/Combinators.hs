module Parser.Combinators
  ( try
  , option
  , between
  , many1
  , sepBy
  , sepBy1
  , choice
  , count
  , chainl1
  , chainr1
  , eof
  , notFollowedBy
  , lookAhead
  , manyTill
  ) where

import Control.Applicative (Alternative(..))
import Parser.Core
import Parser.Error

-- | Try a parser; on failure, reset state (backtracking)
try :: Parser a -> Parser a
try (Parser p) = Parser $ \st ->
  case p st of
    Left (ParseError _ expected found) ->
      Left (ParseError (statePos st) expected found)
    right -> right

-- | Try parser, return default on failure
option :: a -> Parser a -> Parser a
option def p = p <|> pure def

-- | Parse something between open and close delimiters
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- | One or more
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- | Zero or more separated by separator
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | One or more separated by separator
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | Try each parser in order
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

-- | Exactly n repetitions
count :: Int -> Parser a -> Parser [a]
count n p
  | n <= 0    = pure []
  | otherwise = (:) <$> p <*> count (n - 1) p

-- | Left-associative chain: expr op expr op expr -> ((expr op expr) op expr)
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest a = (do f <- op
                 b <- p
                 rest (f a b))
             <|> pure a

-- | Right-associative chain: expr op expr op expr -> (expr op (expr op expr))
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest a = (do f <- op
                 b <- chainr1 p op
                 pure (f a b))
             <|> pure a

-- | Succeed only at end of input
eof :: Parser ()
eof = Parser $ \st ->
  case stateInput st of
    [] -> Right ((), st)
    _  -> Left (expectedError "end of input" st)

-- | Succeed only if the parser fails (consumes no input)
notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser $ \st ->
  case p st of
    Left _  -> Right ((), st)
    Right _ -> Left (expectedError "not followed by" st)

-- | Run parser without consuming input
lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \st ->
  case p st of
    Left err     -> Left err
    Right (a, _) -> Right (a, st)

-- | Parse items until end parser succeeds
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = (end *> pure []) <|> ((:) <$> p <*> manyTill p end)
