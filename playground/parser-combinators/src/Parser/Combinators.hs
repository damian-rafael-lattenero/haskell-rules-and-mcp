module Parser.Combinators
  ( choice
  , option
  , optional
  , between
  , sepBy
  , sepBy1
  , endBy
  , endBy1
  , count
  , chainl1
  , chainr1
  , manyTill
  , notFollowedBy
  , lookAhead
  , try
  ) where

import Control.Applicative (Alternative(..), (<|>))
import Parser.Core (Parser(..), ParseState(..), ParseResult(..))
import Parser.Error (ParseError(..))

-- | Try each parser in order, return the first success
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

-- | Try the parser; if it fails, return the default value
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- | Try the parser; return Nothing on failure, Just on success
optional :: Parser a -> Parser (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

-- | Parse something between an open and close delimiter
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- | Zero or more occurrences separated by sep
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | One or more occurrences separated by sep
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | Zero or more occurrences terminated by sep
endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p <* sep)

-- | One or more occurrences terminated by sep
endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 p sep = some (p <* sep)

-- | Exactly n occurrences
count :: Int -> Parser a -> Parser [a]
count n p
  | n <= 0    = pure []
  | otherwise = (:) <$> p <*> count (n - 1) p

-- | Left-associative chain: p `op` p `op` p ...
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- p; rest (f x y)) <|> pure x

-- | Right-associative chain: p `op` (p `op` p)
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- chainr1 p op; pure (f x y)) <|> pure x

-- | Parse many occurrences of p until end succeeds
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = (end *> pure []) <|> ((:) <$> p <*> manyTill p end)

-- | Succeed only if the given parser fails (doesn't consume input)
notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser $ \s -> case p s of
  Failure _  -> Success () s
  Success _ _ -> Failure (ParseError (statePos s) [] (Just "unexpected success"))

-- | Try parser without consuming input on success
lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \s -> case p s of
  Success a _ -> Success a s
  Failure e   -> Failure e

-- | Try parser, restoring state on failure (backtracking)
try :: Parser a -> Parser a
try (Parser p) = Parser $ \s -> case p s of
  Failure (ParseError _ expected msg) ->
    Failure (ParseError (statePos s) expected msg)
  success -> success
