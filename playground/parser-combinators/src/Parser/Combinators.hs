module Parser.Combinators
  ( many1
  , choice
  , option
  , optional_
  , between
  , sepBy
  , sepBy1
  , endBy
  , count
  , chainl1
  , chainr1
  , notFollowedBy
  , lookAhead
  , manyTill
  ) where

import Control.Applicative (Alternative(..))
import Parser.Core (Parser(..), Result(..), State(..))
import Parser.Error (ParseError(..))

-- | One or more occurrences
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- | Try each parser in order, return first success
choice :: [Parser a] -> Parser a
choice = foldl (<|>) empty

-- | Parse with a default value on failure
option :: a -> Parser a -> Parser a
option def p = p <|> pure def

-- | Optional parser, discard result
optional_ :: Parser a -> Parser ()
optional_ p = (() <$ p) <|> pure ()

-- | Parse something between open and close delimiters
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- | Zero or more separated values
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | One or more separated values
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | Zero or more values each followed by separator
endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p <* sep)

-- | Exactly n occurrences
count :: Int -> Parser a -> Parser [a]
count n p
  | n <= 0    = pure []
  | otherwise = (:) <$> p <*> count (n - 1) p

-- | Left-associative operator chain: expr (op expr)*
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y))
             <|> pure x

-- | Right-associative operator chain: expr op (expr op ...)*
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- chainr1 p op
                 pure (f x y))
             <|> pure x

-- | Succeed only if the parser fails (doesn't consume input)
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = Parser $ \s -> case runParser p s of
  Success _ _ -> Failure (errorAt s)
  Failure _   -> Success () s
  where
    errorAt st = ParseError (statePos st) [] "unexpected match"

-- | Apply parser without consuming input
lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \s -> case runParser p s of
  Success a _ -> Success a s
  failure     -> failure

-- | Parse until end parser succeeds
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = (end *> pure []) <|> ((:) <$> p <*> manyTill p end)
