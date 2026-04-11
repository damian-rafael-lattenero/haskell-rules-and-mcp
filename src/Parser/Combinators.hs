module Parser.Combinators
  ( sepBy
  , sepBy1
  , between
  , chainl1
  , chainr1
  , option
  , notFollowedBy
  ) where

import Control.Applicative (Alternative(..))

import Parser.Core

-- | Parse one or more occurrences separated by a separator
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | Parse zero or more occurrences separated by a separator
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | Parse something between an opening and closing parser
between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

-- | Left-associative chain: parse one or more items connected by operators
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- p; rest (f x y)) <|> pure x

-- | Right-associative chain: parse one or more items connected by operators
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do f <- op; y <- chainr1 p op; pure (f x y)) <|> pure x

-- | Try a parser, returning a default value on failure
option :: a -> Parser a -> Parser a
option def p = p <|> pure def

-- | Succeed if the given parser fails (without consuming input)
notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser $ \input pos ->
  case p input pos of
    Left _  -> Right ((), input, pos)
    Right _ -> Left (ParseError "not followed by" "unexpected match" pos)
