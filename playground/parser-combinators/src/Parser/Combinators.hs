module Parser.Combinators
  ( choice
  , option
  , between
  , many1
  , sepBy
  , sepBy1
  , endBy
  , endBy1
  , chainl1
  , chainr1
  , count
  , lookAhead
  , notFollowedBy
  ) where

import Control.Applicative (Alternative(..))
import Parser.Core (Parser(..))
import Parser.Error (ParseError(..))

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

option :: a -> Parser a -> Parser a
option x p = p <|> pure x

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p <* sep)

endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 p sep = many1 (p <* sep)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- p
                 rest (f x y))
             <|> pure x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= rest
  where
    rest x = (do f <- op
                 y <- chainr1 p op
                 pure (f x y))
             <|> pure x

count :: Int -> Parser a -> Parser [a]
count n p
  | n <= 0    = pure []
  | otherwise = (:) <$> p <*> count (n - 1) p

lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \s pos ->
  case p s pos of
    Left err        -> Left err
    Right (a, _, _) -> Right (a, s, pos)

notFollowedBy :: Parser a -> Parser ()
notFollowedBy (Parser p) = Parser $ \s pos ->
  case p s pos of
    Left _  -> Right ((), s, pos)
    Right _ -> Left (ParseError pos ["not followed by"] "")
