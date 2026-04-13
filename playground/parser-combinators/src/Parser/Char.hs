module Parser.Char
  ( satisfy
  , char
  , anyChar
  , string
  , digit
  , letter
  , alphaNum
  , space
  , spaces
  , newline
  , oneOf
  , noneOf
  ) where

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Control.Applicative (Alternative(..))
import Parser.Core
import Parser.Error

-- | The fundamental character parser — all others build on this
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \st ->
  case stateInput st of
    []     -> Left (unexpectedError st)
    (c:cs)
      | predicate c -> Right (c, ParseState cs (updatePos c (statePos st)))
      | otherwise   -> Left (unexpectedError st)

char :: Char -> Parser Char
char c = satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy (const True)

string :: String -> Parser String
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs

digit :: Parser Char
digit = satisfy isDigit

letter :: Parser Char
letter = satisfy isAlpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many space

newline :: Parser Char
newline = char '\n'

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (`notElem` cs)
