module Parser.Char
  ( char
  , anyChar
  , string
  , digit
  , letter
  , upper
  , lower
  , alphaNum
  , space
  , spaces
  , newline
  , oneOf
  , noneOf
  ) where

import Data.Char (isDigit, isAlpha, isUpper, isLower, isAlphaNum, isSpace)
import Control.Applicative (many)
import Parser.Core (Parser, satisfy, label)

-- | Match a specific character
char :: Char -> Parser Char
char c = satisfy [c] (== c)

-- | Match any single character
anyChar :: Parser Char
anyChar = satisfy "any character" (const True)

-- | Match an exact string
string :: String -> Parser String
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs

-- | Match a decimal digit
digit :: Parser Char
digit = label "digit" $ satisfy "digit" isDigit

-- | Match an alphabetic character
letter :: Parser Char
letter = label "letter" $ satisfy "letter" isAlpha

-- | Match an uppercase letter
upper :: Parser Char
upper = label "uppercase" $ satisfy "uppercase" isUpper

-- | Match a lowercase letter
lower :: Parser Char
lower = label "lowercase" $ satisfy "lowercase" isLower

-- | Match an alphanumeric character
alphaNum :: Parser Char
alphaNum = label "alphanumeric" $ satisfy "alphanumeric" isAlphaNum

-- | Match a single whitespace character
space :: Parser Char
space = label "space" $ satisfy "space" isSpace

-- | Skip zero or more whitespace characters
spaces :: Parser String
spaces = many space

-- | Match a newline character
newline :: Parser Char
newline = char '\n'

-- | Match one of the given characters
oneOf :: [Char] -> Parser Char
oneOf cs = satisfy ("one of " ++ show cs) (`elem` cs)

-- | Match any character not in the given list
noneOf :: [Char] -> Parser Char
noneOf cs = satisfy ("none of " ++ show cs) (`notElem` cs)
