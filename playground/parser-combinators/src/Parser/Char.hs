module Parser.Char
  ( char
  , anyChar
  , string
  , oneOf
  , noneOf
  , digit
  , letter
  , alphaNum
  , space
  , spaces
  , newline
  , upper
  , lower
  ) where

import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace, isUpper, isLower)
import Control.Applicative (Alternative(..))
import Parser.Core (Parser, satisfy, label)

-- | Match a specific character
char :: Char -> Parser Char
char c = label [c] $ satisfy (== c)

-- | Match any single character
anyChar :: Parser Char
anyChar = label "any character" $ satisfy (const True)

-- | Match an exact string
string :: String -> Parser String
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs

-- | Match any character in the list
oneOf :: [Char] -> Parser Char
oneOf cs = label ("one of " ++ show cs) $ satisfy (`elem` cs)

-- | Match any character NOT in the list
noneOf :: [Char] -> Parser Char
noneOf cs = label ("none of " ++ show cs) $ satisfy (`notElem` cs)

-- | Match a digit character [0-9]
digit :: Parser Char
digit = label "digit" $ satisfy isDigit

-- | Match a letter [a-zA-Z]
letter :: Parser Char
letter = label "letter" $ satisfy isAlpha

-- | Match alphanumeric character
alphaNum :: Parser Char
alphaNum = label "alphanumeric" $ satisfy isAlphaNum

-- | Match a single whitespace character
space :: Parser Char
space = label "space" $ satisfy isSpace

-- | Skip zero or more whitespace characters
spaces :: Parser String
spaces = many space

-- | Match a newline character
newline :: Parser Char
newline = label "newline" $ char '\n'

-- | Match an uppercase letter
upper :: Parser Char
upper = label "uppercase letter" $ satisfy isUpper

-- | Match a lowercase letter
lower :: Parser Char
lower = label "lowercase letter" $ satisfy isLower
