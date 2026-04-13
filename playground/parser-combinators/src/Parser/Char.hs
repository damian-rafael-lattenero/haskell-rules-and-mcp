module Parser.Char
  ( char
  , string
  , anyChar
  , digit
  , letter
  , upper
  , lower
  , alphaNum
  , space
  , spaces
  , oneOf
  , noneOf
  , newline
  ) where

import Data.Char (isDigit, isAlpha, isUpper, isLower, isAlphaNum, isSpace)
import Control.Applicative (Alternative(..))
import Parser.Core (Parser, satisfy, label)

char :: Char -> Parser Char
char c = satisfy [c] (== c)

string :: String -> Parser String
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs

anyChar :: Parser Char
anyChar = satisfy "any character" (const True)

digit :: Parser Char
digit = label "digit" $ satisfy "digit" isDigit

letter :: Parser Char
letter = label "letter" $ satisfy "letter" isAlpha

upper :: Parser Char
upper = label "uppercase letter" $ satisfy "uppercase letter" isUpper

lower :: Parser Char
lower = label "lowercase letter" $ satisfy "lowercase letter" isLower

alphaNum :: Parser Char
alphaNum = label "alphanumeric" $ satisfy "alphanumeric" isAlphaNum

space :: Parser Char
space = label "space" $ satisfy "space" isSpace

spaces :: Parser String
spaces = many space

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy ("one of " ++ show cs) (`elem` cs)

noneOf :: [Char] -> Parser Char
noneOf cs = satisfy ("none of " ++ show cs) (`notElem` cs)

newline :: Parser Char
newline = label "newline" $ char '\n'
