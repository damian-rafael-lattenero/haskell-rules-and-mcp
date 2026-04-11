module Parser.Char
  ( letter
  , digit
  , alphaNum
  , upper
  , lower
  , space
  , spaces
  , lexeme
  , symbol
  , natural
  , integer
  , identifier
  , upperIdentifier
  , reserved
  , parens
  , comma
  , semicolon
  , operator
  ) where

import Data.Char (isAlpha, isDigit, isAlphaNum, isUpper, isLower, isSpace)
import Control.Applicative (Alternative(..))

import Parser.Core
import Parser.Combinators

-- | Parse a letter
letter :: Parser Char
letter = satisfy "letter" isAlpha

-- | Parse a digit
digit :: Parser Char
digit = satisfy "digit" isDigit

-- | Parse an alphanumeric character
alphaNum :: Parser Char
alphaNum = satisfy "alphanumeric" isAlphaNum

-- | Parse an uppercase letter
upper :: Parser Char
upper = satisfy "uppercase letter" isUpper

-- | Parse a lowercase letter
lower :: Parser Char
lower = satisfy "lowercase letter" isLower

-- | Parse a whitespace character
space :: Parser Char
space = satisfy "space" isSpace

-- | Consume zero or more spaces
spaces :: Parser ()
spaces = () <$ many space

-- | Consume trailing whitespace after a parser
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

-- | Parse a specific string token, consuming trailing whitespace
symbol :: String -> Parser String
symbol s = lexeme (string s)

-- | Parse a natural number (non-negative integer)
natural :: Parser Int
natural = lexeme $ do
  digits <- some digit
  pure (read digits)

-- | Parse an integer (possibly negative)
integer :: Parser Int
integer = lexeme $ do
  sign <- option id (negate <$ char '-')
  digits <- some digit
  pure (sign (read digits))

-- | Parse a lowercase identifier (starts with lowercase, then alphanumeric/underscore/prime)
identifier :: Parser String
identifier = lexeme $ do
  c <- lower
  cs <- many (alphaNum <|> char '_' <|> char '\'')
  pure (c : cs)

-- | Parse an uppercase identifier (starts with uppercase)
upperIdentifier :: Parser String
upperIdentifier = lexeme $ do
  c <- upper
  cs <- many (alphaNum <|> char '_' <|> char '\'')
  pure (c : cs)

-- | Parse a reserved keyword, ensuring it is not a prefix of an identifier
reserved :: String -> Parser ()
reserved kw = lexeme $ do
  _ <- string kw
  notFollowedBy (alphaNum <|> char '_' <|> char '\'')

-- | Parse something in parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse a comma token
comma :: Parser ()
comma = () <$ symbol ","

-- | Parse a semicolon token
semicolon :: Parser ()
semicolon = () <$ symbol ";"

-- | Parse an operator symbol (exact match, not prefix of longer operator)
-- For multi-char ops like "<=", ">=", "/=", "==", "&&", "||", "->"
operator :: String -> Parser String
operator op = lexeme $ do
  _ <- string op
  notFollowedBy (satisfy "operator char" isOpChar)
  pure op

-- | Characters that can appear in operators
isOpChar :: Char -> Bool
isOpChar c = c `elem` ("+-*/.=<>!&|" :: [Char])
