module Parser.Core
  ( Parser(..)
  , runParser
  , satisfy
  , eof
  , label
  , try
  ) where

import Control.Applicative (Alternative(..))
import Parser.Error

newtype Parser a = Parser
  { unParser :: String -> Pos -> Either ParseError (a, String, Pos) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s pos ->
    case p s pos of
      Left err           -> Left err
      Right (a, rest, pos') -> Right (f a, rest, pos')

instance Applicative Parser where
  pure a = Parser $ \s pos -> Right (a, s, pos)
  Parser pf <*> Parser pa = Parser $ \s pos ->
    case pf s pos of
      Left err            -> Left err
      Right (f, s', pos') -> case pa s' pos' of
        Left err             -> Left err
        Right (a, s'', pos'') -> Right (f a, s'', pos'')

instance Monad Parser where
  Parser pa >>= f = Parser $ \s pos ->
    case pa s pos of
      Left err           -> Left err
      Right (a, s', pos') -> unParser (f a) s' pos'

instance Alternative Parser where
  empty = Parser $ \_ pos -> Left (ParseError pos [] "")
  Parser p1 <|> Parser p2 = Parser $ \s pos ->
    case p1 s pos of
      Right result -> Right result
      Left err1    -> case p2 s pos of
        Right result -> Right result
        Left err2    -> Left (mergeErrors err1 err2)

runParser :: Parser a -> String -> Either ParseError (a, String, Pos)
runParser (Parser p) s = p s initialPos

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy desc predicate = Parser $ \s pos ->
  case s of
    []                 -> Left (expectedErr pos desc)
    (c:cs) | predicate c -> Right (c, cs, advancePos pos c)
           | otherwise   -> Left (ParseError pos [desc] [c])

eof :: Parser ()
eof = Parser $ \s pos ->
  case s of
    [] -> Right ((), s, pos)
    (c:_) -> Left (ParseError pos ["end of input"] [c])

label :: String -> Parser a -> Parser a
label desc (Parser p) = Parser $ \s pos ->
  case p s pos of
    Left (ParseError pos' _ found) | pos' == pos ->
      Left (ParseError pos' [desc] found)
    result -> result

try :: Parser a -> Parser a
try (Parser p) = Parser $ \s pos ->
  case p s pos of
    Left (ParseError _ expected found) ->
      Left (ParseError pos expected found)
    right -> right
