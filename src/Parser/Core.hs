module Parser.Core
  ( Parser(Parser)
  , ParseError(..)
  , runParser
  , parse
  , satisfy
  , char
  , string
  , eof
  , failWith
  , ppParseError
  ) where

import Control.Applicative (Alternative(..))

-- | Parse error with position info
data ParseError = ParseError
  { peExpected :: String
  , peFound    :: String
  , pePos      :: Int
  } deriving (Show, Eq)

-- | A parser consumes a String at a position and returns either
-- an error or a result with remaining input and new position.
newtype Parser a = Parser
  { runParser :: String -> Int -> Either ParseError (a, String, Int) }

-- | Run a parser on input and extract just the result
parse :: Parser a -> String -> Either ParseError a
parse p input = case runParser p input 0 of
  Left err        -> Left err
  Right (a, _, _) -> Right a

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input pos ->
    case p input pos of
      Left err              -> Left err
      Right (a, rest, pos') -> Right (f a, rest, pos')

instance Applicative Parser where
  pure a = Parser $ \input pos -> Right (a, input, pos)
  Parser pf <*> Parser pa = Parser $ \input pos ->
    case pf input pos of
      Left err               -> Left err
      Right (f, rest, pos')  -> case pa rest pos' of
        Left err                -> Left err
        Right (a, rest', pos'') -> Right (f a, rest', pos'')

instance Monad Parser where
  Parser pa >>= f = Parser $ \input pos ->
    case pa input pos of
      Left err              -> Left err
      Right (a, rest, pos') -> runParser (f a) rest pos'

instance Alternative Parser where
  empty = Parser $ \_ pos ->
    Left (ParseError "something" "end of alternatives" pos)
  Parser p1 <|> Parser p2 = Parser $ \input pos ->
    case p1 input pos of
      Right result -> Right result
      Left err1    -> case p2 input pos of
        Right result -> Right result
        Left err2    -> Left (furthest err1 err2)

-- | Parse a single character satisfying a predicate
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy desc predicate = Parser $ \input pos ->
  case input of
    (c:cs) | predicate c -> Right (c, cs, pos + 1)
    (c:_)                -> Left (ParseError desc [c] pos)
    []                   -> Left (ParseError desc "end of input" pos)

-- | Parse a specific character
char :: Char -> Parser Char
char c = satisfy [c] (== c)

-- | Parse a specific string
string :: String -> Parser String
string []     = pure []
string (c:cs) = (:) <$> char c <*> string cs

-- | Succeed only at end of input
eof :: Parser ()
eof = Parser $ \input pos ->
  case input of
    []    -> Right ((), "", pos)
    (c:_) -> Left (ParseError "end of input" [c] pos)

-- | Fail with a custom message
failWith :: String -> Parser a
failWith msg = Parser $ \_ pos ->
  Left (ParseError msg "parse failure" pos)

-- | Pick the error that got furthest in the input
furthest :: ParseError -> ParseError -> ParseError
furthest e1 e2
  | pePos e1 >= pePos e2 = e1
  | otherwise            = e2

-- | Pretty-print a parse error
ppParseError :: ParseError -> String
ppParseError (ParseError expected found pos)
  | found == "parse failure" =
      "parse error at position " ++ show pos ++ ": " ++ expected
  | otherwise =
      "parse error at position " ++ show pos
        ++ ": expected " ++ expected
        ++ ", found " ++ found
