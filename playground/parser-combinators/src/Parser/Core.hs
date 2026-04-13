module Parser.Core
  ( Parser(..)
  , ParseState(..)
  , ParseResult(..)
  , mkState
  , satisfy
  , eof
  , failWith
  , label
  ) where

import Control.Applicative (Alternative(..))
import Parser.Error

-- | The parser state: remaining input + current position
data ParseState = ParseState
  { stateInput :: !String
  , statePos   :: !Pos
  } deriving (Show, Eq)

-- | Result of a parse attempt
data ParseResult a
  = Success a ParseState
  | Failure ParseError
  deriving (Show)

-- | A parser: takes state, returns result
newtype Parser a = Parser { runParser :: ParseState -> ParseResult a }

-- | Create initial parse state from input string
mkState :: String -> ParseState
mkState input = ParseState input initialPos

-- Functor

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> case p s of
    Success a s' -> Success (f a) s'
    Failure e    -> Failure e

-- Applicative

instance Applicative Parser where
  pure a = Parser $ \s -> Success a s
  (Parser pf) <*> (Parser pa) = Parser $ \s -> case pf s of
    Failure e    -> Failure e
    Success f s' -> case pa s' of
      Failure e     -> Failure e
      Success a s'' -> Success (f a) s''

-- Monad

instance Monad Parser where
  (Parser pa) >>= f = Parser $ \s -> case pa s of
    Failure e    -> Failure e
    Success a s' -> runParser (f a) s'

-- Alternative (backtracking choice + many/some)

instance Alternative Parser where
  empty = Parser $ \s -> Failure (ParseError (statePos s) [] Nothing)
  (Parser p1) <|> (Parser p2) = Parser $ \s -> case p1 s of
    Success a s' -> Success a s'
    Failure e1   -> case p2 s of
      Success a s' -> Success a s'
      Failure e2   -> Failure (mergeError e1 e2)

-- | The fundamental parser: consume one char if predicate holds
satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy desc predicate = Parser $ \(ParseState input pos) -> case input of
  (c:cs) | predicate c ->
    Success c (ParseState cs (advancePos pos c))
  _ -> Failure (ParseError pos [ExpectedSatisfy desc] Nothing)

-- | Succeed only at end of input
eof :: Parser ()
eof = Parser $ \(ParseState input pos) -> case input of
  [] -> Success () (ParseState [] pos)
  _  -> Failure (ParseError pos [ExpectedEOF] Nothing)

-- | Fail with a custom message
failWith :: String -> Parser a
failWith msg = Parser $ \s ->
  Failure (ParseError (statePos s) [] (Just msg))

-- | Attach a label to a parser (replaces expected on failure)
label :: String -> Parser a -> Parser a
label desc (Parser p) = Parser $ \s -> case p s of
  Success a s' -> Success a s'
  Failure (ParseError pos _ msg) ->
    Failure (ParseError pos [ExpectedSatisfy desc] msg)
