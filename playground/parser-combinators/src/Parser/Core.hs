module Parser.Core
  ( Parser(..)
  , State(..)
  , Result(..)
  , mkState
  , satisfy
  , eof
  , label
  , try
  ) where

import Control.Applicative (Alternative(..))
import Parser.Error (Pos(..), ParseError(..), initialPos, updatePos)

-- | Parser state: remaining input + current position
data State = State
  { stateInput :: String
  , statePos   :: !Pos
  } deriving (Show, Eq)

-- | Parse result: success with value and new state, or failure with error
data Result a
  = Success a State
  | Failure ParseError
  deriving (Show)

-- | A parser is a function from state to result
newtype Parser a = Parser { runParser :: State -> Result a }

-- | Create initial parser state from input string
mkState :: String -> State
mkState input = State input initialPos

-- | Parse a character satisfying a predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \(State input pos) ->
  case input of
    []                  -> Failure (ParseError pos [] "end of input")
    (c:cs) | predicate c -> Success c (State cs (updatePos pos c))
           | otherwise   -> Failure (ParseError pos [] [c])

-- | Succeed only at end of input
eof :: Parser ()
eof = Parser $ \(State input pos) ->
  case input of
    [] -> Success () (State [] pos)
    (c:_) -> Failure (ParseError pos ["end of input"] [c])

-- | Label a parser for better error messages
label :: String -> Parser a -> Parser a
label expected p = Parser $ \s -> case runParser p s of
  Failure (ParseError pos _ found) -> Failure (ParseError pos [expected] found)
  success                          -> success

-- | Try a parser, restoring state on failure (backtracking)
try :: Parser a -> Parser a
try p = Parser $ \s -> case runParser p s of
  Failure e -> Failure (e { errorPos = statePos s })
  success   -> success

instance Functor Parser where
  fmap f p = Parser $ \s -> case runParser p s of
    Success a s' -> Success (f a) s'
    Failure e    -> Failure e

instance Applicative Parser where
  pure a = Parser $ \s -> Success a s
  pf <*> pa = Parser $ \s -> case runParser pf s of
    Failure e   -> Failure e
    Success f s' -> case runParser pa s' of
      Failure e    -> Failure e
      Success a s'' -> Success (f a) s''

instance Monad Parser where
  pa >>= f = Parser $ \s -> case runParser pa s of
    Failure e    -> Failure e
    Success a s' -> runParser (f a) s'

instance Alternative Parser where
  empty = Parser $ \s -> Failure (ParseError (statePos s) [] "")
  p1 <|> p2 = Parser $ \s -> case runParser p1 s of
    Failure _ -> runParser p2 s
    success   -> success
