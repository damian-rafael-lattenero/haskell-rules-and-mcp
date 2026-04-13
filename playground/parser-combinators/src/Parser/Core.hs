module Parser.Core
  ( Parser(..)
  , ParseState(..)
  , mkState
  , unexpectedError
  , expectedError
  ) where

import Control.Applicative (Alternative(..))
import Parser.Error

-- | Internal parser state
data ParseState = ParseState
  { stateInput :: String
  , statePos   :: SourcePos
  } deriving (Show, Eq)

-- | The core parser type
newtype Parser a = Parser
  { unParser :: ParseState -> Either ParseError (a, ParseState)
  }

mkState :: String -> ParseState
mkState input = ParseState input initialPos

unexpectedError :: ParseState -> ParseError
unexpectedError st = case stateInput st of
  []    -> ParseError (statePos st) [] Nothing
  (c:_) -> ParseError (statePos st) [] (Just c)

expectedError :: String -> ParseState -> ParseError
expectedError label st = case stateInput st of
  []    -> ParseError (statePos st) [label] Nothing
  (c:_) -> ParseError (statePos st) [label] (Just c)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \st ->
    case p st of
      Left err       -> Left err
      Right (a, st') -> Right (f a, st')

instance Applicative Parser where
  pure a = Parser $ \st -> Right (a, st)
  (Parser pf) <*> (Parser pa) = Parser $ \st ->
    case pf st of
      Left err       -> Left err
      Right (f, st') -> case pa st' of
        Left err        -> Left err
        Right (a, st'') -> Right (f a, st'')

instance Monad Parser where
  (Parser pa) >>= f = Parser $ \st ->
    case pa st of
      Left err       -> Left err
      Right (a, st') -> unParser (f a) st'

instance Alternative Parser where
  empty = Parser $ \st -> Left (unexpectedError st)
  (Parser p1) <|> (Parser p2) = Parser $ \st ->
    case p1 st of
      Right result -> Right result
      Left err1    -> case p2 st of
        Right result -> Right result
        Left err2    -> Left (mergeErrors err1 err2)
