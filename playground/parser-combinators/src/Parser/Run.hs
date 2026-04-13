module Parser.Run
  ( parse
  , parseWith
  , parseTest
  ) where

import Parser.Core
import Parser.Error

-- | Run a parser on a string, returning either an error or the result
parse :: Parser a -> String -> Either ParseError a
parse p input = fmap fst (unParser p (mkState input))

-- | Run a parser with explicit state, returning result and remaining state
parseWith :: Parser a -> ParseState -> Either ParseError (a, ParseState)
parseWith = unParser

-- | Run a parser and return a human-readable result string
parseTest :: Show a => Parser a -> String -> String
parseTest p input =
  case parse p input of
    Left err -> showError err
    Right a  -> show a
