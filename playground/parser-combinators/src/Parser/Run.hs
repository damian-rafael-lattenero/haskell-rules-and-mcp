module Parser.Run
  ( parse
  , parseComplete
  , parseTest
  ) where

import Parser.Core (Parser(..), Result(..), mkState, eof)
import Parser.Error (formatError)

-- | Run a parser on a string, return Either with formatted error
parse :: Parser a -> String -> Either String a
parse p input = case runParser p (mkState input) of
  Success a _ -> Right a
  Failure e   -> Left (formatError e)

-- | Run a parser and require all input to be consumed
parseComplete :: Parser a -> String -> Either String a
parseComplete p input = case runParser (p <* eof) (mkState input) of
  Success a _ -> Right a
  Failure e   -> Left (formatError e)

-- | Run a parser and print the result (for REPL use)
parseTest :: Show a => Parser a -> String -> String
parseTest p input = case parse p input of
  Right a  -> "OK: " ++ show a
  Left err -> "ERROR:\n" ++ err
