module Parser.Run
  ( parse
  , parseAll
  , parseTest
  ) where

import Parser.Error (ParseError, formatError)
import Parser.Core (Parser, runParser, eof)

parse :: Parser a -> String -> Either ParseError a
parse p s = fmap (\(a, _, _) -> a) (runParser p s)

parseAll :: Parser a -> String -> Either ParseError a
parseAll p s = parse (p <* eof) s

parseTest :: Show a => Parser a -> String -> IO ()
parseTest p s =
  case parse p s of
    Left err -> putStrLn (formatError err)
    Right a  -> print a
