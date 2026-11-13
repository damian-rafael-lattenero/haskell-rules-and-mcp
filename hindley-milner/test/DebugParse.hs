#!/usr/bin/env cabal
{- cabal:
build-depends: base, megaparsec, text
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text, pack)
import Data.Void

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

pLit :: Parser Integer
pLit = lexeme L.decimal

pVar :: Parser Text
pVar = lexeme (some alphaNumChar)

pTerm :: Parser Text
pTerm = do
  r <- some (pLit <|> (pack <$> pVar))
  pure $ pack $ show r

pLet :: Parser Text
pLet = do
  _ <- symbol "let"
  name <- pVar
  _ <- symbol "="
  rhs <- pTerm
  _ <- symbol "in"
  body <- pTerm
  pure $ "Let " <> name <> " = " <> rhs <> " in " <> body

runMyParser :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
runMyParser p = parse (spaceConsumer *> p <* eof) "<input>"

main :: IO ()
main = do
  let input = pack "let x = 1 in x"
  case runMyParser pLet input of
    Left err -> putStrLn $ "FAIL: " ++ errorBundlePretty err
    Right r -> putStrLn $ "OK: " ++ show r