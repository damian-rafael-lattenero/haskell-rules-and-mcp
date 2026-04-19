{- |
Module      : Expr.Pretty
Description : Pretty-printer + total parser. Fully parenthesized output
so the grammar has no precedence ambiguity. `readMaybe` for integer
conversion, `eof` rejects trailing garbage, every failure path returns
`Nothing`.
-}
module Expr.Pretty (
    pretty,
    parse,
) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Functor (($>))
import Text.ParserCombinators.ReadP (
    ReadP,
    between,
    char,
    eof,
    munch,
    munch1,
    pfail,
    readP_to_S,
    satisfy,
    skipSpaces,
    (+++),
 )
import Text.Read (readMaybe)

import Expr.Syntax (Expr (..))

pretty :: Expr -> String
pretty (Lit n) = show n
pretty (Var x) = x
pretty (Neg e) = "-(" <> pretty e <> ")"
pretty (Add a b) = "(" <> pretty a <> " + " <> pretty b <> ")"
pretty (Mul a b) = "(" <> pretty a <> " * " <> pretty b <> ")"

parse :: String -> Maybe Expr
parse input = case readP_to_S (skipSpaces *> exprP <* skipSpaces <* eof) input of
    [(e, "")] -> Just e
    _ -> Nothing

exprP :: ReadP Expr
exprP = litP +++ negP +++ binOpP +++ varP

litP :: ReadP Expr
litP = do
    sign <- (char '-' $> negate) +++ pure id
    digits <- munch1 isDigit
    case readMaybe digits of
        Just n -> pure (Lit (sign n))
        Nothing -> pfail

negP :: ReadP Expr
negP = do
    _ <- char '-'
    e <- between (char '(') (char ')') (skipSpaces *> exprP <* skipSpaces)
    pure (Neg e)

binOpP :: ReadP Expr
binOpP = between (char '(') (char ')') $ do
    skipSpaces
    a <- exprP
    skipSpaces
    op <- (Add <$ char '+') +++ (Mul <$ char '*')
    skipSpaces
    b <- exprP
    skipSpaces
    pure (op a b)

varP :: ReadP Expr
varP = do
    c <- satisfy isAlpha
    cs <- munch isAlphaNum
    pure (Var (c : cs))
