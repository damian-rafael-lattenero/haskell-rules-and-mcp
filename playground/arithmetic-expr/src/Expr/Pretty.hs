-- | Pretty-printing and parsing of arithmetic expressions.
--
-- The format is fully parenthesized to avoid ambiguity:
--
-- @
--   Lit n     →  "n"          (digits only, n >= 0)
--   Var x     →  "x"          (letter followed by alphanums)
--   Neg e     →  "(-e)"
--   Add e1 e2 →  "(e1 + e2)"
--   Mul e1 e2 →  "(e1 * e2)"
-- @
--
-- Roundtrip guarantee: @parseExpr (pretty e) == Right e@
-- for any 'Expr' whose 'Lit' nodes carry non-negative values.
module Expr.Pretty
    ( pretty
    , parseExpr
    ) where

import Text.Parsec (ParseError)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Expr.Syntax

-- | Render an expression as a fully-parenthesized string.
pretty :: Expr -> String
pretty = \case
    Lit n     -> show n
    Var x     -> x
    Neg e     -> "(-" <> pretty e <> ")"
    Add e1 e2 -> "(" <> pretty e1 <> " + " <> pretty e2 <> ")"
    Mul e1 e2 -> "(" <> pretty e1 <> " * " <> pretty e2 <> ")"

-- | Parse a fully-parenthesized arithmetic expression.
-- Returns 'Left' with a parse error on failure.
parseExpr :: String -> Either ParseError Expr
parseExpr = P.parse (exprP <* P.eof) "<input>"

-- Parser internals ----------------------------------------------------

exprP :: Parser Expr
exprP = P.spaces *> (parenExprP P.<|> litP P.<|> varP)

-- | Parse a parenthesized form: either @(-e)@ (Neg) or @(e1 op e2)@.
-- Disambiguated by the character after @(@:
--   '-' → Neg,  anything else → binary op.
parenExprP :: Parser Expr
parenExprP = do
    _ <- P.char '('
    e <- negP P.<|> binOpP
    _ <- P.char ')'
    pure e

negP :: Parser Expr
negP = P.char '-' >> Neg <$> exprP

binOpP :: Parser Expr
binOpP = do
    e1 <- exprP
    P.spaces
    op <- P.char '+' P.<|> P.char '*'
    P.spaces
    e2 <- exprP
    pure $ if op == '+' then Add e1 e2 else Mul e1 e2

-- | Parse a non-negative integer literal.
litP :: Parser Expr
litP = Lit . read <$> P.many1 P.digit

-- | Parse a variable name: letter followed by zero or more alphanumerics.
varP :: Parser Expr
varP = do
    c  <- P.letter
    cs <- P.many P.alphaNum
    pure (Var (c : cs))
