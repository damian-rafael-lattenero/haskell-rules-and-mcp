-- |
-- Module      : Expr.Pretty
-- Description : Pretty-printer and parser with round-trip guarantee.
--
-- The output is fully parenthesized so the grammar has no precedence
-- ambiguity — trading readability for a trivially total parser. The parser
-- is built with 'ReadP' (base), so it is total: malformed input returns
-- 'Nothing' rather than throwing. Only input that is entirely consumed and
-- matches the grammar is accepted; trailing garbage is rejected explicitly.
module Expr.Pretty
  ( pretty
  , parse
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Text.ParserCombinators.ReadP
  ( ReadP
  , between
  , char
  , eof
  , munch
  , munch1
  , pfail
  , readP_to_S
  , satisfy
  , skipSpaces
  , (+++)
  )
import Text.Read (readMaybe)

import Expr.Syntax (Expr (..))

--------------------------------------------------------------------------------
-- Pretty
--------------------------------------------------------------------------------

-- | Render an expression. Fully parenthesized; 'Lit' uses Haskell's default
-- 'show' so negative numbers print with a leading @-@.
pretty :: Expr -> String
pretty (Lit n)   = show n
pretty (Var x)   = x
pretty (Neg e)   = "-(" <> pretty e <> ")"
pretty (Add a b) = "(" <> pretty a <> " + " <> pretty b <> ")"
pretty (Mul a b) = "(" <> pretty a <> " * " <> pretty b <> ")"

--------------------------------------------------------------------------------
-- Parse
--------------------------------------------------------------------------------

-- | Total parser. Returns 'Nothing' on malformed input. Requires that the
-- entire input be consumed (no trailing garbage).
parse :: String -> Maybe Expr
parse input = case readP_to_S (skipSpaces *> exprP <* skipSpaces <* eof) input of
  [(e, "")] -> Just e
  _         -> Nothing

exprP :: ReadP Expr
exprP = litP +++ negP +++ binOpP +++ varP

-- | Integer literal. Handles an optional leading minus so that 'pretty'
-- emitting @show (Lit (-5)) == "-5"@ round-trips back to the same 'Lit'.
litP :: ReadP Expr
litP = do
  sign <- ((char '-' *> pure negate) +++ pure id)
  digits <- munch1 isDigit
  case readMaybe digits of
    Just n  -> pure (Lit (sign n))
    Nothing -> pfail  -- overflow or unexpected; reject gracefully

-- | Negation: @-( expr )@ — the parens disambiguate from a literal with a
-- minus sign.
negP :: ReadP Expr
negP = do
  _ <- char '-'
  e <- between (char '(') (char ')') (skipSpaces *> exprP <* skipSpaces)
  pure (Neg e)

-- | Binary operator: @( a OP b )@ where OP is @+@ or @*@.
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

-- | Identifier: @[A-Za-z][A-Za-z0-9]*@. Rejects reserved-ish starts so a
-- leading digit routes to 'litP' instead.
varP :: ReadP Expr
varP = do
  c  <- satisfy isAlpha
  cs <- munch isAlphaNum
  pure (Var (c : cs))
