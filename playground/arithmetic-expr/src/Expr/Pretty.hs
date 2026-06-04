-- | Pretty-printer and simple parser for 'Expr'.
--
-- Syntax:
--   * Literals:         @42@, @-7@
--   * Variables:        @x@, @myVar@
--   * Addition:         @(e + e)@
--   * Multiplication:   @(e * e)@
--   * Negation:         @(~ e)@   ← uses @~@ to avoid ambiguity with negative literals
--
-- Round-trip property: @parse (pretty e) == Just e@ for any 'Expr'.
module Expr.Pretty
  ( pretty
  , parse
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

import Expr.Syntax (Expr (..))

-- | Pretty-print an expression.
--
-- >>> pretty (Add (Lit 1) (Lit 2))
-- "(1 + 2)"
-- >>> pretty (Mul (Neg (Lit 3)) (Lit 4))
-- "((~ 3) * 4)"
-- >>> pretty (Lit (-7))
-- "-7"
-- >>> pretty (Var "x")
-- "x"
pretty :: Expr -> String
pretty (Lit n)     = show n
pretty (Var v)     = v
pretty (Add e1 e2) = "(" <> pretty e1 <> " + " <> pretty e2 <> ")"
pretty (Mul e1 e2) = "(" <> pretty e1 <> " * " <> pretty e2 <> ")"
pretty (Neg e)     = "(~ " <> pretty e <> ")"

-- | Parse a pretty-printed expression.
--
-- Returns 'Nothing' on any parse error.
-- Round-trip: @parse (pretty e) == Just e@ for all 'Expr'.
parse :: String -> Maybe Expr
parse s = case exprP (dropSpaces s) of
  Just (e, rest) | all isSpace rest -> Just e
  _                                 -> Nothing

-- ---------------------------------------------------------------------------
-- Internal hand-rolled total parser
-- ---------------------------------------------------------------------------

dropSpaces :: String -> String
dropSpaces = dropWhile isSpace

-- | Parse one expression.
exprP :: String -> Maybe (Expr, String)
exprP ('(' : rest) = parenP (dropSpaces rest)
exprP s            = atomP s

-- | Parse a parenthesised form: negation @(~ e)@ or binary op @(e op e)@.
parenP :: String -> Maybe (Expr, String)
parenP ('~' : rest) =
  -- Negation: (~ <expr>)
  case exprP (dropSpaces rest) of
    Just (e, after) ->
      case dropSpaces after of
        (')' : t) -> Just (Neg e, t)
        _         -> Nothing
    Nothing -> Nothing
parenP rest =
  -- Binary op: (<expr> op <expr>)
  case exprP (dropSpaces rest) of
    Just (e1, after1) ->
      case dropSpaces after1 of
        (op : rest2) | op == '+' || op == '*' ->
          case exprP (dropSpaces rest2) of
            Just (e2, after2) ->
              case dropSpaces after2 of
                (')' : t) ->
                  let node = if op == '+' then Add e1 e2 else Mul e1 e2
                  in Just (node, t)
                _ -> Nothing
            Nothing -> Nothing
        _ -> Nothing
    Nothing -> Nothing

-- | Parse an atom: integer literal (possibly negative) or variable name.
atomP :: String -> Maybe (Expr, String)
atomP ('-' : c : rest)
  | isDigit c =
      let (digits, remainder) = span isDigit (c : rest)
      in Just (Lit (negate (read digits)), remainder)
atomP (c : rest)
  | isDigit c =
      let (digits, remainder) = span isDigit (c : rest)
      in Just (Lit (read digits), remainder)
  | isAlpha c =
      let (name, remainder) = span isAlphaNum (c : rest)
      in Just (Var name, remainder)
atomP _ = Nothing
