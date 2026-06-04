-- | Pretty-printer and parser for lambda expressions.
--
-- Syntax:
--   * Integers:     @42@, @-7@
--   * Booleans:     @true@, @false@
--   * Variables:    @x@, @myVar@
--   * Lambda:       @(\\x. e)@
--   * Application:  @(f a)@
--   * Let:          @(let x = e1 in e2)@
--
-- Round-trip: @parse (pretty e) == Just e@
module Lambda.Pretty
  ( pretty
  , parse
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Lambda.Syntax (Expr (..), Lit (..))

-- | Pretty-print an expression.
pretty :: Expr -> String
pretty (Lit (LInt n))   = show n
pretty (Lit (LBool b))  = if b then "true" else "false"
pretty (Var x)          = x
pretty (Lam x e)        = "(\\" <> x <> ". " <> pretty e <> ")"
pretty (App f a)        = "(" <> pretty f <> " " <> pretty a <> ")"
pretty (Let x e1 e2)    =
  "(let " <> x <> " = " <> pretty e1 <> " in " <> pretty e2 <> ")"

-- | Parse a pretty-printed expression.
parse :: String -> Maybe Expr
parse s = case exprP (dropSpaces s) of
  Just (e, rest) | all isSpace rest -> Just e
  _                                 -> Nothing

-- ---------------------------------------------------------------------------
-- Internal total parser
-- ---------------------------------------------------------------------------

dropSpaces :: String -> String
dropSpaces = dropWhile isSpace

exprP :: String -> Maybe (Expr, String)
exprP ('(' : rest) = parenP (dropSpaces rest)
exprP s            = atomP s

parenP :: String -> Maybe (Expr, String)
-- Lambda: (\x. e)
parenP ('\\' : rest) =
  let (x, after) = span isAlphaNum (dropSpaces rest)
  in if null x then Nothing
     else case dropSpaces after of
       ('.' : body) -> do
         (e, after2) <- exprP (dropSpaces body)
         case dropSpaces after2 of
           (')' : t) -> Just (Lam x e, t)
           _         -> Nothing
       _ -> Nothing
-- Let: (let x = e1 in e2)
parenP ('l':'e':'t':' ':rest) =
  let (x, after) = span isAlphaNum (dropSpaces rest)
  in if null x then Nothing
     else case dropSpaces after of
       ('=' : after2) -> do
         (e1, after3) <- exprP (dropSpaces after2)
         case dropSpaces after3 of
           ('i':'n':' ':after4) -> do
             (e2, after5) <- exprP (dropSpaces after4)
             case dropSpaces after5 of
               (')' : t) -> Just (Let x e1 e2, t)
               _         -> Nothing
           _ -> Nothing
       _ -> Nothing
-- Application: (f a)
parenP rest = do
  (f, after1) <- exprP (dropSpaces rest)
  (a, after2) <- exprP (dropSpaces after1)
  case dropSpaces after2 of
    (')' : t) -> Just (App f a, t)
    _         -> Nothing

atomP :: String -> Maybe (Expr, String)
-- Negative integer literal
atomP ('-' : c : rest) | isDigit c =
  let (digits, remainder) = span isDigit (c : rest)
  in  Just (Lit (LInt (negate (read digits))), remainder)
-- Non-negative integer literal
atomP (c : rest) | isDigit c =
  let (digits, remainder) = span isDigit (c : rest)
  in  Just (Lit (LInt (read digits)), remainder)
-- Boolean / variable
atomP (c : rest) | isAlpha c =
  let (word, remainder) = span isAlphaNum (c : rest)
  in  case word of
        "true"  -> Just (Lit (LBool True),  remainder)
        "false" -> Just (Lit (LBool False), remainder)
        _       -> Just (Var word, remainder)
atomP _ = Nothing
