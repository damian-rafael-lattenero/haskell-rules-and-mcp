module Expr.Simplify
  ( simplify
  , simplifyOnce
  ) where

import Expr.Syntax (Expr (..))

simplify :: Expr -> Expr
simplify e =
  let e' = simplifyOnce e
  in  if e' == e then e else simplify e'

simplifyOnce :: Expr -> Expr
simplifyOnce = \case
  Lit n        -> Lit n
  Var v        -> Var v
  Neg e        -> simplifyNeg (simplifyOnce e)
  Add l r      -> simplifyAdd (simplifyOnce l) (simplifyOnce r)
  Mul l r      -> simplifyMul (simplifyOnce l) (simplifyOnce r)

simplifyNeg :: Expr -> Expr
simplifyNeg = \case
  Neg e                -> e
  Lit n | safeNeg n    -> Lit (negate n)
  e                    -> Neg e

simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd l r = case (l, r) of
  (Lit 0, x)           -> x
  (x, Lit 0)           -> x
  (Lit a, Lit b)
    | Just c <- safeAdd a b -> Lit c
  _                    -> Add l r

simplifyMul :: Expr -> Expr -> Expr
simplifyMul l r = case (l, r) of
  (Lit 0, _)           -> Lit 0
  (_, Lit 0)           -> Lit 0
  (Lit 1, x)           -> x
  (x, Lit 1)           -> x
  (Lit a, Lit b)
    | Just c <- safeMul a b -> Lit c
  _                    -> Mul l r

safeNeg :: Int -> Bool
safeNeg n = n /= minBound

safeAdd :: Int -> Int -> Maybe Int
safeAdd a b =
  let r = toInteger a + toInteger b
  in  if r < toInteger (minBound :: Int) || r > toInteger (maxBound :: Int)
        then Nothing
        else Just (fromInteger r)

safeMul :: Int -> Int -> Maybe Int
safeMul a b =
  let r = toInteger a * toInteger b
  in  if r < toInteger (minBound :: Int) || r > toInteger (maxBound :: Int)
        then Nothing
        else Just (fromInteger r)
