{- |
Module      : Expr.Simplify
Description : Semantics-preserving algebraic rewrites. 0*x omitted on
purpose because it would change error semantics for unbound variables.
-}
module Expr.Simplify (
    simplify,
) where

import Expr.Syntax (Expr (..))

simplify :: Expr -> Expr
simplify = rewrite . descend
  where
    descend (Neg e) = Neg (simplify e)
    descend (Add a b) = Add (simplify a) (simplify b)
    descend (Mul a b) = Mul (simplify a) (simplify b)
    descend leaf = leaf

    rewrite (Neg (Neg e)) = e
    rewrite (Neg (Lit n)) = Lit (negate n)
    rewrite (Add (Lit a) (Lit b)) = Lit (a + b)
    rewrite (Add (Lit 0) e) = e
    rewrite (Add e (Lit 0)) = e
    rewrite (Mul (Lit a) (Lit b)) = Lit (a * b)
    rewrite (Mul (Lit 1) e) = e
    rewrite (Mul e (Lit 1)) = e
    rewrite e = e
