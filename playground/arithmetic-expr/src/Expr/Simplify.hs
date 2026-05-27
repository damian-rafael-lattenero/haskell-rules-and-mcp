-- | Algebraic simplification of arithmetic expressions.
module Expr.Simplify
    ( simplify
    , simplifyStep
    ) where

import Expr.Syntax

-- | One pass of simplification rules, applied bottom-up within one step.
-- Does NOT recurse to a fixed point — use 'simplify' for that.
simplifyStep :: Expr -> Expr
simplifyStep = \case
    -- Constant folding ------------------------------------------------
    Add (Lit a) (Lit b) -> Lit (a + b)
    Mul (Lit a) (Lit b) -> Lit (a * b)
    Neg (Lit a)         -> Lit (negate a)
    -- Additive identity: 0 + x = x, x + 0 = x -------------------------
    Add (Lit 0) e       -> e
    Add e (Lit 0)       -> e
    -- Multiplicative identity: 1 * x = x, x * 1 = x -------------------
    Mul (Lit 1) e       -> e
    Mul e (Lit 1)       -> e
    -- Annihilation: 0 * x = 0, x * 0 = 0 ------------------------------
    Mul (Lit 0) _       -> Lit 0
    Mul _ (Lit 0)       -> Lit 0
    -- Double negation: -(-x) = x ---------------------------------------
    Neg (Neg e)         -> e
    -- Recurse into subterms --------------------------------------------
    Add e1 e2           -> Add (simplifyStep e1) (simplifyStep e2)
    Mul e1 e2           -> Mul (simplifyStep e1) (simplifyStep e2)
    Neg e               -> Neg (simplifyStep e)
    -- Leaf nodes are already fully simplified --------------------------
    e                   -> e

-- | Repeatedly apply 'simplifyStep' until a fixed point is reached.
--
-- Laws (tested in Spec.hs):
--   * eval env (simplify e) == eval env e   -- evaluation preservation
--   * simplify (simplify e) == simplify e   -- idempotence
--   * simplify (Add (Lit 0) e) == simplify e
--   * simplify (Mul (Lit 1) e) == simplify e
--   * simplify (Neg (Neg e)) == simplify e
simplify :: Expr -> Expr
simplify e =
    let e' = simplifyStep e
    in  if e' == e then e else simplify e'
