-- | Expression simplifier: applies algebraic rewriting rules.
--
-- Rules applied (one bottom-up pass, repeated to fixpoint):
--
--   * Constant folding:        @Lit m + Lit n  →  Lit (m+n)@
--   * Additive identity:       @0 + e  →  e@,  @e + 0  →  e@
--   * Multiplicative identity: @1 * e  →  e@,  @e * 1  →  e@
--   * Zero annihilator:        @0 * Lit n  →  Lit 0@,  @Lit n * 0  →  Lit 0@
--                              (only when the OTHER operand is a Lit — safe with
--                               partial eval because Lit never fails)
--   * Double negation:         @Neg (Neg e)  →  e@
--   * Neg of literal:          @Neg (Lit n)  →  Lit (-n)@
module Expr.Simplify
  ( simplify
  ) where

import Expr.Syntax (Expr (..))

-- | Repeatedly apply simplification until a fixpoint.
simplify :: Expr -> Expr
simplify e =
  let e' = step e
  in if e' == e then e else simplify e'

-- | Single bottom-up simplification pass.
step :: Expr -> Expr
step (Add e1 e2) = simplifyAdd (step e1) (step e2)
step (Mul e1 e2) = simplifyMul (step e1) (step e2)
step (Neg e)     = simplifyNeg (step e)
step e           = e          -- Lit, Var — already normal

-- | Smart constructor for Add.
simplifyAdd :: Expr -> Expr -> Expr
simplifyAdd (Lit 0) e       = e
simplifyAdd e       (Lit 0) = e
simplifyAdd (Lit m) (Lit n) = Lit (m + n)
simplifyAdd e1      e2      = Add e1 e2

-- | Smart constructor for Mul.
--
-- The zero-annihilator (@0 * e = 0@) is only applied when the OTHER
-- operand is @Lit n@ — this guarantees that operand always evaluates
-- successfully, so dropping it cannot change 'eval' from Left to Right.
simplifyMul :: Expr -> Expr -> Expr
simplifyMul (Lit 0) (Lit _)  = Lit 0     -- 0 * constant → 0  (safe)
simplifyMul (Lit _) (Lit 0)  = Lit 0     -- constant * 0 → 0  (safe)
simplifyMul (Lit 1) e        = e
simplifyMul e       (Lit 1)  = e
simplifyMul (Lit m) (Lit n)  = Lit (m * n)
simplifyMul e1      e2       = Mul e1 e2

-- | Smart constructor for Neg.
simplifyNeg :: Expr -> Expr
simplifyNeg (Neg e)  = e
simplifyNeg (Lit n)  = Lit (negate n)
simplifyNeg e        = Neg e
