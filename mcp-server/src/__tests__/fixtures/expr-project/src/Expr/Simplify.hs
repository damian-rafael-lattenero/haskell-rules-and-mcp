-- |
-- Module      : Expr.Simplify
-- Description : Semantics-preserving algebraic rewrites.
--
-- Every rule here is eval-sound: @eval env (simplify e) == eval env e@ for all
-- environments @env@. We deliberately do NOT apply the zero-absorbing rule
-- @0 * x ==> 0@ because @x@ may contain an unbound variable that would make
-- @eval env (Mul (Var \"x\") (Lit 0))@ fail with 'UnboundVariable' while
-- @eval env (Lit 0)@ would succeed — they disagree in the error domain.
module Expr.Simplify
  ( simplify
  ) where

import Expr.Syntax (Expr (..))

-- | Apply semantics-preserving rewrites bottom-up, then at the root. Runs
-- once — idempotence is a tested property.
simplify :: Expr -> Expr
simplify = rewrite . descend
  where
    -- Recurse into sub-expressions first so we constant-fold leaves before
    -- deciding what the outer node should become.
    descend (Neg e)   = Neg (simplify e)
    descend (Add a b) = Add (simplify a) (simplify b)
    descend (Mul a b) = Mul (simplify a) (simplify b)
    descend leaf      = leaf  -- Lit / Var

    -- Root rewrites. Each pattern is eval-sound.
    rewrite (Neg (Neg e))         = e                        -- double negation
    rewrite (Neg (Lit n))         = Lit (negate n)           -- fold constant negation
    rewrite (Add (Lit a) (Lit b)) = Lit (a + b)              -- fold addition
    rewrite (Add (Lit 0) e)       = e                        -- left identity
    rewrite (Add e (Lit 0))       = e                        -- right identity
    rewrite (Mul (Lit a) (Lit b)) = Lit (a * b)              -- fold multiplication
    rewrite (Mul (Lit 1) e)       = e                        -- left identity
    rewrite (Mul e (Lit 1))       = e                        -- right identity
    rewrite e                     = e
