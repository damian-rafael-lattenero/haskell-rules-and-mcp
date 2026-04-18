-- |
-- Module      : Expr.Eval
-- Description : Evaluate 'Expr' against an 'Env', reporting structured errors.
--
-- Arithmetic uses 'Int' and therefore wraps on overflow; in particular
-- @negate minBound == minBound@. This is inherited from 'Expr.Syntax' and
-- acceptable for a teaching evaluator. Replace 'Int' with 'Integer' in
-- 'Expr.Syntax' if you need unbounded semantics.
module Expr.Eval
  ( eval
  , evalClosed
  ) where

import Expr.Syntax
  ( Env
  , Error (..)
  , Expr (..)
  , emptyEnv
  , lookupVar
  )

-- | Evaluate an expression in an environment. Short-circuits on the first
-- unbound variable encountered in left-to-right traversal.
eval :: Env -> Expr -> Either Error Int
eval _   (Lit n)   = Right n
eval env (Var x)   = lookupVar x env
eval env (Neg e)   = negate <$> eval env e
eval env (Add a b) = (+) <$> eval env a <*> eval env b
eval env (Mul a b) = (*) <$> eval env a <*> eval env b

-- | Evaluate an expression that must be closed. Any free variable produces
-- an 'UnboundVariable' error.
evalClosed :: Expr -> Either Error Int
evalClosed = eval emptyEnv
