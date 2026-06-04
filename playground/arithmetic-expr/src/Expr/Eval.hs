-- | Expression evaluator: reduces an 'Expr' to an 'Int' given an 'Env'.
module Expr.Eval
  ( eval
  ) where

import qualified Data.Map.Strict as Map

import Expr.Syntax (Env, Error (..), Expr (..))

-- | Evaluate an expression under a variable environment.
--
-- Returns @Left (UnboundVariable name)@ if a 'Var' is not found.
eval :: Env -> Expr -> Either Error Int
eval _   (Lit n)     = Right n
eval env (Var name)  =
  case Map.lookup name env of
    Just v  -> Right v
    Nothing -> Left (UnboundVariable name)
eval env (Add e1 e2) = (+) <$> eval env e1 <*> eval env e2
eval env (Mul e1 e2) = (*) <$> eval env e1 <*> eval env e2
eval env (Neg e)     = negate <$> eval env e
