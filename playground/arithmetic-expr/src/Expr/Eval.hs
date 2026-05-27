-- | Evaluate arithmetic expressions under a variable environment.
module Expr.Eval
    ( eval
    ) where

import qualified Data.Map.Strict as Map

import Expr.Syntax

-- | Evaluate an expression, returning either an 'Error' or the integer result.
--
-- >>> import qualified Data.Map.Strict as Map
-- >>> eval (Map.fromList [("x", 3)]) (Add (Var "x") (Lit 1))
-- Right 4
-- >>> eval Map.empty (Var "y")
-- Left (UnboundVar "y")
eval :: Env -> Expr -> Either Error Int
eval _   (Lit n)     = Right n
eval env (Add e1 e2) = (+) <$> eval env e1 <*> eval env e2
eval env (Mul e1 e2) = (*) <$> eval env e1 <*> eval env e2
eval env (Neg e)     = negate <$> eval env e
eval env (Var x)     = maybe (Left (UnboundVar x)) Right (Map.lookup x env)
