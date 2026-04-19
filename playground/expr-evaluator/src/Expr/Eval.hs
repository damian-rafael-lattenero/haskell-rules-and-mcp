{- |
Module      : Expr.Eval
Description : Evaluate 'Expr' against an 'Env'. Short-circuits on the
first unbound variable. 'Int' wraps on overflow silently.
-}
module Expr.Eval (
    eval,
    evalClosed,
) where

import Expr.Syntax (
    Env,
    Error (..),
    Expr (..),
    emptyEnv,
    lookupVar,
 )

eval :: Env -> Expr -> Either Error Int
eval _ (Lit n) = Right n
eval env (Var x) = lookupVar x env
eval env (Neg e) = negate <$> eval env e
eval env (Add a b) = (+) <$> eval env a <*> eval env b
eval env (Mul a b) = (*) <$> eval env a <*> eval env b

evalClosed :: Expr -> Either Error Int
evalClosed = eval emptyEnv
