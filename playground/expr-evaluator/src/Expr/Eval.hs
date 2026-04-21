module Expr.Eval
  ( eval
  , evalClosed
  , checkedAdd
  , checkedMul
  , checkedNeg
  ) where

import Expr.Syntax (Env, Error (..), Expr (..), emptyEnv, lookupEnv)

eval :: Env -> Expr -> Either Error Int
eval env = \case
  Lit n     -> Right n
  Var name  -> maybe (Left (UnboundVar name)) Right (lookupEnv name env)
  Neg e     -> eval env e >>= checkedNeg
  Add l r   -> do
    lv <- eval env l
    rv <- eval env r
    checkedAdd lv rv
  Mul l r   -> do
    lv <- eval env l
    rv <- eval env r
    checkedMul lv rv

evalClosed :: Expr -> Either Error Int
evalClosed = eval emptyEnv

checkedAdd :: Int -> Int -> Either Error Int
checkedAdd a b =
  let r = toInteger a + toInteger b
  in  if r < toInteger (minBound :: Int) || r > toInteger (maxBound :: Int)
        then Left Overflow
        else Right (fromInteger r)

checkedMul :: Int -> Int -> Either Error Int
checkedMul a b =
  let r = toInteger a * toInteger b
  in  if r < toInteger (minBound :: Int) || r > toInteger (maxBound :: Int)
        then Left Overflow
        else Right (fromInteger r)

checkedNeg :: Int -> Either Error Int
checkedNeg n
  | n == minBound = Left Overflow
  | otherwise     = Right (negate n)
