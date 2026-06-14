module Lambda.Eval
    ( Value(..)
    , Env
    , eval
    ) where

import qualified Data.Map.Strict as Map
import Lambda.Syntax

data Value
    = VInt  Int
    | VBool Bool
    | VClosure Name Term Env
    deriving (Eq)

instance Show Value where
    show (VInt  n)        = show n
    show (VBool b)        = if b then "true" else "false"
    show (VClosure x _ _) = "<closure " ++ x ++ ">"

type Env = Map.Map Name Value

eval :: Env -> Term -> Either String Value
eval env (Var x) =
    case Map.lookup x env of
        Just v  -> Right v
        Nothing -> Left ("unbound variable: " ++ x)
eval env (Lam x body) =
    Right (VClosure x body env)
eval env (App t1 t2) = do
    v1 <- eval env t1
    v2 <- eval env t2
    case v1 of
        VClosure x body closEnv ->
            eval (Map.insert x v2 closEnv) body
        _ -> Left "applying a non-function"
eval env (Let x t1 t2) = do
    v1 <- eval env t1
    eval (Map.insert x v1 env) t2
eval _   (Lit (LInt  n)) = Right (VInt  n)
eval _   (Lit (LBool b)) = Right (VBool b)
