module LambdaHM.Eval
  ( Value (..)
  , EvalError (..)
  , eval
  , defaultValEnv
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LambdaHM.Syntax

data Value
  = VInt Int
  | VBool Bool
  | VLam Name Expr (Map Name Value)
  | VBuiltin Name (Value -> Either EvalError Value)

instance Show Value where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VLam x _ _) = "<lambda " ++ x ++ ">"
  show (VBuiltin n _) = "<builtin " ++ n ++ ">"

data EvalError
  = UndefinedVariable Name
  | TypeMismatch String
  | NotAFunction Value
  deriving (Show)

type ValEnv = Map Name Value

eval :: ValEnv -> Expr -> Either EvalError Value
eval _ (Lit (LInt n)) = Right (VInt n)
eval _ (Lit (LBool b)) = Right (VBool b)

eval env (Var x) =
  case Map.lookup x env of
    Nothing -> Left (UndefinedVariable x)
    Just v -> Right v

eval env (Lam x body) = Right (VLam x body env)

eval env (App f arg) = do
  vf <- eval env f
  va <- eval env arg
  apply vf va

eval env (Let x e body) = do
  ve <- eval env e
  eval (Map.insert x ve env) body

eval env (If cond t f) = do
  vc <- eval env cond
  case vc of
    VBool True -> eval env t
    VBool False -> eval env f
    _ -> Left (TypeMismatch "if condition must be Bool")

apply :: Value -> Value -> Either EvalError Value
apply (VLam x body closure) arg =
  eval (Map.insert x arg closure) body
apply (VBuiltin _ f) arg = f arg
apply v _ = Left (NotAFunction v)

-- Default value environment matching defaultEnv from Infer
defaultValEnv :: ValEnv
defaultValEnv = Map.fromList
  [ ("add",   builtin2Int "add"  (+))
  , ("sub",   builtin2Int "sub"  (-))
  , ("mul",   builtin2Int "mul"  (*))
  , ("eq",    builtin2Eq  "eq")
  , ("not",   builtinNot)
  , ("and",   builtin2Bool "and" (&&))
  , ("or",    builtin2Bool "or"  (||))
  , ("zero",  VInt 0)
  , ("true",  VBool True)
  , ("false", VBool False)
  ]

builtin2Int :: Name -> (Int -> Int -> Int) -> Value
builtin2Int n f = VBuiltin n $ \a ->
  case a of
    VInt x -> Right $ VBuiltin (n ++ "'") $ \b ->
      case b of
        VInt y -> Right (VInt (f x y))
        _ -> Left (TypeMismatch (n ++ ": expected Int"))
    _ -> Left (TypeMismatch (n ++ ": expected Int"))

builtin2Eq :: Name -> Value
builtin2Eq n = VBuiltin n $ \a ->
  Right $ VBuiltin (n ++ "'") $ \b ->
    case (a, b) of
      (VInt x, VInt y) -> Right (VBool (x == y))
      (VBool x, VBool y) -> Right (VBool (x == y))
      _ -> Left (TypeMismatch "eq: type mismatch")

builtinNot :: Value
builtinNot = VBuiltin "not" $ \a ->
  case a of
    VBool b -> Right (VBool (not b))
    _ -> Left (TypeMismatch "not: expected Bool")

builtin2Bool :: Name -> (Bool -> Bool -> Bool) -> Value
builtin2Bool n f = VBuiltin n $ \a ->
  case a of
    VBool x -> Right $ VBuiltin (n ++ "'") $ \b ->
      case b of
        VBool y -> Right (VBool (f x y))
        _ -> Left (TypeMismatch (n ++ ": expected Bool"))
    _ -> Left (TypeMismatch (n ++ ": expected Bool"))
