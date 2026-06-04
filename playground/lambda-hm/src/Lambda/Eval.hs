-- | Call-by-value big-step evaluator.
module Lambda.Eval
  ( Value (..)
  , EvalError (..)
  , eval
  , evalClosed
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Lambda.Syntax (Expr (..), Lit (..), Name)

-- | Runtime values.
data Value
  = VInt     Int
  | VBool    Bool
  | VClosure Name Expr (Map Name Value)
  deriving (Eq, Show)

data EvalError
  = UnboundVar Name
  | NotAFunction Value
  deriving (Eq, Show)

type Env = Map Name Value

-- | Evaluate under an environment.
eval :: Env -> Expr -> Either EvalError Value
eval _   (Lit (LInt n))  = Right (VInt n)
eval _   (Lit (LBool b)) = Right (VBool b)
eval env (Var x)         =
  maybe (Left (UnboundVar x)) Right (Map.lookup x env)
eval env (Lam x body)    = Right (VClosure x body env)
eval env (App f arg)     = do
  vf <- eval env f
  va <- eval env arg
  case vf of
    VClosure x body cenv -> eval (Map.insert x va cenv) body
    _                    -> Left (NotAFunction vf)
eval env (Let x e1 e2)   = do
  v <- eval env e1
  eval (Map.insert x v env) e2

-- | Evaluate a closed expression (empty environment).
evalClosed :: Expr -> Either EvalError Value
evalClosed = eval Map.empty
