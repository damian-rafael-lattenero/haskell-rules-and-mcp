module LambdaHM
  ( module LambdaHM.Syntax
  , module LambdaHM.Infer
  , module LambdaHM.Eval
  , module LambdaHM.Pretty
  , typecheck
  , run
  , typeAndRun
  ) where

import LambdaHM.Syntax
import LambdaHM.Infer
import LambdaHM.Eval
import LambdaHM.Pretty

typecheck :: Expr -> Either TypeError Type
typecheck = inferExpr defaultEnv

run :: Expr -> Either EvalError Value
run = eval defaultValEnv

typeAndRun :: Expr -> String
typeAndRun expr =
  case inferExpr defaultEnv expr of
    Left err -> "Type error: " ++ show err
    Right t ->
      case eval defaultValEnv expr of
        Left err -> "Runtime error: " ++ show err
        Right v -> prettyExpr expr ++ " : " ++ prettyType t ++ " = " ++ show v
