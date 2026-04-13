module HM.Pretty
  ( ppType
  , ppScheme
  , ppExpr
  , ppError
  , ppInfer
  ) where

import HM.Syntax
import HM.Infer (infer)

-- | Pretty print a type
ppType :: Type -> String
ppType (TVar a)   = a
ppType (TCon c)   = c
ppType (TArr a b) = parensArr a ++ " -> " ++ ppType b
  where
    parensArr (TArr _ _) = "(" ++ ppType a ++ ")"
    parensArr t          = ppType t

-- | Pretty print a type scheme
ppScheme :: Scheme -> String
ppScheme (Forall [] t) = ppType t
ppScheme (Forall vars t) = "forall " ++ unwords vars ++ ". " ++ ppType t

-- | Pretty print an expression
ppExpr :: Expr -> String
ppExpr (EVar x)       = x
ppExpr (ELit (LInt n))  = show n
ppExpr (ELit (LBool b)) = if b then "True" else "False"
ppExpr (ELam x body)  = "\\" ++ x ++ " -> " ++ ppExpr body
ppExpr (EApp f a)      = parensApp f ++ " " ++ parensAtom a
  where
    parensApp (ELam _ _) = "(" ++ ppExpr f ++ ")"
    parensApp _          = ppExpr f
    parensAtom (EApp _ _) = "(" ++ ppExpr a ++ ")"
    parensAtom (ELam _ _) = "(" ++ ppExpr a ++ ")"
    parensAtom e          = ppExpr e
ppExpr (ELet x e1 e2) = "let " ++ x ++ " = " ++ ppExpr e1 ++ " in " ++ ppExpr e2
ppExpr (EIf c t e)    = "if " ++ ppExpr c ++ " then " ++ ppExpr t ++ " else " ++ ppExpr e

-- | Pretty print an inference error
ppError :: InferError -> String
ppError (UnificationFail t1 t2) =
  "Cannot unify " ++ ppType t1 ++ " with " ++ ppType t2
ppError (InfiniteType v t) =
  "Infinite type: " ++ v ++ " ~ " ++ ppType t
ppError (UnboundVariable x) =
  "Unbound variable: " ++ x

-- | Infer and pretty print the result
ppInfer :: Expr -> String
ppInfer expr = case infer expr of
  Right scheme -> ppExpr expr ++ " : " ++ ppScheme scheme
  Left err     -> ppExpr expr ++ " -- ERROR: " ++ ppError err
