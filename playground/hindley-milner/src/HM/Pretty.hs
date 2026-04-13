module HM.Pretty
  ( ppType
  , ppScheme
  , ppExpr
  , ppError
  ) where

import HM.Types
import HM.Syntax
import HM.Unify (TypeError(..))

ppType :: Type -> String
ppType (TVar v)     = v
ppType (TCon c)     = c
ppType (TFun a@(TFun _ _) b) = "(" ++ ppType a ++ ") -> " ++ ppType b
ppType (TFun a b)   = ppType a ++ " -> " ++ ppType b

ppScheme :: Scheme -> String
ppScheme (Forall [] t) = ppType t
ppScheme (Forall vs t) = "forall " ++ unwords vs ++ ". " ++ ppType t

ppExpr :: Expr -> String
ppExpr (Var x)       = x
ppExpr (Lit (LInt n))  = show n
ppExpr (Lit (LBool b)) = show b
ppExpr (App f x)     = ppExpr f ++ " " ++ ppAtom x
ppExpr (Lam x body)  = "\\" ++ x ++ " -> " ++ ppExpr body
ppExpr (Let x e1 e2) = "let " ++ x ++ " = " ++ ppExpr e1 ++ " in " ++ ppExpr e2
ppExpr (If c t f)    = "if " ++ ppExpr c ++ " then " ++ ppExpr t ++ " else " ++ ppExpr f

ppAtom :: Expr -> String
ppAtom e@(Var _) = ppExpr e
ppAtom e@(Lit _) = ppExpr e
ppAtom e         = "(" ++ ppExpr e ++ ")"

ppError :: TypeError -> String
ppError (UnificationFail t1 t2) =
  "Cannot unify " ++ ppType t1 ++ " with " ++ ppType t2
ppError (InfiniteType v t) =
  "Infinite type: " ++ v ++ " ~ " ++ ppType t
ppError (UnboundVariable x) =
  "Unbound variable: " ++ x
