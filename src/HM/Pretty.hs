module HM.Pretty
  ( ppType
  , ppScheme
  , ppExpr
  ) where

import Data.List (intercalate)
import Data.Set qualified as Set

import HM.Syntax

-- | Known operator names for infix pretty-printing
operatorSet :: Set.Set String
operatorSet = Set.fromList ["+", "-", "*", ".", "==", "/=", "<", ">", "<=", ">=", "&&", "||", ":"]

-- | Check if a variable name is a known operator
isOperator :: String -> Bool
isOperator = (`Set.member` operatorSet)

-- | Pretty-print a type
ppType :: Type -> String
ppType (TVar v)     = v
ppType (TCon c)     = c
ppType (TArr a b)   = parensArr a ++ " -> " ++ ppType b
  where
    parensArr (TArr _ _) = "(" ++ ppType a ++ ")"
    parensArr t          = ppType t
ppType (TProd a b)  = "(" ++ ppType a ++ ", " ++ ppType b ++ ")"
ppType (TList t)    = "[" ++ ppType t ++ "]"

-- | Pretty-print a type scheme
ppScheme :: Scheme -> String
ppScheme (Forall [] t) = ppType t
ppScheme (Forall vs t) = "forall " ++ unwords vs ++ ". " ++ ppType t

-- | Pretty-print an expression
ppExpr :: Expr -> String
ppExpr (EVar x)       = x
ppExpr (ELit (LInt n))  = show n
ppExpr (ELit (LBool b)) = show b
ppExpr (EApp (EApp (EVar op) l) r)
  | isOperator op = parensOp l ++ " " ++ op ++ " " ++ parensOp r
  where
    parensOp e@(EApp (EApp (EVar op') _) _)
      | isOperator op' = "(" ++ ppExpr e ++ ")"
    parensOp e = ppExpr e
ppExpr (EApp f a)     = ppExpr f ++ " " ++ parensApp a
  where
    parensApp e@(EApp _ _) = "(" ++ ppExpr e ++ ")"
    parensApp e@(ELam _ _) = "(" ++ ppExpr e ++ ")"
    parensApp e            = ppExpr e
ppExpr (ELam x body)  = "\\" ++ unwords (x : collectLamVars body) ++ " -> " ++ ppExpr (stripLams body)
  where
    collectLamVars (ELam v b) = v : collectLamVars b
    collectLamVars _          = []
    stripLams (ELam _ b) = stripLams b
    stripLams e          = e
ppExpr (ELet x e1 e2)    = "let " ++ x ++ " = " ++ ppExpr e1 ++ " in " ++ ppExpr e2
ppExpr (ELetRec x e1 e2) = "let rec " ++ x ++ " = " ++ ppExpr e1 ++ " in " ++ ppExpr e2
ppExpr (EIf c t e)       = "if " ++ ppExpr c ++ " then " ++ ppExpr t ++ " else " ++ ppExpr e
ppExpr (EPair e1 e2)     = "(" ++ ppExpr e1 ++ ", " ++ ppExpr e2 ++ ")"
ppExpr (EFst e)          = "fst " ++ parensAtom e
ppExpr (ESnd e)          = "snd " ++ parensAtom e
ppExpr (EAnn e t)        = "(" ++ ppExpr e ++ " : " ++ ppType t ++ ")"
ppExpr (EList es)        = "[" ++ intercalate ", " (map ppExpr es) ++ "]"

parensAtom :: Expr -> String
parensAtom e@(EApp _ _) = "(" ++ ppExpr e ++ ")"
parensAtom e@(ELam _ _) = "(" ++ ppExpr e ++ ")"
parensAtom e@(ELet _ _ _) = "(" ++ ppExpr e ++ ")"
parensAtom e@(ELetRec _ _ _) = "(" ++ ppExpr e ++ ")"
parensAtom e@(EIf _ _ _) = "(" ++ ppExpr e ++ ")"
parensAtom e             = ppExpr e
