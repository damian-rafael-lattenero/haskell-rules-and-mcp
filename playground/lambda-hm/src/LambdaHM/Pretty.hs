module LambdaHM.Pretty
  ( prettyType
  , prettyExpr
  , prettyScheme
  , prettyResult
  ) where

import LambdaHM.Syntax

prettyType :: Type -> String
prettyType (TCon c) = c
prettyType (TVar n) = tyVarName n
prettyType (TArr a b) = prettyArg a ++ " -> " ++ prettyType b
  where
    prettyArg t@(TArr _ _) = "(" ++ prettyType t ++ ")"
    prettyArg t = prettyType t

prettyScheme :: Scheme -> String
prettyScheme (Forall [] t) = prettyType t
prettyScheme (Forall vs t) =
  "forall " ++ unwords (map tyVarName vs) ++ ". " ++ prettyType t

tyVarName :: Int -> String
tyVarName n
  | n < 26 = [['a'..'z'] !! n]
  | otherwise = 't' : show n

prettyExpr :: Expr -> String
prettyExpr (Lit (LInt n)) = show n
prettyExpr (Lit (LBool b)) = if b then "true" else "false"
prettyExpr (Var x) = x
prettyExpr (Lam x body) = "λ" ++ x ++ ". " ++ prettyExpr body
prettyExpr (App f arg) = prettyFun f ++ " " ++ prettyAtom arg
  where
    prettyFun e@(Lam _ _) = "(" ++ prettyExpr e ++ ")"
    prettyFun e = prettyExpr e
    prettyAtom e@(App _ _) = "(" ++ prettyExpr e ++ ")"
    prettyAtom e@(Lam _ _) = "(" ++ prettyExpr e ++ ")"
    prettyAtom e = prettyExpr e
prettyExpr (Let x e body) =
  "let " ++ x ++ " = " ++ prettyExpr e ++ " in " ++ prettyExpr body
prettyExpr (If c t f) =
  "if " ++ prettyExpr c ++ " then " ++ prettyExpr t ++ " else " ++ prettyExpr f

prettyResult :: Either String String -> String
prettyResult (Left err) = "Error: " ++ err
prettyResult (Right s) = s
