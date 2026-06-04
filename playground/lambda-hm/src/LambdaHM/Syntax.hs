module LambdaHM.Syntax where

type Name = String

data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq)

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  | Lit Lit
  | If Expr Expr Expr
  deriving (Show, Eq)

type TyVar = Int

data Type
  = TVar TyVar
  | TArr Type Type
  | TCon Name
  deriving (Show, Eq)

data Scheme = Forall [TyVar] Type
  deriving (Show, Eq)

tInt :: Type
tInt = TCon "Int"

tBool :: Type
tBool = TCon "Bool"
