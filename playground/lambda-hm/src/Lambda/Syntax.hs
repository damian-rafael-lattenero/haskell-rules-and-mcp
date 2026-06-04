-- | AST for the lambda calculus with let-bindings and literals.
module Lambda.Syntax
  ( Name
  , Lit (..)
  , Expr (..)
  ) where

-- | Variable name.
type Name = String

-- | Ground literals.
data Lit
  = LInt  Int
  | LBool Bool
  deriving stock (Eq, Show)

-- | Expression AST.
data Expr
  = Var Name            -- ^ Variable reference
  | Lam Name Expr       -- ^ Lambda abstraction: λx. e
  | App Expr Expr       -- ^ Application: f a
  | Let Name Expr Expr  -- ^ Let-binding: let x = e1 in e2
  | Lit Lit             -- ^ Ground literal
  deriving stock (Eq, Show)
