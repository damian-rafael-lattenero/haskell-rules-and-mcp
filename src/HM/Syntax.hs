module HM.Syntax
  ( Expr(..)
  , Lit(..)
  , Type(..)
  , Scheme(..)
  , TVar
  , Name
  ) where

-- | Variable and type variable names
type Name = String
type TVar = String

-- | Literal values
data Lit
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)

-- | Expression AST
data Expr
  = EVar Name              -- ^ Variable reference
  | ELit Lit               -- ^ Literal value
  | EApp Expr Expr         -- ^ Function application
  | ELam Name Expr         -- ^ Lambda abstraction
  | ELet Name Expr Expr    -- ^ Let binding: let x = e1 in e2
  | EIf Expr Expr Expr     -- ^ If-then-else
  deriving (Show, Eq)

-- | Monotypes
data Type
  = TVar TVar              -- ^ Type variable: a, b, ...
  | TCon String            -- ^ Type constructor: Int, Bool
  | TArr Type Type         -- ^ Function type: a -> b
  deriving (Show, Eq, Ord)

-- | Polytypes (type schemes): forall a b. Type
data Scheme = Forall [TVar] Type
  deriving (Show, Eq)
