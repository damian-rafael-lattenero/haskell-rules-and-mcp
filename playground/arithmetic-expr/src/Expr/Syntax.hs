-- | Core syntax types for the arithmetic expression evaluator.
module Expr.Syntax
  ( Expr (..)
  , Error (..)
  , Env
  ) where

import qualified Data.Map.Strict as Map

-- | Arithmetic expression AST.
data Expr
  = Lit Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  deriving stock (Eq, Show)

-- | Evaluation errors.
data Error
  = UnboundVariable String
  | DivisionByZero
  deriving stock (Eq, Show)

-- | Variable binding environment.
type Env = Map.Map String Int
