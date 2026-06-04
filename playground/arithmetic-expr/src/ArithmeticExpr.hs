-- | Arithmetic Expression Evaluator — top-level re-export.
module ArithmeticExpr
  ( module Expr.Syntax
  , module Expr.Eval
  , module Expr.Simplify
  , module Expr.Pretty
  ) where

import Expr.Arbitrary ()   -- bring Arbitrary Expr into scope
import Expr.Eval
import Expr.Pretty
import Expr.Simplify
import Expr.Syntax
