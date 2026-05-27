-- | Re-export facade for the arithmetic expression evaluator.
module ArithmeticExpr
    ( module Expr.Syntax
    , module Expr.Eval
    , module Expr.Simplify
    , module Expr.Pretty
    ) where

import Expr.Syntax
import Expr.Eval
import Expr.Simplify
import Expr.Pretty
