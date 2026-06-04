-- | Lambda calculus with Hindley-Milner type inference — top-level re-export.
module LambdaHm
  ( module Lambda.Syntax
  , module Lambda.Subst
  , module Lambda.Unify
  , module Lambda.Infer
  , module Lambda.Pretty
  , module Lambda.Eval
  ) where

import Lambda.Arbitrary ()   -- bring Arbitrary instances into scope

import Lambda.Eval
import Lambda.Infer
import Lambda.Pretty
import Lambda.Subst
import Lambda.Syntax
import Lambda.Unify
