{-# OPTIONS_GHC -Wno-orphans #-}

module Expr.Arbitrary where

import Test.QuickCheck

import Expr.Syntax (Expr (..))

instance Arbitrary Expr where
  arbitrary = sized go
    where
      go 0 = oneof
        [ Lit <$> arbitrary
        , Var <$> (listOf1 (elements ['a'..'z']))
        ]
      go n = frequency
        [ (2, Lit <$> arbitrary)
        , (2, Var <$> listOf1 (elements ['a'..'z']))
        , (1, Add <$> go (n `div` 2) <*> go (n `div` 2))
        , (1, Mul <$> go (n `div` 2) <*> go (n `div` 2))
        , (1, Neg <$> go (n `div` 2))
        ]
