module HM.Syntax
  ( Expr(..)
  , Lit(..)
  , Name
  ) where

import Test.QuickCheck (Arbitrary(..), oneof, elements, sized, resize)

type Name = String

data Lit
  = LInt  Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  | Let Name Expr Expr
  | Lit Lit
  | If  Expr Expr Expr
  deriving (Show, Eq, Ord)

instance Arbitrary Lit where
  arbitrary = oneof
    [ LInt  <$> arbitrary
    , LBool <$> arbitrary
    ]

instance Arbitrary Expr where
  arbitrary = sized go
    where
      names = ["x", "y", "z", "f", "g"]
      go 0 = oneof
        [ Var <$> elements names
        , Lit <$> arbitrary
        ]
      go n = oneof
        [ Var <$> elements names
        , Lit <$> arbitrary
        , App <$> sub <*> sub
        , Lam <$> elements names <*> sub
        , Let <$> elements names <*> sub <*> sub
        , If  <$> sub <*> sub <*> sub
        ]
        where sub = resize (n `div` 3) arbitrary
