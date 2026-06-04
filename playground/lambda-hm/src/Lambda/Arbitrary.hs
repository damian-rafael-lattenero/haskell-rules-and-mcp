{-# OPTIONS_GHC -Wno-orphans #-}

-- | QuickCheck Arbitrary instances for the lambda calculus AST and types.
module Lambda.Arbitrary where

import Test.QuickCheck
import Lambda.Syntax (Expr (..), Lit (..))
import Lambda.Subst  (Ty (..))

-- | Generate a short lowercase variable name.
genName :: Gen String
genName = elements [ [c] | c <- ['a'..'z'] ]

instance Arbitrary Lit where
  arbitrary = oneof
    [ LInt  <$> arbitrary
    , LBool <$> arbitrary
    ]

instance Arbitrary Expr where
  arbitrary = sized go
    where
      go 0 = oneof
        [ Var <$> genName
        , Lit <$> arbitrary
        ]
      go n = frequency
        [ (2, Var <$> genName)
        , (1, Lam <$> genName <*> go (n `div` 2))
        , (1, App <$> go (n `div` 2) <*> go (n `div` 2))
        , (1, Let <$> genName <*> go (n `div` 2) <*> go (n `div` 2))
        , (2, Lit <$> arbitrary)
        ]

-- | Generate a closed expression (no free variables).
-- Variables are only introduced by Lam/Let and referenced within their scope.
newtype ClosedExpr = ClosedExpr { getClosedExpr :: Expr }
  deriving (Eq, Show)

genClosedExpr :: [String] -> Int -> Gen Expr
genClosedExpr env 0 =
  if null env
    then Lit <$> arbitrary
    else oneof [ Lit <$> arbitrary, Var <$> elements env ]
genClosedExpr env n = frequency
  [ (3, Lit <$> arbitrary)
  , (if null env then 0 else 2, Var <$> elements env)
  , (2, do x <- genName
           body <- genClosedExpr (x : env) (n `div` 2)
           return (Lam x body))
  , (1, App <$> genClosedExpr env (n `div` 2)
            <*> genClosedExpr env (n `div` 2))
  , (1, do x  <- genName
           e1 <- genClosedExpr env       (n `div` 2)
           e2 <- genClosedExpr (x : env) (n `div` 2)
           return (Let x e1 e2))
  ]

instance Arbitrary ClosedExpr where
  arbitrary = ClosedExpr <$> sized (genClosedExpr [])

instance Arbitrary Ty where
  arbitrary = sized go
    where
      genTVar = TVar <$> genName
      go 0 = oneof [ genTVar, TVar . pure <$> elements ['a'..'e'] ]
      go n = frequency
        [ (2, genTVar)
        , (1, TArr <$> go (n `div` 2) <*> go (n `div` 2))
        , (1, elements [TVar "Int", TVar "Bool"])
        ]
