module HM.Syntax
  ( Name
  , Lit(..)
  , Expr(..)
  , Type(..)
  , Scheme(..)
  , TVar
  , InferError(..)
  ) where

import Test.QuickCheck (Arbitrary(..), oneof, sized, resize, elements)

-- | Variable names
type Name = String

-- | Type variable names
type TVar = String

-- | Literal values
data Lit
  = LInt  Integer
  | LBool Bool
  deriving (Show, Eq, Ord)

-- | Expressions in the simply-typed lambda calculus with let-polymorphism
data Expr
  = EVar  Name           -- ^ Variable reference
  | ELit  Lit            -- ^ Literal value
  | EApp  Expr Expr      -- ^ Function application
  | ELam  Name Expr      -- ^ Lambda abstraction
  | ELet  Name Expr Expr -- ^ Let binding (polymorphic)
  | EIf   Expr Expr Expr -- ^ If-then-else
  deriving (Show, Eq)

-- | Monomorphic types
data Type
  = TVar  TVar         -- ^ Type variable
  | TCon  String       -- ^ Type constructor (Int, Bool)
  | TArr  Type Type    -- ^ Function type (a -> b)
  deriving (Show, Eq, Ord)

-- | Polymorphic type scheme: forall a1 a2 ... an. type
data Scheme = Forall [TVar] Type
  deriving (Show, Eq)

-- | Type inference errors
data InferError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Name
  deriving (Show, Eq)

-- Arbitrary instances for QuickCheck

instance Arbitrary Lit where
  arbitrary = oneof
    [ LInt  <$> arbitrary
    , LBool <$> arbitrary
    ]

instance Arbitrary Type where
  arbitrary = sized go
    where
      go 0 = oneof
        [ TVar <$> elements ["a", "b", "c", "d"]
        , TCon <$> elements ["Int", "Bool"]
        ]
      go n = oneof
        [ TVar <$> elements ["a", "b", "c", "d"]
        , TCon <$> elements ["Int", "Bool"]
        , TArr <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
        ]

instance Arbitrary Scheme where
  arbitrary = Forall <$> subsetOf ["a", "b", "c"] <*> arbitrary
    where
      subsetOf xs = do
        flags <- mapM (\_ -> arbitrary) xs
        pure [x | (x, True) <- zip xs (flags :: [Bool])]

instance Arbitrary Expr where
  arbitrary = sized go
    where
      names = ["x", "y", "z", "f", "g"]
      go 0 = oneof
        [ EVar <$> elements names
        , ELit <$> arbitrary
        ]
      go n = oneof
        [ EVar <$> elements names
        , ELit <$> arbitrary
        , EApp <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
        , ELam <$> elements names <*> resize (n - 1) arbitrary
        , ELet <$> elements names <*> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
        , EIf  <$> resize (n `div` 3) arbitrary <*> resize (n `div` 3) arbitrary <*> resize (n `div` 3) arbitrary
        ]

instance Arbitrary InferError where
  arbitrary = oneof
    [ UnificationFail <$> arbitrary <*> arbitrary
    , InfiniteType <$> elements ["a", "b", "c"] <*> arbitrary
    , UnboundVariable <$> elements ["x", "y", "z", "unknown"]
    ]
