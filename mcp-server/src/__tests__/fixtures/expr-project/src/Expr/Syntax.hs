-- |
-- Module      : Expr.Syntax
-- Description : AST, errors and environment for the arithmetic expression language.
--
-- Uses 'Int' for the value domain — platform-sized, so very large computations
-- can overflow silently. For an evaluator used only in tests this is acceptable;
-- to harden, swap 'Int' for 'Integer' or add an overflow-checking wrapper.
module Expr.Syntax
  ( Expr (..)
  , Error (..)
  , Env
  , emptyEnv
  , extendEnv
  , lookupVar
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Test.QuickCheck (Arbitrary (..), elements, oneof, resize, sized)

-- | Arithmetic expression tree.
data Expr
  = Lit Int
  | Var String
  | Neg Expr
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show)

-- | Evaluation errors. Closed sum so pattern matches are exhaustive with -Wall.
data Error
  = UnboundVariable String
  deriving (Eq, Show)

-- | Variable environment.
type Env = Map String Int

emptyEnv :: Env
emptyEnv = Map.empty

-- | Extend the environment with a fresh binding. Latest write wins.
extendEnv :: String -> Int -> Env -> Env
extendEnv = Map.insert

-- | Lookup with a structured error instead of a partial 'Maybe'.
lookupVar :: String -> Env -> Either Error Int
lookupVar name env = case Map.lookup name env of
  Just v  -> Right v
  Nothing -> Left (UnboundVariable name)

-- Arbitrary instances kept alongside the types so QuickCheck tests don't need
-- orphan instances. Variable names come from a small pool so generated
-- expressions have a realistic chance of hitting an extended 'Env'.
instance Arbitrary Expr where
  arbitrary = sized go
    where
      go 0 = oneof
        [ Lit <$> arbitrary
        , Var <$> elements ["x", "y", "z", "foo", "bar"]
        ]
      go n = let sub = resize (n `div` 3) arbitrary
             in oneof
        [ Lit <$> arbitrary
        , Var <$> elements ["x", "y", "z", "foo", "bar"]
        , Neg <$> sub
        , Add <$> sub <*> sub
        , Mul <$> sub <*> sub
        ]

instance Arbitrary Error where
  arbitrary = UnboundVariable <$> elements ["x", "y", "z", "foo", "bar"]
