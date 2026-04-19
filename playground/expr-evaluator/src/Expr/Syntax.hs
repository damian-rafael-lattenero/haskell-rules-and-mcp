{- |
Module      : Expr.Syntax
Description : AST, errors and environment for the arithmetic language.
-}
module Expr.Syntax (
    Expr (..),
    Error (..),
    Env,
    emptyEnv,
    extendEnv,
    lookupVar,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Test.QuickCheck (Arbitrary (..), elements, oneof, resize, sized)

data Expr
    = Lit Int
    | Var String
    | Neg Expr
    | Add Expr Expr
    | Mul Expr Expr
    deriving (Eq, Show)

newtype Error
    = UnboundVariable String
    deriving (Eq, Show)

type Env = Map String Int

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: String -> Int -> Env -> Env
extendEnv = Map.insert

lookupVar :: String -> Env -> Either Error Int
lookupVar name env = case Map.lookup name env of
    Just v -> Right v
    Nothing -> Left (UnboundVariable name)

instance Arbitrary Expr where
    arbitrary = sized go
      where
        go 0 =
            oneof
                [ Lit <$> arbitrary
                , Var <$> elements ["x", "y", "z", "foo", "bar"]
                ]
        go n =
            let sub = resize (n `div` 3) arbitrary
             in oneof
                    [ Lit <$> arbitrary
                    , Var <$> elements ["x", "y", "z", "foo", "bar"]
                    , Neg <$> sub
                    , Add <$> sub <*> sub
                    , Mul <$> sub <*> sub
                    ]

instance Arbitrary Error where
    arbitrary = UnboundVariable <$> elements ["x", "y", "z", "foo", "bar"]
