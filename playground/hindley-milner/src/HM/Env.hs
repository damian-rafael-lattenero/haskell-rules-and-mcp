module HM.Env
  ( TypeEnv(..)
  , emptyEnv
  , extend
  , remove
  , lookupEnv
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Arbitrary(..), listOf, elements, arbitrary)

import HM.Syntax
import HM.Subst

-- | Type environment: maps variable names to their type schemes
newtype TypeEnv = TypeEnv (Map Name Scheme)
  deriving (Show, Eq)

-- | Empty type environment
emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

-- | Extend the environment with a new binding
extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv (Map.insert x s env)

-- | Remove a binding from the environment
remove :: TypeEnv -> Name -> TypeEnv
remove (TypeEnv env) x = TypeEnv (Map.delete x env)

-- | Look up a name in the environment
lookupEnv :: Name -> TypeEnv -> Maybe Scheme
lookupEnv x (TypeEnv env) = Map.lookup x env

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)
  ftv (TypeEnv env) = ftv (Map.elems env)

instance Arbitrary TypeEnv where
  arbitrary = do
    keys <- listOf (elements ["x", "y", "z", "f", "g"])
    vals <- mapM (\_ -> arbitrary) keys
    pure (TypeEnv (Map.fromList (zip keys vals)))
