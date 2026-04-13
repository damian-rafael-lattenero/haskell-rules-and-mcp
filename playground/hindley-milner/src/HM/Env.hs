module HM.Env
  ( TypeEnv(..)
  , emptyEnv
  , extend
  , remove
  , envLookup
  , generalize
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import HM.Types

newtype TypeEnv = TypeEnv (Map.Map String Scheme)
  deriving (Show, Eq)

instance Substitutable TypeEnv where
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)
  ftv (TypeEnv env) = ftv (Map.elems env)

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

extend :: TypeEnv -> (String, Scheme) -> TypeEnv
extend (TypeEnv env) (x, sc) = TypeEnv (Map.insert x sc env)

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) x = TypeEnv (Map.delete x env)

envLookup :: String -> TypeEnv -> Maybe Scheme
envLookup x (TypeEnv env) = Map.lookup x env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall (Set.toList vs) t
  where vs = ftv t `Set.difference` ftv env
