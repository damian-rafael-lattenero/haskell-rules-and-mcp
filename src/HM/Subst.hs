module HM.Subst
  ( Subst
  , Substitutable(..)
  , nullSubst
  , composeSubst
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HM.Syntax

-- | A substitution maps type variables to types
type Subst = Map.Map TVar Type

-- | The empty substitution
nullSubst :: Subst
nullSubst = Map.empty

-- | Compose two substitutions: apply s1 then s2
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- | Things that support substitution and free type variables
class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply _ (TCon c)     = TCon c
  apply s t@(TVar v)   = Map.findWithDefault t v s
  apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)

  ftv (TCon _)     = Set.empty
  ftv (TVar v)     = Set.singleton v
  ftv (TArr t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall vs t) = Forall vs (apply s' t)
    where s' = foldr Map.delete s vs

  ftv (Forall vs t) = ftv t `Set.difference` Set.fromList vs

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty
