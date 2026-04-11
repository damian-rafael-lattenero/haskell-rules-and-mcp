module HM.Unify
  ( TypeError(..)
  , unify
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import HM.Syntax
import HM.Subst

-- | Type errors that can occur during unification or inference
data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable Name
  deriving (Show, Eq)

-- | Unify two types, producing a substitution or an error
unify :: Type -> Type -> Either TypeError Subst
unify (TArr l1 r1) (TArr l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  pure (composeSubst s2 s1)
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify (TCon a) (TCon b)
  | a == b    = pure nullSubst
  | otherwise = Left (UnificationFail (TCon a) (TCon b))
unify t1 t2 = Left (UnificationFail t1 t2)

-- | Bind a type variable to a type, with occurs check
bind :: TVar -> Type -> Either TypeError Subst
bind v t
  | t == TVar v       = pure nullSubst
  | v `Set.member` ftv t = Left (InfiniteType v t)
  | otherwise         = pure (Map.singleton v t)
