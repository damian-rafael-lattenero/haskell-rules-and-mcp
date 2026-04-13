module HM.Unify
  ( unify
  , occursIn
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import HM.Syntax
import HM.Subst

-- | Unify two types, producing a substitution or an error
unify :: Type -> Type -> Either InferError Subst
unify (TArr l1 r1) (TArr l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  pure (s2 `compose` s1)
unify (TVar a) t = bind a t
unify t (TVar a) = bind a t
unify (TCon a) (TCon b)
  | a == b    = Right emptySubst
  | otherwise = Left (UnificationFail (TCon a) (TCon b))
unify t1 t2 = Left (UnificationFail t1 t2)

-- | Bind a type variable to a type, with occurs check
bind :: TVar -> Type -> Either InferError Subst
bind a t
  | t == TVar a    = Right emptySubst
  | a `occursIn` t = Left (InfiniteType a t)
  | otherwise      = Right (Subst (Map.singleton a t))

-- | Check if a type variable occurs in a type (prevents infinite types)
occursIn :: TVar -> Type -> Bool
occursIn a t = a `Set.member` ftv t
