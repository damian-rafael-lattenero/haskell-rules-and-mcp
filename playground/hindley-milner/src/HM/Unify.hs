module HM.Unify
  ( unify
  , TypeError(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import HM.Types

data TypeError
  = UnificationFail Type Type
  | InfiniteType TVar Type
  | UnboundVariable String
  deriving (Show, Eq)

unify :: Type -> Type -> Either TypeError Subst
unify (TFun l1 r1) (TFun l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  pure (composeSubst s2 s1)
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify (TCon a) (TCon b)
  | a == b    = pure emptySubst
  | otherwise = Left (UnificationFail (TCon a) (TCon b))
unify t1 t2 = Left (UnificationFail t1 t2)

bind :: TVar -> Type -> Either TypeError Subst
bind v t
  | t == TVar v        = pure emptySubst
  | v `Set.member` ftv t = Left (InfiniteType v t)
  | otherwise          = pure (Map.singleton v t)
