module Language.HM.Unify
  ( Subst
  , emptySubst
  , composeSubst
  , applySubst
  , applySubstScheme
  , unify
  , unifyMany
  , occursCheck
  , UnifyError(..)
  ) where

import Control.Monad (foldM)
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.HM.Types

type Subst = Map TyVar Type

emptySubst :: Subst
emptySubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1

applySubst :: Subst -> Type -> Type
applySubst s (TVar v)     = Map.findWithDefault (TVar v) v s
applySubst _ (TCon c)     = TCon c
applySubst s (TApp t1 t2) = TApp (applySubst s t1) (applySubst s t2)

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Forall vars ty) = Forall vars (applySubst s' ty)
  where
    s' = foldr Map.delete s vars

occursCheck :: TyVar -> Type -> Bool
occursCheck v (TVar v')     = v == v'
occursCheck _ (TCon _)      = False
occursCheck v (TApp t1 t2)  = occursCheck v t1 || occursCheck v t2

data UnifyError
  = OccursCheckFailed TyVar Type
  | UnificationFailed Type Type
  deriving (Eq, Show)

unify :: Type -> Type -> Either UnifyError Subst
unify t1 t2 = unify' t1 t2 emptySubst

unify' :: Type -> Type -> Subst -> Either UnifyError Subst
unify' (TVar v) t s
  | TVar v == t = Right s
  | occursCheck v t = Left (OccursCheckFailed v t)
  | otherwise = Right (Map.insert v t s)
unify' t (TVar v) s = unify' (TVar v) t s
unify' (TCon c1) (TCon c2) s
  | c1 == c2 = Right s
  | otherwise = Left (UnificationFailed (TCon c1) (TCon c2))
unify' (TApp f1 a1) (TApp f2 a2) s = do
  s1 <- unify' f1 f2 s
  unify' (applySubst s1 a1) (applySubst s1 a2) s1
unify' t1 t2 _ = Left (UnificationFailed t1 t2)

unifyMany :: [(Type, Type)] -> Either UnifyError Subst
unifyMany pairs = foldM (\(s :: Subst) (t1, t2) -> do
  s' <- unify t1 t2
  pure (composeSubst s' s)
  ) emptySubst pairs