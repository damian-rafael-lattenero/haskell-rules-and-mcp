-- | Robinson unification for HM type inference.
module Lambda.Unify
  ( UnifyError (..)
  , unify
  ) where

import qualified Data.Set as Set

import Lambda.Subst
  ( TVar, Ty (..), Subst
  , emptySubst, singleSubst, composeSubst
  , applySubst, freeVars
  )

data UnifyError
  = OccursCheck TVar Ty   -- ^ v occurs free in t
  | TypeMismatch Ty Ty    -- ^ incompatible constructors
  deriving stock (Eq, Show)

-- | Most-general unifier: find Subst s such that applySubst s t1 == applySubst s t2.
unify :: Ty -> Ty -> Either UnifyError Subst
unify (TVar v) t                  = bindVar v t
unify t        (TVar v)           = bindVar v t
unify (TCon a) (TCon b)
  | a == b                        = Right emptySubst
unify (TArr l1 r1) (TArr l2 r2)  = do
  s1 <- unify l1 l2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  Right (composeSubst s2 s1)
unify t1 t2                       = Left (TypeMismatch t1 t2)

bindVar :: TVar -> Ty -> Either UnifyError Subst
bindVar v (TVar w) | v == w = Right emptySubst
bindVar v t
  | v `Set.member` freeVars t    = Left (OccursCheck v t)
  | otherwise                     = Right (singleSubst v t)
