module Language.HM.Infer
  ( infer
  , inferWith
  , InferM
  , runInferM
  , InferError(..)
  ) where

import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.HM.Types
import Language.HM.Env
import Language.HM.Unify
import Language.HM.Error

type InferM = ExceptT InferError (State InferState)

runInferM :: InferM a -> Either InferError (a, InferState)
runInferM m = case runState (runExceptT m) initialInferState of
  (Left err, _) -> Left err
  (Right a, s)  -> Right (a, s)

infer :: Env -> Term -> Either InferError (Subst, Type)
infer env term = case runInferM (inferWith env term) of
  Left err -> Left err
  Right ((subst, ty), _) -> Right (subst, ty)

inferWith :: Env -> Term -> InferM (Subst, Type)
inferWith env term = case term of
  Var x pos -> inferVar env x pos
  Lam x body pos -> inferLam env x body pos
  App f a pos -> inferApp env f a pos
  Let x rhs body pos -> inferLet env x rhs body pos
  Lit lit pos -> inferLit lit pos
  Ann e ty pos -> inferAnn env e ty pos

inferVar :: Env -> Name -> Pos -> InferM (Subst, Type)
inferVar env x pos = do
  case lookupEnv x env of
    Nothing -> throwError (UnboundVar x pos)
    Just scheme -> do
      ty <- lift $ instantiate scheme
      pure (emptySubst, ty)

inferLam :: Env -> Name -> Term -> Pos -> InferM (Subst, Type)
inferLam env x body pos = do
  argTy <- TVar <$> lift freshTyVar
  let env' = extendEnv x (Forall [] argTy) env
  (subst, bodyTy) <- inferWith env' body
  let resTy = TApp (TApp (TCon "->") (applySubst subst argTy)) bodyTy
  pure (subst, resTy)

inferApp :: Env -> Term -> Term -> Pos -> InferM (Subst, Type)
inferApp env f a pos = do
  (subst1, fTy) <- inferWith env f
  (subst2, aTy) <- inferWith (applySubstEnv subst1 env) a
  resTy <- TVar <$> lift freshTyVar
  s3 <- liftUnify $ unify (applySubst subst2 fTy) (TApp (TApp (TCon "->") aTy) resTy)
  let finalSubst = composeSubst s3 (composeSubst subst2 subst1)
  pure (finalSubst, applySubst s3 resTy)

inferLet :: Env -> Name -> Term -> Term -> Pos -> InferM (Subst, Type)
inferLet env x rhs body pos = do
  (subst1, rhsTy) <- inferWith env rhs
  let env' = applySubstEnv subst1 env
  let scheme = generalize env' rhsTy
  let env'' = extendEnv x scheme env'
  (subst2, bodyTy) <- inferWith env'' body
  let finalSubst = composeSubst subst2 subst1
  pure (finalSubst, bodyTy)

inferLit :: Literal -> Pos -> InferM (Subst, Type)
inferLit (LInt _) _ = pure (emptySubst, TCon "Int")
inferLit (LBool _) _ = pure (emptySubst, TCon "Bool")

inferAnn :: Env -> Term -> Type -> Pos -> InferM (Subst, Type)
inferAnn env e expectedTy pos = do
  (subst, inferredTy) <- inferWith env e
  s2 <- liftUnify $ unify (applySubst subst inferredTy) expectedTy
  let finalSubst = composeSubst s2 subst
  pure (finalSubst, applySubst s2 expectedTy)

applySubstEnv :: Subst -> Env -> Env
applySubstEnv s (Env m) = Env (Map.map (applySubstScheme s) m)

liftUnify :: Either UnifyError a -> InferM a
liftUnify (Left e) = throwError (convertUnifyError e)
liftUnify (Right a) = pure a

convertUnifyError :: UnifyError -> InferError
convertUnifyError (OccursCheckFailed v ty) = OccursCheck v ty noPos
convertUnifyError (UnificationFailed t1 t2) = UnificationFail t1 t2 noPos