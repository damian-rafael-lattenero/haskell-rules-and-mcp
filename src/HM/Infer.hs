module HM.Infer
  ( TypeEnv
  , inferExpr
  , runInfer
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

import Control.Monad.Except
import Control.Monad.State

import HM.Syntax
import HM.Subst
import HM.Unify

-- | Type environment: maps variable names to type schemes
type TypeEnv = Map.Map Name Scheme

-- | Inference monad: state for fresh vars + errors
type Infer a = ExceptT TypeError (State Int) a

-- | Run inference on an expression, returning its type or an error
runInfer :: Expr -> Either TypeError Scheme
runInfer expr = inferExpr Map.empty expr

-- | Generate a fresh type variable
fresh :: Infer Type
fresh = do
  n <- get
  put (n + 1)
  pure (TVar ("t" ++ show n))

-- | Instantiate a scheme: replace bound vars with fresh vars
instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
  freshVars <- mapM (const fresh) vs
  let s = Map.fromList (zip vs freshVars)
  pure (apply s t)

-- | Generalize a type over the free vars not in the environment
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall (Set.toList freeVars) t
  where freeVars = ftv t `Set.difference` ftv (Map.elems env)

-- | Extend the environment with a new binding
extend :: TypeEnv -> (Name, Scheme) -> TypeEnv
extend env (x, sc) = Map.insert x sc env

-- | Top-level: infer type of expression in a given environment
inferExpr :: TypeEnv -> Expr -> Either TypeError Scheme
inferExpr env expr =
  case evalState (runExceptT (infer env expr)) 0 of
    Left err      -> Left err
    Right (s, t)  -> Right (generalize Map.empty (apply s t))

-- | Infer the type of an expression in a given environment
infer :: TypeEnv -> Expr -> Infer (Subst, Type)

infer _env (ELit (LInt _))  = pure (nullSubst, TCon "Int")
infer _env (ELit (LBool _)) = pure (nullSubst, TCon "Bool")

infer env (EVar x) =
  case Map.lookup x env of
    Nothing -> throwError (UnboundVariable x)
    Just sc -> do
      t <- instantiate sc
      pure (nullSubst, t)

infer env (ELam x body) = do
  tv <- fresh
  let env' = extend env (x, Forall [] tv)
  (s1, t1) <- infer env' body
  pure (s1, TArr (apply s1 tv) t1)

infer env (EApp func arg) = do
  tv <- fresh
  (s1, t1) <- infer env func
  (s2, t2) <- infer (apply s1 <$> env) arg
  s3 <- liftEither (unify (apply s2 t1) (TArr t2 tv))
  pure (composeSubst s3 (composeSubst s2 s1), apply s3 tv)

infer env (ELet x e1 e2) = do
  (s1, t1) <- infer env e1
  let env' = apply s1 <$> env
      sc   = generalize env' t1
  (s2, t2) <- infer (extend env' (x, sc)) e2
  pure (composeSubst s2 s1, t2)

infer env (EIf cond thn els) = do
  (s1, t1) <- infer env cond
  (s2, t2) <- infer (apply s1 <$> env) thn
  (s3, t3) <- infer (apply (composeSubst s2 s1) <$> env) els
  s4 <- liftEither (unify t1 (TCon "Bool"))
  s5 <- liftEither (unify (apply s4 t2) (apply s4 t3))
  let s = foldr1 composeSubst [s5, s4, s3, s2, s1]
  pure (s, apply s5 t2)

infer env (ELetRec x e1 e2) = do
  tv <- fresh
  let env' = extend env (x, Forall [] tv)
  (s1, t1) <- infer env' e1
  s2 <- liftEither (unify (apply s1 tv) t1)
  let s = composeSubst s2 s1
      env'' = apply s <$> env
      sc = generalize env'' (apply s tv)
  (s3, t2) <- infer (extend env'' (x, sc)) e2
  pure (composeSubst s3 s, t2)

infer env (EPair e1 e2) = do
  (s1, t1) <- infer env e1
  (s2, t2) <- infer (apply s1 <$> env) e2
  pure (composeSubst s2 s1, TProd (apply s2 t1) t2)

infer env (EFst e) = do
  tv1 <- fresh
  tv2 <- fresh
  (s1, t1) <- infer env e
  s2 <- liftEither (unify t1 (TProd tv1 tv2))
  pure (composeSubst s2 s1, apply s2 tv1)

infer env (ESnd e) = do
  tv1 <- fresh
  tv2 <- fresh
  (s1, t1) <- infer env e
  s2 <- liftEither (unify t1 (TProd tv1 tv2))
  pure (composeSubst s2 s1, apply s2 tv2)

infer env (EAnn e annTy) = do
  (s1, t1) <- infer env e
  s2 <- liftEither (unify t1 annTy)
  pure (composeSubst s2 s1, apply s2 annTy)
