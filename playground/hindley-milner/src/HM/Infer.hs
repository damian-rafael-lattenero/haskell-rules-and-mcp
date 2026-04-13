module HM.Infer
  ( infer
  , inferExpr
  , runInfer
  , Infer
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except

import HM.Syntax
import HM.Subst
import HM.Env
import HM.Unify

-- | The inference monad: error handling + fresh variable supply
type Infer a = ExceptT InferError (State Int) a

-- | Run the inference monad
runInfer :: Infer a -> Either InferError a
runInfer m = evalState (runExceptT m) 0

-- | Generate a fresh type variable
fresh :: Infer Type
fresh = do
  n <- get
  put (n + 1)
  pure (TVar (letters !! n))
  where
    letters = [c : s | s <- "" : map show [1 :: Int ..], c <- ['a'..'z']]

-- | Instantiate a scheme: replace bound variables with fresh ones
instantiate :: Scheme -> Infer Type
instantiate (Forall vars t) = do
  freshVars <- mapM (const fresh) vars
  let s = Subst (Map.fromList (zip vars freshVars))
  pure (apply s t)

-- | Generalize a type over the free variables not in the environment
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vars t
  where
    vars = Set.toList (ftv t `Set.difference` ftv env)

-- | Infer the type of an expression, returning (substitution, type)
inferExpr :: TypeEnv -> Expr -> Infer (Subst, Type)
inferExpr env expr = case expr of

  ELit (LInt _)  -> pure (emptySubst, TCon "Int")
  ELit (LBool _) -> pure (emptySubst, TCon "Bool")

  EVar x -> case lookupEnv x env of
    Nothing -> throwError (UnboundVariable x)
    Just s  -> do
      t <- instantiate s
      pure (emptySubst, t)

  ELam x body -> do
    tv <- fresh
    let env' = extend env (x, Forall [] tv)
    (s1, t1) <- inferExpr env' body
    pure (s1, TArr (apply s1 tv) t1)

  EApp fun arg -> do
    tv <- fresh
    (s1, t1) <- inferExpr env fun
    (s2, t2) <- inferExpr (apply s1 env) arg
    s3 <- liftUnify (apply s2 t1) (TArr t2 tv)
    pure (s3 `compose` s2 `compose` s1, apply s3 tv)

  ELet x e1 e2 -> do
    (s1, t1) <- inferExpr env e1
    let env'  = apply s1 env
        scheme = generalize env' t1
        env'' = extend env' (x, scheme)
    (s2, t2) <- inferExpr env'' e2
    pure (s2 `compose` s1, t2)

  EIf cond thenE elseE -> do
    (s1, t1) <- inferExpr env cond
    (s2, t2) <- inferExpr (apply s1 env) thenE
    (s3, t3) <- inferExpr (apply (s2 `compose` s1) env) elseE
    s4 <- liftUnify (apply (s3 `compose` s2) t1) (TCon "Bool")
    s5 <- liftUnify (apply (s4 `compose` s3) t2) (apply s4 t3)
    let finalSubst = s5 `compose` s4 `compose` s3 `compose` s2 `compose` s1
    pure (finalSubst, apply (s5 `compose` s4 `compose` s3) t2)

-- | Lift unify into the Infer monad
liftUnify :: Type -> Type -> Infer Subst
liftUnify t1 t2 = case unify t1 t2 of
  Left  err -> throwError err
  Right s   -> pure s

-- | Infer the type of a top-level expression
infer :: Expr -> Either InferError Scheme
infer expr = runInfer $ do
  (s, t) <- inferExpr emptyEnv expr
  pure (generalize emptyEnv (apply s t))
