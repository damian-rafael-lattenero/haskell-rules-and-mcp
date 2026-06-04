-- | Hindley-Milner type inference (Algorithm W).
module Lambda.Infer
  ( TypeError (..)
  , Env
  , infer
  , typeOf
  ) where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set

import Lambda.Syntax  (Expr (..), Lit (..), Name)
import Lambda.Subst
import Lambda.Unify   (UnifyError, unify)

-- | Typing environment: maps term variables to type schemes.
type Env = Map Name Scheme

data TypeError
  = UnboundVariable Name
  | UnificationError UnifyError
  deriving stock (Eq, Show)

-- | Inference monad: fresh-variable counter + error propagation.
type Infer a = StateT Int (Either TypeError) a

-- | Generate a fresh type variable.
fresh :: Infer Ty
fresh = do
  n <- get
  put (n + 1)
  return (TVar ("a" <> show n))

-- | Replace bound variables with fresh ones.
instantiate :: Scheme -> Infer Ty
instantiate (Forall vs t) = do
  freshVs <- mapM (const fresh) vs
  let s = Map.fromList (zip vs freshVs)
  return (applySubst s t)

-- | Close over a type w.r.t. an environment — bind free vars not in env.
generalise :: Env -> Ty -> Scheme
generalise env t =
  let fvT   = freeVars t
      fvEnv = freeVarsEnv env
      bound = Set.toList (fvT `Set.difference` fvEnv)
  in  Forall bound t

-- | Algorithm W: return (most-general substitution, inferred type).
infer :: Env -> Expr -> Infer (Subst, Ty)
infer _   (Lit (LInt _))  = return (emptySubst, TCon "Int")
infer _   (Lit (LBool _)) = return (emptySubst, TCon "Bool")

infer env (Var x) =
  case Map.lookup x env of
    Nothing  -> lift (Left (UnboundVariable x))
    Just sch -> do
      t <- instantiate sch
      return (emptySubst, t)

infer env (Lam x body) = do
  tv <- fresh
  let env' = Map.insert x (Forall [] tv) env
  (s, t) <- infer env' body
  return (s, TArr (applySubst s tv) t)

infer env (App f arg) = do
  tv         <- fresh
  (s1, tf)   <- infer env f
  (s2, targ) <- infer (Map.map (applyScheme s1) env) arg
  s3         <- liftUnify (applySubst s2 tf) (TArr targ tv)
  let s = composeSubst s3 (composeSubst s2 s1)
  return (s, applySubst s3 tv)

infer env (Let x e1 e2) = do
  (s1, t1) <- infer env e1
  let env'  = Map.map (applyScheme s1) env
      sch   = generalise env' t1
      env'' = Map.insert x sch env'
  (s2, t2) <- infer env'' e2
  return (composeSubst s2 s1, t2)

liftUnify :: Ty -> Ty -> Infer Subst
liftUnify t1 t2 =
  lift $ either (Left . UnificationError) Right (unify t1 t2)

-- | Infer the type of a closed expression.
typeOf :: Expr -> Either TypeError Ty
typeOf e = do
  (s, t) <- evalStateT (infer Map.empty e) 0
  return (applySubst s t)
