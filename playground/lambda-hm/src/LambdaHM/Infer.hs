module LambdaHM.Infer
  ( infer
  , inferExpr
  , TypeError (..)
  , Env
  , emptyEnv
  , extendEnv
  , defaultEnv
  ) where

import Control.Monad.State
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LambdaHM.Syntax

-- Substitution: maps type variables to types
type Subst = Map TyVar Type

emptySubst :: Subst
emptySubst = Map.empty

-- Type environment: maps term variables to type schemes
newtype Env = Env (Map Name Scheme)
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty

extendEnv :: Name -> Scheme -> Env -> Env
extendEnv x s (Env m) = Env (Map.insert x s m)

lookupEnv :: Name -> Env -> Maybe Scheme
lookupEnv x (Env m) = Map.lookup x m

data TypeError
  = UnboundVariable Name
  | UnificationFail Type Type
  | InfiniteType TyVar Type
  deriving (Show, Eq)

-- Inference monad: State for fresh variable supply + Either for errors
type Infer a = StateT Int (Either TypeError) a

fresh :: Infer Type
fresh = do
  n <- get
  put (n + 1)
  return (TVar n)

throwError :: TypeError -> Infer a
throwError = lift . Left

-- Apply substitution to a type
applySubst :: Subst -> Type -> Type
applySubst s (TVar v) = Map.findWithDefault (TVar v) v s
applySubst s (TArr t1 t2) = TArr (applySubst s t1) (applySubst s t2)
applySubst _ (TCon c) = TCon c

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Forall vs t) =
  let s' = foldr Map.delete s vs
  in Forall vs (applySubst s' t)

applySubstEnv :: Subst -> Env -> Env
applySubstEnv s (Env m) = Env (Map.map (applySubstScheme s) m)

-- Free type variables
ftvType :: Type -> [TyVar]
ftvType (TVar v) = [v]
ftvType (TArr t1 t2) = nub (ftvType t1 ++ ftvType t2)
ftvType (TCon _) = []

ftvScheme :: Scheme -> [TyVar]
ftvScheme (Forall vs t) = filter (`notElem` vs) (ftvType t)

ftvEnv :: Env -> [TyVar]
ftvEnv (Env m) = nub (concatMap ftvScheme (Map.elems m))

-- Compose substitutions: apply s1 after s2
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1

-- Unification (Robinson's algorithm)
unify :: Type -> Type -> Either TypeError Subst
unify (TCon a) (TCon b)
  | a == b = Right emptySubst
unify (TVar v) t = bindVar v t
unify t (TVar v) = bindVar v t
unify (TArr l1 r1) (TArr l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (applySubst s1 r1) (applySubst s1 r2)
  return (composeSubst s2 s1)
unify t1 t2 = Left (UnificationFail t1 t2)

bindVar :: TyVar -> Type -> Either TypeError Subst
bindVar v (TVar v') | v == v' = Right emptySubst
bindVar v t
  | v `elem` ftvType t = Left (InfiniteType v t)
  | otherwise = Right (Map.singleton v t)

-- Generalise a type over variables free in the type but not in the env
generalize :: Env -> Type -> Scheme
generalize env t =
  let vs = filter (`notElem` ftvEnv env) (ftvType t)
  in Forall vs t

-- Instantiate a scheme with fresh type variables
instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
  freshVars <- mapM (const fresh) vs
  let s = Map.fromList (zip vs (map id freshVars))
      unwrap (TVar n) = Map.findWithDefault (TVar n) n s
      unwrap (TArr a b) = TArr (unwrap a) (unwrap b)
      unwrap c@(TCon _) = c
  return (unwrap t)

-- Core inference: returns (substitution, inferred type)
infer :: Env -> Expr -> Infer (Subst, Type)
infer _ (Lit (LInt _)) = return (emptySubst, tInt)
infer _ (Lit (LBool _)) = return (emptySubst, tBool)

infer env (Var x) =
  case lookupEnv x env of
    Nothing -> throwError (UnboundVariable x)
    Just sc -> do
      t <- instantiate sc
      return (emptySubst, t)

infer env (Lam x body) = do
  tv <- fresh
  let env' = extendEnv x (Forall [] tv) env
  (s, t) <- infer env' body
  return (s, TArr (applySubst s tv) t)

infer env (App f arg) = do
  tv <- fresh
  (s1, tf) <- infer env f
  (s2, ta) <- infer (applySubstEnv s1 env) arg
  s3 <- lift $ unify (applySubst s2 tf) (TArr ta tv)
  return (composeSubst s3 (composeSubst s2 s1), applySubst s3 tv)

infer env (Let x e body) = do
  (s1, te) <- infer env e
  let env' = applySubstEnv s1 env
      sc = generalize env' te
      env'' = extendEnv x sc env'
  (s2, tb) <- infer env'' body
  return (composeSubst s2 s1, tb)

infer env (If cond t f) = do
  (s1, tc) <- infer env cond
  s2 <- lift $ unify (applySubst s1 tc) tBool
  let s12 = composeSubst s2 s1
  (s3, tt) <- infer (applySubstEnv s12 env) t
  let s123 = composeSubst s3 s12
  (s4, tf') <- infer (applySubstEnv s123 env) f
  s5 <- lift $ unify (applySubst s4 tt) tf'
  let sfinal = composeSubst s5 (composeSubst s4 s123)
  return (sfinal, applySubst s5 tf')

-- Top-level entry: infer and apply substitution
inferExpr :: Env -> Expr -> Either TypeError Type
inferExpr env expr =
  case runStateT (infer env expr) 0 of
    Left err -> Left err
    Right ((s, t), _) -> Right (applySubst s t)

-- Default environment with arithmetic primitives
defaultEnv :: Env
defaultEnv = Env $ Map.fromList
  [ ("add",  Forall [] (TArr tInt (TArr tInt tInt)))
  , ("sub",  Forall [] (TArr tInt (TArr tInt tInt)))
  , ("mul",  Forall [] (TArr tInt (TArr tInt tInt)))
  , ("eq",   Forall [0] (TArr (TVar 0) (TArr (TVar 0) tBool)))
  , ("not",  Forall [] (TArr tBool tBool))
  , ("and",  Forall [] (TArr tBool (TArr tBool tBool)))
  , ("or",   Forall [] (TArr tBool (TArr tBool tBool)))
  , ("zero", Forall [] tInt)
  , ("true", Forall [] tBool)
  , ("false",Forall [] tBool)
  ]
