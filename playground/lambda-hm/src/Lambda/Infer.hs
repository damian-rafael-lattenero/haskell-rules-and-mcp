module Lambda.Infer
    ( TypeEnv
    , Subst
    , InferM
    , infer
    , inferScheme
    , runInfer
    , emptyEnv
    , applySubst
    , composeSubst
    , unify
    ) where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Data.List (nub, (\\))

import Lambda.Syntax

-- ---------------------------------------------------------------------------
-- Substitution

type Subst = Map.Map Name Type

emptySubst :: Subst
emptySubst = Map.empty

applySubst :: Subst -> Type -> Type
applySubst s (TVar a)   = Map.findWithDefault (TVar a) a s
applySubst _ TInt        = TInt
applySubst _ TBool       = TBool
applySubst s (TArr a b) = TArr (applySubst s a) (applySubst s b)

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Forall vs t) =
    Forall vs (applySubst (foldr Map.delete s vs) t)

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1

-- ---------------------------------------------------------------------------
-- Type environment

type TypeEnv = Map.Map Name Scheme

emptyEnv :: TypeEnv
emptyEnv = Map.empty

applySubstEnv :: Subst -> TypeEnv -> TypeEnv
applySubstEnv s = Map.map (applySubstScheme s)

freeInEnv :: TypeEnv -> Set.Set Name
freeInEnv env = Set.fromList $ concatMap freeTVarsScheme (Map.elems env)

-- ---------------------------------------------------------------------------
-- Inference monad

type InferM a = ExceptT String (State Int) a

runInfer :: InferM a -> Either String a
runInfer m = evalState (runExceptT m) 0

fresh :: InferM Type
fresh = do
    n <- get
    put (n + 1)
    return (TVar ("t" ++ show n))

-- ---------------------------------------------------------------------------
-- Unification

occursIn :: Name -> Type -> Bool
occursIn a (TVar b)   = a == b
occursIn _ TInt        = False
occursIn _ TBool       = False
occursIn a (TArr x y) = occursIn a x || occursIn a y

unify :: Type -> Type -> InferM Subst
unify (TVar a) t
    | TVar a == t = return emptySubst
    | occursIn a t = throwError ("occurs check failed: " ++ a ++ " in " ++ show t)
    | otherwise    = return (Map.singleton a t)
unify t (TVar a) = unify (TVar a) t
unify TInt  TInt  = return emptySubst
unify TBool TBool = return emptySubst
unify (TArr a1 b1) (TArr a2 b2) = do
    s1 <- unify a1 a2
    s2 <- unify (applySubst s1 b1) (applySubst s1 b2)
    return (composeSubst s2 s1)
unify t1 t2 = throwError ("cannot unify " ++ show t1 ++ " with " ++ show t2)

-- ---------------------------------------------------------------------------
-- Generalisation / instantiation

generalise :: TypeEnv -> Type -> Scheme
generalise env t =
    let vs = nub (freeTVars t) \\ Set.toList (freeInEnv env)
    in  Forall vs t

instantiate :: Scheme -> InferM Type
instantiate (Forall vs t) = do
    freshVars <- mapM (const fresh) vs
    let s = Map.fromList (zip vs freshVars)
    return (applySubst s t)

-- ---------------------------------------------------------------------------
-- Algorithm W

infer :: TypeEnv -> Term -> InferM (Subst, Type)
infer env (Var x) =
    case Map.lookup x env of
        Nothing -> throwError ("unbound variable: " ++ x)
        Just sc -> do
            t <- instantiate sc
            return (emptySubst, t)

infer env (Lam x body) = do
    tv <- fresh
    let env' = Map.insert x (Forall [] tv) env
    (s, t) <- infer env' body
    return (s, TArr (applySubst s tv) t)

infer env (App t1 t2) = do
    tv <- fresh
    (s1, ty1) <- infer env t1
    (s2, ty2) <- infer (applySubstEnv s1 env) t2
    s3 <- unify (applySubst s2 ty1) (TArr ty2 tv)
    return (composeSubst s3 (composeSubst s2 s1), applySubst s3 tv)

infer env (Let x t1 t2) = do
    (s1, ty1) <- infer env t1
    let env'  = applySubstEnv s1 env
        sc    = generalise env' ty1
        env'' = Map.insert x sc env'
    (s2, ty2) <- infer env'' t2
    return (composeSubst s2 s1, ty2)

infer _   (Lit (LInt  _)) = return (emptySubst, TInt)
infer _   (Lit (LBool _)) = return (emptySubst, TBool)

-- | Infer the most general type scheme for a closed term
inferScheme :: TypeEnv -> Term -> Either String Scheme
inferScheme env t = runInfer $ do
    (s, ty) <- infer env t
    let ty' = applySubst s ty
        env' = applySubstEnv s env
    return (generalise env' ty')
