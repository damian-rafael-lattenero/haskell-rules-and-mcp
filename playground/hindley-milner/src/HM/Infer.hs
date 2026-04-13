module HM.Infer
  ( infer
  , inferExpr
  , InferState(..)
  , runInfer
  , fresh
  , instantiate
  ) where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Except
import HM.Syntax
import HM.Types
import HM.Env
import HM.Unify

newtype InferState = InferState { count :: Int }
  deriving (Show)

type Infer a = ExceptT TypeError (State InferState) a

initState :: InferState
initState = InferState 0

runInfer :: Infer a -> Either TypeError a
runInfer m = evalState (runExceptT m) initState

fresh :: Infer Type
fresh = do
  s <- get
  put s { count = count s + 1 }
  let name = letters !! count s
  pure (TVar name)
  where
    letters = [l : n | n <- "" : map show [(1 :: Int)..], l <- ['a'..'z']]

instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
  vs' <- mapM (const fresh) vs
  let s = Map.fromList (zip vs vs')
  pure (apply s t)

inferLit :: Lit -> Type
inferLit (LInt _)  = typeInt
inferLit (LBool _) = typeBool

inferExpr :: TypeEnv -> Expr -> Infer (Subst, Type)
inferExpr _   (Lit lit) = pure (emptySubst, inferLit lit)
inferExpr env (Var x) =
  case envLookup x env of
    Nothing -> throwError (UnboundVariable x)
    Just sc -> do
      t <- instantiate sc
      pure (emptySubst, t)
inferExpr env (Lam x body) = do
  tv <- fresh
  let env' = extend (remove env x) (x, Forall [] tv)
  (s1, t1) <- inferExpr env' body
  pure (s1, TFun (apply s1 tv) t1)
inferExpr env (App e1 e2) = do
  tv <- fresh
  (s1, t1) <- inferExpr env e1
  (s2, t2) <- inferExpr (apply s1 env) e2
  s3 <- liftEither (unify (apply s2 t1) (TFun t2 tv))
  pure (composeSubst s3 (composeSubst s2 s1), apply s3 tv)
inferExpr env (Let x e1 e2) = do
  (s1, t1) <- inferExpr env e1
  let env' = apply s1 env
      sc   = generalize env' t1
  (s2, t2) <- inferExpr (extend env' (x, sc)) e2
  pure (composeSubst s2 s1, t2)
inferExpr env (If cond tr fl) = do
  (s1, t1) <- inferExpr env cond
  (s2, t2) <- inferExpr (apply s1 env) tr
  (s3, t3) <- inferExpr (apply (composeSubst s2 s1) env) fl
  s4 <- liftEither (unify (apply (composeSubst s3 s2) t1) typeBool)
  s5 <- liftEither (unify (apply s4 t3) (apply (composeSubst s4 s3) t2))
  pure (composeSubst s5 (composeSubst s4 (composeSubst s3 (composeSubst s2 s1))), apply s5 (apply s4 t3))

infer :: TypeEnv -> Expr -> Either TypeError Type
infer env expr = runInfer $ do
  (s, t) <- inferExpr env expr
  pure (apply s t)
