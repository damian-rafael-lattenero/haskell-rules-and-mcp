module Language.HM.Env
  ( Env(..)
  , emptyEnv
  , lookupEnv
  , extendEnv
  , generalize
  , instantiate
  , freshTyVar
  , freshTyVars
  , InferState(..)
  , initialInferState
  ) where

import Control.Monad (replicateM)
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.HM.Types

newtype Env = Env (Map Name Scheme)
  deriving (Eq, Show)

emptyEnv :: Env
emptyEnv = Env Map.empty

lookupEnv :: Name -> Env -> Maybe Scheme
lookupEnv x (Env m) = Map.lookup x m

extendEnv :: Name -> Scheme -> Env -> Env
extendEnv x s (Env m) = Env (Map.insert x s m)

generalize :: Env -> Type -> Scheme
generalize (Env env) ty = Forall stdVars ty'
  where
    envVars = foldMap (freeTyVars . schemeType) (Map.elems env)
    freeVars = filter (`notElem` envVars) (nubOrd (freeTyVars ty))
    -- Rename free vars to standard names a, b, c...
    stdVars = [ [toEnum (fromEnum 'a' + i)] | i <- [0 .. length freeVars - 1] ]
    subst = Map.fromList (zip freeVars (map TVar stdVars))
    ty' = applySubst subst ty

nubOrd :: Ord a => [a] -> [a]
nubOrd = Map.keys . Map.fromList . map (\x -> (x, ()))

schemeType :: Scheme -> Type
schemeType (Forall _ ty) = ty

freeTyVars :: Type -> [TyVar]
freeTyVars (TVar v)     = [v]
freeTyVars (TCon _)     = []
freeTyVars (TApp t1 t2) = freeTyVars t1 ++ freeTyVars t2

instantiate :: Scheme -> State InferState Type
instantiate (Forall vars ty) = do
  freshNames <- freshTyVars (length vars)
  let subst = Map.fromList (zip vars (map TVar freshNames))
  pure $ applySubst subst ty

applySubst :: Map TyVar Type -> Type -> Type
applySubst s (TVar v)     = Map.findWithDefault (TVar v) v s
applySubst _ (TCon c)     = TCon c
applySubst s (TApp t1 t2) = TApp (applySubst s t1) (applySubst s t2)

data InferState = InferState
  { nextTyVar :: Int
  } deriving (Eq, Show)

initialInferState :: InferState
initialInferState = InferState 0

freshTyVar :: State InferState TyVar
freshTyVar = do
  s <- get
  put s { nextTyVar = nextTyVar s + 1 }
  pure $ "t" ++ show (nextTyVar s)

freshTyVars :: Int -> State InferState [TyVar]
freshTyVars n = replicateM n freshTyVar