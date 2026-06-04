-- | Types and substitutions for HM type inference.
module Lambda.Subst
  ( TVar
  , Ty (..)
  , Scheme (..)
  , Subst
  , emptySubst
  , singleSubst
  , composeSubst
  , applySubst
  , applyScheme
  , freeVars
  , freeVarsScheme
  , freeVarsEnv
  ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

-- | Type variable name.
type TVar = String

-- | Monotypes.
data Ty
  = TVar TVar      -- ^ Type variable
  | TArr Ty Ty     -- ^ Function type: a → b
  | TCon String    -- ^ Type constructor: Int, Bool
  deriving stock (Eq, Show)

-- | Polytypes (type schemes): ∀ a₁…aₙ. τ
data Scheme = Forall [TVar] Ty
  deriving stock (Eq, Show)

-- | Substitution: finite map from type variables to types.
type Subst = Map TVar Ty

emptySubst :: Subst
emptySubst = Map.empty

singleSubst :: TVar -> Ty -> Subst
singleSubst = Map.singleton

-- | Apply a substitution to a monotype.
applySubst :: Subst -> Ty -> Ty
applySubst s (TVar v)     = Map.findWithDefault (TVar v) v s
applySubst s (TArr t1 t2) = TArr (applySubst s t1) (applySubst s t2)
applySubst _ (TCon c)     = TCon c

-- | Compose: @composeSubst s1 s2@ applies s2 first, then s1.
--
-- Law: applySubst (composeSubst s1 s2) t = applySubst s1 (applySubst s2 t)
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (applySubst s1) s2 `Map.union` s1

-- | Free type variables of a monotype.
freeVars :: Ty -> Set TVar
freeVars (TVar v)     = Set.singleton v
freeVars (TArr t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars (TCon _)     = Set.empty

-- | Apply a substitution to a type scheme (avoids capturing bound vars).
applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (Forall vs t) =
  let s' = foldr Map.delete s vs
  in  Forall vs (applySubst s' t)

-- | Free type variables of a scheme.
freeVarsScheme :: Scheme -> Set TVar
freeVarsScheme (Forall vs t) =
  freeVars t `Set.difference` Set.fromList vs

-- | Free type variables of a typing environment.
freeVarsEnv :: Map String Scheme -> Set TVar
freeVarsEnv = foldMap freeVarsScheme
