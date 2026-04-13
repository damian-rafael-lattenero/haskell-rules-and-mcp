module HM.Subst
  ( Subst(..)
  , Substitutable(..)
  , emptySubst
  , compose
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.QuickCheck (Arbitrary(..), elements, listOf, arbitrary)

import HM.Syntax

-- | A substitution maps type variables to types
newtype Subst = Subst (Map TVar Type)
  deriving (Show, Eq)

-- | Empty substitution
emptySubst :: Subst
emptySubst = Subst Map.empty

-- | Compose two substitutions: apply s1 then s2
-- (s2 `compose` s1) means: first apply s1, then apply s2
compose :: Subst -> Subst -> Subst
compose s2@(Subst m2) (Subst m1) =
  Subst (Map.map (apply s2) m1 `Map.union` m2)

-- | Things that can have substitutions applied and free type variables extracted
class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set TVar

instance Substitutable Type where
  apply (Subst s) t = case t of
    TVar a   -> Map.findWithDefault t a s
    TCon c   -> TCon c
    TArr l r -> TArr (apply (Subst s) l) (apply (Subst s) r)

  ftv (TVar a)   = Set.singleton a
  ftv (TCon _)   = Set.empty
  ftv (TArr l r) = ftv l `Set.union` ftv r

instance Substitutable Scheme where
  apply (Subst s) (Forall vars t) =
    Forall vars (apply (Subst (foldr Map.delete s vars)) t)

  ftv (Forall vars t) = ftv t `Set.difference` Set.fromList vars

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

instance Arbitrary Subst where
  arbitrary = do
    keys <- listOf (elements ["a", "b", "c", "d"])
    vals <- mapM (\_ -> arbitrary) keys
    pure (Subst (Map.fromList (zip keys vals)))
