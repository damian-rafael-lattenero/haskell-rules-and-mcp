module HM.Types
  ( Type(..)
  , Scheme(..)
  , TVar
  , Subst
  , Substitutable(..)
  , emptySubst
  , composeSubst
  , typeInt
  , typeBool
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import Test.QuickCheck (Arbitrary(..), oneof, elements, sized, resize)

type TVar = String

data Type
  = TVar TVar
  | TCon String
  | TFun Type Type
  deriving (Show, Eq, Ord)

data Scheme = Forall [TVar] Type
  deriving (Show, Eq, Ord)

type Subst = Map.Map TVar Type

emptySubst :: Subst
emptySubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set TVar

instance Substitutable Type where
  apply s t@(TVar v)   = Map.findWithDefault t v s
  apply _ t@(TCon _)   = t
  apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)

  ftv (TVar v)     = Set.singleton v
  ftv (TCon _)     = Set.empty
  ftv (TFun t1 t2) = ftv t1 `Set.union` ftv t2

instance Substitutable Scheme where
  apply s (Forall vs t) = Forall vs (apply s' t)
    where s' = foldr Map.delete s vs
  ftv (Forall vs t) = ftv t `Set.difference` Set.fromList vs

instance Substitutable a => Substitutable [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

typeInt :: Type
typeInt = TCon "Int"

typeBool :: Type
typeBool = TCon "Bool"

instance Arbitrary Type where
  arbitrary = sized go
    where
      vars = ["a", "b", "c", "d"]
      go 0 = oneof
        [ TVar <$> elements vars
        , pure typeInt
        , pure typeBool
        ]
      go n = oneof
        [ TVar <$> elements vars
        , pure typeInt
        , pure typeBool
        , TFun <$> sub <*> sub
        ]
        where sub = resize (n `div` 2) arbitrary

instance Arbitrary Scheme where
  arbitrary = do
    t <- arbitrary
    let vs = Set.toList (ftv t)
    pure (Forall vs t)
