module Language.HM.Types
  ( Type(..)
  , Scheme(..)
  , Term(..)
  , Literal(..)
  , TyVar
  , TyCon
  , Name
  , Pos(..)
  , HasPos(..)
  , noPos
  ) where

type TyVar = String
type TyCon = String
type Name  = String

data Type
  = TVar TyVar
  | TCon TyCon
  | TApp Type Type
  deriving (Eq, Ord, Show)

data Scheme = Forall [TyVar] Type
  deriving (Eq, Ord, Show)

data Literal
  = LInt Integer
  | LBool Bool
  deriving (Eq, Ord, Show)

data Pos = Pos
  { posLine   :: Int
  , posColumn :: Int
  , posOffset :: Int
  } deriving (Eq, Ord, Show)

noPos :: Pos
noPos = Pos 0 0 0

class HasPos a where
  getPos :: a -> Pos

instance HasPos Pos where
  getPos = id

data Term
  = Var Name Pos
  | Lam Name Term Pos
  | App Term Term Pos
  | Let Name Term Term Pos
  | Lit Literal Pos
  | Ann Term Type Pos
  deriving (Eq, Show)

instance HasPos Term where
  getPos (Var _ p)     = p
  getPos (Lam _ _ p)   = p
  getPos (App _ _ p)   = p
  getPos (Let _ _ _ p) = p
  getPos (Lit _ p)     = p
  getPos (Ann _ _ p)   = p