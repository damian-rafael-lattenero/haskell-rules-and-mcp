module Expr.Syntax
  ( Expr (..)
  , Error (..)
  , Env
  , emptyEnv
  , extendEnv
  , lookupEnv
  , depth
  , size
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data Expr
  = Lit !Int
  | Var !String
  | Neg !Expr
  | Add !Expr !Expr
  | Mul !Expr !Expr
  deriving stock (Eq, Show)

data Error
  = UnboundVar !String
  | Overflow
  | DepthExceeded
  | SizeExceeded
  | ParseError
  deriving stock (Eq, Show)

type Env = Map String Int

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: String -> Int -> Env -> Env
extendEnv = Map.insert

lookupEnv :: String -> Env -> Maybe Int
lookupEnv = Map.lookup

depth :: Expr -> Int
depth = \case
  Lit _     -> 1
  Var _     -> 1
  Neg e     -> 1 + depth e
  Add l r   -> 1 + max (depth l) (depth r)
  Mul l r   -> 1 + max (depth l) (depth r)

size :: Expr -> Int
size = \case
  Lit _     -> 1
  Var _     -> 1
  Neg e     -> 1 + size e
  Add l r   -> 1 + size l + size r
  Mul l r   -> 1 + size l + size r
