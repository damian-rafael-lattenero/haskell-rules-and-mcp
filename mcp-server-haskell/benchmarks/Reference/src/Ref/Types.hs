-- | Algebraic data types for the reference project.
module Ref.Types
  ( Category (..)
  , Priority (..)
  , Item (..)
  , Result (..)
  , isSuccess
  ) where

import Data.Text (Text)
import Ref.Core (ItemId, Label, Score)

data Category
  = CategoryA
  | CategoryB
  | CategoryC
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Priority
  = Low
  | Medium
  | High
  | Critical
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Item = Item
  { itemId       :: !ItemId
  , itemLabel    :: !Label
  , itemScore    :: !Score
  , itemCategory :: !Category
  , itemPriority :: !Priority
  , itemTags     :: ![Text]
  }
  deriving stock (Eq, Show)

data Result a
  = Success !a
  | Failure !Text
  deriving stock (Eq, Show, Functor)

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess (Failure _) = False
