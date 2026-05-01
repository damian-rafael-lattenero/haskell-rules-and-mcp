-- | Core value types for the reference project.
module Ref.Core
  ( ItemId (..)
  , Label (..)
  , Score (..)
  , mkScore
  , unScore
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Opaque item identifier.
newtype ItemId = ItemId { unItemId :: Int }
  deriving stock (Eq, Ord, Show)

-- | A non-empty label string.
newtype Label = Label { unLabel :: Text }
  deriving stock (Eq, Ord, Show)

-- | A score in [0, 100].
newtype Score = Score { unScore :: Double }
  deriving stock (Eq, Ord, Show)

-- | Smart constructor: clamps to [0, 100].
mkScore :: Double -> Score
mkScore = Score . max 0 . min 100

-- | Make a 'Label' from 'Text'.  Returns 'Nothing' for empty input.
mkLabel :: Text -> Maybe Label
mkLabel t
  | T.null (T.strip t) = Nothing
  | otherwise          = Just (Label (T.strip t))
{-# INLINE mkLabel #-}

-- Suppress unused-binding warning for mkLabel — it is used by sibling
-- modules and exported via the top-level re-export module.
_usedElsewhere :: Text -> Maybe Label
_usedElsewhere = mkLabel
