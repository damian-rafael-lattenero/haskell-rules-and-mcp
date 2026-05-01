-- | Validation logic for the reference project.
module Ref.Validate
  ( ValidationError (..)
  , validateItem
  , validateScore
  , validateTags
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Ref.Core (Score (..), unScore)
import Ref.Types (Item (..), Result (..))

data ValidationError
  = ScoreOutOfRange !Double
  | LabelEmpty
  | TooManyTags !Int
  | TagTooLong !Text
  deriving stock (Eq, Show)

-- | Validate an 'Item' and return a structured error on failure.
validateItem :: Item -> Result Item
validateItem item
  | T.null (T.strip label) = Failure "label is empty"
  | score < 0 || score > 100 =
      Failure ("score " <> T.pack (show score) <> " is out of range [0,100]")
  | length tags > 20 =
      Failure ("too many tags: " <> T.pack (show (length tags)))
  | otherwise = Success item
  where
    label = T.pack (show (itemLabel item))
    score = unScore (itemScore item)
    tags  = itemTags item

-- | Validate a raw 'Score' value.
validateScore :: Double -> Either ValidationError Score
validateScore d
  | d < 0 || d > 100 = Left (ScoreOutOfRange d)
  | otherwise         = Right (minBound { unScore = d })
  where
    -- trick to construct Score without re-exporting the constructor
    minBound :: Score
    minBound = Score 0

-- | Validate a list of tag strings.
validateTags :: [Text] -> Either ValidationError [Text]
validateTags ts
  | length ts > 20 = Left (TooManyTags (length ts))
  | Just long <- findLong ts = Left (TagTooLong long)
  | otherwise = Right ts
  where
    findLong = foldr (\t acc -> if T.length t > 64 then Just t else acc) Nothing
