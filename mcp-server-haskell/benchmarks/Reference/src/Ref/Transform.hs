-- | Data transformation functions for the reference project.
module Ref.Transform
  ( normaliseScore
  , boostPriority
  , addTag
  , removeTags
  , mapItems
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Ref.Core (Score (..), mkScore, unScore)
import Ref.Types (Item (..), Priority (..))

-- | Normalise all scores in a list to the range [lo, hi] using
-- min-max scaling.  Returns the list unchanged if all scores are equal.
normaliseScore :: Double -> Double -> [Item] -> [Item]
normaliseScore lo hi items =
  let scores  = map (unScore . itemScore) items
      minS    = minimum scores
      maxS    = maximum scores
      range   = maxS - minS
  in if range == 0
       then items
       else map (rescale minS range) items
  where
    rescale minS range item =
      let raw    = unScore (itemScore item)
          scaled = lo + (raw - minS) / range * (hi - lo)
      in item { itemScore = mkScore scaled }

-- | Upgrade the priority of an item if its score exceeds a threshold.
boostPriority :: Double -> Item -> Item
boostPriority threshold item
  | unScore (itemScore item) >= threshold
  , itemPriority item < Critical
  = item { itemPriority = succ (itemPriority item) }
  | otherwise = item

-- | Add a tag to an item, deduplicating.
addTag :: Text -> Item -> Item
addTag tag item
  | tag `elem` itemTags item = item
  | otherwise                = item { itemTags = tag : itemTags item }

-- | Remove all tags matching a predicate.
removeTags :: (Text -> Bool) -> Item -> Item
removeTags p item = item { itemTags = filter (not . p) (itemTags item) }

-- | Map a function over a list of items.
mapItems :: (Item -> Item) -> [Item] -> [Item]
mapItems = map
