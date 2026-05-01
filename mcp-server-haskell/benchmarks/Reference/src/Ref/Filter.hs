-- | Filtering operations for the reference project.
module Ref.Filter
  ( byCategory
  , byPriority
  , byScoreRange
  , byTag
  , topN
  ) where

import Data.List (sortOn)
import qualified Data.Ord

import Ref.Core (Score (..), unScore)
import Ref.Types (Category, Item (..), Priority)

-- | Keep only items in the given category.
byCategory :: Category -> [Item] -> [Item]
byCategory cat = filter ((== cat) . itemCategory)

-- | Keep only items at or above the given priority.
byPriority :: Priority -> [Item] -> [Item]
byPriority p = filter ((>= p) . itemPriority)

-- | Keep only items whose score falls in [lo, hi].
byScoreRange :: Double -> Double -> [Item] -> [Item]
byScoreRange lo hi = filter (\i -> s i >= lo && s i <= hi)
  where s = unScore . itemScore

-- | Keep only items that carry a specific tag.
byTag :: String -> [Item] -> [Item]
byTag tag = filter (elem (read tag) . map show . itemTags)

-- | Return the top-N items by score (descending), tie-broken by 'itemId'.
topN :: Int -> [Item] -> [Item]
topN n = take n . sortOn (Data.Ord.Down . itemScore)
