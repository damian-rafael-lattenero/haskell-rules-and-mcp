-- | Top-level re-export module for the reference project.
--
-- Clients can import just @Ref@ to get the most commonly used types
-- and functions, without pulling in the full module hierarchy.
module Ref
  ( -- * Core types
    ItemId (..)
  , Label (..)
  , Score (..)
  , mkScore
    -- * ADTs
  , Category (..)
  , Priority (..)
  , Item (..)
  , Result (..)
  , isSuccess
    -- * Codec
  , encodeItem
  , decodeItem
  , encodeResult
    -- * Parse
  , parseTags
  , parseScore
  , parsePriority
    -- * Validate
  , ValidationError (..)
  , validateItem
    -- * Transform
  , normaliseScore
  , boostPriority
  , addTag
  , removeTags
    -- * Filter
  , byCategory
  , byPriority
  , byScoreRange
  , topN
    -- * Stats
  , mean
  , median
  , stdDev
  , scoreHistogram
    -- * Util
  , chunksOf
  , deduplicate
  , safeHead
  , safeLast
  ) where

import Ref.Codec   (decodeItem, encodeItem, encodeResult)
import Ref.Core    (ItemId (..), Label (..), Score (..), mkScore)
import Ref.Filter  (byCategory, byPriority, byScoreRange, topN)
import Ref.Parse   (parsePriority, parseScore, parseTags)
import Ref.Stats   (mean, median, scoreHistogram, stdDev)
import Ref.Transform (addTag, boostPriority, normaliseScore, removeTags)
import Ref.Types   (Category (..), Item (..), Priority (..), Result (..), isSuccess)
import Ref.Util    (chunksOf, deduplicate, safeHead, safeLast)
import Ref.Validate (ValidationError (..), validateItem)
