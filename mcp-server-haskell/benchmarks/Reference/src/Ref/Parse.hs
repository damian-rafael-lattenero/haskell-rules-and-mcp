-- | Text parsing utilities for the reference project.
module Ref.Parse
  ( parseTags
  , parseScore
  , parsePriority
  , splitOn
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import Ref.Core (Score, mkScore)
import Ref.Types (Priority (..))

-- | Split a comma-separated tag string into a list of trimmed labels.
-- Empty segments are silently dropped.
--
-- >>> parseTags "foo , bar , baz"
-- ["foo","bar","baz"]
parseTags :: Text -> [Text]
parseTags = filter (not . T.null) . map T.strip . T.splitOn ","

-- | Parse a score from a 'Text' decimal representation.
-- Returns 'Nothing' for non-numeric or out-of-range input.
parseScore :: Text -> Maybe Score
parseScore t = case readMaybe (T.unpack t) :: Maybe Double of
  Nothing -> Nothing
  Just d  -> if d < 0 || d > 100 then Nothing else Just (mkScore d)

-- | Parse a 'Priority' from its lower-case text form.
parsePriority :: Text -> Maybe Priority
parsePriority t = case T.toLower (T.strip t) of
  "low"      -> Just Low
  "medium"   -> Just Medium
  "high"     -> Just High
  "critical" -> Just Critical
  _          -> Nothing

-- | Split 'Text' on a delimiter, returning non-empty segments.
splitOn :: Text -> Text -> [Text]
splitOn delim = filter (not . T.null) . T.splitOn delim
