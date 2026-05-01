-- | JSON encoding\/decoding for reference-project types.
module Ref.Codec
  ( encodeItem
  , decodeItem
  , encodeResult
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Ref.Core (ItemId (..), Label (..), Score (..), mkScore)
import Ref.Types (Category (..), Item (..), Priority (..), Result (..))

instance ToJSON ItemId where
  toJSON (ItemId n) = toJSON n

instance FromJSON ItemId where
  parseJSON v = ItemId <$> parseJSON v

instance ToJSON Label where
  toJSON (Label t) = toJSON t

instance FromJSON Label where
  parseJSON v = Label <$> parseJSON v

instance ToJSON Score where
  toJSON (Score d) = toJSON d

instance FromJSON Score where
  parseJSON v = mkScore <$> parseJSON v

instance ToJSON Category where
  toJSON CategoryA = "A"
  toJSON CategoryB = "B"
  toJSON CategoryC = "C"

instance FromJSON Category where
  parseJSON = withText "Category" $ \case
    "A" -> pure CategoryA
    "B" -> pure CategoryB
    "C" -> pure CategoryC
    t   -> fail ("unknown category: " <> T.unpack t)

instance ToJSON Priority where
  toJSON Low      = "low"
  toJSON Medium   = "medium"
  toJSON High     = "high"
  toJSON Critical = "critical"

instance FromJSON Priority where
  parseJSON = withText "Priority" $ \case
    "low"      -> pure Low
    "medium"   -> pure Medium
    "high"     -> pure High
    "critical" -> pure Critical
    t          -> fail ("unknown priority: " <> T.unpack t)

instance ToJSON Item where
  toJSON i = object
    [ "id"       .= itemId       i
    , "label"    .= itemLabel    i
    , "score"    .= itemScore    i
    , "category" .= itemCategory i
    , "priority" .= itemPriority i
    , "tags"     .= itemTags     i
    ]

instance FromJSON Item where
  parseJSON = withObject "Item" $ \o ->
    Item
      <$> o .: "id"
      <*> o .: "label"
      <*> o .: "score"
      <*> o .: "category"
      <*> o .: "priority"
      <*> o .:? "tags" .!= []

-- | Encode an 'Item' to a JSON 'Value'.
encodeItem :: Item -> Value
encodeItem = toJSON

-- | Decode an 'Item' from a JSON 'Value'.
decodeItem :: Value -> Result Item
decodeItem v = case fromJSON v of
  Error   msg -> Failure (T.pack msg)
  Success i   -> Success i

-- | Encode a 'Result' to JSON.
encodeResult :: ToJSON a => Result a -> Value
encodeResult (Success a) = object ["ok" .= True,  "value" .= a]
encodeResult (Failure t) = object ["ok" .= False, "error" .= t]
