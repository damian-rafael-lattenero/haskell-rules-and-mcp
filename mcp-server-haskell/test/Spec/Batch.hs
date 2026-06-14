-- | Unit tests for the @ghc_batch@ tool — JSON-shape parsing (#22: {tool,args}
-- and {name,arguments}), double-wrap prevention (#175), and empty-actions
-- warning (#249).
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions.
module Spec.Batch
  ( testBatchParsesToolArgs
  , testBatchParsesNameArgs
  , testBatchResultNotDoubleWrapped
  , testBatchEmptyActionsWarning
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Aeson (object, (.=))
import Data.Text (Text)

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.Batch as Batch
import HaskellFlows.Tool.Batch (BatchArgs (..), unwrapResult)
import HaskellFlows.Mcp.Protocol (ToolCall (..))

import Spec.Helpers (decodeToolResult)

-- | Issue #22: @ghc_batch@ advertises @{tool, args}@ via its
-- @inputSchema@ — parsing must accept that shape.
testBatchParsesToolArgs :: IO Bool
testBatchParsesToolArgs =
  let raw = object
        [ "actions" .=
            [ object
                [ "tool" .= ("ghc_type" :: Text)
                , "args" .= object [ "expression" .= ("reverse" :: Text) ]
                ]
            ]
        ]
  in case A.fromJSON raw :: A.Result BatchArgs of
       A.Success ba -> case baActions ba of
         [tc] -> pure
           ( tcName tc == "ghc_type"
           && tcArguments tc
                == object [ "expression" .= ("reverse" :: Text) ]
           )
         _ -> pure False
       A.Error _ -> pure False

-- | Issue #22 continued: the MCP-native @{name, arguments}@ shape
-- must also parse (both shapes route through the same dispatcher).
testBatchParsesNameArgs :: IO Bool
testBatchParsesNameArgs =
  let raw = object
        [ "actions" .=
            [ object
                [ "name"      .= ("ghc_eval" :: Text)
                , "arguments" .= object [ "expression" .= ("1+1" :: Text) ]
                ]
            ]
        ]
  in case A.fromJSON raw :: A.Result BatchArgs of
       A.Success ba -> case baActions ba of
         [tc] -> pure (tcName tc == "ghc_eval")
         _    -> pure False
       A.Error _ -> pure False

-- | Issue #175: @ghc_batch@ sub-results must NOT be double-wrapped.
-- 'unwrapResult' must peel off the wire envelope so agents see the
-- domain JSON directly.
testBatchResultNotDoubleWrapped :: IO Bool
testBatchResultNotDoubleWrapped = do
  let innerPayload = object [ "value" .= ("42" :: Text) ]
      tr           = Env.toolResponseToResult (Env.mkOk innerPayload)
  case unwrapResult tr of
    A.Object obj ->
      pure (AKM.member (AKey.fromString "status") obj
         && not (AKM.member (AKey.fromString "content") obj))
    _ -> pure False

-- | Issue #249: @ghc_batch@ with an empty actions list must return
-- status='ok' AND a non-empty 'warning' field.
testBatchEmptyActionsWarning :: IO Bool
testBatchEmptyActionsWarning = do
  let noopDispatch _ = pure (Env.toolResponseToResult (Env.mkOk (A.object [])))
      args = A.object [ "actions" .= ([] :: [A.Value]) ]
  tr <- Batch.runHandle noopDispatch args
  pure $ case decodeToolResult tr of
    Right env ->
         Env.reStatus env == Env.StatusOk
      && case Env.reResult env of
           Just (A.Object obj) ->
             AKM.member (AKey.fromText "warning") obj
             && case AKM.lookup (AKey.fromText "total") obj of
                  Just (A.Number 0) -> True
                  _                 -> False
           _ -> False
    Left _ -> False
