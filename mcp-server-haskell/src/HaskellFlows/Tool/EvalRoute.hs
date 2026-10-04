-- | Descriptor for @ghc_eval@.
--
-- Execution lives in "HaskellFlows.Tool.IdeBacked" ('handleEval'):
-- Server dispatch routes GhcEval through 'IdeBacked.routeIde' BEFORE
-- the registry handler is consulted, so @handle@ here is an unreachable
-- backstop — it exists so the registry stays exhaustive and the
-- tools/list schema keeps a home next to its tool.
module HaskellFlows.Tool.EvalRoute
  ( descriptor
  , handle
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import HaskellFlows.Mcp.Envelope
  ( ErrorKind (InternalError)
  , ToolResponse
  , mkErrorEnvelope
  , mkFailed
  )
import HaskellFlows.Mcp.Protocol (ToolDescriptor (..))
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName = toolNameText GhcEval
    , tdDescription =
        "PURPOSE: Evaluate a single Haskell expression in-process via the "
          <> "ghcide session. "
          <> "WHEN: checking a value, a pure function result, or an IO "
          <> "action's output mid-session. "
          <> "WHEN NOT: ghc_property(action=check) to test a property over "
          <> "many inputs; ghc_inspect(action=type) for just the type. "
          <> "PREREQUISITES: none — the session auto-boots on first use and "
          <> "anchors on the project's modules. "
          <> "OUTPUT: {result} — show-wrapped for pure exprs, IO String "
          <> "otherwise, plus the anchor module that scoped it. "
          <> "SEE ALSO: ghc_inspect(action=type), ghc_property."
    , tdInputSchema =
        object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .=
                      ("Expression to evaluate. Examples: \"1 + 2\", \
                       \\"map (+1) [1..5]\", \"fmap show Nothing\"" :: Text)
                  ]
              ]
          , "required" .= ["expression" :: Text]
          , "additionalProperties" .= False
          ]
    }

-- | Unreachable: 'IdeBacked.routeIde' serves every GhcEval call.
handle :: env -> Value -> IO ToolResponse
handle _ _ =
  pure
    ( mkFailed
        ( mkErrorEnvelope
            InternalError
            ( "ghc_eval is served by the in-process ghcide route; "
                <> "reaching this handler is a dispatch regression"
            )
        )
    )
