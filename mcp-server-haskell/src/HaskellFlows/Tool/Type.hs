-- | @ghc_type@ — Phase-2 tool (GHC-API migrated).
--
-- Mirrors @mcp-server/src/tools/type-check.ts@: accepts a Haskell
-- expression, returns a parsed @{ expression, type }@ JSON payload.
--
-- Post-migration it calls 'GHC.exprType' via 'withGhcSession' instead
-- of shelling @:t@ into a subprocess ghci. The response schema is
-- unchanged — scenarios that check the JSON shape (@FlowExploratory@,
-- @FlowOversizedInput@, …) still see the same wire format.
--
-- Boundary safety: the expression argument still routes through
-- 'sanitizeExpression', so a newline or the legacy framing sentinel
-- is rejected up-front. In-process GHC wouldn't desync on a newline
-- the way subprocess-ghci did, but preserving the contract keeps the
-- rejection-surface identical to the ghci-backed version.
module HaskellFlows.Tool.Type
  ( descriptor
  , handle
  , queryExprType
  , TypeArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC (Ghc, TcRnExprMode (TM_Inst), exprType)
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (CommandError (..), sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

-- | The schema surfaced to clients via @tools/list@.
descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcType
    , tdDescription =
        "Get the type of a Haskell expression via the GHC API. "
          <> "Use this to verify types of subexpressions before composing them, "
          <> "or to understand what type a function expects/returns."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("The Haskell expression to type-check. Examples: \
                       \\"map (+1)\", \"foldr\", \"Just . show\"" :: Text)
                  ]
              ]
          , "required"             .= ["expression" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype TypeArgs = TypeArgs
  { taExpression :: Text
  }
  deriving stock (Show)

instance FromJSON TypeArgs where
  parseJSON = withObject "TypeArgs" $ \o ->
    TypeArgs <$> o .: "expression"

-- | Handle a @tools/call@ for @ghc_type@.
handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (TypeArgs expr) ->
    case sanitizeExpression expr of
      Left cmdErr ->
        pure (errorResult (formatCommandError cmdErr))
      Right safe -> do
        eRes <- try (withGhcSession ghcSess (queryExprType safe))
        case eRes of
          Left (se :: SomeException) ->
            pure (errorResult (renderGhcException se))
          Right tyText ->
            pure (successResult expr tyText)

-- | Single in-process query: ask GHC for the type, render it.
-- Runs inside a 'withGhcSession' call so the auto-loaded interactive
-- context (Prelude + every module in the module graph) is live.
queryExprType :: Text -> Ghc Text
queryExprType safe = do
  ty <- exprType TM_Inst (T.unpack safe)
  pure (T.pack (showPprUnsafe ty))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Success payload. Schema matches the pre-migration shape: the
-- subprocess-ghci version parsed @":t expr"@ output into @expression@
-- + @type@ via 'HaskellFlows.Parser.Type.parseTypeOutput'; the GHC-API
-- version already has the two halves split, so no parsing needed.
successResult :: Text -> Text -> ToolResult
successResult originalExpr tyRendered =
  let payload = object
        [ "success"    .= True
        , "expression" .= originalExpr
        , "type"       .= tyRendered
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False
        , "error"   .= msg
        ]))
      ]
    , trIsError = True
    }

formatCommandError :: CommandError -> Text
formatCommandError = \case
  ContainsNewline ->
    "expression must be a single line (no newline characters allowed)"
  ContainsSentinel ->
    "expression contains the internal framing sentinel and was rejected"
  EmptyInput ->
    "expression is empty"
  InputTooLarge sz cap ->
    "expression is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

-- | Render a GHC-API exception into a concise user-facing string.
-- The default 'Show' instance for 'SourceError' dumps Haddock-quoted
-- SDoc which is readable-enough for the MCP client; we prepend a
-- hint so the LLM can distinguish compile-time from schema errors.
renderGhcException :: SomeException -> Text
renderGhcException se = T.pack ("expression did not type-check: " <> show se)

-- | UTF-8-safe JSON → Text.
encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
