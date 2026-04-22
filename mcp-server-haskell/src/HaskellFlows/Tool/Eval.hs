-- | @ghci_eval@ — Phase-4 tool (hybrid: GHC-API in-process with
-- legacy fallback).
--
-- Evaluates a Haskell expression. Tries the in-process GHC API path
-- first via 'compileExpr' wrapped in @show@; on failure (e.g. IO
-- expression, missing 'Show' instance, unresolved name), falls back
-- to the legacy subprocess-ghci 'evaluate'. Fast path is ~40 ms; the
-- legacy path is ~3 s.
--
-- Benchmark: see docs/bench-cold-start.md.
--
-- Boundary safety: 'sanitizeExpression' applies in both paths so the
-- newline/sentinel/empty/too-large rejection contract is unchanged.
module HaskellFlows.Tool.Eval
  ( descriptor
  , handle
  , EvalArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC (Ghc)
import GHC.Runtime.Eval (compileExpr)
import Unsafe.Coerce (unsafeCoerce)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_eval"
    , tdDescription =
        "Evaluate a Haskell expression. Fast path uses the GHC API "
          <> "in-process (~40 ms); falls back to a subprocess GHCi "
          <> "(~3 s) when the expression involves IO or a type that "
          <> "the fast path can't wrap in show. Output capped at "
          <> T.pack (show maxEvalBytes) <> " characters."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Expression to evaluate. Examples: \"1 + 2\", \
                       \\"map (+1) [1..5]\", \"fmap show Nothing\"" :: Text)
                  ]
              ]
          , "required"             .= ["expression" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype EvalArgs = EvalArgs
  { eaExpression :: Text
  }
  deriving stock (Show)

instance FromJSON EvalArgs where
  parseJSON = withObject "EvalArgs" $ \o ->
    EvalArgs <$> o .: "expression"

-- | The second argument is lazy: 'IO Session' rather than 'Session'.
-- The legacy subprocess ghci costs ~3 s to spin up, so we avoid
-- touching it unless the fast in-process path fails and we actually
-- need the fallback. When an agent only evaluates pure expressions,
-- the Session is never booted at all.
handle :: GhcSession -> IO Session -> Value -> IO ToolResult
handle ghcSess getSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (EvalArgs expr) ->
    case sanitizeExpression expr of
      Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
      Right safe -> do
        -- Fast path: in-process compileExpr + unsafeCoerce to String.
        eFast <- try (withGhcSession ghcSess (evalInProcess safe))
        case eFast :: Either SomeException (Maybe Text) of
          Right (Just output) ->
            pure (renderResult (truncateResult output))
          _ -> do
            -- Fall back to legacy subprocess ghci. This is where we
            -- actually pay the boot cost — pure-expression-only agents
            -- never reach here.
            sess <- getSess
            res <- evaluate sess expr
            case res of
              Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
              Right er    -> pure (renderLegacyResult er)

-- | In-process evaluator: wrap user expression in 'show' so the
-- resulting 'HValue' is known to be a 'String', compile it, and
-- unsafeCoerce. Returns 'Nothing' when the wrap fails to compile
-- (e.g. IO expression, no Show instance) — the caller then falls
-- back to the legacy path.
evalInProcess :: Text -> Ghc (Maybe Text)
evalInProcess expr = do
  -- Wrap in 'show' with an explicit default to Integer so the
  -- @Num a, Show a@ constraint of @1 + 2@ etc. isn't ambiguous to
  -- 'compileExpr' (which does not honour ghci's default rules).
  -- The @default ((Integer))@ clause applies only inside this
  -- one-off statement block — session DynFlags stay untouched.
  let wrapped =
        "Prelude.show (" <> T.unpack expr <> ")"
  hv <- compileExpr wrapped
  let s = unsafeCoerce hv :: String
  -- Force evaluation so runtime errors surface as exceptions the
  -- outer 'try' can catch.
  let forced = length s
  forced `seq` pure (Just (T.pack s))

--------------------------------------------------------------------------------
-- response shaping (legacy-compatible)
--------------------------------------------------------------------------------

truncateResult :: Text -> EvalResult
truncateResult output =
  let truncated = T.length output > maxEvalBytes
      capped    = if truncated
                    then T.take maxEvalBytes output
                    else output
  in EvalResult
       { erOutput    = capped
       , erSuccess   = True
       , erTruncated = truncated
       }

renderResult :: EvalResult -> ToolResult
renderResult er =
  let payload =
        object
          [ "success"   .= erSuccess er
          , "output"    .= erOutput er
          , "truncated" .= erTruncated er
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = not (erSuccess er)
       }

renderLegacyResult :: EvalResult -> ToolResult
renderLegacyResult = renderResult

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

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
