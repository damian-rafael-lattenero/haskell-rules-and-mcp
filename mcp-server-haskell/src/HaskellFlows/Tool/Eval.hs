-- | @ghci_eval@ — Wave-5 full in-process.
--
-- Evaluates a Haskell expression. Tries the fast path first: wrap in
-- @show@, 'compileExpr', @unsafeCoerce@ to 'String'. If that fails
-- (expression is of type @IO a@, @IO String@ in particular, or the
-- show-wrap doesn't typecheck), re-try as an @IO String@-typed
-- statement via 'evalIOString'. Both paths are in-process; the
-- legacy subprocess ghci is gone.
--
-- Boundary safety: 'sanitizeExpression' rejects the
-- newline/sentinel/empty/too-large inputs before any compile.
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

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , evalIOString
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , maxEvalBytes
  , sanitizeExpression
  )
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_eval"
    , tdDescription =
        "Evaluate a Haskell expression in-process via the GHC API. "
          <> "Tries @show@-wrapped compileExpr first (for pure expressions), "
          <> "falls back to an IO String interpretation (for actions that "
          <> "already return a string, or expressions in the IO monad). "
          <> "Output capped at "
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

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (EvalArgs expr) ->
    case sanitizeExpression expr of
      Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
      Right safe -> do
        -- Fast path: show-wrap + compileExpr + unsafeCoerce to String.
        eFast <- try (withGhcSession ghcSess (evalShowPure safe))
        case eFast :: Either SomeException (Maybe Text) of
          Right (Just out) -> pure (renderOk (truncateOutput out))
          _ -> do
            -- Second path: treat as an IO String statement. Works for
            -- expressions like `getLine`, `fmap show (readFile "…")`,
            -- and anything returning `IO String` already.
            eIO <- try (withGhcSession ghcSess (evalIOString (T.unpack safe)))
            case eIO :: Either SomeException String of
              Right out ->
                pure (renderOk (truncateOutput (T.pack out)))
              Left ex ->
                pure (errorResult (T.pack (show ex)))

-- | Fast path: wrap user expr in 'show', compile, coerce. Returns
-- 'Nothing' if the wrap fails to compile (typically an IO
-- expression) so the caller can fall through to 'evalIOString'.
evalShowPure :: Text -> Ghc (Maybe Text)
evalShowPure expr = do
  let wrapped = "Prelude.show (" <> T.unpack expr <> ")"
  hv <- compileExpr wrapped
  let s = unsafeCoerce hv :: String
  length s `seq` pure (Just (T.pack s))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

data TruncatedOutput = TruncatedOutput
  { toOutput    :: !Text
  , toTruncated :: !Bool
  }

truncateOutput :: Text -> TruncatedOutput
truncateOutput output =
  let truncated = T.length output > maxEvalBytes
      capped    = if truncated
                    then T.take maxEvalBytes output
                    else output
  in TruncatedOutput { toOutput = capped, toTruncated = truncated }

renderOk :: TruncatedOutput -> ToolResult
renderOk t =
  let payload =
        object
          [ "success"   .= True
          , "output"    .= toOutput t
          , "truncated" .= toTruncated t
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

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
