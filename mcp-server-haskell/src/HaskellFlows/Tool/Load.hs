-- | @ghci_load@ — Phase-3 tool (GHC-API migrated).
--
-- Loads the project in-process via 'loadAndCaptureDiagnostics' and
-- returns a JSON summary of captured errors + warnings. Schema kept
-- compatible with the pre-migration shape so scenarios that check
-- @{success, errors, warnings, summary}@ stay green.
--
-- Scope delta from the legacy tool:
--
-- * @module_path@ is still validated through 'mkModulePath' (security:
--   path-traversal refusal preserved). The in-process backend always
--   re-loads the full project rather than a single file — the auto-load
--   enumerates @src\/@ + @app\/@ and loads everything. This is wider
--   than @:l src\/Foo.hs@ but cheap because GHC skips unchanged modules,
--   and for MCP usage the distinction rarely matters.
--
-- * @diagnostics=true@ flips the load flavour from 'Strict' to
--   'Deferred' — enables @-fdefer-type-errors -fdefer-typed-holes@ so
--   hole / deferred-error warnings appear in the returned list.
module HaskellFlows.Tool.Load
  ( descriptor
  , handle
  , LoadArgs (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (Deferred, Strict)
  , loadAndCaptureDiagnostics
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import HaskellFlows.Types

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_load"
    , tdDescription =
        "Load or reload Haskell modules via the GHC API. Returns structured "
          <> "compilation errors and warnings. Pass diagnostics=true to "
          <> "enable a deferred pass (-fdefer-type-errors -fdefer-typed-holes) "
          <> "that surfaces holes and deferred-type-error warnings."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Path to a module to load, relative to the project \
                       \directory. Omit to reload current modules. NOTE: "
                    <> "post-migration the backend always re-loads the full \
                       \project tree; the path is still validated for "
                    <> "traversal safety but doesn't narrow the load." :: Text)
                  ]
              , "diagnostics" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("When true, runs in deferred mode to surface typed \
                       \holes and deferred-type-error warnings. Default: false."
                       :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data LoadArgs = LoadArgs
  { laModulePath  :: !(Maybe Text)
  , laDiagnostics :: !Bool
  }
  deriving stock (Show)

instance FromJSON LoadArgs where
  parseJSON = withObject "LoadArgs" $ \o -> do
    mp <- o .:? "module_path"
    dx <- o .:? "diagnostics" .!= False
    pure LoadArgs { laModulePath = mp, laDiagnostics = dx }

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (LoadArgs mPath useDeferred) ->
    case validatePath pd mPath of
      Left errMsg -> pure (errorResult errMsg)
      Right () -> do
        let flavour = if useDeferred then Deferred else Strict
        (success, diags) <- loadAndCaptureDiagnostics ghcSess flavour
        pure (okResult success diags)

-- | Security gate: reject paths that would escape the project. We
-- don't USE the resolved path (the in-process backend reloads the
-- whole tree) but we still refuse malicious input up-front so the
-- rejection surface matches the pre-migration contract.
validatePath :: ProjectDir -> Maybe Text -> Either Text ()
validatePath _ Nothing  = Right ()
validatePath pd (Just p) = case mkModulePath pd (T.unpack p) of
  Left err -> Left (formatPathError err)
  Right _  -> Right ()

--------------------------------------------------------------------------------
-- response shaping (schema preserved)
--------------------------------------------------------------------------------

okResult :: Bool -> [GhcError] -> ToolResult
okResult success diags =
  let errs  = filter ((== SevError)   . geSeverity) diags
      warns = filter ((== SevWarning) . geSeverity) diags
      payload =
        object
          [ "success"  .= (success && null errs)
          , "errors"   .= errs
          , "warnings" .= warns
          , "summary"  .= summarise success errs warns
          , "raw"      .= ("" :: Text)
            -- 'raw' retained for schema compat — the in-process backend
            -- has no stdout stream to capture, so this is always "".
          ]
  in ToolResult
       { trContent = [ TextContent (encodeText payload) ]
       , trIsError = not (success && null errs)
       }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeText (object
        [ "success" .= False
        , "error"   .= msg
        ])) ]
    , trIsError = True
    }

summarise :: Bool -> [GhcError] -> [GhcError] -> Text
summarise ok errs warns
  | not (null errs) = T.pack (show (length errs)) <> " error(s)"
  | ok && null warns = "Compiled OK. No issues."
  | ok = "Compiled OK. " <> T.pack (show (length warns)) <> " warning(s)."
  | otherwise = "Compilation produced no errors but load reported failure."

encodeText :: Value -> Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p ->
    "Project directory is not absolute: " <> p
  PathEscapesProject a p _ ->
    "module_path '" <> a <> "' escapes project directory " <> p
