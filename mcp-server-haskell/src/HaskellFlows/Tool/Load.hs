-- | @ghci_load@ — hybrid (Phase-3 session-sync refactor).
--
-- Loads via the legacy 'Session' (still authoritative for QC /
-- regression / determinism / eval which haven't migrated yet), then
-- invalidates the 'GhcSession' auto-load cache so the next Phase-2
-- tool call re-scans disk and sees the fresh module graph.
--
-- Pure-in-process migration lands in Phase 4+ once eval/QC move off
-- legacy.
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

import HaskellFlows.Ghc.ApiSession (GhcSession, invalidateLoadCache)
import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error
import HaskellFlows.Types

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_load"
    , tdDescription =
        "Load or reload Haskell modules in GHCi. Returns parsed compilation "
          <> "errors and warnings. Pass diagnostics=true to additionally run "
          <> "a deferred pass (-fdefer-type-errors -fdefer-typed-holes) and "
          <> "surface typed holes discovered that way."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Path to a module to load, relative to the project \
                       \directory. Omit to reload current modules." :: Text)
                  ]
              , "diagnostics" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("When true, runs a second deferred pass to extract \
                       \typed holes and deferred-type-error warnings. \
                       \Default: false." :: Text)
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

handle :: GhcSession -> Session -> ProjectDir -> Value -> IO ToolResult
handle ghcSess sess pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (LoadArgs Nothing _) -> do
    result <- reload sess
    invalidateLoadCache ghcSess
    pure (okResult result [])
  Right (LoadArgs (Just p) dx) -> case mkModulePath pd (T.unpack p) of
    Left err -> pure (errorResult (formatPathError err))
    Right mp -> do
      strict <- loadModuleWith sess mp Strict
      let strictDiags = parseGhcErrors (grOutput strict)
      finalResult <- if dx
        then do
          deferred <- loadModuleWith sess mp Deferred
          let extraDiags = parseGhcErrors (grOutput deferred)
              merged     = mergeDiags strictDiags extraDiags
          pure (okResult strict merged)
        else pure (okResult strict strictDiags)
      invalidateLoadCache ghcSess
      pure finalResult

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

okResult :: GhciResult -> [GhcError] -> ToolResult
okResult gr diags =
  let errs  = filter ((== SevError) . geSeverity) diags
      warns = filter ((== SevWarning) . geSeverity) diags
      payload =
        object
          [ "success"  .= (grSuccess gr && null errs)
          , "errors"   .= errs
          , "warnings" .= warns
          , "summary"  .= summarise (grSuccess gr) errs warns
          , "raw"      .= grOutput gr
          ]
  in ToolResult
       { trContent = [ TextContent (encodeText payload) ]
       , trIsError = not (grSuccess gr) || not (null errs)
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

mergeDiags :: [GhcError] -> [GhcError] -> [GhcError]
mergeDiags strictDiags deferredDiags =
  strictDiags <> filter (not . alreadyReported) deferredDiags
  where
    seen = map posKey strictDiags
    alreadyReported d = posKey d `elem` seen
    posKey d = (geFile d, geLine d, geColumn d, geMessage d)

summarise :: Bool -> [GhcError] -> [GhcError] -> Text
summarise ok errs warns
  | not (null errs) = T.pack (show (length errs)) <> " error(s)"
  | ok && null warns = "Compiled OK. No issues."
  | ok = "Compiled OK. " <> T.pack (show (length warns)) <> " warning(s)."
  | otherwise = "Compilation produced no errors but GHCi reported failure."

encodeText :: Value -> Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p ->
    "Project directory is not absolute: " <> p
  PathEscapesProject a p _ ->
    "module_path '" <> a <> "' escapes project directory " <> p
