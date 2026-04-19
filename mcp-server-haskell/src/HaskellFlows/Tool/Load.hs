-- | @ghci_load@ — the first tool ported to Haskell.
--
-- Responsibility mirrors @mcp-server/src/tools/load-module.ts@'s
-- @handleLoadSingle@ at a simplified level: receive a module path, load it
-- in the persistent GHCi session, parse the resulting diagnostics, and
-- return a JSON summary the agent can act on.
--
-- Security note: the 'module_path' argument is routed through 'mkModulePath',
-- so traversal outside the project directory is rejected at the boundary —
-- the handler itself cannot produce an escaping path.
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

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error
import HaskellFlows.Types

-- | The schema surfaced to clients via @tools/list@.
--
-- Phase 3 adds the optional @diagnostics@ flag. When true the tool runs a
-- second deferred pass to surface holes and deferred-type-error warnings
-- on top of the strict diagnostics. Agents that just want a compile gate
-- should leave it off; agents driving property-first development should
-- turn it on.
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

-- | Handle a @tools/call@ for @ghci_load@.
--
-- When @diagnostics@ is enabled and a concrete @module_path@ was given,
-- run the load twice: first strict (authoritative errors), then deferred
-- (holes + relaxed warnings). The strict pass is the source of truth for
-- @success@; the deferred pass contributes extra warnings that weren't
-- visible under strict compilation.
handle :: Session -> ProjectDir -> Value -> IO ToolResult
handle sess pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (LoadArgs Nothing _) -> do
    result <- reload sess
    pure (okResult result [])
  Right (LoadArgs (Just p) dx) -> case mkModulePath pd (T.unpack p) of
    Left err -> pure (errorResult (formatPathError err))
    Right mp -> do
      strict <- loadModuleWith sess mp Strict
      let strictDiags = parseGhcErrors (grOutput strict)
      if dx
        then do
          deferred <- loadModuleWith sess mp Deferred
          let extraDiags = parseGhcErrors (grOutput deferred)
              merged     = mergeDiags strictDiags extraDiags
          pure (okResult strict merged)
        else pure (okResult strict strictDiags)

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
    { trContent = [ TextContent (encodeText (object [ "error" .= msg ])) ]
    , trIsError = True
    }

-- | Combine diagnostics from the strict and deferred passes.
--
-- Strict errors are always kept (they're the compile-gate truth). The
-- deferred pass contributes anything the strict pass didn't already
-- report, deduplicated by source position + message so the agent doesn't
-- see the same warning twice in the merged view.
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

-- | UTF-8-safe JSON → Text. Avoids the @T.pack . show . encode@ path that
-- would render non-ASCII as escaped Haskell string literals on the wire.
encodeText :: Value -> Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p ->
    "Project directory is not absolute: " <> p
  PathEscapesProject a p _ ->
    "module_path '" <> a <> "' escapes project directory " <> p
