-- | @ghc_load@ — full GhcSession (Wave 2).
--
-- Loads the project via 'loadForTarget' (cabal-aware stanza flags)
-- and returns parsed diagnostics (errors + warnings) sourced directly
-- from GHC's typechecker via the logger hook. When the caller passes
-- @diagnostics=true@, the same target is re-loaded with @Deferred@
-- flavour so typed holes and deferred type errors surface as
-- warnings.
--
-- Response shape matches the legacy ghc_load for backward
-- compatibility with existing e2e scenarios: success, errors,
-- warnings, summary, raw.
module HaskellFlows.Tool.Load
  ( descriptor
  , handle
  , LoadArgs (..)
  , checkPathExists
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified System.Directory as Dir
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , loadForTarget
  , targetForPath
  , firstTestSuiteOrLibrary
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , renderGhciStyle
  )
import HaskellFlows.Types

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcLoad
    , tdDescription =
        "Load or reload Haskell modules via the in-process GHC API. "
          <> "Returns structured compilation errors and warnings. Pass "
          <> "diagnostics=true to additionally run a deferred pass "
          <> "(-fdefer-type-errors -fdefer-typed-holes) and surface typed "
          <> "holes discovered that way."
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

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (LoadArgs mModPath dx) -> do
    eTgt <- case mModPath of
      Nothing -> Right <$> firstTestSuiteOrLibrary ghcSess
      Just p  -> case mkModulePath pd (T.unpack p) of
        Left pathErr -> pure (Left (formatPathError pathErr))
        Right _      -> do
          -- Issue #79: targetForPath silently falls back to
          -- TargetLibrary for any path that doesn't match the
          -- test/, app/, or bench/ prefix — including paths that
          -- don't exist on disk. Verify existence here so callers
          -- get a clear "file not found" instead of a misleading
          -- whole-library load with unrelated warnings.
          existsCheck <- checkPathExists pd p
          case existsCheck of
            Left missingMsg -> pure (Left missingMsg)
            Right ()        -> Right <$> targetForPath ghcSess (T.unpack p)
    case eTgt of
      Left errMsg -> pure (errorResult errMsg)
      Right tgt -> do
        -- Strict first gives agents the canonical error set.
        -- diagnostics=true merges a Deferred pass so typed holes
        -- and deferred-type-errors also show up as warnings.
        eStrict <- try (loadForTarget ghcSess tgt Strict)
        case eStrict :: Either SomeException (Bool, [GhcError]) of
          Left ex ->
            pure (errorResult ("loadForTarget failed: " <> T.pack (show ex)))
          Right (strictOk, strictDiags) ->
            if dx
              then do
                eDef <- try (loadForTarget ghcSess tgt Deferred)
                case eDef :: Either SomeException (Bool, [GhcError]) of
                  Left _  -> pure (okResult strictOk strictDiags)
                  Right (_, deferredDiags) ->
                    let merged = mergeDiags strictDiags deferredDiags
                    in pure (okResult strictOk merged)
              else pure (okResult strictOk strictDiags)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

okResult :: Bool -> [GhcError] -> ToolResult
okResult ok diags =
  let errs  = filter ((== SevError)   . geSeverity) diags
      warns = filter ((== SevWarning) . geSeverity) diags
      succ_ = ok && null errs
      payload =
        object
          [ "success"  .= succ_
          , "errors"   .= errs
          , "warnings" .= warns
          , "summary"  .= summarise ok errs warns
          , "raw"      .= renderGhciStyle diags
          ]
  in ToolResult
       { trContent = [ TextContent (encodeText payload) ]
       , trIsError = not succ_
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
  | otherwise = "Compilation produced no errors but GHC reported failure."

encodeText :: Value -> Text
encodeText = TL.toStrict . TLE.decodeUtf8 . encode

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p ->
    "Project directory is not absolute: " <> p
  PathEscapesProject a p _ ->
    "module_path '" <> a <> "' escapes project directory " <> p

-- | Verify that a syntactically-valid module_path actually points to
-- an existing file under the project root. Exported so the test suite
-- can lock the issue-#79 contract without having to spin up a full
-- GhcSession. Note: callers must run 'mkModulePath' first; this helper
-- only checks existence, not escape.
checkPathExists :: ProjectDir -> Text -> IO (Either Text ())
checkPathExists pd rel = do
  let resolved = unProjectDir pd </> T.unpack rel
  exists <- Dir.doesFileExist resolved
  pure $ if exists
    then Right ()
    else Left ("module_path does not exist: " <> rel)
