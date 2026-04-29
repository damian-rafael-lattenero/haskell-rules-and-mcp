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
import qualified System.Directory as Dir
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , enumerateHaskellSources
  , loadForTarget
  , targetForPath
  , firstTestSuiteOrLibrary
  )
import qualified HaskellFlows.Mcp.Envelope as Env
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
    pure (parseErrorResult parseError)
  Right (LoadArgs mModPath dx) -> do
    eTgt <- case mModPath of
      Nothing -> do
        -- Issue #84: when the caller didn't pin a module_path, we
        -- pre-flight the project layout. Empty src/+app/ produces
        -- 'success=true' from the GHC backend (nothing to compile,
        -- so nothing fails). That's a misleading green: the agent
        -- is then told the project compiled cleanly when in fact
        -- there was nothing on disk to compile. Surface it as
        -- status='no_match' with kind='module_not_in_graph' so the
        -- consumer routes to ghc_create_project / ghc_add_modules
        -- instead of charging into ghc_suggest.
        sourceCount <- countHaskellSources pd
        if sourceCount == 0
          then pure (Left EmptyProject)
          else Right <$> firstTestSuiteOrLibrary ghcSess
      Just p  -> case mkModulePath pd (T.unpack p) of
        Left pathErr -> pure (Left (PathRefused (formatPathError pathErr)))
        Right _      -> do
          -- Issue #79: targetForPath silently falls back to
          -- TargetLibrary for any path that doesn't match the
          -- test/, app/, or bench/ prefix — including paths that
          -- don't exist on disk. Verify existence here so callers
          -- get a clear "file not found" instead of a misleading
          -- whole-library load with unrelated warnings.
          existsCheck <- checkPathExists pd p
          case existsCheck of
            Left missingMsg -> pure (Left (PathMissing missingMsg))
            Right ()        -> Right <$> targetForPath ghcSess (T.unpack p)
    case eTgt of
      Left EmptyProject       -> pure emptyProjectResult
      Left (PathRefused m)    -> pure (pathTraversalResult m)
      Left (PathMissing m)    -> pure (pathMissingResult m)
      Right tgt -> do
        -- Strict first gives agents the canonical error set.
        -- diagnostics=true merges a Deferred pass so typed holes
        -- and deferred-type-errors also show up as warnings.
        eStrict <- try (loadForTarget ghcSess tgt Strict)
        case eStrict :: Either SomeException (Bool, [GhcError]) of
          Left ex ->
            pure (subprocessResult
                    ("loadForTarget failed: " <> T.pack (show ex)))
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

-- | Issue #84: pre-flight signal for the no-args target path.
-- 'EmptyProject' is the case the issue closes; the path-error
-- variants exist so we keep them distinct on the wire.
data PreflightFailure
  = EmptyProject
  | PathRefused !Text
  | PathMissing !Text

-- | Issue #84: count Haskell sources under @<project>/src@ and
-- @<project>/app@. Mirrors the discovery logic inside
-- 'loadProjectWithFlavour' so the empty-project signal we emit at
-- the tool boundary stays consistent with what the loader would
-- actually attempt to compile.
countHaskellSources :: ProjectDir -> IO Int
countHaskellSources pd = do
  let root = unProjectDir pd
      searchDirs = [root </> "src", root </> "app"]
  files <- enumerateHaskellSources searchDirs
  pure (length files)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: a successful load → status='ok' (success
-- path) or status='failed' kind='compile_error' (errors). The
-- diagnostic detail ('errors', 'warnings', 'summary', 'raw')
-- stays under 'result' so callers can render the GHCi-style
-- output unchanged.
okResult :: Bool -> [GhcError] -> ToolResult
okResult ok diags =
  let errs  = filter ((== SevError)   . geSeverity) diags
      warns = filter ((== SevWarning) . geSeverity) diags
      succ_ = ok && null errs
      payload =
        object
          [ "errors"   .= errs
          , "warnings" .= warns
          , "summary"  .= summarise ok errs warns
          , "raw"      .= renderGhciStyle diags
          ]
  in if succ_
       then Env.toolResponseToResult (Env.mkOk payload)
       else
         let envErr   = Env.mkErrorEnvelope Env.CompileError
                          (summarise ok errs warns)
             response = (Env.mkFailed envErr) { Env.reResult = Just payload }
         in Env.toolResponseToResult response

-- | Issue #90 Phase C: caller-side parse failure → 'missing_arg'
-- or 'type_mismatch'.
parseErrorResult :: String -> ToolResult
parseErrorResult err =
  let kind | "key" `isInfixOfStr` err = Env.MissingArg
           | otherwise                = Env.TypeMismatch
      envErr = (Env.mkErrorEnvelope kind
                  (T.pack ("Invalid arguments: " <> err)))
                    { Env.eeCause = Just (T.pack err) }
  in Env.toolResponseToResult (Env.mkFailed envErr)
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Issue #90 Phase C: 'mkModulePath' rejected the input → that's
-- a path-traversal refusal.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

-- | Issue #79: module_path resolved fine but the file isn't on
-- disk → kind='module_path_does_not_exist'.
pathMissingResult :: Text -> ToolResult
pathMissingResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.ModulePathDoesNotExist msg))

-- | Issue #84: empty project (no src/ or app/ Haskell sources)
-- on the no-args target path → status='no_match' with
-- kind='module_not_in_graph'. The pre-envelope shape was a
-- false 'success=true' with 'summary="Compiled OK. No issues."',
-- which routed agents to ghc_suggest on a project that didn't
-- exist yet. Now consumers branch on status and route to
-- ghc_create_project / ghc_add_modules instead.
emptyProjectResult :: ToolResult
emptyProjectResult =
  let payload = object
        [ "loaded"      .= (0 :: Int)
        , "summary"     .= ( "No Haskell sources found under \
                            \src/ or app/." :: Text )
        , "remediation" .= ( "Create the project with ghc_create_project \
                            \or add modules with ghc_add_modules before \
                            \calling ghc_load." :: Text )
        ]
      envErr   = Env.mkErrorEnvelope Env.ModuleNotInGraph
                   ( "ghc_load found no Haskell sources to compile. The \
                     \project has neither a src/ nor an app/ tree with \
                     \.hs files." :: Text )
      response = (Env.mkNoMatch payload) { Env.reError = Just envErr }
  in Env.toolResponseToResult response

-- | Issue #90 Phase C: GHC API exception → kind='subprocess_error'.
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))

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
