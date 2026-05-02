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
    -- * Exposed for unit tests
  , mergeDiags
  , parseHsSourceDirs
  , isUnderAnySourceDir
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isSpace)
import Data.List (isPrefixOf, nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir
import System.FilePath (takeExtension, (</>))

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , enumerateHaskellSources
  , loadForTarget
  , targetForPath
  , firstTestSuiteOrLibrary
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
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
    pure (formatParseError parseError)
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
            Right () -> do
              -- Issue #110: reject files outside declared hs-source-dirs.
              -- A file that exists on disk but is not under any declared
              -- source directory will never be found by GHCi's module
              -- search — the load would succeed with misleading "OK" output
              -- while actually loading an unrelated target.
              mBadDirs <- checkNotInSourceDirs pd (T.unpack p)
              case mBadDirs of
                Just dirs -> pure (Left (OutsideSourceDirs p dirs))
                Nothing   -> Right <$> targetForPath ghcSess (T.unpack p)
    case eTgt of
      Left EmptyProject                     -> pure emptyProjectResult
      Left (PathRefused m)                  -> pure (pathTraversalResult m)
      Left (PathMissing m)                  -> pure (pathMissingResult m)
      Left (OutsideSourceDirs modPath dirs) -> pure (outsideSourceDirsResult modPath dirs)
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
                  Right (deferredOk, deferredDiags) ->
                    let merged = mergeDiags strictDiags deferredDiags
                    in pure (okResult deferredOk merged)
              else pure (okResult strictOk strictDiags)

-- | Issue #84: pre-flight signal for the no-args target path.
-- 'EmptyProject' is the case the issue closes; the path-error
-- variants exist so we keep them distinct on the wire.
-- 'OutsideSourceDirs' (#110) carries the module_path and the
-- list of declared source dirs — both are needed to build the
-- error message outside the 'Just p' branch scope.
data PreflightFailure
  = EmptyProject
  | PathRefused !Text
  | PathMissing !Text
  | OutsideSourceDirs !Text ![FilePath]

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
        , "remediation" .= ( "Create the project with ghc_project(action=\"create\") \
                            \or add modules with ghc_modules(action=\"add\") before \
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

-- | Issue #110: file exists but is outside every declared
-- @hs-source-dirs@ → status='failed', kind='outside_source_dirs'.
-- The @result@ payload gives the agent exactly what it needs to
-- locate the right stanza and fix the path.
outsideSourceDirsResult :: Text -> [FilePath] -> ToolResult
outsideSourceDirsResult modPath sourceDirs =
  let dirsText = T.intercalate ", " (map T.pack sourceDirs)
      msg = modPath
              <> " is not under any declared hs-source-dirs ("
              <> dirsText <> ")"
      payload =
        object
          [ "module_path"    .= modPath
          , "hs_source_dirs" .= map T.pack sourceDirs
          , "remediation"    .=
              ( "Move the file under an existing hs-source-dirs \
                \(e.g. src/), or add its parent directory to the \
                \relevant stanza in the .cabal file." :: Text )
          ]
      envErr = (Env.mkErrorEnvelope Env.OutsideSourceDirs msg)
                 { Env.eeRemediation =
                     Just "Move the file under a declared hs-source-dirs \
                          \or extend the relevant .cabal stanza."
                 }
      response = (Env.mkFailed envErr) { Env.reResult = Just payload }
  in Env.toolResponseToResult response

-- | Merge strict and deferred diagnostic passes.  When both passes
-- report a diagnostic at the same (file, line, col), prefer the
-- deferred version: it carries the full warning text (e.g. a typed-
-- hole 'Found hole: …' message) whereas the strict version only
-- carries the abbreviated 'error' severity.  Deduplication by
-- (file, line, col) alone — not by message — fixes F-23 where
-- 'diagnostics=true' reported typed holes as errors because the
-- strict "error" entry shadowed the deferred "warning" entry that
-- contained the hole text.
mergeDiags :: [GhcError] -> [GhcError] -> [GhcError]
mergeDiags strictDiags deferredDiags =
  filter (\d -> posKey d `notElem` deferredPosSet) strictDiags
    <> deferredDiags
  where
    deferredPosSet = map posKey deferredDiags
    posKey d = (geFile d, geLine d, geColumn d)

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

--------------------------------------------------------------------------------
-- Issue #110: hs-source-dirs validation
--------------------------------------------------------------------------------

-- | Issue #110: return 'Just sourceDirs' when @relPath@ is not under
-- any declared @hs-source-dirs@ in the project's .cabal file.
-- Returns 'Nothing' — check passes or is skipped — when:
--   * no .cabal is found (no project layout yet),
--   * the .cabal cannot be read (I/O error, permission),
--   * or the file IS under a declared source directory.
--
-- When the .cabal exists but declares no @hs-source-dirs@ field at
-- all, the Cabal-spec default of @.@ (project root) applies and every
-- relative path passes by definition.
checkNotInSourceDirs :: ProjectDir -> FilePath -> IO (Maybe [FilePath])
checkNotInSourceDirs pd relPath = do
  mCabal <- findCabalFile' pd
  case mCabal of
    Nothing -> pure Nothing    -- no .cabal: skip check gracefully
    Just cf -> do
      mBody <- try (TIO.readFile cf) :: IO (Either SomeException Text)
      case mBody of
        Left _     -> pure Nothing  -- can't read: skip gracefully
        Right body ->
          let dirs      = parseHsSourceDirs body
              effective = if null dirs then ["."] else nub dirs
          in if isUnderAnySourceDir effective relPath
               then pure Nothing
               else pure (Just effective)

-- | Locate the unique @.cabal@ file directly under the project root.
-- Returns 'Nothing' when the directory doesn't exist, there is no
-- @.cabal@, or there is more than one (ambiguous).
--
-- Private to this module — 'CheckProject' carries its own copy to
-- avoid a coupling between two tools that should stay independent.
findCabalFile' :: ProjectDir -> IO (Maybe FilePath)
findCabalFile' pd = do
  let root = unProjectDir pd
  exists <- Dir.doesDirectoryExist root
  if not exists then pure Nothing else do
    entries <- Dir.listDirectory root
    let cabals = [ root </> e | e <- entries, takeExtension e == ".cabal" ]
    case cabals of
      [one] -> pure (Just one)
      _     -> pure Nothing

-- | Parse every @hs-source-dirs:@ field from a .cabal file body and
-- return the union of all declared directories (across all stanzas).
-- Exported for unit tests.
--
-- * Handles inline content (@hs-source-dirs: src lib@) and
--   continuation lines.
-- * Strips inline @-- comment@ fragments before tokenising.
-- * Returns an empty list when no @hs-source-dirs@ field is found;
--   callers must treat the empty list as the Cabal default of @.@.
parseHsSourceDirs :: Text -> [FilePath]
parseHsSourceDirs body = go (T.lines body) []
  where
    go []        acc = reverse acc
    go (ln:rest) acc
      | Just inlineTail <- stripHsSourceDirsHeader ln =
          let (contLines, after) = span isContinuation rest
              payload = inlineTail : map T.strip contLines
              dirs    = concatMap dirsIn payload
          in go after (dirs <> acc)
      | otherwise = go rest acc

    stripHsSourceDirsHeader ln =
      let lower = T.toLower (T.stripStart ln)
      in if "hs-source-dirs:" `T.isPrefixOf` lower
           then Just (inlineAfter ln)
           else Nothing

    inlineAfter ln =
      let r = T.dropWhile (/= ':') (T.stripStart ln)
      in T.strip (T.drop 1 r)

    -- A continuation line is one that starts with whitespace and
    -- whose first non-space token does not contain a colon (which
    -- would signal a new field).
    isContinuation ln =
      let stripped = T.stripStart ln
      in not (T.null stripped)
         && not (T.null (T.takeWhile isSpace ln))
         && not (T.any (== ':') (T.takeWhile (not . isSpace) stripped))

    dirsIn t =
      let noComment = T.strip (fst (T.breakOn "--" t))
      in [ T.unpack tok
         | tok <- T.words (T.replace "," " " noComment)
         , not (T.null tok)
         ]

-- | Return 'True' when @filePath@ is directly under at least one of
-- the given source directories. The special directory @"."@ matches
-- every path (project-root default). Exported for unit tests.
isUnderAnySourceDir :: [FilePath] -> FilePath -> Bool
isUnderAnySourceDir dirs filePath =
  any (\d -> d == "." || (d ++ "/") `isPrefixOf` filePath) dirs
