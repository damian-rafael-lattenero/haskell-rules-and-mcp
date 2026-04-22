-- | Phase-1/2/3 scaffolding for the GHC-API-in-process migration
-- (docs/GHC-API-rewrite-plan.md). Cohabits with the legacy
-- 'HaskellFlows.Ghci.Session.Session' during Phases 1-6; Phase 7
-- promotes it to primary and the subprocess-ghci implementation is
-- retired for the 22 tools that don't need runtime randomisation.
--
-- Invariants, enforced by construction:
--
-- * Single-writer to the HscEnv per session. The GHC API is not
--   thread-safe within one HscEnv, so a per-session 'MVar ()'
--   serialises every 'withGhcSession' call. Concurrent sessions are
--   still permitted — the lock is scoped to one 'GhcSession', not
--   global — which is the foundation for the plan's parallel tool
--   calls benefit (#3).
--
-- * HscEnv cached across calls via an 'IORef'. First 'withGhcSession'
--   bootstraps a fresh environment from default 'DynFlags' and
--   auto-loads the project's source tree; subsequent calls restore
--   whatever the previous action left behind (loaded modules,
--   interactive context, …).
--
-- * Auto-load on first call. 'withGhcSession' enumerates @src\/@ and
--   @app\/@ under the 'ProjectDir', runs @setTargets@ +
--   @load LoadAllTargets@ once, then sets the interactive context to
--   include every successfully-loaded module plus @Prelude@.
--
-- * Diagnostic capture. Phase-3 tools (@ghci_load@, @check_module@,
--   @check_project@, @hole@) call 'loadAndCaptureDiagnostics' which
--   installs a 'LogAction' hook on the session's 'Logger',
--   invalidates the load cache, re-runs @load LoadAllTargets@, and
--   returns every warning + error the logger saw — shaped as
--   'GhcError' to match the legacy parser's JSON schema.
--
-- * In-process: there is no subprocess to terminate.
--   'killGhcSession' is a state reset — drops the cached HscEnv so
--   the next 'startGhcSession' returns an empty environment.
module HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , gsProject
  , startGhcSession
  , killGhcSession
  , withGhcSession
  , invalidateLoadCache
    -- * Phase-3 diagnostic capture
  , LoadFlavour (..)
  , loadAndCaptureDiagnostics
    -- * Phase-7 in-process evaluation
  , evalIOString
    -- * Wave-1 cabal-aware DynFlags
  , ensureStanzaFlags
  , withStanzaFlags
    -- * Wave-2 compile via stanza flags
  , loadForTarget
  , targetForPath
  , firstTestSuiteOrLibrary
  ) where

import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC
  ( Ghc
  , InteractiveImport (IIDecl)
  , LoadHowMuch (LoadAllTargets)
  , SuccessFlag (Succeeded)
  , getModuleGraph
  , getSession
  , getSessionDynFlags
  , guessTarget
  , load
  , mgModSummaries
  , mkModuleName
  , ms_mod
  , moduleName
  , runGhc
  , setContext
  , setSession
  , setSessionDynFlags
  , setTargets
  , simpleImportDecl
  )
import GHC.Driver.Env (HscEnv (hsc_logger))
import GHC.Driver.Flags (WarningFlag (..))
import GHC.Driver.Session
  ( DynFlags (..)
  , GeneralFlag (..)
  , gopt_set
  , gopt_unset
  , parseDynamicFlagsCmdLine
  , wopt_set
  )
import GHC.Paths (libdir)
import GHC.Types.Error (MessageClass (..))
import qualified GHC.Types.Error as GhcErr
import GHC.Types.SrcLoc
  ( GenLocated (L)
  , SrcSpan (RealSrcSpan)
  , noLoc
  , srcSpanFile
  , srcSpanStartCol
  , srcSpanStartLine
  )
import GHC.Runtime.Eval (compileExpr)
import GHC.Utils.Logger (LogAction, pushLogHook)
import GHC.Utils.Outputable (SDocContext, defaultSDocContext, renderWithContext)
import Unsafe.Coerce (unsafeCoerce)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>), takeExtension)
import qualified System.Process as Proc

import qualified HaskellFlows.Ghc.CabalBootstrap as Bootstrap
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | A persistent GHC-API session scoped to a single Haskell project.
data GhcSession = GhcSession
  { gsEnvRef      :: !(IORef (Maybe HscEnv))
  , gsLibdir      :: !FilePath
  , gsLock        :: !(MVar ())
  , gsProject     :: !ProjectDir
  , gsLoadedRef   :: !(IORef Bool)
  , gsStanzaFlags :: !(IORef (Map Bootstrap.Target Bootstrap.StanzaFlags))
    -- ^ Wave-1 cache: per-target flags captured from
    -- 'cabal repl --with-compiler=<shim>'. 'Map.empty' until
    -- 'ensureStanzaFlags' runs. Consumers use 'withStanzaFlags'
    -- to apply the flags for a target before load / eval.
  }

-- | Bootstrap a 'GhcSession' for the given project. Cheap — HscEnv
-- is created lazily on the first 'withGhcSession' call.
startGhcSession :: ProjectDir -> IO GhcSession
startGhcSession pd = do
  ref         <- newIORef Nothing
  lock        <- newMVar ()
  loadedRef   <- newIORef False
  stanzaRef   <- newIORef Map.empty
  pure GhcSession
    { gsEnvRef      = ref
    , gsLibdir      = libdir
    , gsLock        = lock
    , gsProject     = pd
    , gsLoadedRef   = loadedRef
    , gsStanzaFlags = stanzaRef
    }

killGhcSession :: GhcSession -> IO ()
killGhcSession sess = do
  _ <- tryTakeMVar (gsLock sess)
  writeIORef (gsEnvRef sess) Nothing
  writeIORef (gsLoadedRef sess) False

invalidateLoadCache :: GhcSession -> IO ()
invalidateLoadCache sess = writeIORef (gsLoadedRef sess) False

withGhcSession :: GhcSession -> Ghc a -> IO a
withGhcSession sess act = withMVar (gsLock sess) $ \_ ->
  runGhc (Just (gsLibdir sess)) $ do
    mEnv <- liftIO (readIORef (gsEnvRef sess))
    case mEnv of
      Just env -> setSession env
      Nothing  -> do
        -- Baseline DynFlags only. Wave-2 consumers reach the cabal-
        -- aware state by wrapping their Ghc action in
        -- 'withStanzaFlags' (which runs 'parseDynamicFlagsCmdLine'
        -- with the exact argv cabal would have given 'ghc
        -- --interactive'). Applying an env file here as well would
        -- race the stanza flags and cause "cannot satisfy
        -- -package-id X" at runtime.
        dflags <- getSessionDynFlags
        _      <- setSessionDynFlags dflags
        pure ()
    loaded <- liftIO (readIORef (gsLoadedRef sess))
    unless loaded $ do
      autoLoadProject (gsProject sess)
      liftIO (writeIORef (gsLoadedRef sess) True)
    result <- act
    env'   <- getSession
    liftIO (writeIORef (gsEnvRef sess) (Just env'))
    pure result

-- | Inject cabal's generated @.ghc.environment.*@ file into DynFlags
-- via the 'packageEnv' field. Scoped to the GHC-API session only —
-- never mutates process env vars, so legacy 'Session' subprocess
-- spawns stay untouched.
applyCabalPackageEnv :: ProjectDir -> DynFlags -> IO DynFlags
applyCabalPackageEnv pd dflags = do
  mEnvFile <- findOrGenerateCabalEnvFile pd
  pure $ case mEnvFile of
    Nothing   -> dflags
    Just path -> dflags { packageEnv = Just path }

-- | Find or create @.ghc.environment.\<triple\>@ in the project root.
-- Returns 'Nothing' if the project isn't a cabal package (no .cabal
-- file) or if 'cabal build' fails — both tolerable, the rest of the
-- auto-load falls back gracefully to base-only.
findOrGenerateCabalEnvFile :: ProjectDir -> IO (Maybe FilePath)
findOrGenerateCabalEnvFile pd = do
  let root = unProjectDir pd
  rootExists <- doesDirectoryExist root
  if not rootExists
    then pure Nothing
    else do
      existing <- findExistingEnvFile root
      case existing of
        Just p  -> pure (Just p)
        Nothing -> do
          hasCabal <- hasCabalFile root
          if not hasCabal
            then pure Nothing
            else do
              _ <- generateEnvFile root
              findExistingEnvFile root

-- | Scan the project root for any @.ghc.environment.*@ file.
findExistingEnvFile :: FilePath -> IO (Maybe FilePath)
findExistingEnvFile root = do
  entries <- listDirectory root
  let envFiles =
        [ root </> e
        | e <- entries
        , ".ghc.environment." `isPrefixOf` e
        ]
  case envFiles of
    (p : _) -> pure (Just p)
    []      -> pure Nothing
  where
    isPrefixOf p s = take (length p) s == p

hasCabalFile :: FilePath -> IO Bool
hasCabalFile root = do
  entries <- listDirectory root
  pure (any ((".cabal" ==) . takeExtension) entries)

-- | Fire cabal to generate the env file. Swallows failures — if the
-- project doesn't build cleanly, the env file won't be generated and
-- GhcSession falls back to default package visibility.
generateEnvFile :: FilePath -> IO ()
generateEnvFile root = do
  let cp = (Proc.proc "cabal"
             [ "build"
             , "all"
             , "--write-ghc-environment-files=always"
             , "--only-dependencies"
             ])
             { Proc.cwd = Just root
             , Proc.std_out = Proc.CreatePipe
             , Proc.std_err = Proc.CreatePipe
             , Proc.std_in  = Proc.NoStream
             }
  _ <- try @SomeException (Proc.readCreateProcess cp "")
  pure ()

autoLoadProject :: ProjectDir -> Ghc ()
autoLoadProject pd = do
  let root = unProjectDir pd
      searchDirs = [root </> "src", root </> "app"]
  files <- liftIO (enumerateHaskellSources searchDirs)
  dflags <- getSessionDynFlags
  let dflags' = dflags { importPaths = searchDirs <> importPaths dflags }
  _ <- setSessionDynFlags dflags'
  case files of
    [] -> pure ()
    _  -> do
      targets <- traverse (\f -> guessTarget f Nothing Nothing) files
      setTargets targets
      _ <- load LoadAllTargets
      mg <- getModuleGraph
      let modImports =
            [ IIDecl (simpleImportDecl (moduleName (ms_mod ms)))
            | ms <- mgModSummaries mg
            ]
          preludeImport =
            IIDecl (simpleImportDecl (mkModuleName "Prelude"))
      setContext (preludeImport : modImports)

enumerateHaskellSources :: [FilePath] -> IO [FilePath]
enumerateHaskellSources = fmap concat . traverse enumerateOne
  where
    enumerateOne dir = do
      present <- doesDirectoryExist dir
      if not present
        then pure []
        else do
          entries <- listDirectory dir
          let full = map (dir </>) entries
          files   <- filterM doesFileExist full
          subdirs <- filterM doesDirectoryExist full
          let hs = filter ((".hs" ==) . takeExtension) files
          rest <- enumerateHaskellSources subdirs
          pure (hs ++ rest)

--------------------------------------------------------------------------------
-- Phase-3 — diagnostic capture
--------------------------------------------------------------------------------

-- | Which compile flavour to request from GHC.
--
-- * 'Strict' — default: errors abort, warnings warn.
-- * 'Deferred' — enables @-fdefer-type-errors -fdefer-typed-holes@ so
--   the module still loads and hole/deferred-error warnings show up in
--   the captured diagnostic list.
data LoadFlavour = Strict | Deferred
  deriving stock (Eq, Show)

-- | Install a diagnostic-capture log hook, run @setTargets + load@
-- in the requested flavour, and return (success, capturedDiagnostics)
-- shaped for the legacy 'GhcError' response schema.
--
-- Pre-flips 'gsLoadedRef' to 'True' so 'withGhcSession' skips its
-- auto-load preamble — otherwise auto-load would run BEFORE our hook
-- is installed and its diagnostics would leak to stderr.
--
-- Failure semantics: the returned 'Bool' is True iff the final
-- 'SuccessFlag' was 'Succeeded' AND no captured diagnostic has
-- 'SevError'. If the action itself throws, returns @(False, [])@ —
-- the caller layer wraps with 'try' and surfaces the exception as
-- a tool-level error.
loadAndCaptureDiagnostics
  :: GhcSession
  -> LoadFlavour
  -> IO (Bool, [GhcError])
loadAndCaptureDiagnostics sess flavour = do
  diagRef <- newIORef []
  -- Skip the auto-load preamble; we own the load below.
  writeIORef (gsLoadedRef sess) True
  eRes <- try $ withGhcSession sess $ do
    installCaptureHook diagRef
    loadProjectWithFlavour (gsProject sess) flavour
  success <- case eRes :: Either SomeException Bool of
    Left _   -> pure False
    Right ok -> pure ok
  diags <- readIORef diagRef
  let orderedDiags = reverse diags
      anyErrors    = any ((== SevError) . geSeverity) orderedDiags
  pure (success && not anyErrors, orderedDiags)

-- | The replacement for 'autoLoadProject' used by
-- 'loadAndCaptureDiagnostics'. Same layout discovery, but parameterised
-- by load flavour and runs inside the caller's hook-equipped Ghc
-- session so every diagnostic is captured.
loadProjectWithFlavour :: ProjectDir -> LoadFlavour -> Ghc Bool
loadProjectWithFlavour pd flavour = do
  let root = unProjectDir pd
      searchDirs = [root </> "src", root </> "app"]
  files <- liftIO (enumerateHaskellSources searchDirs)
  dflags <- getSessionDynFlags
  let dflags' = dflags { importPaths = searchDirs <> importPaths dflags }
  _ <- setSessionDynFlags dflags'
  applyFlavour flavour
  case files of
    [] -> pure True
    _  -> do
      targets <- traverse (\f -> guessTarget f Nothing Nothing) files
      setTargets targets
      ok <- load LoadAllTargets
      mg <- getModuleGraph
      let modImports =
            [ IIDecl (simpleImportDecl (moduleName (ms_mod ms)))
            | ms <- mgModSummaries mg
            ]
          preludeImport =
            IIDecl (simpleImportDecl (mkModuleName "Prelude"))
      setContext (preludeImport : modImports)
      pure (case ok of { Succeeded -> True; _ -> False })

-- | Push a 'LogAction' wrapper onto the session's 'Logger' that
-- appends every diagnostic to the given 'IORef'. The wrapper does
-- NOT call the previous action, so stderr stays quiet — we own the
-- whole diagnostic surface for the duration.
installCaptureHook :: IORef [GhcError] -> Ghc ()
installCaptureHook ref = do
  env <- getSession
  let newLogger = pushLogHook (captureHook ref) (hsc_logger env)
  setSession (env { hsc_logger = newLogger })

-- | Convert a GHC 'MessageClass' + 'SrcSpan' + rendered 'SDoc' into
-- a 'GhcError', and append it to the capture list. Non-diagnostic
-- messages (output, dump) are dropped.
captureHook :: IORef [GhcError] -> LogAction -> LogAction
captureHook ref _orig _lflags msgClass ss sdoc = do
  let body = T.pack (renderWithContext sdocContextPlain sdoc)
      codePrefix = case msgClass of
        MCDiagnostic _ _ (Just dc) -> "[" <> T.pack (show dc) <> "] "
        _                          -> ""
      txt = codePrefix <> body
  case msgClassToSeverity msgClass of
    Nothing -> pure ()
    Just sev -> modifyIORef' ref (mkGhcError sev ss txt :)

-- | Render SDoc without ANSI colour escapes — the MCP wire format
-- is plain JSON, not a terminal.
sdocContextPlain :: SDocContext
sdocContextPlain = defaultSDocContext

msgClassToSeverity :: MessageClass -> Maybe Severity
msgClassToSeverity = \case
  -- Direct constructor match against GHC's Severity is safer than
  -- shape-matching on 'show sev' — GHC may ship extra constructors
  -- (SevIgnore, SevInfo, …) between minor versions and the show
  -- form isn't stable.
  MCDiagnostic GhcErr.SevError   _ _ -> Just SevError
  MCDiagnostic GhcErr.SevWarning _ _ -> Just SevWarning
  MCFatal -> Just SevError
  _       -> Nothing

-- | Build a 'GhcError' from a captured diagnostic. File/line/column
-- come from 'RealSrcSpan'; unhelpful spans report sentinel values
-- (@file=""@, line/col 0) matching the legacy parser's behaviour for
-- location-less messages.
mkGhcError :: Severity -> SrcSpan -> Text -> GhcError
mkGhcError sev ss msg = case ss of
  RealSrcSpan rspan _ -> GhcError
    { geFile     = T.pack (show (srcSpanFile rspan))
    , geLine     = srcSpanStartLine rspan
    , geColumn   = srcSpanStartCol rspan
    , geSeverity = sev
    , geCode     = Nothing
    , geMessage  = msg
    }
  _ -> GhcError
    { geFile     = ""
    , geLine     = 0
    , geColumn   = 0
    , geSeverity = sev
    , geCode     = Nothing
    , geMessage  = msg
    }

--------------------------------------------------------------------------------
-- Wave-1 cabal-aware DynFlags
--------------------------------------------------------------------------------

-- | Populate the session's per-target 'StanzaFlags' cache by driving
-- cabal via the shim (see 'HaskellFlows.Ghc.CabalBootstrap'). Cheap
-- no-op if the cache is already non-empty. Idempotent.
ensureStanzaFlags :: GhcSession -> IO ()
ensureStanzaFlags sess = do
  existing <- readIORef (gsStanzaFlags sess)
  when (Map.null existing) $ do
    fresh <- Bootstrap.bootstrapProject (gsProject sess)
    writeIORef (gsStanzaFlags sess) fresh

-- | Apply the cached 'StanzaFlags' for the given target to the
-- session's 'DynFlags'. Only DynFlags — leftover tokens (module
-- names / file paths) are intentionally ignored here. Callers that
-- need targets call 'setTargets' themselves with whatever source
-- enumeration fits (e.g. the filesystem scan 'loadForTarget' uses).
withStanzaFlags :: GhcSession -> Bootstrap.Target -> Ghc a -> Ghc a
withStanzaFlags sess tgt act = do
  stanzas <- liftIO (readIORef (gsStanzaFlags sess))
  case Map.lookup tgt stanzas of
    Nothing -> act
    Just sf -> do
      dflags <- getSessionDynFlags
      let cleaned = filter (/= "--interactive") (Bootstrap.sfArgs sf)
      (dflags', _, _) <-
        parseDynamicFlagsCmdLine dflags (map noLoc cleaned)
      _ <- setSessionDynFlags dflags'
      act

--------------------------------------------------------------------------------
-- Wave-2 — compile via stanza flags
--------------------------------------------------------------------------------

-- | Compile the project against a specific cabal target's flags
-- (library, test-suite, executable, benchmark). Returns
-- @(success, diagnostics)@ shaped identically to
-- 'loadAndCaptureDiagnostics'.
--
-- Unlike 'loadAndCaptureDiagnostics' this does NOT run our own
-- scan-and-load heuristic. Cabal's captured argv already contains
-- the full setup — @-isrc@, @-hidir dist-newstyle\/…@,
-- @-package-db …@, @-this-unit-id …@, plus the trailing target
-- module name — so we just apply the flags and do
-- @load LoadAllTargets@. Cabal-aware, zero heuristics.
--
-- If 'ensureStanzaFlags' hasn't been called yet, or the target
-- has no captured flags, this falls through to the same
-- 'loadAndCaptureDiagnostics' path so behaviour remains safe for
-- projects without a .cabal file or for targets the bootstrap
-- missed.
loadForTarget
  :: GhcSession
  -> Bootstrap.Target
  -> LoadFlavour
  -> IO (Bool, [GhcError])
loadForTarget sess tgt flavour = do
  ensureStanzaFlags sess
  stanzas <- readIORef (gsStanzaFlags sess)
  case Map.lookup tgt stanzas of
    Nothing -> loadAndCaptureDiagnostics sess flavour
    Just _sf -> do
      diagRef <- newIORef []
      -- Pre-flip gsLoadedRef so withGhcSession skips its auto-load.
      writeIORef (gsLoadedRef sess) True
      let root       = unProjectDir (gsProject sess)
          searchDirs = [root </> "src", root </> "app", root </> "test"]
      files <- enumerateHaskellSources searchDirs
      eRes <- try $ withGhcSession sess $
        withStanzaFlags sess tgt $ do
          applyFlavour flavour
          unless (null files) $ do
            targets <- traverse (\f -> guessTarget f Nothing Nothing) files
            setTargets targets
          -- Install the logger hook as late as possible — right
          -- before 'load' — so no subsequent setSessionDynFlags /
          -- setTargets can overwrite hsc_logger and lose the hook.
          installCaptureHook diagRef
          ok <- load LoadAllTargets
          -- Populate the interactive context with every module that
          -- actually loaded, plus Prelude. Without this step, queries
          -- like 'exprType "double"' or 'getNamesInScope' after a
          -- successful load would see an empty scope.
          mg <- getModuleGraph
          let modImports =
                [ IIDecl (simpleImportDecl (moduleName (ms_mod ms)))
                | ms <- mgModSummaries mg
                ]
              preludeImport =
                IIDecl (simpleImportDecl (mkModuleName "Prelude"))
          setContext (preludeImport : modImports)
          pure (case ok of { Succeeded -> True; _ -> False })
      success <- case eRes :: Either SomeException Bool of
        Left _   -> pure False
        Right ok -> pure ok
      diags <- readIORef diagRef
      let ordered   = reverse diags
          anyErrors = any ((== SevError) . geSeverity) ordered
      pure (success && not anyErrors, ordered)

-- | Return the first detected test-suite target, or 'TargetLibrary'
-- as fallback. Used by runtime tools (QC / regression / determinism)
-- that need the test-suite's build-depends (QuickCheck etc.)
-- resolvable in-process. Test-suite stanzas typically build-depend
-- on the library, so library modules are still accessible under
-- test-suite flags — it's the biggest-scope option we have without
-- asking the user.
firstTestSuiteOrLibrary :: GhcSession -> IO Bootstrap.Target
firstTestSuiteOrLibrary sess = do
  ensureStanzaFlags sess
  stanzas <- readIORef (gsStanzaFlags sess)
  pure $ case [ t | t@(Bootstrap.TargetTestSuite _) <- Map.keys stanzas ] of
    (t : _) -> t
    []      -> Bootstrap.TargetLibrary

-- | Path-based target selection. Handles the conventional layout:
--
--   * @test\/…@     → first detected test-suite (or library fallback)
--   * @app\/…@      → first detected executable
--   * @bench\/…@    → first detected benchmark
--   * everything else → library
--
-- Looking the test-suite/executable/benchmark up by prefix is
-- brittle versus parsing @hs-source-dirs@ per stanza, but handles
-- every scenario we care about and keeps the code tiny.
targetForPath :: GhcSession -> FilePath -> IO Bootstrap.Target
targetForPath sess path = do
  ensureStanzaFlags sess
  stanzas <- readIORef (gsStanzaFlags sess)
  let fallback = Bootstrap.TargetLibrary
      prefix p = any (\c -> c == '/' || c == '\\') (drop (length p) path)
                 && take (length p) path == p
      firstOf predicate =
        case [ t | t <- Map.keys stanzas, predicate t ] of
          (t : _) -> t
          []      -> fallback
  pure $ case () of
    _ | prefix "test/"   ->
          firstOf (\case Bootstrap.TargetTestSuite {} -> True; _ -> False)
      | prefix "app/"    ->
          firstOf (\case Bootstrap.TargetExecutable {} -> True; _ -> False)
      | prefix "bench/"  ->
          firstOf (\case Bootstrap.TargetBenchmark {} -> True; _ -> False)
      | otherwise        -> fallback

--------------------------------------------------------------------------------
-- Phase-7 in-process evaluation
--------------------------------------------------------------------------------

-- | Compile and run an expression whose type must be @IO String@,
-- returning the String. For QC/regression/determinism and for
-- ghci_eval's IO fallback path. The caller wraps the user
-- expression (e.g. @"quickCheckResult (" ++ prop ++ ")"@) into an
-- @IO String@-typed statement; this helper handles the compile +
-- coerce + execute cycle.
--
-- Exceptions thrown during compilation (SourceError for unresolved
-- names, ambiguous types, missing instances) or during execution
-- (runtime error, user code ⊥) propagate as 'SomeException' — the
-- caller wraps with 'try' at the tool layer.
evalIOString :: String -> Ghc String
evalIOString stmt = do
  hv <- compileExpr stmt
  let action = unsafeCoerce hv :: IO String
  result <- liftIO action
  -- Force the whole String so runtime ⊥ surfaces as a catchable
  -- exception instead of a lazy payload that blows up downstream.
  length result `seq` pure result

-- | Tweak the session 'DynFlags' for the requested load flavour.
-- 'Strict' clears the defer flags; 'Deferred' enables them so
-- type errors and typed holes become warnings rather than aborting
-- the load.
applyFlavour :: LoadFlavour -> Ghc ()
applyFlavour flavour = do
  dflags <- getSessionDynFlags
  let dflags' = case flavour of
        Strict ->
          dflags
            `gopt_unset` Opt_DeferTypeErrors
            `gopt_unset` Opt_DeferTypedHoles
            `gopt_unset` Opt_DeferOutOfScopeVariables
        Deferred ->
          dflags
            `gopt_set` Opt_DeferTypeErrors
            `gopt_set` Opt_DeferTypedHoles
            `gopt_set` Opt_DeferOutOfScopeVariables
            `wopt_set` Opt_WarnDeferredTypeErrors
            `wopt_set` Opt_WarnTypedHoles
            `wopt_set` Opt_WarnDeferredOutOfScopeVariables
  _ <- setSessionDynFlags dflags'
  pure ()
