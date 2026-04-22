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
  ) where

import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, withMVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
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
  , wopt_set
  )
import GHC.Paths (libdir)
import GHC.Types.Error (MessageClass (..))
import GHC.Types.SrcLoc
  ( SrcSpan (RealSrcSpan)
  , srcSpanFile
  , srcSpanStartCol
  , srcSpanStartLine
  )
import GHC.Utils.Logger (LogAction, pushLogHook)
import GHC.Utils.Outputable (SDocContext, defaultSDocContext, renderWithContext)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>), takeExtension)

import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | A persistent GHC-API session scoped to a single Haskell project.
data GhcSession = GhcSession
  { gsEnvRef    :: !(IORef (Maybe HscEnv))
  , gsLibdir    :: !FilePath
  , gsLock      :: !(MVar ())
  , gsProject   :: !ProjectDir
  , gsLoadedRef :: !(IORef Bool)
  }

-- | Bootstrap a 'GhcSession' for the given project. Cheap — HscEnv
-- is created lazily on the first 'withGhcSession' call.
startGhcSession :: ProjectDir -> IO GhcSession
startGhcSession pd = do
  ref       <- newIORef Nothing
  lock      <- newMVar ()
  loadedRef <- newIORef False
  pure GhcSession
    { gsEnvRef    = ref
    , gsLibdir    = libdir
    , gsLock      = lock
    , gsProject   = pd
    , gsLoadedRef = loadedRef
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
  let txt = T.pack (renderWithContext sdocContextPlain sdoc)
  case msgClassToSeverity msgClass of
    Nothing -> pure ()
    Just sev -> modifyIORef' ref (mkGhcError sev ss txt :)

-- | Render SDoc without ANSI colour escapes — the MCP wire format
-- is plain JSON, not a terminal.
sdocContextPlain :: SDocContext
sdocContextPlain = defaultSDocContext

msgClassToSeverity :: MessageClass -> Maybe Severity
msgClassToSeverity = \case
  MCDiagnostic sev _ _ -> case show sev of
    s | "Err"  `isPrefix` s -> Just SevError
      | "War"  `isPrefix` s -> Just SevWarning
      | otherwise           -> Nothing
  MCFatal -> Just SevError
  _       -> Nothing
  where
    isPrefix p s = take (length p) s == p

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
