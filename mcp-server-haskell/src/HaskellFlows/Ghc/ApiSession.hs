-- | Phase-1/2 scaffolding for the GHC-API-in-process migration
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
--   include every successfully-loaded module plus @Prelude@. This is
--   the in-process equivalent of @cabal repl@'s implicit boot-time
--   load — without it, Phase-2 read-only tools would only see Prelude
--   bindings, which breaks every scenario that queries a local
--   binding.
--
-- * In-process: there is no subprocess to terminate.
--   'killGhcSession' is effectively a state reset — it drops the
--   cached HscEnv so the next 'startGhcSession' returns an empty
--   environment rather than inheriting stale linker/module state.
module HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , gsProject
  , startGhcSession
  , killGhcSession
  , withGhcSession
  , invalidateLoadCache
  ) where

import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, withMVar)
import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC
  ( Ghc
  , InteractiveImport (IIDecl)
  , LoadHowMuch (LoadAllTargets)
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
import GHC.Driver.Env (HscEnv)
import GHC.Driver.Session (DynFlags (..))
import GHC.Paths (libdir)
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , listDirectory
  )
import System.FilePath ((</>), takeExtension)

import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | A persistent GHC-API session scoped to a single Haskell project.
--
-- Field selectors are exported sparingly — only what the server
-- layer needs to read ('gsProject'). The HscEnv cache, libdir, lock,
-- and load-cache flag are implementation details and stay hidden.
data GhcSession = GhcSession
  { gsEnvRef    :: !(IORef (Maybe HscEnv))
  , gsLibdir    :: !FilePath
  , gsLock      :: !(MVar ())
  , gsProject   :: !ProjectDir
  , gsLoadedRef :: !(IORef Bool)
    -- ^ 'True' once 'autoLoadProject' has run once. Tools that mutate
    -- the project on disk (Phase 3+ ghci_load, check_module,
    -- refactor, …) flip this back via 'invalidateLoadCache' so the
    -- next 'withGhcSession' re-enumerates and re-loads.
  }

-- | Bootstrap a 'GhcSession' for the given project. Cheap — does not
-- touch the GHC API yet. The HscEnv is created lazily on the first
-- 'withGhcSession' call, so a server that sustains a 'GhcSession'
-- slot but never dispatches an in-process tool pays nothing.
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

-- | Release any cached GHC state. Idempotent and exception-safe —
-- the 'tryTakeMVar' drains the lock non-blockingly so a call made
-- mid-flight (e.g. during an eviction after a timeout) cannot
-- deadlock waiting on an in-flight action that will never return.
killGhcSession :: GhcSession -> IO ()
killGhcSession sess = do
  _ <- tryTakeMVar (gsLock sess)
  writeIORef (gsEnvRef sess) Nothing
  writeIORef (gsLoadedRef sess) False

-- | Mark the session's auto-load cache as stale so the next
-- 'withGhcSession' re-enumerates source files and re-runs
-- @setTargets + load@. Called by Phase-3+ tools that edit the
-- project on disk (ghci_load with a new path, check_module after a
-- refactor, add_modules, remove_modules, …).
invalidateLoadCache :: GhcSession -> IO ()
invalidateLoadCache sess = writeIORef (gsLoadedRef sess) False

-- | Execute a 'Ghc' action under the session's lock. Threads the
-- cached HscEnv through, auto-loads the project on first call, and
-- persists any state changes back to the IORef on exit.
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

-- | Discover the project's Haskell sources under @src\/@ and @app\/@,
-- register them as GHC targets, compile them, and set the interactive
-- context to every module that made it plus Prelude.
--
-- Failures are swallowed silently at the @load@ level: if the project
-- doesn't compile, the interactive context still has Prelude, so
-- Prelude-scoped queries (@:t map@, …) still work. The scenarios
-- that check-against-load-failure go through the dedicated
-- 'ghci_check_module' / 'ghci_check_project' tools and are
-- unaffected.
autoLoadProject :: ProjectDir -> Ghc ()
autoLoadProject pd = do
  let root = unProjectDir pd
      searchDirs = [root </> "src", root </> "app"]
  files <- liftIO (enumerateHaskellSources searchDirs)
  -- Wire search dirs into importPaths so intra-project imports resolve
  -- (GHC uses importPaths when a target file does @import Foo.Bar@).
  dflags <- getSessionDynFlags
  let dflags' = dflags { importPaths = searchDirs <> importPaths dflags }
  _ <- setSessionDynFlags dflags'
  case files of
    [] -> pure ()
    _  -> do
      targets <- traverse (\f -> guessTarget f Nothing Nothing) files
      setTargets targets
      _ <- load LoadAllTargets
      -- Rebuild interactive context from the module graph: whatever
      -- actually loaded is importable; anything that failed is
      -- silently dropped.
      mg <- getModuleGraph
      let modImports =
            [ IIDecl (simpleImportDecl (moduleName (ms_mod ms)))
            | ms <- mgModSummaries mg
            ]
          preludeImport =
            IIDecl (simpleImportDecl (mkModuleName "Prelude"))
      setContext (preludeImport : modImports)

-- | Recursively enumerate @*.hs@ files under the given directories.
-- Directories that don't exist are skipped silently — a project
-- without an @app\/@ is fine.
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
