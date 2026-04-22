-- | Phase-1 scaffolding for the GHC-API-in-process migration
-- (docs/GHC-API-rewrite-plan.md). Cohabits with the legacy
-- 'HaskellFlows.Ghci.Session.Session' during Phases 1-6; Phase 7 promotes
-- it to primary and the subprocess-ghci implementation is retired for
-- the 22 tools that don't need runtime randomisation.
--
-- Invariants, enforced by construction:
--
-- * Single-writer to the HscEnv per session. The GHC API is not
--   thread-safe within one HscEnv, so a per-session 'MVar ()'
--   serialises every 'withGhcSession' call. Concurrent sessions are
--   still permitted — the lock is scoped to one 'GhcSession', not
--   global — which is the foundation the plan's \"parallel tool
--   calls\" benefit (#3) eventually builds on.
--
-- * HscEnv cached across calls via an 'IORef'. The first
--   'withGhcSession' boots a fresh environment from default
--   'DynFlags'; subsequent calls restore whatever the previous
--   action left behind (loaded modules, interactive context, …).
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
  ) where

import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, withMVar)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC
  ( Ghc
  , getSession
  , getSessionDynFlags
  , runGhc
  , setSession
  , setSessionDynFlags
  )
import GHC.Driver.Env (HscEnv)
import GHC.Paths (libdir)

import HaskellFlows.Types (ProjectDir)

-- | A persistent GHC-API session scoped to a single Haskell project.
--
-- Field selectors are exported sparingly — only what the server layer
-- needs to read ('gsProject'). The HscEnv cache, libdir, and lock are
-- implementation details and stay hidden.
data GhcSession = GhcSession
  { gsEnvRef  :: !(IORef (Maybe HscEnv))
  , gsLibdir  :: !FilePath
  , gsLock    :: !(MVar ())
  , gsProject :: !ProjectDir
  }

-- | Bootstrap a 'GhcSession' for the given project. Cheap — does not
-- touch the GHC API yet. The HscEnv is created lazily on the first
-- 'withGhcSession' call, so a server that sustains a 'GhcSession'
-- slot but never dispatches an in-process tool pays nothing.
startGhcSession :: ProjectDir -> IO GhcSession
startGhcSession pd = do
  ref  <- newIORef Nothing
  lock <- newMVar ()
  pure GhcSession
    { gsEnvRef  = ref
    , gsLibdir  = libdir
    , gsLock    = lock
    , gsProject = pd
    }

-- | Release any cached GHC state. Idempotent and exception-safe —
-- the 'tryTakeMVar' drains the lock non-blockingly so a call made
-- mid-flight (e.g. during an eviction after a timeout) cannot
-- deadlock waiting on an in-flight action that will never return.
killGhcSession :: GhcSession -> IO ()
killGhcSession sess = do
  _ <- tryTakeMVar (gsLock sess)
  writeIORef (gsEnvRef sess) Nothing

-- | Execute a 'Ghc' action under the session's lock, threading the
-- cached HscEnv through. First call bootstraps fresh state from
-- default 'DynFlags'; later calls restore the HscEnv from the IORef
-- so loaded modules and interactive context persist.
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
    result <- act
    env'   <- getSession
    liftIO (writeIORef (gsEnvRef sess) (Just env'))
    pure result
