-- | The process's ORIGINAL working directory, captured once at
-- startup — BEFORE any ghcide session boots and chdirs into its
-- project root ('Ghc.IdeSession.bootIdeSession' must anchor implicit
-- cradle discovery at the workspace root, exactly like HLS).
--
-- Why this exists (W3.1 dogfood finding #1): after the first session
-- boot, every RELATIVE path argument a client sends resolves from
-- INSIDE the project — @ghc_project(action=\"create\",
-- path=\"../playground/demo\")@ silently landed in
-- @playground/playground/demo@, and the second create attempt
-- reported the phantom files as a clash. Client-supplied paths that
-- address the OUTSIDE world must resolve against the startup cwd,
-- the same anchor @HASKELL_PROJECT_DIR@ itself gets.
module HaskellFlows.Mcp.ServerStartup
  ( startupDir
  , captureStartupDir
  , setStartupDir
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Directory (getCurrentDirectory)
import System.IO.Unsafe (unsafePerformIO)

-- | NOINLINE + eager capture: 'captureStartupDir' runs first thing in
-- @main@, so the lazy default can never be forced after a chdir in
-- practice — but the eager write makes that a guarantee, not a hope.
{-# NOINLINE startupDirRef #-}
startupDirRef :: IORef FilePath
startupDirRef = unsafePerformIO (newIORef =<< getCurrentDirectory)

-- | Record the process cwd. Call exactly once, at the very top of
-- @main@, before any tool can serve.
captureStartupDir :: IO ()
captureStartupDir = getCurrentDirectory >>= writeIORef startupDirRef

-- | Explicit override — unit tests only (simulate a launch cwd
-- without forking a process).
setStartupDir :: FilePath -> IO ()
setStartupDir = writeIORef startupDirRef

-- | The cwd the server was launched from.
startupDir :: IO FilePath
startupDir = readIORef startupDirRef
