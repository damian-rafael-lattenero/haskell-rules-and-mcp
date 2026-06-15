-- | Shared subprocess combinator for all tools that shell out to external
-- binaries (hlint, fourmolu, cabal, hoogle, …).
--
-- Every spawn goes through 'runArgv': argv-form only (no shell), with a
-- hard timeout that terminates the child on expiry so no orphaned processes
-- are left behind.
module HaskellFlows.Util.Process
  ( SubprocessResult (..)
  , SubprocessOutcome (..)
  , runArgv
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, tryReadMVar)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode)
import System.IO (hGetContents')
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )

import HaskellFlows.Config (Micros (..))

data SubprocessResult = SubprocessResult
  { srExit   :: !ExitCode
  , srStdout :: !Text
  , srStderr :: !Text
  } deriving (Eq, Show)

data SubprocessOutcome
  = Completed !SubprocessResult
  | TimedOut
  deriving (Eq, Show)

-- | Run @cmd args@ as a subprocess in argv-form (no shell interpolation).
-- Captures both stdout and stderr on background threads. Terminates the
-- child and returns 'TimedOut' if @budget@ microseconds elapse before the
-- process exits.
runArgv
  :: Micros         -- ^ wall-clock budget
  -> Maybe FilePath -- ^ working directory (@Nothing@ = inherit)
  -> FilePath       -- ^ executable
  -> [String]       -- ^ arguments
  -> IO SubprocessOutcome
runArgv budget mCwd cmd args = do
  let cp = (proc cmd args)
             { cwd     = mCwd
             , std_in  = NoStream
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  outVar   <- newEmptyMVar
  errVar   <- newEmptyMVar
  timedOut <- newEmptyMVar
  _ <- forkIO (hGetContents' hOut >>= putMVar outVar)
  _ <- forkIO (hGetContents' hErr >>= putMVar errVar)
  -- System.Timeout.timeout cannot interrupt waitForProcess on Linux: the
  -- thread blocks inside waitpid() (uninterruptible blocking FFI) and never
  -- receives the async exception. Use a dedicated timer thread that sends
  -- SIGTERM after the budget, letting waitForProcess return naturally.
  _ <- forkIO $ do
    threadDelay (unMicros budget)
    terminateProcess ph
    putMVar timedOut ()
  ec     <- waitForProcess ph
  killed <- isJust <$> tryReadMVar timedOut
  if killed
    then pure TimedOut
    else do
      o <- takeMVar outVar
      e <- takeMVar errVar
      pure $ Completed SubprocessResult
        { srExit   = ec
        , srStdout = T.pack o
        , srStderr = T.pack e
        }
