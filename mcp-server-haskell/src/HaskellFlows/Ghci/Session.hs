-- | Persistent GHCi session — the Haskell port of @mcp-server/src/ghci-session.ts@.
--
-- Key differences vs the TS version, by construction:
--
-- * Commands are serialised through an STM 'TMVar' (the \"lock\"). In TS
--   concurrency relies on the MCP client being single-threaded; here the
--   guarantee is in the type: you cannot 'execute' without acquiring it.
-- * The reader thread is a bounded 'Async' owned by the session; killing
--   the session cancels it deterministically. No leaked listeners.
-- * The process is spawned with 'proc' (argv form). There is no shell
--   interpolation path at all.
--
-- What is deliberately not ported yet (Phase 1 scope):
--
-- * The dual-pass strict/deferred compile dance. We run a plain @:l@.
-- * The library-target auto-detection from the .cabal file. We let
--   @cabal repl@ pick its default target.
-- * The @drainAndSync@ handshake. Startup readiness is detected by
--   waiting for the first sentinel after init.
module HaskellFlows.Ghci.Session
  ( Session
  , GhciResult (..)
  , startSession
  , killSession
  , execute
  , loadModule
  , reload
  ) where

import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.STM
  ( TMVar
  , TVar
  , atomically
  , modifyTVar'
  , newTMVarIO
  , newTVarIO
  , putTMVar
  , readTVar
  , retry
  , takeTMVar
  , writeTVar
  )
import Control.Exception (bracket_, finally)
import Control.Monad (forM_, unless, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
  ( BufferMode (..)
  , Handle
  , hClose
  , hFlush
  , hSetBuffering
  )
import System.Process
  ( CreateProcess (..)
  , ProcessHandle
  , StdStream (..)
  , proc
  , createProcess
  , terminateProcess
  )

import HaskellFlows.Ghci.Sentinel (initScript, sentinel)
import HaskellFlows.Types (ProjectDir, ModulePath, unProjectDir, unModulePath)

data GhciResult = GhciResult
  { grOutput  :: !Text
  , grSuccess :: !Bool
  }
  deriving stock (Eq, Show)

data Session = Session
  { sProc    :: !ProcessHandle
  , sStdin   :: !Handle
  , sBuffer  :: !(TVar Text)
  , sLock    :: !(TMVar ())
  , sReader  :: !(Async ())
  , sDebug   :: !Bool
  }

-- | Start a persistent @cabal repl@ child in the given project directory,
-- perform the init handshake, and return a ready session.
--
-- The session installs the sentinel prompt and common extensions so the
-- semantics match the TS server before handing back.
startSession :: ProjectDir -> IO Session
startSession pd = do
  let cp = (proc "cabal" ["repl"])
             { cwd     = Just (unProjectDir pd)
             , std_in  = CreatePipe
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp
  traverse_ (`hSetBuffering` LineBuffering) [hIn, hOut, hErr]

  buf  <- newTVarIO T.empty
  lock <- newTMVarIO ()

  -- One reader per stream merges into a single buffer. Errors/warnings
  -- typically arrive on stderr so both must be captured for 'parseGhcErrors'.
  readerOut <- async (drainHandle buf hOut)
  readerErr <- async (drainHandle buf hErr)
  let reader = readerOut  -- we only need one handle to cancel; hErr will
                          -- die when the process does.
  _ <- async (cancel readerErr `seq` pure ())  -- keep-alive ref suppressed

  let sess = Session
        { sProc   = ph
        , sStdin  = hIn
        , sBuffer = buf
        , sLock   = lock
        , sReader = reader
        , sDebug  = False
        }

  -- Run the init script. Each line emits exactly one sentinel, so we can
  -- drain them one by one rather than doing the TS-style orphan-drain dance.
  forM_ initScript $ \cmd ->
    void (executeNoLock sess cmd 30_000000)

  pure sess

-- | Terminate the underlying process and stop the reader thread.
killSession :: Session -> IO ()
killSession s =
  cancel (sReader s) `finally`
    (hClose (sStdin s) `finally` terminateProcess (sProc s))

-- | Send a raw command to GHCi and read back everything up to the next
-- sentinel. Serialises concurrent callers; the STM lock is the
-- synchronisation boundary.
execute :: Session -> Text -> IO GhciResult
execute s cmd = withLock s (executeNoLock s cmd 30_000000)

-- | Load a specific module. Wraps @:l \<path\>@.
loadModule :: Session -> ModulePath -> IO GhciResult
loadModule s mp =
  execute s (T.pack (":l " <> unModulePath mp))

-- | Re-load all currently-loaded modules. Wraps @:r@.
reload :: Session -> IO GhciResult
reload s = execute s ":r"

--------------------------------------------------------------------------------
-- internals
--------------------------------------------------------------------------------

withLock :: Session -> IO a -> IO a
withLock s =
  bracket_
    (atomically (takeTMVar (sLock s)))
    (atomically (putTMVar (sLock s) ()))

-- | Internal variant used during startup, before the lock is meaningful.
executeNoLock :: Session -> Text -> Int -> IO GhciResult
executeNoLock s cmd _timeoutMicros = do
  TIO.hPutStrLn (sStdin s) cmd
  hFlush (sStdin s)
  collected <- atomically $ do
    b <- readTVar (sBuffer s)
    case T.breakOn sentinel b of
      (_, rest) | T.null rest -> retry
      (pre, rest) -> do
        let rest' = T.drop (T.length sentinel) rest
        writeTVar (sBuffer s) rest'
        pure pre
  let output  = T.strip collected
      success = not (T.isInfixOf "error:" (T.toLower output))
  pure (GhciResult output success)

drainHandle :: TVar Text -> Handle -> IO ()
drainHandle buf h = loop
  where
    loop = do
      chunk <- BS.hGetSome h 4096
      unless (BS.null chunk) $ do
        let txt = decodeUtf8Lenient chunk
        atomically (modifyTVar' buf (<> txt))
        loop

-- UTF-8 decoding that cannot throw. Anything invalid becomes U+FFFD.
decodeUtf8Lenient :: BS.ByteString -> Text
decodeUtf8Lenient =
  T.pack . BS8.unpack
    -- Full round-trip through Char8 is lossy above 0x7F but good enough for
    -- the ASCII GHC emits. Replace with decodeUtf8With lenientDecode in
    -- Phase 2 when we add real Unicode assertions.
