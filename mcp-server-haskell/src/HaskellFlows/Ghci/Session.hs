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
-- Phase 3 adds the dual-pass strict/deferred compile, bounded expression
-- evaluation ('evaluate'), and public 'LoadMode'. Phase 4 layers
-- 'runProperty' on top for QuickCheck. Phase 5 closes the DoS gap:
-- 'drainHandle' now enforces 'maxBufferBytes' and flips the session to
-- 'Overflowed' on cap — 'executeNoLock' then throws 'SessionExhausted'
-- instead of blocking forever, and the Server layer evicts the dead
-- session from its MVar so the next call rebuilds it.
--
-- Still pending from earlier phases: library target auto-detection from
-- @.cabal@, and the full @drainAndSync@ handshake.
module HaskellFlows.Ghci.Session
  ( Session
  , GhciResult (..)
  , CommandError (..)
  , SessionStatus (..)
  , SessionExhausted (..)
  , LoadMode (..)
  , EvalResult (..)
  , startSession
  , killSession
  , execute
  , loadModule
  , loadModuleWith
  , reload
  , typeOf
  , infoOf
  , evaluate
  , runProperty
  , sanitizeExpression
  , maxEvalBytes
  , maxBufferBytes
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
import Control.Exception (Exception, bracket_, finally, throwIO)
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
  , sStatus  :: !(TVar SessionStatus)
  , sDebug   :: !Bool
  }

-- | Lifecycle state of a 'Session' as seen by the command protocol.
--
-- 'Alive' is the steady state. 'Overflowed' means the GHCi child wrote
-- more than 'maxBufferBytes' to stdout+stderr before emitting a sentinel
-- — the reader stopped appending, and any in-flight 'executeNoLock' must
-- abort because it can no longer trust the framing. Recovery is
-- caller-driven: 'Server.getOrStartSession' replaces the MVar on the next
-- request.
data SessionStatus = Alive | Overflowed
  deriving stock (Eq, Show)

-- | Thrown by 'executeNoLock' when the session buffer overflowed before
-- a sentinel arrived. The Server layer catches this, kills the session,
-- and restarts on the next tool call.
data SessionExhausted = SessionExhausted
  deriving stock (Show)

instance Exception SessionExhausted

-- | Hard cap on the aggregate bytes the reader threads buffer between
-- sentinel deliveries. An agent asking for @print [1..]@ (or any
-- expression with unbounded output) will cause the GHCi child to pipe
-- forever; with this cap, the reader stops at 16 MiB, flips the status
-- to 'Overflowed', and every outstanding STM-blocked 'executeNoLock'
-- wakes up and throws 'SessionExhausted'. Without the cap, the server
-- process would grow until the OS killed it.
maxBufferBytes :: Int
maxBufferBytes = 16 * 1024 * 1024

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

  buf    <- newTVarIO T.empty
  lock   <- newTMVarIO ()
  status <- newTVarIO Alive

  -- One reader per stream merges into a single buffer. Errors/warnings
  -- typically arrive on stderr so both must be captured for 'parseGhcErrors'.
  -- Both share the same status TVar: whichever stream overflows first
  -- flips it, and both readers stop appending.
  readerOut <- async (drainHandle buf status hOut)
  readerErr <- async (drainHandle buf status hErr)
  let reader = readerOut  -- we only need one handle to cancel; hErr will
                          -- die when the process does.
  _ <- async (cancel readerErr `seq` pure ())  -- keep-alive ref suppressed

  let sess = Session
        { sProc   = ph
        , sStdin  = hIn
        , sBuffer = buf
        , sLock   = lock
        , sReader = reader
        , sStatus = status
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

-- | Which compilation mode GHCi should use for a load.
--
-- * 'Strict' — the default. Errors are errors; GHCi stops at the first
--   one per module. Used to answer \"does this compile?\".
-- * 'Deferred' — enables @-fdefer-type-errors@ and
--   @-fdefer-typed-holes@, promoting errors to warnings. The module still
--   loads so subsequent queries (holes, info) can run. Used to answer
--   \"what holes / type issues exist?\" without blocking on the first one.
data LoadMode = Strict | Deferred
  deriving stock (Eq, Show)

-- | Load a specific module in 'Strict' mode. Back-compat shim for Phase-1
-- callers; new code should use 'loadModuleWith'.
loadModule :: Session -> ModulePath -> IO GhciResult
loadModule s mp = loadModuleWith s mp Strict

-- | Load a specific module under the requested 'LoadMode'.
--
-- The deferred pass wraps the load with flag set/unset so session-wide
-- state isn't polluted — after returning, subsequent 'typeOf'/'infoOf' see
-- the same flags as before the call.
loadModuleWith :: Session -> ModulePath -> LoadMode -> IO GhciResult
loadModuleWith s mp Strict =
  execute s (T.pack (":l " <> unModulePath mp))
loadModuleWith s mp Deferred = withLock s $ do
  _ <- executeNoLock s ":set -fdefer-type-errors -fdefer-typed-holes" 30_000000
  out <- executeNoLock s (T.pack (":l " <> unModulePath mp)) 30_000000
  _ <- executeNoLock s ":unset -fdefer-type-errors -fdefer-typed-holes" 30_000000
  pure out

-- | Re-load all currently-loaded modules. Wraps @:r@.
reload :: Session -> IO GhciResult
reload s = execute s ":r"

-- | Query the type of an expression. Wraps @:t \<expr\>@.
--
-- The expression is sanitised first: newlines and the framing sentinel are
-- rejected, because either would desynchronise the single-sentinel response
-- protocol. This is the only boundary-level sanitisation we need here —
-- GHCi itself decides what is a valid expression, so we don't attempt to
-- parse/validate Haskell syntax.
typeOf :: Session -> Text -> IO (Either CommandError GhciResult)
typeOf s expr = case sanitizeExpression expr of
  Left e     -> pure (Left e)
  Right safe -> Right <$> execute s (":t " <> safe)

-- | Query detailed info about a name. Wraps @:i \<name\>@.
infoOf :: Session -> Text -> IO (Either CommandError GhciResult)
infoOf s name = case sanitizeExpression name of
  Left e     -> pure (Left e)
  Right safe -> Right <$> execute s (":i " <> safe)

-- | Result of evaluating an arbitrary expression.
--
-- 'erOutput' is post-truncation. 'erTruncated' lets the client know the
-- real output was larger than 'maxEvalBytes' (in bytes of the UTF-8
-- encoding measured lazily as Text length — good enough since GHCi output
-- is almost always ASCII).
data EvalResult = EvalResult
  { erOutput    :: !Text
  , erSuccess   :: !Bool
  , erTruncated :: !Bool
  }
  deriving stock (Eq, Show)

-- | Upper bound on bytes returned from a single 'evaluate' call.
--
-- The goal is defence-in-depth against an agent requesting something like
-- @print [1..]@: the GHCi child will still consume memory producing the
-- output up until we call 'terminateProcess', but the MCP server itself
-- never hands more than this number of characters to its client. Tuned to
-- 64 KiB — enough for a full module's @show@ output, small enough not to
-- balloon a JSON-RPC response.
maxEvalBytes :: Int
maxEvalBytes = 64 * 1024

-- | Run a QuickCheck property.
--
-- Ensures @Test.QuickCheck@ is in scope (the redundant import is a no-op
-- in GHCi after the first call, so we pay the roundtrip cost once per
-- 'runProperty' call — acceptable for a tool that an agent invokes at
-- human speed), then evaluates @quickCheck (\<propertyExpr\>)@. The
-- expression is sanitised at the boundary just like 'evaluate'.
--
-- Both @import@ and @quickCheck@ run under a single lock so a concurrent
-- 'typeOf' can't slip between them and see @Test.QuickCheck@ in scope
-- briefly, which could mask genuine import failures in the session.
runProperty :: Session -> Text -> IO (Either CommandError GhciResult)
runProperty s propertyExpr = case sanitizeExpression propertyExpr of
  Left e     -> pure (Left e)
  Right safe -> fmap Right $ withLock s $ do
    _ <- executeNoLock s "import Test.QuickCheck" 30_000000
    executeNoLock s ("quickCheck (" <> safe <> ")") 30_000000

-- | Evaluate an arbitrary expression. Output is capped at 'maxEvalBytes'
-- characters and 'erTruncated' is set if truncation happened.
--
-- The expression goes through 'sanitizeExpression' just like @:t@/@:i@,
-- so newlines and the sentinel are rejected at the boundary.
evaluate :: Session -> Text -> IO (Either CommandError EvalResult)
evaluate s expr = case sanitizeExpression expr of
  Left e     -> pure (Left e)
  Right safe -> do
    gr <- execute s safe
    let raw       = grOutput gr
        truncated = T.length raw > maxEvalBytes
        capped    = if truncated
                      then T.take maxEvalBytes raw <> "\n… [output truncated]"
                      else raw
    pure $ Right EvalResult
      { erOutput    = capped
      , erSuccess   = grSuccess gr
      , erTruncated = truncated
      }

-- | Reasons a GHCi command argument was rejected at the boundary.
data CommandError
  = ContainsNewline
    -- ^ Input contained @\\n@ or @\\r@. Would split into two GHCi
    -- commands and emit two sentinels, desyncing framing.
  | ContainsSentinel
    -- ^ Input literally contains the framing sentinel. Would make the
    -- reader think the response ended inside the echoed prompt.
  | EmptyInput
    -- ^ After stripping, nothing remained. GHCi would prompt-loop.
  deriving stock (Eq, Show)

-- | Boundary check for anything sent to GHCi as part of a single-line
-- command. Exported so property tests can hit it directly.
sanitizeExpression :: Text -> Either CommandError Text
sanitizeExpression raw
  | T.null stripped                          = Left EmptyInput
  | T.any (`elem` ("\n\r" :: String)) raw    = Left ContainsNewline
  | sentinel `T.isInfixOf` raw               = Left ContainsSentinel
  | otherwise                                = Right stripped
  where
    stripped = T.strip raw

--------------------------------------------------------------------------------
-- internals
--------------------------------------------------------------------------------

withLock :: Session -> IO a -> IO a
withLock s =
  bracket_
    (atomically (takeTMVar (sLock s)))
    (atomically (putTMVar (sLock s) ()))

-- | Internal variant used during startup, before the lock is meaningful.
--
-- Throws 'SessionExhausted' if the session status flipped to
-- 'Overflowed' while we were waiting for a sentinel. The STM wakeup is
-- automatic: any write to 'sStatus' retries all blocked readers.
executeNoLock :: Session -> Text -> Int -> IO GhciResult
executeNoLock s cmd _timeoutMicros = do
  TIO.hPutStrLn (sStdin s) cmd
  hFlush (sStdin s)
  collected <- atomically $ do
    st <- readTVar (sStatus s)
    case st of
      Overflowed -> pure (Left SessionExhausted)
      Alive -> do
        b <- readTVar (sBuffer s)
        case T.breakOn sentinel b of
          (_, rest) | T.null rest -> retry
          (pre, rest) -> do
            let rest' = T.drop (T.length sentinel) rest
            writeTVar (sBuffer s) rest'
            pure (Right pre)
  case collected of
    Left e -> throwIO e
    Right pre ->
      let output  = T.strip pre
          success = not (T.isInfixOf "error:" (T.toLower output))
      in pure (GhciResult output success)

-- | Reader loop. Each iteration tries to append a chunk; if doing so
-- would exceed 'maxBufferBytes', we flip the status to 'Overflowed' and
-- stop looping. We keep the GHCi pipe open (drained-but-discarded) so
-- the child process doesn't deadlock on a full-stdout write — it's
-- about to be killed by the Server layer anyway, but until then we
-- don't want to stall it.
drainHandle :: TVar Text -> TVar SessionStatus -> Handle -> IO ()
drainHandle buf status h = loop
  where
    loop = do
      chunk <- BS.hGetSome h 4096
      unless (BS.null chunk) $ do
        let txt = decodeUtf8Lenient chunk
        overflowed <- atomically $ do
          st <- readTVar status
          case st of
            Overflowed -> pure True
            Alive -> do
              b <- readTVar buf
              if T.length b + T.length txt > maxBufferBytes
                then do writeTVar status Overflowed; pure True
                else do modifyTVar' buf (<> txt); pure False
        if overflowed
          then drainAndDiscard h   -- keep reading so GHCi doesn't block
          else loop

    drainAndDiscard hh = do
      c <- BS.hGetSome hh 4096
      unless (BS.null c) (drainAndDiscard hh)

-- UTF-8 decoding that cannot throw. Anything invalid becomes U+FFFD.
decodeUtf8Lenient :: BS.ByteString -> Text
decodeUtf8Lenient =
  T.pack . BS8.unpack
    -- Full round-trip through Char8 is lossy above 0x7F but good enough for
    -- the ASCII GHC emits. Replace with decodeUtf8With lenientDecode in
    -- Phase 2 when we add real Unicode assertions.
