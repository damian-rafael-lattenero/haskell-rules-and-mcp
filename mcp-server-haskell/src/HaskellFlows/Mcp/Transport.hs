-- | Stdio transport for the MCP server — read newline-delimited JSON
-- from stdin, dispatch to 'handleRequest', write newline-delimited JSON
-- to stdout.
--
-- F2 rewrite — three structural changes over the sequential loop:
--
-- 1. __Concurrent dispatch__ (F13/F15): the reader thread only reads.
-- Every request runs on its own worker thread (bounded by
-- 'maxConcurrentCalls'); a wedged handler can no longer block the
-- reader or other requests. Responses may arrive out of order — JSON-RPC
-- ids carry the correlation.
--
-- 2. __Deliver-once gate + watchdog__ (F13 amplifier): each request
-- gets an MVar gate. Worker and watchdog race to close it; whoever is
-- first writes the response, the other's write is dropped. The
-- watchdog fires after the outer tool ceiling (plus grace) and
-- delivers a JSON-RPC timeout error — covering handlers stuck in
-- uninterruptible sections where 'System.Timeout' cannot fire.
--
-- 3. __Background warmup__: with @HASKELL_FLOWS_BACKEND=ghcide@, the
-- session boots in the background at startup so the first ghcide tool
-- call is warm. Disable with @HASKELL_FLOWS_WARMUP=0@.
--
-- Handler exceptions now produce a JSON-RPC internal-error response
-- instead of being swallowed into a stderr log (the old silent-drop).
module HaskellFlows.Mcp.Transport
  ( runStdioTransport
  , deliverOnce
  , maxConcurrentCalls
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar, withMVar)
import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception (SomeException, finally, try)
import Control.Monad (unless, void, when)
import Data.Aeson (eitherDecodeStrict', encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import System.Environment (lookupEnv)
import System.IO
  ( BufferMode (..)
  , hFlush
  , hPutStr
  , hPutStrLn
  , hSetBuffering
  , isEOF
  , stderr
  , stdin
  , stdout
  )
import Text.Read (readMaybe)
import qualified Data.Text as T

import HaskellFlows.Config (Limits (..), unMicros)
import HaskellFlows.Ghc.IdeSession (BackendChoice (..))
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.Server (Server (..), handleRequest)
import HaskellFlows.Tool.IdeBacked (warmupIdeSession)

-- | Maximum in-flight tool requests (env-overridable). Beyond this,
-- workers queue on the semaphore — the reader never blocks.
maxConcurrentCalls :: IO Int
maxConcurrentCalls = do
  mv <- lookupEnv "HASKELL_FLOWS_MAX_CONCURRENT_CALLS"
  pure $ case mv >>= readMaybe of
    Just n | n >= 1 -> n
    _ -> 4

-- | First caller wins: close the gate and run the action (True), or
-- find it already closed and skip (False). Shared by the worker
-- delivery and the watchdog — exactly one response per request ever
-- hits the wire.
deliverOnce :: MVar () -> IO () -> IO Bool
deliverOnce gate action = do
  won <- tryTakeMVar gate
  case won of
    Just () -> action >> pure True
    Nothing -> pure False

runStdioTransport :: Server -> IO ()
runStdioTransport srv = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  writeLock <- newMVar ()
  sem <- newQSem =<< maxConcurrentCalls
  warmupInBackground
  loop sem writeLock
  where
    -- Grace over Server.runTool's own ceiling: the inner timeout gets
    -- to fire first with its structured envelope; the watchdog is the
    -- last resort for uninterruptible sections.
    budgetMicros = unMicros (outerToolCeiling (srvLimits srv)) + 5_000_000

    loop sem wl = do
      eof <- isEOF
      unless eof $ do
        line <- BS.hGetLine stdin
        case eitherDecodeStrict' line of
          Left parseErrTxt ->
            hPutStrLn stderr ("[haskell-flows] parse error: " <> parseErrTxt)
          Right req -> route sem wl req
        loop sem wl

    -- Notifications run inline (no response, cheap bookkeeping);
    -- requests go to a bounded worker pool with a watchdog.
    route sem wl req = case reqId req of
      Nothing -> void (handleRequest srv req)
      Just rid -> do
        gate <- newMVar ()
        void . forkIO $
          (waitQSem sem >> worker wl gate rid req)
            `finally` signalQSem sem
        void (forkIO (void (watchdog wl gate rid)))

    worker wl gate rid req = do
      result <- try (handleRequest srv req) :: IO (Either SomeException (Maybe Response))
      let mresp = case result of
            Left ex ->
              Just (Response rid (Left (internalErr ("handler threw: " <> T.pack (show ex)))))
            Right r -> r
      won <- deliverOnce gate $ case mresp of
        Nothing -> pure ()
        Just resp -> writeResponse wl resp
      unless won $
        hPutStrLn stderr "[haskell-flows] late response dropped (watchdog answered first)"

    watchdog wl gate rid = do
      threadDelay budgetMicros
      won <-
        deliverOnce gate $ do
          hPutStrLn stderr
            ("[haskell-flows] watchdog: no response for this request after "
               <> show (budgetMicros `div` 1_000_000)
               <> "s — delivering timeout; worker may be wedged uninterruptibly")
          writeResponse wl (Response rid (Left (internalErr "tool call exceeded the outer ceiling (watchdog)")))
      pure won

    writeResponse wl resp = withMVar wl $ \_ -> do
      BL.hPutStr stdout (encode resp)
      BS.hPutStr stdout "\n"
      hFlush stdout

    warmupInBackground = when (srvBackend srv == BackendGhcide) $ do
      w <- lookupEnv "HASKELL_FLOWS_WARMUP"
      when (w /= Just "0") $ void $ forkIO $ do
        hPutStrLn stderr "[haskell-flows] warmup: booting ghcide session in background"
        warmupIdeSession (srvIdeSession srv) (srvProjectDir srv)
        hPutStrLn stderr "[haskell-flows] warmup: ghcide session ready"
