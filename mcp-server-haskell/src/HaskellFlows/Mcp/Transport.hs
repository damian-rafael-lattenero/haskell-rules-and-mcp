-- | Stdio transport for the MCP server — read newline-delimited JSON from
-- stdin, dispatch to 'handleRequest', write newline-delimited JSON to
-- stdout.
--
-- The TS MCP SDK supports a Content-Length framed mode as well; Claude
-- Desktop / Code use the newline-delimited mode for stdio servers, which
-- is what we implement here. A future phase can add the framed mode
-- behind a CLI flag if an integration requires it.
module HaskellFlows.Mcp.Transport
  ( runStdioTransport
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Aeson (eitherDecodeStrict', encode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import System.IO
  ( BufferMode (..)
  , hFlush
  , hPutStrLn
  , hSetBuffering
  , isEOF
  , stderr
  , stdin
  , stdout
  )

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.Server (Server, handleRequest)

runStdioTransport :: Server -> IO ()
runStdioTransport srv = do
  -- MCP: stdout is the protocol channel, stderr is for diagnostics.
  -- LineBuffering on stdout guarantees we flush per JSON message so the
  -- client never blocks waiting for a newline we have already written.
  hSetBuffering stdin  LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  loop
  where
    loop = do
      eof <- isEOF
      unless eof $ do
        line <- BS.hGetLine stdin
        case eitherDecodeStrict' line of
          Left parseErrTxt ->
            -- No id = no reply recipient. Log and keep going.
            hPutStrLn stderr ("[haskell-flows] parse error: " <> parseErrTxt)
          Right req -> do
            result <- try (handleRequest srv req) :: IO (Either SomeException (Maybe Response))
            case result of
              Left ex ->
                hPutStrLn stderr
                  ("[haskell-flows] handler threw: " <> show ex)
              Right Nothing -> pure ()
              Right (Just resp) -> do
                BL.hPutStr stdout (encode resp)
                BS.hPutStr stdout "\n"
                hFlush stdout
        loop
