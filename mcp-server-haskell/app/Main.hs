-- | @haskell-flows-mcp@ — MCP server entry point.
--
-- Default mode (no args / unknown args): runs the stdio JSON-RPC
-- transport, the only protocol-supported runtime mode today.
--
-- Recognised flags (all exit immediately after emitting the
-- requested information; never start the transport):
--
--   @--version@ | @-v@   — print the binary version + MCP
--                            protocol version + a one-line
--                            build summary, then exit 0.
--   @--help@    | @-h@   — print usage + the recognised flags,
--                            then exit 0.
--
-- The flag-parsing layer is deliberately tiny — no
-- 'optparse-applicative' dependency; the only state we need to
-- communicate at the binary boundary is "are we starting the
-- transport, or not?". Adding a real CLI parser becomes worth
-- it when the binary grows subcommands.
module Main where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn, stderr)

import HaskellFlows.Mcp.Server    (defaultServer)
import HaskellFlows.Mcp.Transport (runStdioTransport)

-- | Issue #99: version + protocol pinning at the binary
-- boundary. Hardcoded here so a host can run
-- @haskell-flows-mcp --version@ and learn what it's actually
-- talking to without spinning up the full server. Keep these
-- in sync with the cabal @version:@ field and the
-- @InitializeResult.protocolVersion@ emitted by the server.
binaryVersion :: String
binaryVersion = "0.1.0.0"

mcpProtocolVersion :: String
mcpProtocolVersion = "2025-06-18"

main :: IO ()
main = do
  args <- getArgs
  when (any isVersionFlag args) $ do
    putStrLn ("haskell-flows-mcp " <> binaryVersion)
    putStrLn ("MCP protocol: " <> mcpProtocolVersion)
    exitSuccess
  when (any isHelpFlag args) $ do
    printUsage
    exitSuccess
  -- Unknown flags route to stderr but DO NOT abort — keeping the
  -- server resilient to a host that injects a stray --debug-foo
  -- it doesn't recognise. Stdio JSON-RPC starts as long as the
  -- recognised flags didn't fire above.
  let unknown = filter (\a -> isFlag a && not (isVersionFlag a || isHelpFlag a)) args
  unless (null unknown) $
    hPutStrLn stderr ("warning: unrecognised flags ignored: " <> show unknown)
  server <- defaultServer
  runStdioTransport server

isFlag :: String -> Bool
isFlag s = "--" `isPrefixOf` s || (length s == 2 && head s == '-')

isVersionFlag :: String -> Bool
isVersionFlag s = s == "--version" || s == "-v"

isHelpFlag :: String -> Bool
isHelpFlag s = s == "--help" || s == "-h"

printUsage :: IO ()
printUsage = do
  putStrLn "haskell-flows-mcp — MCP server for property-first Haskell development"
  putStrLn ""
  putStrLn "Usage: haskell-flows-mcp [FLAGS]"
  putStrLn ""
  putStrLn "Flags:"
  putStrLn "  --version, -v   Print binary + MCP protocol version, exit"
  putStrLn "  --help, -h      Print this usage, exit"
  putStrLn ""
  putStrLn "Default: read JSON-RPC requests from stdin, write responses to stdout."
