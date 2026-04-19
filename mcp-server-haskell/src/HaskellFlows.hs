-- | Top-level re-export façade for the haskell-flows MCP library.
--
-- Consumers (the executable entrypoint, tests) import this module instead
-- of reaching into the internal hierarchy directly.
module HaskellFlows
  ( module HaskellFlows.Mcp.Server
  , module HaskellFlows.Mcp.Transport
  ) where

import HaskellFlows.Mcp.Server
import HaskellFlows.Mcp.Transport
