module Main where

import HaskellFlows.Mcp.Server    (defaultServer)
import HaskellFlows.Mcp.Transport (runStdioTransport)

main :: IO ()
main = do
  server <- defaultServer
  runStdioTransport server
