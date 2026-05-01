-- | Flow: 'ghc_property_audit' Phase 1 — pair-combination
-- skeleton (#64).
--
-- Phase 1 verifies the structural surface of the audit:
--
--   * Empty store → properties_checked=0, pairs_checked=0,
--     wall_time_ms ≥ 0.
--   * Single stored property → pairs_checked=0 (no
--     contradiction possible with itself).
--
-- A scenario that drives the actual contradiction-detection
-- probe through a runnable cabal-repl is deferred to Phase 2,
-- where 'arity-aware-pairing' makes the probe robust enough
-- to evaluate without manual library setup.
module Scenarios.FlowPropertyAudit
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, fieldInt)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold so the property store has a home.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("audit-demo" :: Text) ])

  -- Step 2 — empty-store call. The auditor reports zero pairs
  -- without trying to run any probe.
  t0 <- stepHeader 1 "ghc_property_audit on empty store (#64)"
  rEmpty <- Client.callTool c GhcPropertyStore (object [ "action" .= ("audit" :: Text) ])
  let okEmpty = statusOk rEmpty == Just True
              && fieldInt "properties_checked" rEmpty == Just 0
              && fieldInt "pairs_checked" rEmpty == Just 0
  cEmpty <- liveCheck $ checkPure
    "empty store → properties_checked=0, pairs_checked=0"
    okEmpty
    ("Got: " <> truncRender rEmpty)
  stepFooter 1 t0

  -- Step 3 — plant a single property and audit again. Still no
  -- pairs (n=1 → n*(n-1)/2 = 0).
  t1 <- stepHeader 2 "ghc_property_audit on 1-element store (#64)"
  let storeDir = projectDir </> ".haskell-flows"
  createDirectoryIfMissing True storeDir
  TIO.writeFile (storeDir </> "properties.json")
    "[{\"expression\":\"\\\\x -> x == (x :: Int)\",\
    \\"module\":\"src/Foo.hs\",\"passed\":1,\"updated\":0}]"
  rOne <- Client.callTool c GhcPropertyStore (object [ "action" .= ("audit" :: Text) ])
  let okOne = statusOk rOne == Just True
            && fieldInt "properties_checked" rOne == Just 1
            && fieldInt "pairs_checked" rOne == Just 0
  cOne <- liveCheck $ checkPure
    "1-property store → properties_checked=1, pairs_checked=0"
    okOne
    ("Got: " <> truncRender rOne)
  stepFooter 2 t1

  -- Step 4 — Issue #77: two duplicate-expression rows under
  -- different module shapes (the cascade-of-#74 corruption shape).
  -- After dedupe the audit must see ONE property and emit zero
  -- pairs — not two distinct ones whose conjunction is vacuous.
  t2 <- stepHeader 3 "ghc_property_audit dedupes by expression (#77)"
  TIO.writeFile (storeDir </> "properties.json")
    "[{\"expression\":\"\\\\x -> x == (x :: Int)\",\
    \\"module\":\"Foo\",\"passed\":1,\"updated\":0},\
    \{\"expression\":\"\\\\x -> x == (x :: Int)\",\
    \\"module\":\"src/Foo.hs\",\"passed\":1,\"updated\":0}]"
  rDup <- Client.callTool c GhcPropertyStore (object [ "action" .= ("audit" :: Text) ])
  let okDup = statusOk rDup == Just True
            && fieldInt "properties_checked" rDup == Just 1
            && fieldInt "pairs_checked" rDup == Just 0
  cDup <- liveCheck $ checkPure
    "duplicate expression collapses to 1 property"
    okDup
    ("Got: " <> truncRender rDup)
  stepFooter 3 t2

  pure [cEmpty, cOne, cDup]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
