-- | Unit tests for nextStep routing (#95) and Staleness detection (#280).
-- Tests the post-call hints emitted after Gate, QcExport, Determinism,
-- AddImport, Modules, and other tools.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepUnit
  ( testClampRunsCapsHigh
  , testClampRunsFloorsLow
  , testClampRunsPassThrough
  , testStalenessWired
  , testStalenessIdentityDiffers
  , testStalenessIdentityMatches
  ) where

import qualified Data.Aeson as A
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson.Key as AKey
import Data.Maybe (isNothing)
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.Staleness (StalenessReport (..), binaryIdentityStale)
import HaskellFlows.Mcp.ToolName (ToolName (..))
import qualified HaskellFlows.Tool.Determinism as DeterminismTool

import Spec.Helpers (withTempProject)

-- | Helper: assert the nextStep for a (tool, payload) pair points
-- at a specific follow-up tool.

assertNext :: ToolName -> A.Value -> ToolName -> Bool

assertNext tool payload expected =
  case suggestNext tool True payload of
    Just ns -> nsTool ns == expected
    Nothing -> False

testClampRunsCapsHigh :: IO Bool

testClampRunsCapsHigh =
  pure (DeterminismTool.clampRuns 2000 == DeterminismTool.maxRuns
          && DeterminismTool.maxRuns <= 20)

testClampRunsFloorsLow :: IO Bool

testClampRunsFloorsLow =
  pure (DeterminismTool.clampRuns 0 == 1
          && DeterminismTool.clampRuns (-5) == 1)

testClampRunsPassThrough :: IO Bool

testClampRunsPassThrough =
  pure (DeterminismTool.clampRuns 3 == 3
          && DeterminismTool.clampRuns DeterminismTool.maxRuns
               == DeterminismTool.maxRuns)

testStalenessWired :: IO Bool

testStalenessWired = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Server.hs"
  pure $ T.isInfixOf "import HaskellFlows.Mcp.Staleness" src
      && T.isInfixOf "srvBootPosix"            src
      && T.isInfixOf "srvBinaryPath"           src
      && T.isInfixOf "checkStaleness (srvBinaryPath" src
      && T.isInfixOf "getExecutablePath"       src

-- | #280: when the running binary's real path differs from the installed
-- canonical target, the process is on a stale subprocess — flag it (returning
-- the installed path) rather than the old mtime-only false-negative.

testStalenessIdentityDiffers :: IO Bool

testStalenessIdentityDiffers =
  pure ( binaryIdentityStale "/h/.local/bin/x" "/h/.cabal/store/NEW/x"
           == Just "/h/.cabal/store/NEW/x" )

-- | #280: when the running binary IS the installed target, it's fresh.

testStalenessIdentityMatches :: IO Bool

testStalenessIdentityMatches =
  pure (isNothing (binaryIdentityStale "/h/.cabal/store/A/x" "/h/.cabal/store/A/x"))
