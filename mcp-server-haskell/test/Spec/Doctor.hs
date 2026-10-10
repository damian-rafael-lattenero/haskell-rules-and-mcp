-- | Unit tests for the @--doctor@ render ('HaskellFlows.Mcp.Doctor').
--
-- The probes themselves are IO (they shell out to @tool --version@);
-- what we pin here is the PURE shaping: marks, grouping order,
-- install hints for missing entries, and the gate-only exit verdict.
module Spec.Doctor
  ( testDoctorRenderMarksAndHints
  , testDoctorVerdictGatesOnly
  , testDoctorRenderGroupsHardFirst
  , testDoctorInstallHintsClosedMap
  ) where

import Data.List (isInfixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import HaskellFlows.Mcp.Doctor (doctorOk, renderDoctor)
import HaskellFlows.Tool.ToolchainStatus
  ( Entry (..)
  , installHintFor
  , probeTargets
  )

-- Fixture helpers -----------------------------------------------------------

mkEntry :: Text -> Text -> Bool -> Entry
mkEntry name category ok =
  Entry
    { eName = name
    , eCategory = category
    , eAvailable = ok
    , ePath = if ok then Just ("/usr/bin/" <> T.unpack name) else Nothing
    , eVersion = if ok then Just "9.9.9" else Nothing
    }

healthySet :: [Entry]
healthySet =
  [ mkEntry "ghc" "gate" True
  , mkEntry "cabal" "gate" True
  , mkEntry "hlint" "gate" True
  , mkEntry "fourmolu" "workflow" True
  , mkEntry "hoogle" "query" False
  ]

brokenGateSet :: [Entry]
brokenGateSet =
  [ mkEntry "ghc" "gate" True
  , mkEntry "cabal" "gate" False
  , mkEntry "hlint" "gate" True
  ]

-- Tests ---------------------------------------------------------------------

testDoctorRenderMarksAndHints :: IO Bool
testDoctorRenderMarksAndHints = pure $
  let out = renderDoctor brokenGateSet
  in and
       [ "✓ ghc" `isInfixOf` out
       , "✗ cabal" `isInfixOf` out
       , "fix: ghcup install cabal --set" `isInfixOf` out
       , "FAIL" `isInfixOf` out
       , "cabal" `isInfixOf` out  -- named in the FAIL line
       ]

testDoctorVerdictGatesOnly :: IO Bool
testDoctorVerdictGatesOnly = pure $
     doctorOk healthySet             -- hoogle missing is optional: OK
  && not (doctorOk brokenGateSet)    -- cabal missing is a gate: FAIL
  && doctorOk []                     -- vacuous: nothing hard is missing

testDoctorRenderGroupsHardFirst :: IO Bool
testDoctorRenderGroupsHardFirst = pure $
  -- healthySet: all gates present, hoogle optional missing → OK verdict
  let ls = lines (renderDoctor healthySet)
      hardIx = lineIndex "Hard requirements" ls
      optIx = lineIndex "Optional" ls
  in and
       [ isJust hardIx
       , isJust optIx
       , hardIx < optIx
       , "OK — hard requirements met" `isInfixOf` unlines ls
       , "✗ hoogle" `isInfixOf` unlines ls  -- optional gap still rendered
       ]
  where
    lineIndex needle ls =
      case [i | (i, True) <- zip [0 :: Int ..] (map (needle `isInfixOf`) ls)] of
        (i : _) -> Just i
        []      -> Nothing

testDoctorInstallHintsClosedMap :: IO Bool
testDoctorInstallHintsClosedMap = pure $
  all (\name -> not ("no install hint" `T.isInfixOf` installHintFor name))
    [ n | (n, _, _) <- probeTargets ]
