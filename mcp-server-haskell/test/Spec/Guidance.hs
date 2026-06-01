-- | Unit tests for 'Tool.Deps.parseStanzaSelector', Coverage invoke check,
-- resource-URI registration, and 'Mcp.Guidance' invariants.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Guidance
  ( testParseStanzaAccepts
  , testParseStanzaRejects
  , testCoverageInvokesHpcReport
  , testBajaRegistered
  , testResourcesRulesRead
  , testResourcesUnknown
  , testGuidanceDynamicCount
  , testGuidanceListsEveryTool
  , testGuidanceNoRetiredVocab
  , testGuidanceMdNoRetiredVocab
  , testGuidanceMentionsApi
  , testGuidanceMdMentionsApi
  , testGuidanceNoRetiredRegression
  , testGuidanceMdNoRetiredRegression
  , testGuidanceMarkdownListsEveryTool
  , testGuidanceSituationNonEmpty
  , testGuidanceNoPhantomSession
  , testDepsDescriptorNoPhantom
  , testDepsHintNoPhantom
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import HaskellFlows.Mcp.Protocol (ToolDescriptor (..))
import qualified HaskellFlows.Mcp.Guidance as Guidance
import qualified HaskellFlows.Mcp.Resources as Resources
import HaskellFlows.Tool.Deps (parseStanzaSelector)
import qualified HaskellFlows.Tool.Coverage as CoverageTool
import HaskellFlows.Types (mkProjectDir)
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import HaskellFlows.Mcp.ResourceUri
  ( ResourceUri (..)
  , resourceUriText
  )
import qualified HaskellFlows.Mcp.ResourceUri as ResourceUri
import HaskellFlows.Mcp.ToolName
  ( ToolName (..)
  , allToolNames
  )

testParseStanzaAccepts :: IO Bool
testParseStanzaAccepts = pure $
     accepts "library"          ("library", Nothing)
  && accepts "test-suite"       ("test-suite", Nothing)
  && accepts "test-suite:foo"   ("test-suite", Just "foo")
  && accepts "executable:bar"   ("executable", Just "bar")
  where
    accepts raw expected = case parseStanzaSelector raw of
      Right got -> got == expected
      Left  _   -> False

testParseStanzaRejects :: IO Bool
testParseStanzaRejects = pure $
     rejects "foo-suite"          -- unknown kind
  && rejects "library:"           -- empty name after colon
  && rejects "test-suite:bad name" -- space in name
  && rejects "test-suite/$(id)"   -- shell metacharacters
  && rejects ""                   -- empty string
  where
    rejects raw = case parseStanzaSelector raw of
      Left  _ -> True
      Right _ -> False

-- | Phase 11b F-09: @ghc_coverage@ always returned
-- @summary="No coverage metrics parsed from the cabal output"@ under
-- GHC 9.12 + cabal 3.14 because those versions of cabal no longer
-- echo the HPC summary on stdout — they only write HTML. Fix wires
-- a post-@cabal test@ @hpc report@ call into the pipeline whose text
-- output the parser already understands. The regression check here is
-- narrow: pin the static shape of Tool/Coverage.hs so a future edit
-- can't accidentally drop the @hpc report@ invocation.
testCoverageInvokesHpcReport :: IO Bool
testCoverageInvokesHpcReport = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Coverage.hs"
  pure $ T.isInfixOf "runHpcReport"         src
      && T.isInfixOf "enrichWithHpcReport"  src
      && T.isInfixOf "findTixFile"          src
      && T.isInfixOf "\"hpc\""              src
      && T.isInfixOf "\"--hpcdir=\""        src `ellipticalOr`
         T.isInfixOf "--hpcdir="            src

-- | Rescues us from the difference between `"--hpcdir="` as a
-- quoted literal in source and the concatenation form. Either is fine.
ellipticalOr :: Bool -> Bool -> Bool
ellipticalOr = (||)

-- | Phase 11n: 4 BAJA bundle tools registered in the inventory.
testBajaRegistered :: IO Bool
testBajaRegistered = pure $
  all (`elem` allToolNameTexts)
    [ "ghc_browse"
    , "ghc_quickcheck"  -- #94 Phase C: ghc_determinism merged in (runs>=N)
    , "ghc_property_store"  -- #94 Phase C step 6: subsumes ghc_property_lifecycle
    , "ghc_toolchain"  -- #94 Phase C: subsumes ghc_toolchain_warmup
    ]

-- | Phase 11l: resources/read for the rules URI returns the
-- embedded markdown; unknown URIs return Nothing.
-- | Phase 11m + BUG-09: the workflow-rules resource body is
-- rendered dynamically from the live tool descriptor list, not
-- a stale hand-edited string. Pin both the URI advertisement
-- and the body's dynamic shape.
testResourcesRulesRead :: IO Bool
testResourcesRulesRead = pure $
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
      advertised =
        ResourceUri.resourceUriText ResourceUri.WorkflowRules
          `elem` Resources.knownResourceUris
  in advertised
     && T.isInfixOf "haskell-flows" md
     && T.isInfixOf "situation" (T.toLower md)
     && T.isInfixOf (T.pack (show (length allToolDescriptors))) md

testResourcesUnknown :: IO Bool
testResourcesUnknown =
  pure ("haskell-flows://nonexistent" `notElem` Resources.knownResourceUris)

-- | BUG-05: @initialize.instructions@ used to hard-code "25 tools".
-- The fix derives the tool count from 'allToolDescriptors'. Pin
-- that the rendered text contains the live count and not any of
-- the historic stale counts.
testGuidanceDynamicCount :: IO Bool
testGuidanceDynamicCount = do
  let instructions  = Guidance.sessionInstructionsText allToolDescriptors
      liveCount     = T.pack (show (length allToolDescriptors))
      staleCounts   = ["25 tools", "26 tools", "27 tools", "28 tools"]
      hasLive       = T.isInfixOf (liveCount <> " tools") instructions
      hasAnyStale   = any (`T.isInfixOf` instructions) staleCounts
  pure (hasLive && not hasAnyStale)

-- | BUG-05: every registered tool's name must appear in the
-- rendered instructions. If a new tool ships without a mention,
-- this test fails — the forcing function that keeps the docs in
-- sync with the registry.
testGuidanceListsEveryTool :: IO Bool
testGuidanceListsEveryTool = do
  let instructions = Guidance.sessionInstructionsText allToolDescriptors
  pure (all (`T.isInfixOf` instructions) allToolNameTexts)

-- | Issue #56: the rules text emitted by 'ghc_bootstrap' is
-- baked into the binary via 'workflowRulesMarkdown' /
-- 'sessionInstructionsText'. It used to document the retired
-- subprocess GHCi model ('SessionStatus = Alive | Overflowed |
-- Dead', 'executeNoLock', 'registerDelay', 'GHCi death')  —
-- vocabulary that has nothing to do with the in-process GHC API
-- session that's actually running. Agents debugging timeouts
-- looked for invariants that didn't exist.
--
-- Pin both halves of the contract:
--   * The retired-model words must NOT appear in the rendered text.
--   * The new model words MUST appear so the bake-source isn't
--     accidentally cleared.

testGuidanceNoRetiredVocab :: IO Bool
testGuidanceNoRetiredVocab = do
  let txt = Guidance.sessionInstructionsText allToolDescriptors
      retiredTerms =
        [ "SessionStatus"
        , "executeNoLock"
        , "registerDelay"
        , "GHCi death"
        ]
  pure (not (any (`T.isInfixOf` txt) retiredTerms))

testGuidanceMdNoRetiredVocab :: IO Bool
testGuidanceMdNoRetiredVocab = do
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
      retiredTerms =
        [ "SessionStatus"
        , "executeNoLock"
        , "registerDelay"
        , "GHCi death"
        ]
  pure (not (any (`T.isInfixOf` md) retiredTerms))

testGuidanceMentionsApi :: IO Bool
testGuidanceMentionsApi = do
  let txt = T.toLower (Guidance.sessionInstructionsText allToolDescriptors)
      newTerms = map T.toLower
        [ "in-process"
        , "HscEnv"
        , "MVar"
        , "resetHscEnvInPlace"
        ]
  pure (all (`T.isInfixOf` txt) newTerms)

testGuidanceMdMentionsApi :: IO Bool
testGuidanceMdMentionsApi = do
  let md = T.toLower (Guidance.workflowRulesMarkdown allToolDescriptors)
      newTerms = map T.toLower
        [ "in-process"
        , "HscEnv"
        , "MVar"
        , "resetHscEnvInPlace"
        ]
  pure (all (`T.isInfixOf` md) newTerms)

-- | #124: the plain-text guidance must not reference the retired
-- @ghc_regression@ tool (replaced by @ghc_property_store@ in #94 Phase C).
testGuidanceNoRetiredRegression :: IO Bool
testGuidanceNoRetiredRegression = do
  let txt = Guidance.sessionInstructionsText allToolDescriptors
  pure (not ("ghc_regression" `T.isInfixOf` txt))

-- | #124: the markdown guidance must not reference the retired
-- @ghc_regression@ tool.
testGuidanceMdNoRetiredRegression :: IO Bool
testGuidanceMdNoRetiredRegression = do
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
  pure (not ("ghc_regression" `T.isInfixOf` md))

-- | BUG-09: the markdown resource must match the plain-text
-- instructions in tool coverage — both are derived from the same
-- 'allToolDescriptors', so neither can omit a tool.
testGuidanceMarkdownListsEveryTool :: IO Bool
testGuidanceMarkdownListsEveryTool = do
  let md = Guidance.workflowRulesMarkdown allToolDescriptors
  pure (all (`T.isInfixOf` md) allToolNameTexts)

-- | BUG-05: the situation-tool table is the curated map from
-- "user intent" to tool. Must be non-empty and every row's tool
-- must actually be in the registry. Post-issue-#44 the @srTool@
-- field is a 'ToolName' constructor, so this is now a pure
-- ADT-membership check (the wire form is impossible to typo).
testGuidanceSituationNonEmpty :: IO Bool
testGuidanceSituationNonEmpty = pure $
     not (null Guidance.situationTable)
  && all (\r -> Guidance.srTool r `elem` allToolNames) Guidance.situationTable

-- | BUG-19: @ghc_session@ is a TS-era tool name that does not
-- exist in the Haskell MCP. The phantom reference used to leak
-- into @ghc_deps@' description and hint. Pin that no guidance
-- text mentions the phantom tool.
testGuidanceNoPhantomSession :: IO Bool
testGuidanceNoPhantomSession = do
  let instructions = Guidance.sessionInstructionsText allToolDescriptors
      md           = Guidance.workflowRulesMarkdown   allToolDescriptors
      phantom      = "ghc_session"
  pure $ not (phantom `T.isInfixOf` instructions)
      && not (phantom `T.isInfixOf` md)

-- | BUG-19 companion: the @ghc_deps@ tool descriptor used to say
-- \"run ghc_session(action='restart')\". Pin that the description
-- no longer mentions the phantom tool.
testDepsDescriptorNoPhantom :: IO Bool
testDepsDescriptorNoPhantom = do
  let depsDesc = head [ tdDescription d | d <- allToolDescriptors
                                        , tdName d == "ghc_deps" ]
  pure (not ("ghc_session" `T.isInfixOf` depsDesc))

-- | BUG-19 companion: the @ghc_deps@ add/remove response carried
-- a @hint@ string instructing the agent to call @ghc_session@.
-- Pin that the live Deps source no longer embeds the phantom.
testDepsHintNoPhantom :: IO Bool
testDepsHintNoPhantom = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Deps.hs"
  pure (not ("ghc_session" `T.isInfixOf` src))
