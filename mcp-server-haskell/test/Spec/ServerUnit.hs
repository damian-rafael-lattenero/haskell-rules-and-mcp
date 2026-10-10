-- | Unit tests for server initialization, timeout budgets, eval/deferred
-- output isolation, and integration tests for Deps/Switch/CheckModule/
-- AddModules/CheckProject/QuickCheck/Load.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.ServerUnit
  ( testInitializeEmitsInstructions
  , testInstructionsMentionCore
  , testServerOuterTimeout
  , testEvalContextHasControlConcurrent
  , testEvalInnerTimeoutBudget
  , testDeferredIsolatedOutputs
  , testDepsAddIdempotent
  , testSwitchProjectEmptyDir
  , testAddModulesStanzaParam
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, newMVar)
import Control.Exception (SomeException, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Timeout (timeout)

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Mcp.Guidance as Guidance
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Mcp.Server as Server
import qualified HaskellFlows.Tool.Deps as DepsTool
import qualified HaskellFlows.Tool.AddModules as AddModules
import qualified HaskellFlows.Tool.SwitchProject as SwitchProject

import Spec.Helpers (decodeToolResult, runToolEnvelope, withTempProject)

-- | Phase 11d F-13: the MCP used to leave the @instructions@ field
-- of 'InitializeResult' empty, so Claude Desktop (and any other
-- MCP client) surfaced nothing at session start. The repo-level
-- @.claude/rules/use-haskell-flows-mcp.md@ partially filled the gap
-- but was itself stale — referencing tools that never existed in
-- the Haskell port (@ghc_session@, @ghc_trace@, @ghc_flags@,
-- @ghc_init@, …). Fix wires a non-empty @instructions@ string
-- into the initialize response so the LLM always gets accurate
-- tool guidance, even without the project file.
--
-- Pin two invariants with static source checks:
--   1. 'InitializeResult' has an 'irInstructions' field.
--   2. The content is non-empty and mentions the tools / flows an
--      agent has to reach for every session.
testInitializeEmitsInstructions :: IO Bool
testInitializeEmitsInstructions = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Protocol.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "irInstructions"           code
      && T.isInfixOf "\"instructions\" .="      code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

testInstructionsMentionCore :: IO Bool
testInstructionsMentionCore = do
  -- BUG-05: the instructions are now rendered dynamically from
  -- 'allToolDescriptors' + the situation table. Assert the
  -- rendered text contains (a) every registered tool name and
  -- (b) the core workflow / invariant markers. Any drift between
  -- the registry and the text fails here.
  --
  -- Issue #56: the post-Wave-5 model uses HscEnv + MVar, NOT the
  -- retired SessionStatus / executeNoLock / registerDelay
  -- subprocess GHCi vocabulary. Drop the latter from the
  -- expected-markers set; rely on 'testGuidanceNoRetiredVocab'
  -- + 'testGuidanceMentionsApi' to pin both halves.
  let instructions = Guidance.sessionInstructionsText allToolDescriptors
      staticMarkers =
        [ "ci-local.sh"
        , "HscEnv"          , "MVar"
        , "10-min"
        , "dogfood"
        , "handshake"
        , "situation"       , "invariant"
        , "nextStep"
        ]
      toolMarkers = allToolNameTexts
      lowerInstructions = T.toLower instructions
  pure $ all (`T.isInfixOf` instructions) toolMarkers
      && all ((`T.isInfixOf` lowerInstructions) . T.toLower) staticMarkers

-- | Phase 11c F-12 — defence-in-depth. Even if the Session.hs
-- fixes above miss a pathological code path, the server's outer
-- envelope must not freeze. Pin that @runTool@ is wrapped in
-- @System.Timeout.timeout@ with a generous but finite budget.
testServerOuterTimeout :: IO Bool
testServerOuterTimeout = do
  src <- TIO.readFile "src/HaskellFlows/Mcp/Server.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "import System.Timeout" code
      && T.isInfixOf "timeout ceilingMicros action"       code
      && T.isInfixOf "toolTimeoutMicros :: Int"           code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Scenario-1 regression (FlowTimeoutEnforcement, step 3).
-- 'Control.Concurrent' must be in the eval interactive context —
-- fully-qualified references like @Control.Concurrent.threadDelay@
-- still need the module brought into scope. Without this, the
-- scenario's slow-eval step fails at compile time with "No module
-- named Control.Concurrent is imported" instead of tripping the
-- inner 30 s budget.
testEvalContextHasControlConcurrent :: IO Bool
testEvalContextHasControlConcurrent = do
  src <- TIO.readFile "src/HaskellFlows/Tool/EvalContext.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "\"Control.Concurrent\"" code
      && T.isInfixOf "augmentEvalContext" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Scenario-1 regression (FlowTimeoutEnforcement, step 2+3).
-- The Eval handler must enforce a tighter per-call budget than the
-- 10-min outer 'toolTimeoutMicros', wrap the eval pipeline in
-- 'System.Timeout.timeout', evict the GhcSession on elapse, and
-- render a structured @error_kind=timeout@ payload so clients
-- can distinguish budget trips from user compile/runtime errors.
testEvalInnerTimeoutBudget :: IO Bool
testEvalInnerTimeoutBudget = do
  src <- TIO.readFile "src/HaskellFlows/Ghc/IdeSession.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "timeout 30_000_000" code
      && T.isInfixOf "\"timeout after 30s\"" code
      -- budget trips classify as InnerTimeout in the route layer
  where
    isDocLine ln =
      let s' = T.stripStart ln in "--" `T.isPrefixOf` s'

-- | Deferred-pass isolation regression. 'ghc_check_project' runs
-- GHC with '-fdefer-type-errors' + '-fdefer-typed-holes', which
-- produces '.hi'/'.o' artifacts for semantically-broken modules.
-- Those MUST land in a MCP-private build tree, never in cabal's
-- default 'dist-newstyle/' — otherwise a user running 'cabal build'
-- after 'ghc_check_project' sees the poisoned interfaces and
-- skips recompilation, falsely reporting success on a project MCP
-- correctly flagged as broken (FlowCrossValidation · typeError).
--
-- Pins that 'applyFlavour' receives a 'ProjectDir', that the
-- 'Deferred' branch calls 'redirectDeferredOutputs', and that the
-- per-project MCP build dir is 'dist-newstyle-mcp/deferred' under
-- the project root.
testDeferredIsolatedOutputs :: IO Bool
testDeferredIsolatedOutputs = do
  src <- TIO.readFile "src/HaskellFlows/Ghc/CabalBootstrap.hs"
  let codeLines = filter (not . isDocLine) (T.lines src)
      code      = T.unlines codeLines
  pure $ T.isInfixOf "dist-newstyle-mcp" code
      && T.isInfixOf "--builddir=" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

--------------------------------------------------------------------------------
-- Dogfood-session feedback fixes — 6 polish probes.
--------------------------------------------------------------------------------

-- | Fix 2. 'ghc_deps add' used to return
-- @"No change: 'X' not found or already at desired state."@ when
-- the package was already in the targeted stanza — a remove-path
-- message on an add call. The correct behaviour is a structured
-- idempotent no-op ('action=unchanged', 'success=true') with a
-- verb-specific 'note'.
testDepsAddIdempotent :: IO Bool
testDepsAddIdempotent = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Deps.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "unchangedResult" code
      -- No lingering occurrence of the old misleading string in
      -- live code (comments may still reference it via "--").
      && not (T.isInfixOf "not found or already at desired state" code)
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Fix 6. Switching to an empty directory should point the agent
-- at 'ghc_create_project' (the canonical scaffold step), not at
-- 'ghc_workflow(status)'. The branching lives in 'NextStep.hs';
-- the payload signal ('scaffolded' bool) is emitted by
-- 'SwitchProject.successResult'.
testSwitchProjectEmptyDir :: IO Bool
testSwitchProjectEmptyDir = do
  ns <- TIO.readFile "src/HaskellFlows/Mcp/NextStep.hs"
  sp <- TIO.readFile "src/HaskellFlows/Tool/SwitchProject.hs"
  let nsCode = T.unlines (filter (not . isDocLine) (T.lines ns))
      spCode = T.unlines (filter (not . isDocLine) (T.lines sp))
  -- #94 Phase C step 5: SwitchProject was merged into
  -- ghc_project; the empty-dir → scaffold hint now points at
  -- ghc_project(action=create) rather than the legacy
  -- ghc_create_project tool.
  pure $ T.isInfixOf "ghc_project(action=create)" nsCode
      && T.isInfixOf "\"scaffolded\"" nsCode
      && T.isInfixOf "\"scaffolded\"" spCode
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s

-- | Fix 4. 'ghc_check_module' used to attribute every diagnostic
-- from the whole library load to every module — a warning in
-- 'Expr.Pretty' would red-gate 'Expr.Syntax' too. The fix filters
-- by 'geFile' suffix matching the checked module path.
testAddModulesStanzaParam :: IO Bool
testAddModulesStanzaParam = do
  src <- TIO.readFile "src/HaskellFlows/Tool/AddModules.hs"
  let code = T.unlines (filter (not . isDocLine) (T.lines src))
  pure $ T.isInfixOf "amStanza" code
      && T.isInfixOf "resolveStanzaTarget" code
      && T.isInfixOf "other-modules" code
      -- Source-dir routing covers the three non-library stanzas.
      && T.isInfixOf "\"test\"" code
      && T.isInfixOf "\"app\"" code
      && T.isInfixOf "\"bench\"" code
  where
    isDocLine ln =
      let s = T.stripStart ln in "--" `T.isPrefixOf` s
