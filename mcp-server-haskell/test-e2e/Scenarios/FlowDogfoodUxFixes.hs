-- | Flow: exercises the six UX fixes that came out of the
-- @playground/expr-evaluator@ dogfood session. Each step pins one
-- fix end-to-end through the MCP surface:
--
--   Fix 2. 'ghc_deps add' on a package that's already present
--          returns 'action=unchanged' + 'success=true' + a
--          verb-specific note (no more remove-shaped error).
--   Fix 6. 'ghc_switch_project' into an empty directory emits
--          'scaffolded=false' in the payload and its nextStep
--          points at 'ghc_create_project' instead of
--          'ghc_workflow(status)'.
--   Fix 1. 'ghc_add_modules' accepts 'stanza=test-suite', which
--          routes to 'other-modules' in the test-suite stanza and
--          scaffolds the stub under 'test/'.
--   Fix 4. 'ghc_check_module' on a clean module does NOT inherit
--          warnings from broken siblings (each module gate is
--          scoped to its own file).
--   Fix 5. 'ghc_check_project' finds test-suite 'other-modules'
--          under 'test/' (previously only 'src/', 'lib/', and the
--          project root were searched).
--   Fix 3. (checked at the schema / :m + wiring layer via the
--          Spec.hs probe 'testQuickCheckScopeWidening' — a real
--          cabal-v2-repl invocation belongs in the scaffolded
--          slow-path scenarios, not this fast UX probe.)
--
-- Cost: ~3 s (scaffold + 5 tool calls, no cabal v2-repl).
module Scenarios.FlowDogfoodUxFixes
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
import E2E.Envelope (statusOk, fieldBool, fieldText, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Start from an EMPTY subdir so Fix 6's nextStep branch fires on
  -- the very first switch. The outer projectDir passed by Main.hs
  -- is the scaffold substrate for the later switch-back.
  let emptyDir = projectDir </> "empty-slot"
  createDirectoryIfMissing True emptyDir

  -- Step 1 — Fix 6: switch to empty dir.
  t1 <- stepHeader 1
          "Fix 6 · switch to empty dir → nextStep points at ghc_create_project"
  r1 <- Client.callTool c GhcSwitchProject
          (object [ "path" .= T.pack emptyDir ])
  let c1a = checkPure
        "scaffolded=false in payload"
        (fieldBool "scaffolded" r1 == Just False)
        ("Expected 'scaffolded' false for an empty dir. Raw: "
          <> truncRender r1)
      c1b = checkPure
        "nextStep.tool == ghc_create_project"
        (fetchNextStepTool r1 == Just "ghc_create_project")
        ("Expected nextStep.tool to be 'ghc_create_project' \
         \when switching to an empty directory. Got: "
          <> T.pack (show (fetchNextStepTool r1))
          <> ". Raw: " <> truncRender r1)
  cc1a <- liveCheck c1a
  cc1b <- liveCheck c1b
  stepFooter 1 t1

  -- Scaffold the empty slot into a real project. 'ghc_create_project'
  -- writes to whatever the server's projectDir is, which the
  -- previous switch already repointed at 'emptyDir'.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("ux-demo" :: Text) ])

  -- Step 2 — Fix 2: adding an already-present dep is idempotent.
  -- 'base' is baked into every scaffolded library stanza by
  -- 'ghc_create_project', so re-adding it is a guaranteed no-op.
  t2 <- stepHeader 2
          "Fix 2 · ghc_deps add existing package → action=unchanged, success=true"
  r2 <- Client.callTool c GhcDeps
          (object [ "action" .= ("add" :: Text)
                  , "package" .= ("base" :: Text)
                  , "stanza" .= ("library" :: Text)
                  , "version" .= (">= 4.20 && < 5" :: Text)
                  ])
  let c2a = checkPure
        "success=true on idempotent add"
        (statusOk r2 == Just True)
        ("Idempotent add of 'base' should report success=true. Raw: "
          <> truncRender r2)
      c2b = checkPure
        "action=unchanged on idempotent add"
        (fieldText "action" r2 == Just "unchanged")
        ("Expected action='unchanged'. Got: "
          <> T.pack (show (fieldText "action" r2))
          <> ". Raw: " <> truncRender r2)
      c2c = checkPure
        "note mentions 'already present' — not a remove-shaped error"
        (maybe False ("already present" `T.isInfixOf`)
          (fieldText "note" r2))
        ("Note should explicitly say 'already present' to avoid \
         \confusion with the remove path. Got note="
          <> T.pack (show (fieldText "note" r2)))
  cc2a <- liveCheck c2a
  cc2b <- liveCheck c2b
  cc2c <- liveCheck c2c
  stepFooter 2 t2

  -- Step 3 — Fix 1: register a module into the test-suite stanza.
  t3 <- stepHeader 3
          "Fix 1 · ghc_add_modules stanza=test-suite → other-modules + test/"
  r3 <- Client.callTool c GhcModules
          (object [ "action" .= ("add" :: Text), "modules" .= ["Gen" :: Text]
                  , "stanza" .= ("test-suite" :: Text)
                  ])
  let c3a = checkPure
        "stanza label surfaced in payload"
        (fieldText "stanza" r3 == Just "test-suite")
        ("Payload should echo the resolved stanza label. Raw: "
          <> truncRender r3)
      c3b = checkPure
        "field = other-modules (not exposed-modules)"
        (fieldText "field" r3 == Just "other-modules")
        ("test-suite modules land in 'other-modules'; got "
          <> T.pack (show (fieldText "field" r3)))
      c3c = checkPure
        "source_dir = test"
        (fieldText "source_dir" r3 == Just "test")
        ("test-suite stub should scaffold under test/, got "
          <> T.pack (show (fieldText "source_dir" r3)))
  cc3a <- liveCheck c3a
  cc3b <- liveCheck c3b
  cc3c <- liveCheck c3c
  stepFooter 3 t3

  -- Step 4 — Fix 5: check_project sees the test/ module.
  --
  -- We overwrite 'test/Gen.hs' with a tiny compilable stub so the
  -- check_project iteration reaches Gen and gets an 'ok' verdict.
  -- If Fix 5 regressed, Gen would come back 'not_found' because
  -- the resolver would only search src/, lib/, and the project root.
  TIO.writeFile (emptyDir </> "test" </> "Gen.hs")
    "module Gen where\n\ntrivial :: Int\ntrivial = 42\n"
  t4 <- stepHeader 4
          "Fix 5 · check_project resolves test-suite modules under test/"
  r4 <- Client.callTool c GhcCheckProject (object [])
  let c4 = checkPure
        "Gen not reported as not_found"
        (numberOf "not_found" r4 == Just 0)
        ("Expected 0 not_found modules (Gen lives under test/). \
         \not_found="
          <> T.pack (show (numberOf "not_found" r4))
          <> ". Raw: " <> truncRender r4)
  cc4 <- liveCheck c4
  stepFooter 4 t4

  -- Step 5 — Fix 4: warning in one module doesn't taint a sibling.
  -- Scaffold a NEW library module and an adjacent broken one. The
  -- good module's check must not inherit the broken module's
  -- warnings.
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["UxDemo.Good", "UxDemo.Noisy"] :: [Text])
                 , "stanza" .= ("library" :: Text)
                 ])
  -- Good: compiles clean, no warnings.
  TIO.writeFile (emptyDir </> "src" </> "UxDemo" </> "Good.hs")
    "module UxDemo.Good (answer) where\n\n\
    \answer :: Int\n\
    \answer = 42\n"
  -- Noisy: emits an '-Wunused-matches' warning via an unused
  -- argument. Using a bare warning (no error) so compile itself
  -- succeeds.
  TIO.writeFile (emptyDir </> "src" </> "UxDemo" </> "Noisy.hs")
    "module UxDemo.Noisy (noisy) where\n\n\
    \noisy :: Int -> Int\n\
    \noisy unused = 7\n"
  t5 <- stepHeader 5
          "Fix 4 · warning in Noisy does NOT red-gate Good"
  r5 <- Client.callTool c GhcCheckModule
          (object [ "module_path" .= ("src/UxDemo/Good.hs" :: Text)
                  , "warnings_block" .= True
                  ])
  let c5 = checkPure
        "Good reports its own 0 warnings, not Noisy's"
        (fieldBool "overall" r5 == Just True)
        ("Expected overall=true for Good (which has no own warnings) \
         \even though Noisy has a -Wunused-matches. Raw: "
          <> truncRender r5)
  cc5 <- liveCheck c5
  stepFooter 5 t5

  pure [cc1a, cc1b, cc2a, cc2b, cc2c, cc3a, cc3b, cc3c, cc4, cc5]

--------------------------------------------------------------------------------
-- helpers (shaped after the other scenarios — keep small + local)
--------------------------------------------------------------------------------

numberOf :: Text -> Value -> Maybe Int
numberOf k v = case lookupField k v of
  Just (Number n) -> Just (round (realToFrac n :: Double))
  _               -> Nothing

fetchNextStepTool :: Value -> Maybe Text
fetchNextStepTool v = case lookupField "nextStep" v of
  Just ns -> fieldText "tool" ns
  Nothing -> Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
