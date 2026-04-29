-- | End-to-end scenario: drive the MCP to build an arithmetic
-- expression evaluator project from scratch, assert every step.
--
-- Exercises, in order:
--
--    1. 'ghc_workflow(status)'       — initial state + phase classifier.
--    2. 'ghc_create_project'          — scaffold; nextStep chain (BUG-22).
--    3. 'ghc_deps(add, QuickCheck)'   — test-suite dep.
--    4. 'ghc_add_modules'             — register 4 Expr.* modules.
--    5. 'ghc_remove_modules' (BUG-16) — drop the default stub.
--    6. Direct file-IO              — write the 4 source files + test/Gen.
--    7. Cabal other-modules wiring  — register Gen.
--    8. 'ghc_load(test/Gen.hs)'     — BUG-18's 5-module compile.
--    9. 'ghc_suggest("simplify")'   — BUG-03 sibling engine: asserts
--                                       Constant-folding soundness fires
--                                       at High confidence.
--   10. 'ghc_quickcheck' x3         — idempotent / soundness / roundtrip.
--   11. 'ghc_determinism'           — BUG-06 stability.
--   12. 'ghc_regression(list)'      — 3 persisted; no store-cold FS crash
--                                       (BUG-04).
--   13. 'ghc_regression(run)'       — all replay pass.
--   14. 'ghc_quickcheck_export'     — BUG-02 fixed: file contains
--                                       'import Gen', not 'import test.Gen'.
--   15. 'ghc_gate'                  — BUG-01 fixed: structured result,
--                                       no connection-close.
module Scenarios.ExprEvaluator
  ( runExprScenario
  ) where

import Control.Monad (forM)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , beginSection
  , checkJsonField
  , checkJsonFieldMatches
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, fieldInt, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))
import Scenarios.ExprSources
  ( evalSrc
  , genSrc
  , prettySrc
  , simplifySrc
  , syntaxSrc
  )

--------------------------------------------------------------------------------
-- small combinators
--------------------------------------------------------------------------------

mkCheck :: Text -> Bool -> Text -> Check
mkCheck name ok detail = Check
  { cName   = name
  , cOk     = ok
  , cDetail = if ok then "" else detail
  }

-- | Read a top-level string field from a JSON object.
-- | Read a string field. Routes through 'lookupField' from
-- 'E2E.Envelope' so the auto-drill kicks in.
fieldString :: Text -> Value -> Maybe Text
fieldString k v = case lookupField k v of
  Just (String t) -> Just t
  _               -> Nothing

-- | Read a top-level boolean field.

-- | Count entries in an array field. Uses E2E.Envelope's
-- auto-drilling lookupField so the field resolves whether it's
-- at the top level (pre-#90 wire) or nested under @result@
-- (post-#90 envelope).
fieldArrayLen :: Text -> Value -> Maybe Int
fieldArrayLen k v = case lookupField k v of
  Just (Array a) -> Just (V.length a)
  _              -> Nothing

-- | Read a numeric top-level field as 'Int'.

--------------------------------------------------------------------------------
-- scenario
--------------------------------------------------------------------------------

-- | Run the full scenario, given a ready client + the project
-- dir on disk. Each step streams its header + check lines + a
-- duration footer so you can watch progress in real time — a
-- hang is obvious because the last log line is the @[mcp] →@
-- for the stuck call with no matching @[mcp] ←@ reply.
runExprScenario :: Client.McpClient -> FilePath -> IO [Check]
runExprScenario c projectDir = do
  beginSection "Scenario: Arithmetic Expression Evaluator (15 steps)"
  s1  <- runStep 1  "initial workflow(status)"           (step1_initialStatus  c)
  s2  <- runStep 2  "ghc_create_project"                 (step2_scaffold        c)
  s3  <- runStep 3  "ghc_deps(add QuickCheck test-suite)" (step3_addQuickCheck   c)
  s4  <- runStep 4  "ghc_add_modules (4 Expr.*)"          (step4_addModules      c)
  s5  <- runStep 5  "ghc_remove_modules (BUG-16)"         (step5_removeStub      c)
  s6  <- runStep 6  "write 5 source files"                 (step6_writeSources         projectDir)
  s7  <- runStep 7  "wire test-suite other-modules"        (step7_wireOtherModules     projectDir)
  s8  <- runStep 8  "ghc_load(test/Gen.hs)"               (step8_loadAll         c)
  s9  <- runStep 9  "ghc_suggest(simplify) — BUG-03"      (step9_suggestSimplify c)
  s10 <- runStep 10 "ghc_quickcheck × 3 (BUG-04)"         (step10_runProperties  c)
  s11 <- runStep 11 "ghc_determinism"                     (step11_determinism    c)
  s12 <- runStep 12 "ghc_regression(list)"                (step12_regressionList c)
  s13 <- runStep 13 "ghc_regression(run)"                 (step13_regressionRun  c)
  s14 <- runStep 14 "ghc_quickcheck_export (BUG-02)"      (step14_export         c projectDir)
  s15 <- runStep 15 "ghc_gate (BUG-01)"                   (step15_gate           c)
  pure (concat
    [ s1, s2, s3, s4, s5, s6, s7, s8, s9, s10
    , s11, s12, s13, s14, s15 ])

-- | Wrap a step with header + footer + live per-check streaming.
-- The step body is expected to produce a list of 'Check's; this
-- helper prints each one as soon as it's recorded.
runStep :: Int -> Text -> IO [Check] -> IO [Check]
runStep n title body = do
  t0 <- stepHeader n title
  cs <- body
  streamed <- mapM liveCheck cs
  stepFooter n t0
  pure streamed

--------------------------------------------------------------------------------
-- step 1 — initial workflow status
--------------------------------------------------------------------------------

step1_initialStatus :: Client.McpClient -> IO [Check]
step1_initialStatus c = do
  r <- Client.callTool c GhcWorkflow (object [ "action" .= ("status" :: Text) ])
  pure
    [ mkCheck "step 1 · status view carries phase field"
        (isJust (fieldString "phase" r))
        "workflow(status) must include a 'phase' field (BUG-24)"
    , mkCheck "step 1 · staleness report attached"
        (isJust (lookupField "staleness" r))
        "workflow(status) must include a 'staleness' field (BUG-07)"
    , checkJsonFieldMatches
        "step 1 · toolsActive is non-empty"
        r "toolsActive"
        (\case Array a -> not (V.null a); _ -> False)
        "toolsActive should list the registered MCP tools"
    ]

objMap :: Value -> KeyMap.KeyMap Value
objMap (Object o) = o
objMap _          = KeyMap.empty

--------------------------------------------------------------------------------
-- step 2 — scaffold
--------------------------------------------------------------------------------

step2_scaffold :: Client.McpClient -> IO [Check]
step2_scaffold c = do
  r <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("expr-evaluator" :: Text) ])
  let ok = statusOk r == Just True
      chain = fetchChain r
      chainTools = map csToolField chain
  pure
    [ mkCheck "step 2 · create_project success"
        ok "expected success=true"
    , mkCheck "step 2 · nextStep points at ghc_deps"
        (fetchNextStepTool r == Just "ghc_deps")
        "nextStep.tool should be ghc_deps (BUG-06)"
    , mkCheck "step 2 · nextStep chain carries bootstrap plan (BUG-22)"
        (  "ghc_deps"        `elem` chainTools
        && "ghc_add_modules" `elem` chainTools
        && "ghc_load"        `elem` chainTools )
        "chain must include deps + add_modules + load"
    ]

fetchNextStepTool :: Value -> Maybe Text
fetchNextStepTool v = case lookupPath v ["nextStep", "tool"] of
  Just (String t) -> Just t
  _               -> Nothing

fetchChain :: Value -> [Value]
fetchChain v = case lookupPath v ["nextStep", "chain"] of
  Just (Array a) -> V.toList a
  _              -> []

csToolField :: Value -> Text
csToolField v = case lookupPath v ["tool"] of
  Just (String t) -> t
  _               -> ""

-- | Walk a key path through a nested envelope. The first hop
-- uses 'lookupField' from 'E2E.Envelope' so it auto-drills
-- through @result@. Subsequent hops are direct (the deeper
-- objects don't carry an envelope).
lookupPath :: Value -> [Text] -> Maybe Value
lookupPath v = foldl step (Just v)
  where
    step Nothing  _          = Nothing
    step (Just outer) k      = lookupField k outer

--------------------------------------------------------------------------------
-- step 3 — add QuickCheck to test-suite
--------------------------------------------------------------------------------

step3_addQuickCheck :: Client.McpClient -> IO [Check]
step3_addQuickCheck c = do
  r <- Client.callTool c GhcDeps (object
    [ "action"  .= ("add" :: Text)
    , "package" .= ("QuickCheck" :: Text)
    , "version" .= (">= 2.14" :: Text)
    , "stanza"  .= ("test-suite" :: Text)
    ])
  pure
    [ checkJsonField "step 3 · deps add success" r "success" (Bool True)
    , mkCheck "step 3 · hint carries no phantom ghc_session (BUG-19)"
        (not ("ghc_session" `T.isInfixOf`
              fromMaybe "" (fieldString "hint" r)))
        "deps add hint must not reference the removed ghc_session tool"
    ]

--------------------------------------------------------------------------------
-- step 4 — register the 4 Expr.* modules
--------------------------------------------------------------------------------

step4_addModules :: Client.McpClient -> IO [Check]
step4_addModules c = do
  r <- Client.callTool c GhcAddModules (object
    [ "modules" .= (["Expr.Syntax", "Expr.Eval", "Expr.Simplify", "Expr.Pretty"] :: [Text])
    ])
  pure
    [ checkJsonField "step 4 · add_modules success" r "success" (Bool True)
    , mkCheck "step 4 · 4 cabal entries added"
        (fieldArrayLen "cabal_added" r == Just 4)
        "cabal_added should list all 4 new modules"
    ]

--------------------------------------------------------------------------------
-- step 5 — remove the default ExprEvaluator stub via new tool (BUG-16)
--------------------------------------------------------------------------------

step5_removeStub :: Client.McpClient -> IO [Check]
step5_removeStub c = do
  -- Issue #41 added a downstream-importer safety net that refuses
  -- by default if any other source still imports the module being
  -- removed. The scaffolded test/Spec.hs (written by step 2) does
  -- import 'ExprEvaluator (greet)', so the unforced call would now
  -- fail-safe by design. The scenario's intent is "deliberately
  -- drop the default stub before wiring real sources" — exactly
  -- the use-case 'force=true' is for. The follow-up step 7 then
  -- rewrites Spec.hs to import the real test modules, so the
  -- post-step state is consistent.
  r <- Client.callTool c GhcRemoveModules (object
    [ "modules"      .= (["ExprEvaluator"] :: [Text])
    , "delete_files" .= True
    , "force"        .= True
    ])
  pure
    [ checkJsonField "step 5 · remove_modules success (BUG-16)"
        r "success" (Bool True)
    , mkCheck "step 5 · cabal_removed contains ExprEvaluator"
        (case lookupPath r ["cabal_removed"] of
           Just (Array a) -> V.elem (String "ExprEvaluator") a
           _              -> False)
        "cabal_removed must include ExprEvaluator"
    ]

--------------------------------------------------------------------------------
-- step 6 — write the 4 source files (pure file-IO; MCP doesn't
-- dictate source content)
--------------------------------------------------------------------------------

step6_writeSources :: FilePath -> IO [Check]
step6_writeSources projectDir = do
  let write relPath body = do
        let full = projectDir </> relPath
        createDirectoryIfMissing True (takeDir full)
        TIO.writeFile full body
        doesFileExist full
  wSyntax   <- write "src/Expr/Syntax.hs"   syntaxSrc
  wEval     <- write "src/Expr/Eval.hs"     evalSrc
  wSimplify <- write "src/Expr/Simplify.hs" simplifySrc
  wPretty   <- write "src/Expr/Pretty.hs"   prettySrc
  wGen      <- write "test/Gen.hs"          genSrc
  pure
    [ checkPure "step 6 · wrote src/Expr/Syntax.hs"   wSyntax   "file not on disk after write"
    , checkPure "step 6 · wrote src/Expr/Eval.hs"     wEval     "file not on disk after write"
    , checkPure "step 6 · wrote src/Expr/Simplify.hs" wSimplify "file not on disk after write"
    , checkPure "step 6 · wrote src/Expr/Pretty.hs"   wPretty   "file not on disk after write"
    , checkPure "step 6 · wrote test/Gen.hs"          wGen      "file not on disk after write"
    ]
  where
    takeDir p = reverse (dropWhile (/= '/') (reverse p))

--------------------------------------------------------------------------------
-- step 7 — add Gen to the test-suite's other-modules so cabal
-- includes it in the build graph. No MCP tool covers this yet
-- (future BUG: ghc_add_test_modules); direct edit via FS.
--------------------------------------------------------------------------------

step7_wireOtherModules :: FilePath -> IO [Check]
step7_wireOtherModules projectDir = do
  let cabalPath = projectDir </> "expr-evaluator.cabal"
  body <- TIO.readFile cabalPath
  let body'
        | "other-modules:" `T.isInfixOf` body = body  -- already there
        | otherwise = T.replace
            "main-is:          Spec.hs"
            "main-is:          Spec.hs\n    other-modules:    Gen"
            body
  TIO.writeFile cabalPath body'
  body2 <- TIO.readFile cabalPath
  pure
    [ checkPure "step 7 · cabal now lists Gen in other-modules"
        ("other-modules:" `T.isInfixOf` body2 && "Gen" `T.isInfixOf` body2)
        "expected expr-evaluator.cabal to list Gen under other-modules"
    ]

--------------------------------------------------------------------------------
-- step 8 — load Gen.hs (pulls all 4 Expr.* modules + Gen as siblings)
--------------------------------------------------------------------------------

step8_loadAll :: Client.McpClient -> IO [Check]
step8_loadAll c = do
  r <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("test/Gen.hs" :: Text) ])
  pure
    [ checkJsonField "step 8 · load success" r "success" (Bool True)
    , mkCheck "step 8 · no errors"
        (fieldArrayLen "errors" r == Just 0)
        "errors[] should be empty"
    , mkCheck "step 8 · no warnings"
        (fieldArrayLen "warnings" r == Just 0)
        "warnings[] should be empty"
    , mkCheck "step 8 · nextStep points at ghc_suggest"
        (fetchNextStepTool r == Just "ghc_suggest")
        "clean load should push the agent toward ghc_suggest"
    ]

--------------------------------------------------------------------------------
-- step 9 — the BUG-03 killer: sibling-aware suggest MUST fire
-- constant-folding soundness at High confidence.
--------------------------------------------------------------------------------

step9_suggestSimplify :: Client.McpClient -> IO [Check]
step9_suggestSimplify c = do
  r <- Client.callTool c GhcSuggest
         (object [ "function_name" .= ("simplify" :: Text) ])
  let suggestions = case lookupPath r ["suggestions"] of
        Just (Array a) -> V.toList a
        _              -> []
      isSoundnessHigh s =
           (case lookupPath s ["law"] of
              Just (String l) -> l == "Constant-folding soundness"
              _               -> False)
        && (case lookupPath s ["confidence"] of
              Just (String cf) -> cf == "high"
              _                -> False)
      isInvolutiveLow s =
           (case lookupPath s ["law"] of
              Just (String l) -> l == "Involutive"
              _               -> False)
        && (case lookupPath s ["confidence"] of
              Just (String cf) -> cf == "low"
              _                -> False)
      hasSoundnessHigh = any isSoundnessHigh suggestions
      hasInvolutiveLow = any isInvolutiveLow  suggestions
  -- Dropped: "step 9 · suggest success" — BUG-03 + BUG-18 assertions
  -- below are the real oracle. A tool that returned success=true with
  -- no suggestions would fail the BUG-03 check anyway.
  pure
    [ mkCheck "step 9 · Constant-folding soundness fires at HIGH (BUG-03)"
        hasSoundnessHigh
        "the sibling-aware engine must propose constant-folding \
        \soundness at high confidence; if this fails the gatherSiblings \
        \pipeline regressed"
    , mkCheck "step 9 · Involutive is LOW for a normalizer (BUG-18)"
        hasInvolutiveLow
        "involutive for simplify must be low-confidence — normalizers \
        \are idempotent, not involutive"
    ]

--------------------------------------------------------------------------------
-- step 10 — run the three laws. idempotent + soundness + roundtrip
-- must all pass; auto-persist on pass.
--------------------------------------------------------------------------------

step10_runProperties :: Client.McpClient -> IO [Check]
step10_runProperties c = do
  let props =
        [ ("idempotent",
           "\\(x :: Expr) -> simplify (simplify x) == simplify x")
        , ("soundness",
           "\\(env :: Env) (x :: Expr) -> eval env (simplify x) == eval env x")
        , ("roundtrip",
           "\\(x :: Expr) -> parseExpr (pretty x) == Just x")
        ]
  forM props $ \(label, prop) -> do
    r <- Client.callTool c GhcQuickCheck (object
      [ "property" .= (prop :: Text)
      , "module"   .= ("test/Gen.hs" :: Text)
      ])
    let passed = case lookupPath r ["state"] of
                   Just (String "passed") -> True
                   _                      -> False
    pure (mkCheck
      ("step 10 · " <> label <> " passes 100/100")
      passed
      ("expected state=passed; raw: " <> T.pack (show r)))

--------------------------------------------------------------------------------
-- step 11 — determinism: soundness prop must pass every run
--------------------------------------------------------------------------------

step11_determinism :: Client.McpClient -> IO [Check]
step11_determinism c = do
  r <- Client.callTool c GhcDeterminism (object
    [ "property" .= (
        "\\(env :: Env) (x :: Expr) -> eval env (simplify x) == eval env x"
        :: Text)
    , "runs"     .= (3 :: Int)
    -- Same module load-hint shape as step 10's 'ghc_quickcheck'
    -- calls — the property references 'Env' and 'Expr' which
    -- live in the test-suite's Gen module, not in the default
    -- test-suite auto-load set.
    , "module"   .= ("test/Gen.hs" :: Text)
    ])
  pure
    [ checkJsonField "step 11 · determinism success" r "success" (Bool True)
    , mkCheck "step 11 · nextStep points at regression(run)"
        (fetchNextStepTool r == Just "ghc_regression")
        "stable property should push the agent toward regression(run)"
    ]

--------------------------------------------------------------------------------
-- step 12 — regression list (also proves cold-start FS works, BUG-04)
--------------------------------------------------------------------------------

step12_regressionList :: Client.McpClient -> IO [Check]
step12_regressionList c = do
  r <- Client.callTool c GhcRegression
         (object [ "action" .= ("list" :: Text) ])
  pure
    [ checkJsonField "step 12 · regression list success" r "success" (Bool True)
    , mkCheck "step 12 · at least 3 properties persisted"
        (fromIntegral (fromMaybe 0 (fieldInt "count" r)) >= (3 :: Integer))
        "expected 3+ properties in the store (idempotent + soundness + roundtrip)"
    ]
  where
    fromMaybe d Nothing  = d
    fromMaybe _ (Just x) = x

--------------------------------------------------------------------------------
-- step 13 — regression run: every persisted law replays green
--------------------------------------------------------------------------------

step13_regressionRun :: Client.McpClient -> IO [Check]
step13_regressionRun c = do
  r <- Client.callTool c GhcRegression
         (object [ "action" .= ("run" :: Text) ])
  -- Dropped: "step 13 · regression run success" — 'no regressions' is
  -- strictly stronger and catches the real failure shape.
  pure
    [ mkCheck "step 13 · no regressions"
        (fieldArrayLen "regressions" r == Just 0)
        "regressions[] should be empty"
    ]

--------------------------------------------------------------------------------
-- step 14 — export Spec.hs; assert BUG-02 stays fixed: generated
-- Spec contains 'import Gen', not 'import test.Gen'.
--------------------------------------------------------------------------------

step14_export :: Client.McpClient -> FilePath -> IO [Check]
step14_export c projectDir = do
  r <- Client.callTool c GhcQuickCheckExport (object [])
  let success = statusOk r == Just True
      specPath = projectDir </> "test" </> "Spec.hs"
  specExists <- doesFileExist specPath
  specBody <- if specExists then TIO.readFile specPath else pure ""
  pure
    [ mkCheck "step 14 · export success" success "expected success=true"
    , checkPure "step 14 · test/Spec.hs exists on disk"
        specExists "file must be written to projectDir/test/Spec.hs"
    , checkPure "step 14 · generated import is 'import Gen' (BUG-02)"
        ("import Gen" `T.isInfixOf` specBody
         && not ("import test." `T.isInfixOf` specBody))
        "generated Spec.hs must NOT re-introduce 'import test.Gen'"
    ]

--------------------------------------------------------------------------------
-- step 15 — gate. Does NOT assert green (cabal test / build may fail
-- on an E2E runner), but asserts the tool returns a STRUCTURED
-- response with per-step shape rather than crashing the MCP
-- ("Connection closed" was the BUG-01 symptom).
--------------------------------------------------------------------------------

step15_gate :: Client.McpClient -> IO [Check]
step15_gate c = do
  -- Skip the expensive cabal test + cabal build subprocess steps:
  -- the E2E's point is that the tool's shape holds, not that
  -- cabal builds this particular test project under our environment.
  r <- Client.callTool c GhcGate (object
    [ "skip_cabal_test"  .= True
    , "skip_cabal_build" .= True
    ])
  pure
    [ checkPure "step 15 · ghc_gate returned a structured response (BUG-01)"
        (case r of Object _ -> True; _ -> False)
        "gate must not tear down the connection — earlier dogfood (F-22) \
        \had this call return a 'Connection closed' instead of a tool \
        \result"
    , mkCheck "step 15 · gate payload carries steps.regression"
        (KeyMap.member "steps"
          (case lookupPath r ["steps"] of
             Just (Object o) -> o
             _               -> KeyMap.empty)
         || (case lookupPath r ["steps", "regression"] of
               Just _  -> True
               Nothing -> False))
        "gate payload should include a per-step 'regression' field"
    ]

