-- | Flow: full end-to-end dogfood of the arithmetic-expression
-- evaluator, as the user /lived it/ manually in the playground.
--
-- What this scenario proves, concretely:
--
--   * The whole MCP workflow — scaffold + deps + add_modules +
--     write sources + load + check_project + quickcheck + regression
--     — survives a realistic 4-module Haskell project with a
--     test-suite that depends on QuickCheck.
--
--   * The three bugs the dogfood caught and fixed STAY fixed. Each
--     has a dedicated pin in terms the scenario's oracle can judge:
--
--       (a) pretty-printer never paren-wraps 'Neg'. Regression
--           would re-introduce the 'Neg (Neg x)' → "-(-x)" shape
--           that the parser then reads back as a single 'Neg'.
--
--       (b) 'pInsideParens' rejects a leading @-digits@ sequence
--           unless the paren closes immediately after. Regression
--           would re-introduce the greedy 'Mul (Lit 0) (Add (Neg
--           (Lit 0)) (Var "x"))' counterexample, caught by the
--           'prop_prettyRoundtrip' property on its first run.
--
--       (c) 'simplify' is a /refinement/, not a strict preservation
--           — it is allowed to turn @Mul (Lit 0) (Var "undef")@ into
--           @Lit 0@ even though the original expression would 'Left'
--           under an empty environment. The property law encodes
--           "if e has a value, simplify e preserves it" and accepts
--           simplify-introduced-Right as a valid short-circuit.
--
--   * The property store workflow works end-to-end: 'ghc_quickcheck'
--     auto-persists, 'ghc_regression run' replays cleanly without
--     scope-resolution errors. This is the UX hazard the dogfood
--     surfaced (persist with 'src/Expr/Pretty.hs', regression fails
--     with "Variable not in scope" because the property actually
--     lives in 'test/Spec.hs'). The scenario pins the correct usage
--     — @module=test/Spec.hs@ — and asserts regression replays 3/3.
module Scenarios.FlowExprEvaluatorDogfood
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
import E2E.Envelope (statusOk, fieldBool, fieldInt, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))
import Scenarios.ExprEvaluatorDogfoodSources
  ( evalSrc
  , facadeSrc
  , prettySrc
  , simplifySrc
  , specSrc
  , syntaxSrc
  )

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- step 1 · scaffold (create_project + deps + add_modules)
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold · ghc_create_project + 2 deps + 4 modules"
  _ <- Client.callTool c GhcProject
         (object
                     [ "action" .= ("create" :: Text)
            , "name"   .= ("expr-dogfood" :: Text)
            , "module" .= ("Expr"         :: Text)
            ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("containers" :: Text)
         , "stanza"  .= ("library" :: Text)
         , "version" .= (">= 0.6 && < 0.9" :: Text)
         ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  _ <- Client.callTool c GhcModules (object [ "action" .= ("add" :: Text), "modules" .= (["Expr.Syntax", "Expr.Eval", "Expr.Simplify", "Expr.Pretty"]
                      :: [Text])
    ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- step 2 · write the 5 library sources + the test-suite body
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "write · 4 Expr.* modules + Expr facade + test/Spec.hs"
  createDirectoryIfMissing True (projectDir </> "src" </> "Expr")
  TIO.writeFile (projectDir </> "src" </> "Expr" </> "Syntax.hs")    syntaxSrc
  TIO.writeFile (projectDir </> "src" </> "Expr" </> "Eval.hs")      evalSrc
  TIO.writeFile (projectDir </> "src" </> "Expr" </> "Simplify.hs")  simplifySrc
  TIO.writeFile (projectDir </> "src" </> "Expr" </> "Pretty.hs")    prettySrc
  TIO.writeFile (projectDir </> "src" </> "Expr.hs")                 facadeSrc
  TIO.writeFile (projectDir </> "test" </> "Spec.hs")                specSrc
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- step 3 · check_project — all 5 library modules must be green
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "gate · ghc_check_project 5/5 green, -Wall clean"
  cpR <- Client.callTool c GhcCheckProject (object [])
  let cpOverall = fieldBool "overall" cpR == Just True
      cpPassed  = fieldInt "passed" cpR == Just 5
      cpFailed  = fieldInt "failed" cpR == Just 0
  cGate <- liveCheck $ checkPure
    "check_project · overall=true, 5 passed, 0 failed"
    (cpOverall && cpPassed && cpFailed)
    ("The full 4-module library + facade must survive a cold gate. \
     \Any compile error, hole, or -Wall warning breaks this. Raw: "
     <> truncRender cpR)
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- step 4 · load test/Spec.hs so the QuickCheck properties are
  -- in scope for the next steps
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "load · test/Spec.hs (brings prop_* symbols into scope)"
  loadR <- Client.callTool c GhcLoad
             (object [ "module_path" .= ("test/Spec.hs" :: Text) ])
  let loadOk = statusOk loadR == Just True
            && fieldArrayLen "errors" loadR == Just 0
  cLoad <- liveCheck $ checkPure
    "load · test/Spec.hs compiles (QuickCheck + 5 library modules)"
    loadOk
    ("The scenario cannot run properties if the test-suite doesn't \
     \load. Raw: " <> truncRender loadR)
  stepFooter 4 t3

  ----------------------------------------------------------------
  -- step 5 · run the three QuickCheck properties, each 100 tests,
  -- each persisted to the property store under 'test/Spec.hs'
  --
  -- The 'module' argument MUST be 'test/Spec.hs' — persisting
  -- under 'src/Expr/Pretty.hs' (which is where the pretty/parse
  -- logic lives) would leave the regression runner unable to find
  -- 'prop_prettyRoundtrip' in that module's scope. That UX hazard
  -- is exactly what the scenario's next step guards against.
  ----------------------------------------------------------------
  t4 <- stepHeader 5 "properties · 3 × ghc_quickcheck @ 100 tests each"
  rRT <- Client.callTool c GhcQuickCheck (object
    [ "property" .= ("prop_prettyRoundtrip" :: Text)
    , "module"   .= ("test/Spec.hs"          :: Text)
    ])
  cRT <- liveCheck $ checkPure
    "prop_prettyRoundtrip · 100/100 passed"
    (propPassed 100 rRT)
    ("Roundtrip failure indicates either the pretty printer paren-wrapped \
     \'Neg' (reintroducing the '-(-x)' collision with '(-x)' literals) \
     \OR 'pInsideParens' went greedy on a leading '-digits' without the \
     \isSoleNegLit guard. Raw: " <> truncRender rRT)

  rSPM <- Client.callTool c GhcQuickCheck (object
    [ "property" .= ("prop_simplifyPreservesMeaning" :: Text)
    , "module"   .= ("test/Spec.hs"                    :: Text)
    ])
  cSPM <- liveCheck $ checkPure
    "prop_simplifyPreservesMeaning · 100/100 passed (refinement law)"
    (propPassed 100 rSPM)
    ("The refinement law: if e has a value, so does simplify e, and \
     \they match. If this regresses, simplify is either introducing \
     \new errors (bad) or changing defined values (very bad). Raw: "
     <> truncRender rSPM)

  rSI <- Client.callTool c GhcQuickCheck (object
    [ "property" .= ("prop_simplifyIdempotent" :: Text)
    , "module"   .= ("test/Spec.hs"              :: Text)
    ])
  cSI <- liveCheck $ checkPure
    "prop_simplifyIdempotent · 100/100 passed"
    (propPassed 100 rSI)
    ("simplify . simplify == simplify. Regression means a second pass \
     \still rewrites — the rule set is not confluent. Raw: "
     <> truncRender rSI)
  stepFooter 5 t4

  ----------------------------------------------------------------
  -- step 6 · ghc_regression run — proves store roundtrip works
  -- end-to-end when the persisted module path is correct
  ----------------------------------------------------------------
  t5 <- stepHeader 6 "regression · store has 3 props, all replay green"
  regR <- Client.callTool c GhcPropertyStore
            (object [ "action" .= ("run" :: Text), "action" .= ("run" :: Text) ])
  let regPassed = fieldInt "passed" regR == Just 3
      regTotal  = fieldInt "total"  regR == Just 3
      regRegressions = fieldArrayLen "regressions" regR == Just 0
  cReg <- liveCheck $ checkPure
    "regression run · 3/3 stored properties replay, 0 regressions"
    (regPassed && regTotal && regRegressions)
    ("If this fails, the property store's module-path association \
     \is broken: the scope lookup must succeed for 'prop_*' in the \
     \module persisted at quickcheck time. Raw: " <> truncRender regR)
  stepFooter 5 t5

  ----------------------------------------------------------------
  -- step 7 · BUG PINS via ghc_eval
  --
  -- Each eval expression targets ONE of the three dogfood bugs.
  -- The evaluated expression returns a Haskell 'Bool'; we assert
  -- the stdout string contains 'True'.
  ----------------------------------------------------------------
  t6 <- stepHeader 7 "bug pins · 3 targeted ghc_eval probes"

  -- Pin #1: pretty never wraps Neg in parens.
  --   Before fix: pretty (Neg (Neg (Lit 0))) == "-(-0)", which
  --               parse reads back as Neg (Lit 0) — a single Neg.
  --   After fix:  pretty (Neg (Neg (Lit 0))) == "--0", parse is
  --               faithful.
  rPin1 <- Client.callTool c GhcEval (object
    [ "expression" .=
        ("pretty (Neg (Neg (Lit 0))) == \"--0\"" :: Text)
    ])
  cPin1 <- liveCheck $ checkPure
    "pin #1 · pretty (Neg (Neg (Lit 0))) == \"--0\" (no paren wrap)"
    (evalOutputIs "True" rPin1)
    ("Bug pin for the Neg-parens regression. If this prints False, \
     \pretty re-introduced '(-x)' style wrapping for Neg, which the \
     \parser then mis-reads as a single Neg. Raw: "
     <> truncRender rPin1)

  -- Pin #2: pInsideParens does NOT eat a leading '-digits' when
  -- more expression follows before the closing ')'.
  rPin2 <- Client.callTool c GhcEval (object
    [ "expression" .=
        ("parse (pretty (Mul (Lit 0) (Add (Neg (Lit 0)) (Var \"abc\")))) \
          \== Just (Mul (Lit 0) (Add (Neg (Lit 0)) (Var \"abc\")))" :: Text)
    ])
  cPin2 <- liveCheck $ checkPure
    "pin #2 · roundtrip survives '(-0 + x)' — no greedy consumption"
    (evalOutputIs "True" rPin2)
    ("Bug pin for the pInsideParens regression. If this prints False, \
     \the parser ate '-0' as a negative literal and then choked on the \
     \'+' it didn't expect. Raw: " <> truncRender rPin2)

  -- Pin #3: simplify short-circuits 0*x → 0 even when x is an
  -- unbound variable. Refinement, not strict preservation.
  -- Spec.hs imports 'Expr.Syntax (Env, Error, Expr (..))' — it has
-- the types but not the value-level 'emptyEnv'. It /does/ import
-- 'Data.Map.Strict as Map', so 'Map.empty' is the in-scope way to
-- spell an empty env at this point.
  rPin3 <- Client.callTool c GhcEval (object
    [ "expression" .=
        ("eval Map.empty (simplify (Mul (Lit 0) (Var \"noSuchVar\"))) \
          \== Right 0" :: Text)
    ])
  cPin3 <- liveCheck $ checkPure
    "pin #3 · simplify (0 * undef) evaluates to Right 0 (refinement)"
    (evalOutputIs "True" rPin3)
    ("Bug pin for the simplify refinement law. If this prints False, \
     \simplify is no longer eliminating the '0 * x' short-circuit — \
     \the property should keep passing precisely because the original \
     \design allows this fold. Raw: " <> truncRender rPin3)

  stepFooter 7 t6

  pure [cGate, cLoad, cRT, cSPM, cSI, cReg, cPin1, cPin2, cPin3]

--------------------------------------------------------------------------------
-- oracles
--------------------------------------------------------------------------------

-- | True iff a 'ghc_quickcheck' response shows the property passed
-- with at least @minN@ tests (so a minimum-coverage regression like
-- "only ran 1 case" still trips).
propPassed :: Int -> Value -> Bool
propPassed minN v =
  fieldString "state"  v == Just "passed"
  && case fieldInt "passed" v of
       Just n  -> n >= minN
       Nothing -> False

-- | True iff a 'ghc_eval' output string /contains/ the given needle.
-- 'ghc_eval' returns a structured payload with the stringified
-- evaluation in the "output" field.
evalOutputIs :: Text -> Value -> Bool
evalOutputIs needle v = case fieldString "output" v of
  Just s  -> needle `T.isInfixOf` s
  Nothing -> False

--------------------------------------------------------------------------------
-- tiny field accessors
--------------------------------------------------------------------------------

fieldString :: Text -> Value -> Maybe Text
fieldString k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

fieldArrayLen :: Text -> Value -> Maybe Int
fieldArrayLen k v = case lookupField k v of
  Just (Array a) -> Just (length a)
  _              -> Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
