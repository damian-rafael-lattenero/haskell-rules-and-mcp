-- | Flow: mutation-testing sanity — the bug-finding oracle.
--
-- This scenario is deliberately a /real user flow/, not a description
-- of what the MCP currently returns. The question it answers is:
--
--     "If I persist a QuickCheck law, then mutate the code so the law
--      must fail, does @ghc_regression(run)@ actually catch it?"
--
-- The only correct answer is /yes, the mutated property must surface
-- in @regressions[]@/. If the MCP returns @regressions == []@ after
-- a mutation, something in the pipeline is broken — any of:
--
--   * GHCi did not re-read the module after the rewrite
--     (stale bytecode cache / :reload elided).
--   * The deferred-passes bug (F-08) snuck back in and GHCi
--     silently deferred the failing assertion instead of reporting.
--   * The property-store returned cached verdicts from the original
--     pass instead of re-running.
--   * @ghc_regression(run)@ short-circuited the replay.
--
-- Every one of those is a real regression we want to catch. The test
-- exists so those bugs cannot ship unnoticed, not because it documents
-- a current return shape.
module Scenarios.FlowMutation
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
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
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- sources
--
-- Two pure-Int functions with intentionally /different/ law signatures so
-- we can mutate one of them and leave the other stable. That lets the
-- assertion phase distinguish "regression is actually checking the right
-- props" from "regression flags everything because it re-loaded the world".
--
-- * 'add' is commutative. Mutation will break commutativity.
-- * 'double2x' produces even integers. Mutation leaves this alone.
--------------------------------------------------------------------------------

-- | Original source — laws should PASS here.
calcClean :: Text
calcClean = T.unlines
  [ "module Calc where"
  , ""
  , "add :: Int -> Int -> Int"
  , "add x y = x + y"
  , ""
  , "double2x :: Int -> Int"
  , "double2x x = x * 2"
  ]

-- | Mutated source — commutativity MUST fail; 'double2x' untouched.
--
-- The mutation is a single-character swap (@+@ → @-@) inside 'add'.
-- That alone is enough to destroy commutativity ('x-y ≠ y-x' for
-- almost every input).
calcMutated :: Text
calcMutated = T.unlines
  [ "module Calc where"
  , ""
  , "add :: Int -> Int -> Int"
  , "add x y = x - y    -- INTENTIONAL MUTATION: swaps + for -"
  , ""
  , "double2x :: Int -> Int"
  , "double2x x = x * 2"
  ]

--------------------------------------------------------------------------------
-- flow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- (1) scaffold + QuickCheck dep + module + write CLEAN source
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + deps + Calc.hs (clean)"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("mutation-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Calc"] :: [Text]) ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcClean
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (2) persist two laws that PASS on the clean source
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "persist prop_commutative + prop_double2xEven (both pass)"
  rCommClean <- Client.callTool c GhcQuickCheck (object
    [ "property" .= ("\\(x :: Int) (y :: Int) -> add x y == add y x" :: Text)
    , "module"   .= ("src/Calc.hs" :: Text)
    ])
  let commPassedClean = statePassed rCommClean
  cSeed1 <- liveCheck $ checkPure
    "seed · add x y == add y x passes on the clean source (+)"
    commPassedClean
    ("If this fails BEFORE the mutation, the scenario is miswired — \
     \'add' was supposed to be commutative initially. Raw: "
     <> truncRender rCommClean)

  rScaleClean <- Client.callTool c GhcQuickCheck (object
    [ "property" .= ("\\(x :: Int) -> even (double2x x)" :: Text)
    , "module"   .= ("src/Calc.hs" :: Text)
    ])
  let double2xPassedClean = statePassed rScaleClean
  cSeed2 <- liveCheck $ checkPure
    "seed · even (double2x x) passes on the clean source"
    double2xPassedClean
    ("Scenario precondition: double2x x = x*2 should always yield even. \
     \Raw: " <> truncRender rScaleClean)
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (3) MUTATE the source: x+y → x-y
  --
  -- At this point the file on disk disagrees with the last thing
  -- GHCi saw. A correct MCP must pick up the new bytes on the
  -- next touch — not cache the previous load.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "mutate Calc.hs: + becomes - inside add"
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcMutated
  -- Force a reload so the session layer's /visible/ module state
  -- catches up with the disk. If regression-run itself fails to
  -- reload, that is the bug we are hunting — but a conscientious
  -- client would reload first, so do that here.
  rReload <- Client.callTool c GhcLoad
               (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  let reloadedCleanly = statusOk rReload == Just True
                     && fieldArrayLen "errors" rReload == Just 0
  cReload <- liveCheck $ checkPure
    "mutation · module reloads cleanly (mutated source still type-checks)"
    reloadedCleanly
    ("The mutated source must still compile — the mutation only \
     \breaks semantics, not types. Raw: " <> truncRender rReload)
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- (4) BUG-FINDING ORACLE — regression(run) MUST surface the
  --     broken commutativity law and MUST NOT flag double2x.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_regression(run): must detect the mutated commutativity"
  rReg <- Client.callTool c GhcRegression
            (object [ "action" .= ("run" :: Text) ])

  let regs              = regressionExprs rReg
      sawCommutativity  = any (T.isInfixOf "add x y == add y x") regs
      sawScaleFalsely   = any (T.isInfixOf "even (double2x x)")     regs

  cDetect <- liveCheck $ checkPure
    "mutation detected · regressions[] contains the commutativity law"
    sawCommutativity
    ("This is the headline assertion. On a mutated 'add', the stored \
     \law 'add x y == add y x' MUST re-fail. If it did not, something \
     \in {ghci reload, deferred-pass, property-store, regression replay} \
     \is silently swallowing the failure. regressions seen: "
     <> T.pack (show regs)
     <> "  —  full payload: " <> truncRender rReg)

  cNoFalse <- liveCheck $ checkPure
    "no false positive · 'even (double2x x)' is NOT in regressions[]"
    (not sawScaleFalsely)
    ("The mutation did not touch 'double2x' — if regression flags it anyway, \
     \the runner is re-classifying unrelated laws as broken, which would \
     \drown real signal in noise. regressions seen: "
     <> T.pack (show regs))
  stepFooter 4 t3

  pure [cSeed1, cSeed2, cReload, cDetect, cNoFalse]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | A property 'passed' when the tool payload's "state" field is the
-- literal string "passed". Anything else (failed / gave_up / missing)
-- is treated as NOT passed.
statePassed :: Value -> Bool
statePassed v = case lookupField "state" v of
  Just (String s) -> s == "passed"
  _               -> False

-- | Extract the 'expression' field from every entry in a regressions
-- array, ignoring entries that are malformed. Returns empty list if
-- the payload has no 'regressions' key or it's not an array.
regressionExprs :: Value -> [Text]
regressionExprs v = case lookupField "regressions" v of
  Just (Array a) -> [ e | o <- V.toList a
                        , Object kv <- [o]
                        , Just (String e) <- [KeyMap.lookup (Key.fromText "expression") kv]
                    ]
  _ -> []

fieldArrayLen :: Text -> Value -> Maybe Int
fieldArrayLen k v = case lookupField k v of
  Just (Array a) -> Just (V.length a)
  _              -> Nothing

-- | Render a value with a hard cap so failure messages stay legible.
truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
