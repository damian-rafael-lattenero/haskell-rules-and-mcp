-- | Flow: @ghc_property_lifecycle@ — inspect the property store.
--
-- After a successful @ghc_quickcheck@ auto-persists a property,
-- @ghc_property_lifecycle@ exposes the store's contents. Used
-- by agents to prune flaky / obsolete entries before a push.
--
-- #294 regression: a @ghc_quickcheck@ call with @runs >= 2@ routes
-- to the determinism / flakiness handler, which used to run the
-- property (N * qcMaxSuccess generated cases — STRICTLY stronger
-- evidence than a single run) yet drop the regression entirely: the
-- store stayed empty after a clean stability check because the
-- handler was never handed the 'Store'. This scenario now seeds a
-- SECOND property via @runs=2@ and asserts the store grows to two
-- entries, locking the persist-on-all-pass behaviour.
module Scenarios.FlowPropertyLifecycle
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
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | Tiny module so @ghc_quickcheck@ has something to test.
calcSrc :: Text
calcSrc = T.unlines
  [ "module Calc where"
  , ""
  , "double :: Int -> Int"
  , "double x = x * 2"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup — scaffold + add QuickCheck + load a tiny module
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + add QuickCheck + load Calc"
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("proplife-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Calc"] :: [Text]) ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- seed: quickcheck a simple property so it persists.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "quickcheck (auto-persist on pass)"
  _ <- Client.callTool c GhcQuickCheck (object
    [ "property" .= propSingle
    , "module"   .= ("src/Calc.hs" :: Text)
    ])
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- #294: a SECOND property via runs=2 (the determinism path). It
  -- must also auto-persist on all-pass — pre-fix this entry was
  -- silently dropped, so the store stayed at one entry.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "quickcheck runs=2 (determinism path must also persist)"
  rDet <- Client.callTool c GhcQuickCheck (object
    [ "property" .= propRuns2
    , "module"   .= ("src/Calc.hs" :: Text)
    , "runs"     .= (2 :: Int)
    ])
  cDet <- liveCheck $ checkJsonFieldMatches
            "runs=2 routed to the flakiness handler and all runs passed"
            rDet "summary" stabilityPassed
            "expected a determinism summary like 'All 2 runs passed'"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- ghc_property_lifecycle — inspect the store.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_property_lifecycle (inspect store)"
  r <- Client.callTool c GhcPropertyStore (object [ "action" .= ("list" :: Text) ])
  c1 <- liveCheck $ checkJsonField "success" r "success" (Bool True)
  c2 <- liveCheck $ checkJsonFieldMatches
          "store has ≥ 2 properties (single-run + runs=2 both persisted)"
          r "properties" (arrayOfLenAtLeast 2)
          "expected BOTH the single-run and the runs=2 property — if only \
          \one is present the determinism path dropped its regression (#294)"
  c3 <- liveCheck $ checkJsonFieldMatches
          "each entry carries 'expression' + 'passed' fields"
          r "properties" entriesAreWellFormed
          "every property entry should have 'expression' and 'passed' \
          \keys — the minimum the regression runner needs"
  c4 <- liveCheck $ checkJsonFieldMatches
          "the runs=2 property is the one that persisted (#294)"
          r "properties" (storeContainsExpr propRuns2)
          "the runs=2 property's expression must appear in the store"
  stepFooter 4 t3

  pure [cDet, c1, c2, c3, c4]

-- | The single-run seed property.
propSingle :: Text
propSingle = "\\(x :: Int) -> double x == x + x"

-- | A DISTINCT property checked with runs=2 so its presence in the store
-- unambiguously proves the determinism path persisted (#294).
propRuns2 :: Text
propRuns2 = "\\(x :: Int) -> double x == 2 * x"

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

arrayOfLenAtLeast :: Int -> Value -> Bool
arrayOfLenAtLeast n (Array a) = V.length a >= n
arrayOfLenAtLeast _ _         = False

-- | #294: the determinism summary on all-pass reads
-- "All N runs passed — no flakiness observed."
stabilityPassed :: Value -> Bool
stabilityPassed (String s) = "runs passed" `T.isInfixOf` s
stabilityPassed _          = False

-- | #294: True when the property store's array carries an entry whose
-- @expression@ equals the given text — used to prove the runs=2 property
-- (not just SOME property) is the one that persisted.
storeContainsExpr :: Text -> Value -> Bool
storeContainsExpr expr (Array a) = any matches (V.toList a)
  where
    matches (Object o) = case KeyMap.lookup (Key.fromText "expression") o of
      Just (String s) -> s == expr
      _               -> False
    matches _ = False
storeContainsExpr _ _ = False

entriesAreWellFormed :: Value -> Bool
entriesAreWellFormed (Array a) =
  not (V.null a) && all oneEntry (V.toList a)
  where
    oneEntry (Object o) =
         hasKey "expression" o
      && hasKey "passed"     o
    oneEntry _ = False
    hasKey k = KeyMap.member (Key.fromText k)
entriesAreWellFormed _ = False
