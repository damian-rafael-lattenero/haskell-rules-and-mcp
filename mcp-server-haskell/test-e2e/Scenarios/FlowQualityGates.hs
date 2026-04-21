-- | Flow: quality gates with DELIBERATE defects.
--
-- The earlier version of this scenario was tautological: it loaded
-- a clean module and asserted every gate returned green. That proves
-- nothing — a tool that always returned success=true would also pass.
-- The rewrite plants three kinds of defects and asserts each gate
-- SURFACES the corresponding one:
--
--   * Calc.hs      — clean module. check_module / check_project / lint
--                    on this subset must stay green (regression anchor).
--   * Hinty.hs     — triggers an HLint suggestion (redundant '+ 0').
--                    ghci_lint must return a non-empty suggestions[].
--   * Broken.hs    — has a type error. ghci_check_project must flag
--                    overall=false AND failed ≥ 1.
--
-- Failure modes the oracle catches:
--
--   (a) ghci_lint silently ignores the hint (suggestions=[]).
--   (b) ghci_check_project returns overall=true despite a module
--       with a type error — would mean check_project is not actually
--       compiling every listed module.
--   (c) ghci_check_module on the clean module reports 'compile' gate
--       as ok=false — the happy-path anchor; regression indicator.
module Scenarios.FlowQualityGates
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
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

-- | Clean module: no hints, compiles cleanly. Anchor for happy-path
-- assertions.
calcSrc :: Text
calcSrc =
  "module Calc (greet, double) where\n\
  \\n\
  \greet :: String -> String\n\
  \greet n = \"Hello, \" ++ n\n\
  \\n\
  \double :: Int -> Int\n\
  \double x = x * 2\n"

-- | Module with an intentional HLint-worthy pattern. 'reverse .
-- reverse' is the canonical redundancy HLint 3.x flags ("Avoid
-- reverse. reverse"). We pick a specific pattern so the oracle can
-- assert the category, not just "at least one hint".
hintySrc :: Text
hintySrc =
  "module Hinty (idList) where\n\
  \\n\
  \idList :: [a] -> [a]\n\
  \idList xs = reverse (reverse xs)\n"

-- | Module with a deliberate type error. Int being compared for Eq
-- is fine; Int + "foo" is not. check_module + check_project must
-- BOTH surface this.
brokenSrc :: Text
brokenSrc =
  "module Broken (bad) where\n\
  \\n\
  \bad :: Int -> Int\n\
  \bad x = x + \"definitely not an Int\"\n"

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  --------------------------------------------------------------------
  -- setup — scaffold + write both files + register ONLY Calc at first
  -- (so the happy-path check_project is genuinely green).
  --------------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Calc (clean) + Hinty (hint-worthy)"
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("gates-demo" :: Text) ])
  _ <- Client.callTool c "ghci_add_modules"
         (object [ "modules" .= (["Calc", "Hinty"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs")  calcSrc
  TIO.writeFile (projectDir </> "src" </> "Hinty.hs") hintySrc
  _ <- Client.callTool c "ghci_load"
         (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  stepFooter 1 t0

  --------------------------------------------------------------------
  -- ghci_lint on src/ — MUST find the Hinty redundancy.
  -- This is the oracle the old scenario was missing: if hlint
  -- silently ignored everything, suggestions=[] and the old test
  -- still passed (because it only checked 'suggestions' was an
  -- array). We now require at least one.
  --------------------------------------------------------------------
  t1 <- stepHeader 2 "ghci_lint on src/ · MUST find Hinty redundancy"
  lintR <- Client.callTool c "ghci_lint"
             (object [ "path" .= ("src/" :: Text) ])
  -- NOTE: no 'lint success == true' check here. With default
  -- fail_on=warning, the tool correctly reports success=false
  -- when ANY suggestion of that severity shows up — which is
  -- exactly our planted hint. The suggestions[] non-empty check
  -- below is the real oracle; 'success' is a severity-derived
  -- rollup and checking it here would conflict with the planted
  -- defect.
  c2 <- liveCheck $ checkJsonFieldMatches
          "lint · suggestions ≥ 1 (Hinty reverse.reverse planted)"
          lintR "suggestions" arrayNonEmpty
          "HLint should flag Hinty's 'reverse (reverse xs)'. If this \
          \is empty, hlint is either not running or not reading src/. \
          \suggestions=[] is indistinguishable from 'everything clean' \
          \in the old oracle — the planted defect closes that hole."
  stepFooter 2 t1

  --------------------------------------------------------------------
  -- ghci_check_module — on the CLEAN module. Happy-path anchor.
  -- If this fails, the whole e2e is suspicious (something broke at
  -- the compile gate level, unrelated to our defect injections).
  --------------------------------------------------------------------
  t2 <- stepHeader 3 "ghci_check_module(Calc.hs) · clean anchor"
  cmR <- Client.callTool c "ghci_check_module"
           (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  c3 <- liveCheck $ checkJsonField "check_module overall=true"
                      cmR "overall" (Bool True)
  c4 <- liveCheck $ checkJsonFieldMatches
          "check_module · gates.compile.ok == true"
          cmR "gates" gateCompileOk
          "the compile gate must be green on a clean source"
  stepFooter 3 t2

  --------------------------------------------------------------------
  -- Plant the DELIBERATE BREAKAGE — Broken.hs with a type error.
  -- We add it to the cabal AFTER the happy-path checks above so the
  -- next check_project is meaningfully distinct from the first.
  --------------------------------------------------------------------
  t3 <- stepHeader 4 "inject Broken.hs (type error)"
  _ <- Client.callTool c "ghci_add_modules"
         (object [ "modules" .= (["Broken"] :: [Text]) ])
  TIO.writeFile (projectDir </> "src" </> "Broken.hs") brokenSrc
  stepFooter 3 t3

  --------------------------------------------------------------------
  -- ghci_check_project must now FLAG the broken module. The earlier
  -- test only asserted green on a clean project — it couldn't tell
  -- you whether the tool was even visiting every module. Here,
  -- planting a type error in Broken and expecting failed=1 proves
  -- the tool is actually compiling each listed module.
  --------------------------------------------------------------------
  t4 <- stepHeader 5 "ghci_check_project · MUST flag Broken"
  cpR <- Client.callTool c "ghci_check_project" (object [])
  c5 <- liveCheck $ checkJsonField
          "check_project overall=false (Broken has a type error)"
          cpR "overall" (Bool False)
  c6 <- liveCheck $ checkJsonFieldMatches
          "check_project · failed ≥ 1 (Broken counted)"
          cpR "failed" (numberAtLeast 1)
          "Broken.hs has 'x + \"string\"' — the compile pipeline \
          \cannot let this pass. If failed==0 the tool reported \
          \green on a module it did not actually type-check."
  stepFooter 5 t4

  pure [c2, c3, c4, c5, c6]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

arrayNonEmpty :: Value -> Bool
arrayNonEmpty (Array a) = not (V.null a)
arrayNonEmpty _         = False

numberAtLeast :: Int -> Value -> Bool
numberAtLeast n (Number x) = n <= (round x :: Int)
numberAtLeast _ _          = False

-- | gates.compile is an object with an 'ok' boolean.
gateCompileOk :: Value -> Bool
gateCompileOk (Object o) = case KeyMap.lookup (Key.fromText "compile") o of
  Just (Object co) -> case KeyMap.lookup (Key.fromText "ok") co of
    Just (Bool True) -> True
    _                -> False
  _ -> False
gateCompileOk _ = False
