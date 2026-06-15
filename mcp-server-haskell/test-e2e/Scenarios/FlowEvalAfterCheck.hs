-- | Flow: B-4 regression — @ghc_eval@ / @ghc_scratch check@ must keep
-- working AFTER @ghc_check_module@ recompiles a home module.
--
-- THE BUG (surfaced dogfooding lambda-hm-v3): @applyFlavour@ never fixed
-- the GHC backend, so @ghc_check_module@ (StrictFresh + ForceRecomp)
-- recompiled the home module to OBJECT code in the shared HscEnv. The
-- next @ghc_eval@ / @ghc_scratch check@ — which run through the byte-code
-- interpreter (compileExpr / exprType / runDecls) — then failed to link
-- those object symbols:
--
--     GHC.ByteCode.Linker.lookupCE
--     During interactive linking, GHCi couldn't find the following
--     symbol: …_double_closure
--
-- This breaks the central write→check→eval loop the moment a project
-- grows past the first compile. The fix forces 'interpreterBackend' for
-- every home-module load, so check_module emits byte-code and the
-- subsequent eval can link it.
--
-- The oracle is honest: 'ghc_eval "double 21"' must return the
-- mathematically-correct "42" (not a tautology on the tool's own
-- output), and it must do so specifically AFTER a check_module — the one
-- ordering FlowScratchPromote / FlowExploratory never exercised.
module Scenarios.FlowEvalAfterCheck
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
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
import E2E.Envelope (fieldText, lookupField, statusOk)
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- fixture
--------------------------------------------------------------------------------

-- | A module with one verifiable top-level function. 'double 21 == 42'
-- is the independent oracle for the eval step.
calcSrc :: Text
calcSrc =
  "module Calc\n\
  \  ( double\n\
  \  ) where\n\
  \\n\
  \double :: Int -> Int\n\
  \double x = x * 2\n"

modPath :: Text
modPath = "src/Calc.hs"

--------------------------------------------------------------------------------
-- runFlow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 0 — scaffold + write Calc.hs + register + load.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text)
                 , "name"   .= ("eval-after-check" :: Text)
                 ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Calc"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  _ <- Client.callTool c GhcLoad (object [ "module_path" .= modPath ])

  -- Step 1 — check_module: the StrictFresh + ForceRecomp recompile that,
  -- before the fix, left the home module as object code in the env.
  t0 <- stepHeader 1 "ghc_check_module recompiles the home module"
  chkR <- Client.callTool c GhcCheckModule (object [ "module_path" .= modPath ])
  cCheck <- liveCheck $ checkPure
    "check_module → status=ok"
    (statusOk chkR == Just True)
    ("Got: " <> truncRender chkR)
  stepFooter 1 t0

  -- Step 2 — THE oracle: eval a home-module function AFTER the check.
  -- Pre-fix this threw the byte-code linker error; post-fix it returns 42.
  t1 <- stepHeader 2 "ghc_eval of a home-module function post-check"
  evalR <- Client.callTool c GhcEval (object [ "expression" .= ("double 21" :: Text) ])
  let okEval = statusOk evalR == Just True
            && case lookupField "output" evalR of
                 Just (String o) -> "42" `T.isInfixOf` o
                 _               -> False
  cEval <- liveCheck $ checkPure
    "eval(double 21) → status=ok AND output contains 42 (no linker error)"
    okEval
    ("Got: " <> truncRender evalR)
  stepFooter 2 t1

  -- Step 3 — secondary oracle: scratch check that references the same
  -- home-module symbol. Same byte-code-link path; must type-check.
  t2 <- stepHeader 3 "ghc_scratch check referencing the home symbol post-check"
  _ <- Client.callTool c GhcScratch
         (object [ "action" .= ("write" :: Text)
                 , "code"   .= ("useDouble :: Int\nuseDouble = double 10" :: Text)
                 , "kind"   .= ("hypothesis" :: Text)
                 ])
  scR <- Client.callTool c GhcScratch
           (object [ "action" .= ("check" :: Text), "id" .= ("scratch-1" :: Text) ])
  let okScratch = statusOk scR == Just True
               && fieldText "kind" scR == Just "type_ok"
  cScratch <- liveCheck $ checkPure
    "scratch check(useDouble = double 10) → kind=type_ok (no linker error)"
    okScratch
    ("Got: " <> truncRender scR)
  stepFooter 3 t2

  pure [cCheck, cEval, cScratch]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
