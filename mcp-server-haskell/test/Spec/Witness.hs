-- | Unit tests for the @ghc_witness@ property-distribution tool (#220) —
-- the in-process witnessEvalExpr structure, the in-process execution
-- path + fallback, and the compile-error result shape.
--
-- Extracted verbatim from the Spec.hs monolith (#271). These tests are
-- self-contained (no shared fixtures), so the driver keeps their
-- registrations and simply imports the moved functions.
module Spec.Witness
  ( testWitnessEvalExprStructure
  , testWitnessUsesInProcessPath
  , testWitnessInProcessFallback
  , testWitnessCompileErrorResult
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.QuickCheck as QcTool
import qualified HaskellFlows.Tool.Witness as WitnessTool

import Spec.Helpers (decodeToolResult)

-- | #220: 'witnessEvalExpr' must produce a well-formed in-process
-- expression that contains the QC sentinel markers, the
-- 'quickCheckWithResult' call, and the 'Data.Map.toList' + 'labels r'
-- extraction needed for the structured labels path.
testWitnessEvalExprStructure :: IO Bool
testWitnessEvalExprStructure =
  let expr = T.pack (QcTool.witnessEvalExpr "\\x -> x > 0")
  in pure $  T.isInfixOf "quickCheckWithResult"   expr
          && T.isInfixOf "Data.Map.toList"         expr
          && T.isInfixOf "labels r"                expr
          && T.isInfixOf "__QC_OUTPUT_START__"     expr
          && T.isInfixOf "__QC_LABELS_START__"     expr
          && T.isInfixOf "__QC_OUTPUT_END__"       expr
          && T.isInfixOf "__QC_LABELS_END__"       expr
          && T.isInfixOf "stdArgs"                 expr

-- | #220: 'Witness.hs' must call 'runQuickCheckWithLabelsInProcess'
-- (in-process path) rather than 'runQuickCheckWithLabelsViaCabalRepl'
-- (subprocess path) in its 'handle' function.
testWitnessUsesInProcessPath :: IO Bool
testWitnessUsesInProcessPath = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Witness.hs"
  pure $ T.isInfixOf "runQuickCheckWithLabelsInProcess ghcSess" src

-- | #220: 'runQuickCheckWithLabelsInProcess' must fall back to the
-- cabal-repl subprocess path when 'evalIOString' fails (e.g. the
-- user's project has no QuickCheck in its build-depends, so the
-- loaded stanza's package environment excludes Test.QuickCheck).
--
-- The cabal-repl fallback injects @--build-depends=QuickCheck@
-- automatically, so it always works regardless of the project's deps.
-- The fallback is triggered both on Left (compile/runtime exception)
-- and on Right with no sentinel markers (silent type-mismatch).
testWitnessInProcessFallback :: IO Bool
testWitnessInProcessFallback = do
  src <- TIO.readFile "src/HaskellFlows/Tool/QuickCheck.hs"
  pure $  -- Left (exception) branch triggers subprocess fallback
          T.isInfixOf "Left _ex ->" src
       && T.isInfixOf "runQuickCheckWithLabelsViaCabalRepl (gsProject ghcSess)" src
          -- Right (no-sentinel) branch also falls back
       && T.isInfixOf "__QC_OUTPUT_START__" src

-- | #240: 'compileErrorResult' returns a failed ToolResult with
-- kind=compile_error so agents distinguish "0 samples" from "compile error".
testWitnessCompileErrorResult :: IO Bool
testWitnessCompileErrorResult =
  let result = WitnessTool.compileErrorResult "\\x -> notAFunction x" "Not in scope: notAFunction"
  in case decodeToolResult result of
    Left _  -> pure False
    Right r ->
      pure $  Env.reStatus r == Env.StatusFailed
           && maybe False ((== Env.CompileError) . Env.eeKind) (Env.reError r)
