-- | Unit tests for hlint-JSON parsing, cabal-validation helpers,
-- and the typed-hole-fit parser suite (#71, #169, #196). All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.HoleParse
  ( testHlintJson
  , testDuplicateDeps
  , testMissingSynopsis
  , testParseModules
  , testValidFits
  , testValidFitsOperatorBoundary
  , testHoleContinuationDetector
  , testParseFitHasCallStack
  , testSplitFitTypeBoundAt
  , testSplitFitTypeImportedFrom
  , testSplitFitTypeNoAnnotation
  , testRepairConstraintInSource
  , testExtractValidFitsGhc912
  ) where

import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import HaskellFlows.Parser.Hole
  ( HoleFit (..)
  , TypedHole (..)
  , extractValidFits
  , isContinuationFitLine
  , parseFitLine
  , parseTypedHoles
  , repairConstraintInSource
  , splitFitTypeSource
  )
import HaskellFlows.Tool.CheckProject (parseExposedModules)
import HaskellFlows.Tool.Lint (parseHlintJson)
import qualified HaskellFlows.Tool.Lint as LintTool
import HaskellFlows.Tool.Deps (validatePackageName)
import qualified HaskellFlows.Tool.ValidateCabal as VC

testHlintJson :: IO Bool
testHlintJson =
  let raw = T.pack
        "[{\"severity\":\"Warning\",\"hint\":\"Use isNothing\",\
        \\"file\":\"src/Foo.hs\",\"startLine\":10,\"startColumn\":5,\
        \\"from\":\"x == Nothing\",\"to\":\"isNothing x\"}]"
  in pure $ case parseHlintJson raw of
       [s] -> LintTool.sSeverity s == "Warning"
           && LintTool.sHint s     == "Use isNothing"
           && LintTool.sStartLine s == 10
       _ -> False

testDuplicateDeps :: IO Bool
testDuplicateDeps =
  let cabalBody = T.unlines
        [ "library"
        , "    build-depends: base, text, base"
        ]
  in pure $ any (("duplicate-dep" ==) . VC.iKind) (VC.scanCabalText cabalBody)

testMissingSynopsis :: IO Bool
testMissingSynopsis =
  let cabalBody = "cabal-version: 3.0\nname: foo\nversion: 0.1.0.0"
      issues    = VC.scanCabalText cabalBody
  in pure $ any (("missing-synopsis" ==) . VC.iKind) issues
         && any ((VC.CabalSevWarn ==) . VC.iSeverity) issues

testParseModules :: IO Bool
testParseModules =
  let cabalBody = T.unlines
        [ "library"
        , "  exposed-modules:  Foo.Bar"
        , "                    Foo.Baz"
        , "  other-modules:    Foo.Internal"
        , "  build-depends:    base"
        ]
      mods = parseExposedModules cabalBody
  in pure $ "Foo.Bar"      `elem` mods
         && "Foo.Baz"      `elem` mods
         && "Foo.Internal" `elem` mods

testValidFits :: IO Bool
testValidFits =
  let block = T.lines $ T.unlines
        [ "src/Foo.hs:5:5: warning: [GHC-88464]"
        , "    • Found hole: _ :: Int -> Int"
        , "    • Valid hole fits include"
        , "        id :: forall a. a -> a"
        , "        negate :: forall a. Num a => a -> a"
        ]
  in pure $ case extractValidFits block of
       [a, b] -> hfName a == "id"
              && hfName b == "negate"
              && "Num a" `T.isInfixOf` hfType b
       _      -> False

-- | Issue #71: real GHC output for the @_addOp :: Int -> Int -> Int@
-- hole produces fit-head lines like @(-) :: forall a. Num a => …@
-- whose name starts with @(@. Pre-#71 the continuation classifier
-- treated any indented line starting with @(@ as a continuation
-- of the previous fit, so the operator-named fit was absorbed
-- into the preceding entry's @source@ field, dropping the fit
-- entirely from the array and inflating its predecessor's
-- @source@ string.
--
-- Post-#71 we expect 4 distinct fits, each with its own clean
-- @name@ / @type@ / @source@ — including @(-)@ as its own row.
testValidFitsOperatorBoundary :: IO Bool
testValidFitsOperatorBoundary =
  let block = T.lines $ T.unlines
        [ "    • Valid hole fits include"
        , "        addPair :: Int -> Int -> Int"
        , "          (bound at /tmp/Demo.hs:14:1)"
        , "        (-) :: forall a. Num a => a -> a -> a"
        , "          with (-) @Int"
        , "          (imported from `Prelude' at /tmp/Demo.hs:2:8-19)"
        , "        asTypeOf :: forall a. a -> a -> a"
        , "          with asTypeOf @Int"
        , "        const :: forall a b. a -> b -> a"
        , "          with const @Int @Int"
        ]
      fits = extractValidFits block
      names = map hfName fits
      addPairFit = head fits
      addPairSrc = fromMaybe "" (hfSource addPairFit)
  in pure $ length fits == 4
         && names == ["addPair", "(-)", "asTypeOf", "const"]
         && "(bound at" `T.isInfixOf` addPairSrc
         -- Critical: addPair's source must NOT have absorbed
         -- the next fit's identifier or type signature.
         && not ("(-)"            `T.isInfixOf` addPairSrc)
         && not ("forall a. Num"  `T.isInfixOf` addPairSrc)

-- | Issue #71: pin the new contract for the continuation
-- classifier — the type-signature substring is the canonical
-- disambiguator. Three boundary cases:
--
--   * Operator-named fit '(-) :: forall a. Num a => a -> a -> a'
--     IS a fit-head (must NOT be classified as continuation).
--   * Plain '(bound at /tmp/X.hs:1:1)' IS a continuation.
--   * '(imported from ...)' IS a continuation.
testHoleContinuationDetector :: IO Bool
testHoleContinuationDetector =
  pure $  not (isContinuationFitLine "        (-) :: forall a. Num a => a -> a -> a")
       &&      isContinuationFitLine "          (bound at /tmp/X.hs:1:1)"
       &&      isContinuationFitLine "          (imported from `Prelude' at /tmp/X.hs:2:8-19)"
       &&      isContinuationFitLine "          with (-) @Int"
       -- Sanity: parseFitLine on the operator head extracts the
       -- name with parens and an empty source.
       && case parseFitLine "        (-) :: forall a. Num a => a -> a -> a" of
            Just hf -> hfName hf == "(-)"
                    && "Num a" `T.isInfixOf` hfType hf
            Nothing -> False

-- | #169: Functions with @HasCallStack@ / @(?callStack :: CallStack)@
-- constraints were silently dropped from the @validFits@ list because
-- 'parseFitLine' broke on the first @\"(\"@, producing an empty type
-- and triggering the @T.null (T.strip ty)@ guard.  After the fix,
-- the type must survive intact.
testParseFitHasCallStack :: IO Bool
testParseFitHasCallStack =
  -- GHC can render HasCallStack either as the class name or as the
  -- desugared (?callStack :: CallStack) form.
  let line1 = "        error :: HasCallStack => [Char] -> a (imported from GHC.Stack)"
      line2 = "        error :: (?callStack :: CallStack) => [Char] -> a (imported from GHC.Stack)"
      check line = case parseFitLine line of
        Nothing -> False
        Just hf ->
          hfName hf == "error"
          -- Type must include the constraint, not be empty or truncated.
          && (T.isInfixOf "HasCallStack" (hfType hf)
              || T.isInfixOf "callStack" (hfType hf))
          && T.isInfixOf "[Char] -> a" (hfType hf)
  in pure (check line1 && check line2)

-- | #169: 'splitFitTypeSource' must split at \" (bound at \" and
-- return the annotation as the second component.
testSplitFitTypeBoundAt :: IO Bool
testSplitFitTypeBoundAt =
  let (ty, src) = splitFitTypeSource
        "(?callStack :: CallStack) => [Char] -> a (bound at src/Foo.hs:1:1)"
  in pure $
       T.isInfixOf "callStack" ty
    && T.isInfixOf "[Char] -> a" ty
    && T.isInfixOf "bound at" src

-- | #169: 'splitFitTypeSource' must split at \" (imported from \" too.
testSplitFitTypeImportedFrom :: IO Bool
testSplitFitTypeImportedFrom =
  let (ty, src) = splitFitTypeSource
        "Int -> Int -> Int (imported from Data.Bits)"
  in pure $
       ty == "Int -> Int -> Int"
    && T.isInfixOf "Data.Bits" src

-- | #169: when there is no annotation, the full input is returned as
-- the type and the source component is empty.
testSplitFitTypeNoAnnotation :: IO Bool
testSplitFitTypeNoAnnotation =
  let (ty, src) = splitFitTypeSource "forall a. Num a => a -> a -> a"
  in pure $ ty == "forall a. Num a => a -> a -> a" && T.null src

-- | #196: GHC 9.12 wraps @HasCallStack@-constrained types onto a
-- continuation line, landing the constraint in @hfSource@ instead of
-- @hfType@.  'repairConstraintInSource' must move the constraint prefix
-- back into the type field and keep only the @with …@ clause in source.
testRepairConstraintInSource :: IO Bool
testRepairConstraintInSource =
  let -- Simulates the post-collapseFits state for GHC 9.12 output:
      --   cycle :: forall a.
      --             Stack.Types.HasCallStack => [a] -> [a]
      --             with cycle @Char (imported from 'Prelude' ...)
      broken = HoleFit
        { hfName   = "cycle"
        , hfType   = "forall a."
        , hfSource = Just "Stack.Types.HasCallStack => [a] -> [a] with cycle @Char (imported from 'Prelude' ...)"
        }
      repaired = repairConstraintInSource broken
      -- No-op case: source starts with a non-constraint annotation.
      noOp = HoleFit
        { hfName   = "id"
        , hfType   = "forall a. a -> a"
        , hfSource = Just "with id @Char (imported from 'Prelude' ...)"
        }
      noOpRepaired = repairConstraintInSource noOp
  in pure $
       -- Type now carries the full constraint.
       "Stack.Types.HasCallStack" `T.isInfixOf` hfType repaired
       && "[a] -> [a]"            `T.isInfixOf` hfType repaired
       && "forall a."             `T.isInfixOf` hfType repaired
       -- Source kept the with-clause only.
       && case hfSource repaired of
            Just s  -> "with cycle @Char" `T.isInfixOf` s
                    && not ("HasCallStack" `T.isInfixOf` s)
            Nothing -> False
       -- No-op: id's source must not be touched.
       && hfType repaired /= hfType broken   -- repair happened
       && hfType noOpRepaired == hfType noOp -- no-op untouched

-- | #196: end-to-end check through 'extractValidFits' using a GHC 9.12
-- simulated output block where @cycle@'s type is wrapped onto a
-- continuation line.
testExtractValidFitsGhc912 :: IO Bool
testExtractValidFitsGhc912 =
  let block = T.lines $ T.unlines
        -- GHC 9.12 wraps the HasCallStack constraint onto a separate
        -- continuation line (indent=10) instead of keeping it on the
        -- fit-head line.
        [ "    • Valid hole fits include"
        , "        cycle :: forall a."
        , "          Stack.Types.HasCallStack => [a] -> [a]"
        , "          with cycle @Char"
        , "          (imported from 'Prelude' at /tmp/F.hs:1:1-16)"
        , "        tail :: forall a."
        , "          Stack.Types.HasCallStack => [a] -> [a]"
        , "          with tail @Char"
        , "          (imported from 'Prelude' at /tmp/F.hs:1:1-16)"
        ]
      fits = extractValidFits block
  in case fits of
       [cycleFit, tailFit] ->
         pure $
           -- Both fit names parsed correctly.
           hfName cycleFit == "cycle"
           && hfName tailFit  == "tail"
           -- Types must include the full constraint, not be truncated.
           && "HasCallStack" `T.isInfixOf` hfType cycleFit
           && "[a] -> [a]"  `T.isInfixOf` hfType cycleFit
           && "HasCallStack" `T.isInfixOf` hfType tailFit
           && "[a] -> [a]"  `T.isInfixOf` hfType tailFit
           -- The constraint must NOT appear in the source fields.
           && not ("HasCallStack" `T.isInfixOf` fromMaybe "" (hfSource cycleFit))
           && not ("HasCallStack" `T.isInfixOf` fromMaybe "" (hfSource tailFit))
       _ -> pure False
