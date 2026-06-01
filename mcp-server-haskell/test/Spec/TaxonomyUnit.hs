-- | Unit tests for tool taxonomy invariants (#92D), Modules tool,
-- QuickCheck annotation helpers (eta-reduce, inject-annotate),
-- property rendering, and suggest-idempotent/involutive annotated forms.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.TaxonomyUnit
  ( testTaxonomyDocListsAllTools
  , testToolCountWithinCap
  , testEveryToolHasCategory
  , testCategoryCountsMatchTaxonomy
  , testEveryToolHasVersion
  , testToolVersionIsSemverTriple
  , testModulesRegistered
  , testModulesRejectsBadAction
  , testExtractModulesEnvelope
  , testExtractModulesTopLevel
  , testInjectAnnotateBareX
  , testInjectAnnotateXs
  , testInjectAnnotateAlreadyAnnotated
  , testInjectAnnotateNonLambda
  , testEtaReduceBare
  , testEtaReduceAnnotated
  , testEtaReduceList
  , testEtaReduceNestedArrow
  , testEtaReduceNonLambda
  , testRenderPropNoLambda
  , testRenderTestFileNoLambdaAssign
  , testExportOptionsGhcPragma
  , testExportHeaderCurrentToolName
  , testRenderPropSigSingle
  , testRenderPropSigMulti
  , testRenderPropSigNone
  , testRenderTestFileSigPresent
  , testInjectAnnotateMultiParam
  , testInjectAnnotateStringConstrained
  , testInjectAnnotateOperatorOnlyX
  , testSuggestIdempotentAnnotated
  , testSuggestInvolutiveAnnotated
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Char (isDigit)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Server (allToolDescriptors, allToolNameTexts)
import HaskellFlows.Mcp.ToolName
  ( allToolNames, toolCategory, toolVersion
  , ToolCategory (..), ToolName (..), parseToolName, toolCategoryText
  )
import HaskellFlows.Mcp.Protocol (ToolDescriptor (..), ToolContent (..), ToolResult (..))
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Tool.Modules as Modules
import qualified HaskellFlows.Tool.QuickCheckExport as QcExport
import qualified HaskellFlows.Suggest.Rules as SuggestRules

import Spec.Helpers (runToolEnvelope, withTempProject)

import Data.Aeson (object, (.=))
import Data.Maybe (isJust, isNothing)
import qualified Data.Text.IO as TIO
import HaskellFlows.Data.PropertyStore (StoredProperty (..))
import qualified HaskellFlows.Tool.AddImport as AddImport
import HaskellFlows.Parser.TypeSignature (parseSignature)
import HaskellFlows.Suggest.Rules (applyRules, Suggestion (..))

testTaxonomyDocListsAllTools :: IO Bool
testTaxonomyDocListsAllTools = do
  doc <- TIO.readFile "../docs/TOOL_TAXONOMY.md"
  let names   = allToolNameTexts
      missing = [ t | t <- names, not (("`" <> t <> "`") `T.isInfixOf` doc) ]
      total   = length names
      totalOk = ("**" <> T.pack (show total) <> "**") `T.isInfixOf` doc
  unless (null missing) $ do
    putStrLn "TOOL_TAXONOMY.md omits registered wire names:"
    mapM_ (putStrLn . ("  " ++) . T.unpack) missing
  unless totalOk $
    putStrLn ("TOOL_TAXONOMY.md does not state the live tool total (**"
                ++ show total ++ "**).")
  pure (null missing && totalOk)

------------------------------------------------------------------------
-- Issue #94 Phase A — tool taxonomy invariants
------------------------------------------------------------------------

-- | Invariant 1: the total registered-tool count must not exceed the
-- documented cap of 50.  The cap is bumped only via an explicit PR
-- with rationale — this prevents silent surface-bloat regressions.
--
-- Current count: 36 tools.  Cap: 50 (14 slots of headroom).
testToolCountWithinCap :: IO Bool
testToolCountWithinCap = do
  let n   = length allToolNames
      cap = 50 :: Int
  when (n > cap) $
    putStrLn $ "  SURFACE BLOAT: " ++ show n ++ " tools > cap " ++ show cap
  pure (n <= cap)

-- | Invariant 2: every 'ToolName' constructor must map to a non-empty
-- category text string via 'toolCategory' + 'toolCategoryText'.
-- Enforces that adding a new constructor also adds an arm to the
-- 'toolCategory' exhaustive case (otherwise it's a compile error).
testEveryToolHasCategory :: IO Bool
testEveryToolHasCategory = pure $
  not (any (T.null . toolCategoryText . toolCategory) allToolNames)

-- | Invariant 3: the count per category must match the taxonomy
-- published in @docs/TOOL_TAXONOMY.md@ (issue #94 §2).
-- Current breakdown: 27 primitives, 4 composites, 3 gates, 2 control-plane.
testCategoryCountsMatchTaxonomy :: IO Bool
testCategoryCountsMatchTaxonomy = pure $
  countCat CatPrimitive    == 27
  -- ^ #94 Phase B retrofit: GhcModules replaces GhcAddModules +
  -- GhcRemoveModules (36 → 35).
  -- #94 Phase C step 1: GhcDeps action="explain" replaces
  -- GhcDepsExplain outright (35 → 34).
  -- #94 Phase C step 3: ghc_quickcheck runs>=2 replaces
  -- GhcDeterminism outright (34 → 33).
  -- #94 Phase C step 4: ghc_refactor action="move_symbol" replaces
  -- GhcMove outright (33 → 32).
  -- #94 Phase C step 5: GhcProject (action=create|switch|validate
  -- |bootstrap) replaces GhcCreateProject + GhcSwitchProject +
  -- GhcValidateCabal + GhcBootstrap outright (32 → 29 — four
  -- removed, one added).  No deprecation period because the
  -- project has a single internal consumer.
  -- #94 Phase C step 6: GhcPropertyStore (action=list|run|export
  -- |audit) replaces GhcPropertyLifecycle + GhcRegression +
  -- GhcQuickCheckExport + GhcPropertyAudit outright (29 → 26 —
  -- four removed, one added).
  -- #253: GhcScratch — persistent LLM code canvas (26 → 27).
  && countCat CatComposite    ==  4
  && countCat CatGate         ==  3
  && countCat CatControlPlane ==  2
  -- ^ #94 Phase C step 2: GhcToolchain (action="status"|"warmup")
  -- replaces GhcToolchainStatus + GhcToolchainWarmup outright.
  -- Net delta on control-plane: 3 → 2 (two removed, one added).
  where
    countCat c = length [ t | t <- allToolNames, toolCategory t == c ]

------------------------------------------------------------------------
-- Issue #99 Phase B · per-tool version surface
------------------------------------------------------------------------

-- | Invariant: 'toolVersion' returns a non-empty Text for every
-- 'ToolName'. Adding a constructor without an arm in
-- 'HaskellFlows.Mcp.ToolName.toolVersion' is a compile error; this
-- test additionally rejects an empty-string entry sneaking in.
testEveryToolHasVersion :: IO Bool
testEveryToolHasVersion = pure $
  not (any (T.null . toolVersion) allToolNames)

-- | Invariant: every per-tool version parses as a 'MAJOR.MINOR.PATCH'
-- semver triple of non-negative integers. Catches typos like "1.0" or
-- "1.0.0-rc1" creeping into the table without a deliberate decision.
-- Stage A of #99 mandates simple triples; later phases can extend the
-- grammar (pre-release, build metadata) when an actual use case shows up.
testToolVersionIsSemverTriple :: IO Bool
testToolVersionIsSemverTriple = pure $
  all (validSemverTriple . T.unpack . toolVersion) allToolNames
  where
    validSemverTriple s = case wordsBy (== '.') s of
      [a, b, c] -> all isPositiveOrZero [a, b, c]
      _         -> False
    isPositiveOrZero t =
      not (null t)
      && all isDigit t
    -- Local re-impl to avoid pulling in Data.List.Split.
    wordsBy p = foldr step []
      where
        step c acc = case acc of
          (x : xs) | not (p c) -> (c : x) : xs
          _        | not (p c) -> [c] : acc
          _                    -> [] : acc

------------------------------------------------------------------------
-- Issue #94 Phase B · action-discriminated 'modules' primitive
------------------------------------------------------------------------

-- | The new 'GhcModules' constructor must round-trip through
-- 'parseToolName . toolNameText' AND be classified as a primitive
-- (not gate, not composite, not control-plane).  Sanity check that
-- adding the constructor without the corresponding 'toolCategory'
-- arm doesn't slip past the type system (it can't — 'toolCategory'
-- is exhaustive — but the *category* could still be wrong).
testModulesRegistered :: IO Bool
testModulesRegistered = pure $
  parseToolName "ghc_modules" == Just GhcModules
  && toolCategory GhcModules    == CatPrimitive

-- | The dispatcher must refuse an action it does not recognise with
-- a structured response (status=refused), not crash and not silently
-- delegate to the wrong handler.  Mirrors the contract every other
-- action-discriminated primitive (e.g. 'ghc_deps') already honours.
testModulesRejectsBadAction :: IO Bool
testModulesRejectsBadAction =
  case mkProjectDir "/tmp" of
    Left _   -> pure False  -- mkProjectDir failed; cannot run the test
    Right pd -> do
      -- Direct handler call (no JSON-RPC envelope needed): we only
      -- verify that the dispatcher returns isError=true with content
      -- present, which is how 'ToolResult' renders a refused response.
      -- The ProjectDir argument is never read because the action
      -- check fires first.
      ToolResult content isErr <-
        Modules.handle pd
          (object
            [ "action"  .= ("nuke_everything" :: T.Text)
            , "modules" .= (["Foo"] :: [T.Text])
            ])
      pure (isErr && not (null content))

--------------------------------------------------------------------------------
-- Issue #105 · extractModules envelope peeling
--------------------------------------------------------------------------------

-- | Build a 'ToolResult' that mirrors what 'Hoogle.handle' actually
-- produces: the hits list is nested inside the \"result\" sub-object, not
-- at the top level.  The bug was that 'extractModules' looked for
-- \"results\" (wrong key) at the top level (wrong nesting depth).
testExtractModulesEnvelope :: IO Bool
testExtractModulesEnvelope = do
  let hitsPayload = A.object
        [ "hits" .=
            [ A.object ["module" .= ("Data.Maybe" :: T.Text)]
            , A.object ["module" .= ("Data.List"  :: T.Text)]
            ]
        , "count" .= (2 :: Int)
        ]
      tr = Env.toolResponseToResult (Env.mkOk hitsPayload)
      mods = AddImport.extractModules tr
  pure (mods == ["Data.Maybe", "Data.List"])

-- | The old bug looked for \"results\" (plural, wrong key). Verify that
-- a payload with only a \"results\" key — but no \"hits\" — returns [].
-- Regression pin: the wrong key must remain unrecognised.
testExtractModulesTopLevel :: IO Bool
testExtractModulesTopLevel = do
  let rawJson = "{\"status\":\"ok\",\"result\":{\"results\":[{\"module\":\"Data.Maybe\"}]}}"
      tr = ToolResult [TextContent (T.pack rawJson)] False
      mods = AddImport.extractModules tr
  pure (null mods)

--------------------------------------------------------------------------------
-- Issue #104c · injectTypeAnnotations
--------------------------------------------------------------------------------

-- | #172: A bare @\\x ->@ lambda whose parameter is constrained by a
-- named function ('foo x') must NOT be annotated — the type is
-- determined by 'foo' and injecting @:: Int@ would cause a type error.
testInjectAnnotateBareX :: IO Bool
testInjectAnnotateBareX = pure $
  QcExport.injectTypeAnnotations "\\x -> foo x == foo (foo x)"
    == "\\x -> foo x == foo (foo x)"

-- | A parameter whose name ends in @s@ (list convention) acquires @:: [Int]@.
testInjectAnnotateXs :: IO Bool
testInjectAnnotateXs = pure $
  QcExport.injectTypeAnnotations "\\xs -> reverse (reverse xs) == xs"
    == "\\(xs :: [Int]) -> reverse (reverse xs) == xs"

-- | Already-annotated lambda heads pass through unchanged.
testInjectAnnotateAlreadyAnnotated :: IO Bool
testInjectAnnotateAlreadyAnnotated =
  let expr = "\\(x :: Int) -> foo x == x"
  in pure (QcExport.injectTypeAnnotations expr == expr)

-- | Non-lambda expressions are returned verbatim.
testInjectAnnotateNonLambda :: IO Bool
testInjectAnnotateNonLambda =
  let expr = "foo x == foo (foo x)"
  in pure (QcExport.injectTypeAnnotations expr == expr)

-- | #215: bare single param eta-reduces correctly.
testEtaReduceBare :: IO Bool
testEtaReduceBare =
  pure $ QcExport.etaReduceLambda "\\x -> x + 1 == x + 1"
      == Just ("x", "x + 1 == x + 1")

-- | #215: annotated param @(x :: Int)@ eta-reduces preserving the
-- parenthesised annotation intact.
testEtaReduceAnnotated :: IO Bool
testEtaReduceAnnotated =
  pure $ QcExport.etaReduceLambda "\\(x :: Int) -> x + 0 == x"
      == Just ("(x :: Int)", "x + 0 == x")

-- | #215: list param @(xs :: [Int])@ eta-reduces.
testEtaReduceList :: IO Bool
testEtaReduceList =
  pure $ QcExport.etaReduceLambda "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
      == Just ("(xs :: [Int])", "reverse (reverse xs) == xs")

-- | #215: a param with a function-type annotation @(f :: Int -> Int)@
-- contains a nested \" -> \" inside the parens. The paren-aware
-- scanner must not split there; it must find the outer arrow.
testEtaReduceNestedArrow :: IO Bool
testEtaReduceNestedArrow =
  pure $ QcExport.etaReduceLambda "\\(f :: Int -> Int) -> f 0 == 0"
      == Just ("(f :: Int -> Int)", "f 0 == 0")

-- | #215: a non-lambda expression returns @Nothing@.
testEtaReduceNonLambda :: IO Bool
testEtaReduceNonLambda =
  pure $ isNothing (QcExport.etaReduceLambda "foo x == foo (foo x)")

-- | #215: 'renderPropBinding' must NOT emit @= \\@ (the HLint-flagged
-- redundant-lambda pattern) for a plain stored property.
testRenderPropNoLambda :: IO Bool
testRenderPropNoLambda =
  let sp  = StoredProperty
              { spExpression = "\\x -> double (double x) == (4 * x :: Int)"
              , spModule     = Just "src/Scratch.hs"
              , spPassed     = 1
              , spUpdated    = 0
              , spCases     = 0
              }
      -- Access via renderTestFile so we test the full pipeline.
      out = QcExport.renderTestFile [sp]
  in pure $ not ("= \\" `T.isInfixOf` out)

-- | #215: a full 'renderTestFile' output for the canonical property
-- set must contain no @prop_N = \\@ lines at all.
testRenderTestFileNoLambdaAssign :: IO Bool
testRenderTestFileNoLambdaAssign =
  let props =
        [ StoredProperty
            { spExpression = "\\x -> double (double x) == (4 * x :: Int)"
            , spModule     = Just "src/Scratch.hs"
            , spPassed     = 1, spUpdated = 0, spCases = 0 }
        , StoredProperty
            { spExpression = "\\(x :: Int) -> safeDiv (x :: Int) 0 == Nothing"
            , spModule     = Just "src/Scratch.hs"
            , spPassed     = 1, spUpdated = 0, spCases = 0 }
        , StoredProperty
            { spExpression = "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
            , spModule     = Nothing
            , spPassed     = 1, spUpdated = 0, spCases = 0 }
        ]
      out    = QcExport.renderTestFile props
      lines_ = T.lines out
  in pure $ not (any ("= \\" `T.isInfixOf`) lines_)

--------------------------------------------------------------------------------
-- Issue #231 — unused imports + missing-sigs warnings in generated file
--------------------------------------------------------------------------------

-- | #231: generated file contains OPTIONS_GHC pragma suppressing both
-- -Wunused-imports and -Wmissing-signatures, preventing CI failures when
-- cabal test compiles the exported Spec.hs with -Wall.
testExportOptionsGhcPragma :: IO Bool
testExportOptionsGhcPragma =
  let rendered = QcExport.renderTestFile []
  in pure ("{-# OPTIONS_GHC -Wno-unused-imports -Wno-missing-signatures #-}"
           `T.isInfixOf` rendered)

-- Issue #198 — stale tool name + missing type signatures
--------------------------------------------------------------------------------

-- | #198: 'generatedHeader' must reference the current tool name
-- @ghc_property_store@, not the retired @ghc_quickcheck_export@.
testExportHeaderCurrentToolName :: IO Bool
testExportHeaderCurrentToolName =
  pure $  "ghc_property_store" `T.isInfixOf` QcExport.generatedHeader
       && not ("ghc_quickcheck_export" `T.isInfixOf` QcExport.generatedHeader)

-- | #198: 'renderPropSignature' returns a @prop_N :: T -> Bool@ line
-- for a single annotated parameter.
testRenderPropSigSingle :: IO Bool
testRenderPropSigSingle =
  pure $ QcExport.renderPropSignature 1 "(xs :: [Int])"
       == Just "prop_1 :: [Int] -> Bool"

-- | #198: 'renderPropSignature' concatenates multiple param types
-- with @->@ and appends @-> Bool@.
testRenderPropSigMulti :: IO Bool
testRenderPropSigMulti =
  pure $ QcExport.renderPropSignature 2 "(x :: Int) (y :: Int)"
       == Just "prop_2 :: Int -> Int -> Bool"

-- | #198: 'renderPropSignature' returns Nothing for an unannotated
-- bare parameter (no @::@ present in the param string).
testRenderPropSigNone :: IO Bool
testRenderPropSigNone =
  pure (isNothing (QcExport.renderPropSignature 3 "x"))

-- | #198: 'renderTestFile' output must include a type signature line
-- immediately before each @prop_N@ binding that has annotated params.
testRenderTestFileSigPresent :: IO Bool
testRenderTestFileSigPresent =
  let sp = StoredProperty
             { spExpression = "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
             , spModule     = Nothing
             , spPassed     = 1
             , spUpdated    = 0, spCases = 0 }
      out   = QcExport.renderTestFile [sp]
      lns   = T.lines out
      -- The sig line must appear directly before the binding line
      pairs = zip lns (drop 1 lns)
  in pure $ any (\(sig, bind) ->
       "prop_1 :: [Int] -> Bool" `T.isInfixOf` sig &&
       "prop_1 (xs :: [Int])" `T.isInfixOf` bind) pairs

-- | Two bare params — @x@ and @y@ — used only in operator expressions
-- get @:: Int@ (both are unconstrained by any named function).
testInjectAnnotateMultiParam :: IO Bool
testInjectAnnotateMultiParam = pure $
  QcExport.injectTypeAnnotations "\\x y -> x + y == y + x"
    == "\\(x :: Int) (y :: Int) -> x + y == y + x"

-- | #172: A parameter constrained by a String function must NOT
-- receive @:: Int@ — that would produce a type error.
testInjectAnnotateStringConstrained :: IO Bool
testInjectAnnotateStringConstrained = pure $
  QcExport.injectTypeAnnotations "\\x -> reverseStr (reverseStr x) == x"
    == "\\x -> reverseStr (reverseStr x) == x"

-- | #172: A parameter used only in an operator expression (no named
-- function constrains its type) must still get @:: Int@.
testInjectAnnotateOperatorOnlyX :: IO Bool
testInjectAnnotateOperatorOnlyX = pure $
  QcExport.injectTypeAnnotations "\\x -> x + 1 == 1 + x"
    == "\\(x :: Int) -> x + 1 == 1 + x"

--------------------------------------------------------------------------------
-- Issue #104a · Suggest/Rules emits annotated lambda params
--------------------------------------------------------------------------------

-- | The idempotent rule for @a -> a@ must now emit @\\(x :: Int) ->@
-- rather than a bare @\\x ->@.  Without the annotation, exporting the
-- property to a compiled Spec.hs triggers \"Ambiguous type variable\".
testSuggestIdempotentAnnotated :: IO Bool
testSuggestIdempotentAnnotated =
  case parseSignature "a -> a" of
    Nothing  -> pure False
    Just sig ->
      let props = [ sProperty s | s <- applyRules "normalise" sig
                                 , sLaw s == "Idempotent" ]
      in pure $ case props of
           (p:_) -> T.isInfixOf ":: Int" p
                 || T.isInfixOf ":: [Int]" p
           []    -> False

-- | The involutive rule for @a -> a@ must also emit an annotated param.
testSuggestInvolutiveAnnotated :: IO Bool
testSuggestInvolutiveAnnotated =
  case parseSignature "a -> a" of
    Nothing  -> pure False
    Just sig ->
      let props = [ sProperty s | s <- applyRules "rev" sig
                                 , sLaw s == "Involutive" ]
      in pure $ case props of
           (p:_) -> T.isInfixOf ":: Int" p
                 || T.isInfixOf ":: [Int]" p
           []    -> False
