-- | Unit tests for Arbitrary type-param parsing, template rendering,
-- Coverage HPC mix dirs, constructor record forms, Suggest shape-encoding,
-- chooseStoreModule, isSimpleIdent, and parseShowModulesPaths. All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.ArbitraryUnit
  ( testTypeParamsOne
  , testTypeParamsTwo
  , testTypeParamsNone
  , testTemplatePolymorphic
  , testTemplateMultiParam
  , testCoveragePassesAllMixDirs
  , testParseHpcReportText
  , testCtorsRecordStrictWithKindHeader
  , testCtorsInlineRecord2Fields
  , testSuggestEncodeShapeSkipsListRules
  , testChooseStoreModuleIdentWithInfo
  , testChooseStoreModuleIdentNoInfo
  , testChooseStoreModuleLambda
  , testChooseStoreModuleModuleLoc
  , testIsSimpleIdentClassifier
  , testParseShowModulesPathsSimple
  , testParseShowModulesPathsMulti
  , testParseShowModulesPathsGarbage
  ) where

import qualified Data.Text as T
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))

import HaskellFlows.Parser.Coverage (parseCoverage, CoverageReport (..), Metric (..))
import qualified HaskellFlows.Tool.Arbitrary as Arb
import qualified HaskellFlows.Tool.Coverage as CoverageTool
import qualified HaskellFlows.Tool.QuickCheck as QcTool
import qualified HaskellFlows.Tool.Regression as RegTool
import qualified HaskellFlows.Suggest.Rules as SuggestRules

import Spec.Helpers (withTempProject)

import qualified Data.Text.IO as TIO
import HaskellFlows.Tool.Arbitrary
  ( Constructor (..)
  , parseConstructors
  , parseTypeParams
  , renderTemplate
  )
import HaskellFlows.Parser.TypeSignature (parseSignature)
import HaskellFlows.Suggest.Rules (applyRules, sLaw)

testTypeParamsOne :: IO Bool
testTypeParamsOne =
  let raw = T.unlines
        [ "type Run :: * -> *"
        , "data Run a = Run {runLen :: !Int, runVal :: !a}"
        ]
  in pure (parseTypeParams raw == ["a"])

testTypeParamsTwo :: IO Bool
testTypeParamsTwo =
  let raw = T.unlines
        [ "type Map :: * -> * -> *"
        , "data Map k v = Empty | Bin Int k v (Map k v) (Map k v)"
        ]
  in pure (parseTypeParams raw == ["k", "v"])

testTypeParamsNone :: IO Bool
testTypeParamsNone =
  let raw = T.unlines
        [ "type Foo :: *"
        , "data Foo = MkFoo"
        ]
  in pure (null (parseTypeParams raw))

testTemplatePolymorphic :: IO Bool
testTemplatePolymorphic =
  let out = renderTemplate "Run" ["a"]
              [Constructor "Run" (replicate 2 "arbitrary")]
  in pure $
       "instance Arbitrary a => Arbitrary (Run a) where" `T.isInfixOf` out
    && "Run <$> arbitrary <*> arbitrary"                  `T.isInfixOf` out

testTemplateMultiParam :: IO Bool
testTemplateMultiParam =
  let out = renderTemplate "Either" ["a", "b"]
              [ Constructor "Left"  ["arbitrary"]
              , Constructor "Right" ["arbitrary"]
              ]
  in pure $
       "instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where"
         `T.isInfixOf` out
    && "Left <$> arbitrary"                   `T.isInfixOf` out
    && "Right <$> arbitrary"                  `T.isInfixOf` out

-- | Phase 11c F-11: the first F-09 fix shipped with only one
-- derived @--hpcdir@. Cabal 3.14 writes mix files to TWO separate
-- paths (library's @build/extra-compilation-artifacts/hpc/vanilla/mix@
-- + test's @t/<test>/build/…/extra-compilation-artifacts/hpc/vanilla/mix@)
-- and @hpc report@ needs both flags present or it bails with
-- "can not find <pkg>-<ver>-inplace/Module in …". Post-fix,
-- @findMixDirs@ uses a @find -path@ pattern to enumerate every
-- mix dir under @dist-newstyle@, and @runHpcReport@ expands them
-- into a list of @--hpcdir=@ flags. Static source check is the
-- narrowest regression:
testCoveragePassesAllMixDirs :: IO Bool
testCoveragePassesAllMixDirs = do
  src <- TIO.readFile "src/HaskellFlows/Tool/Coverage.hs"
  pure $ T.isInfixOf "findMixDirs"                        src
      && T.isInfixOf "extra-compilation-artifacts"        src
      && T.isInfixOf "[FilePath] -> FilePath"             src
      -- keep the F-09 invariants alongside the F-11 ones
      && T.isInfixOf "findTixFile"                        src

-- | End-to-end smoke of the happy path: 'parseCoverage' must
-- recognise the text shape that @hpc report@ emits under GHC 9.x.
-- Pins both the parser and the enrichment contract together.
testParseHpcReportText :: IO Bool
testParseHpcReportText =
  let sample = T.unlines
        [ " 92% expressions used (12/13)"
        , " 100% boolean coverage (0/0)"
        , " 100% alternatives used (3/3)"
        , " 100% local declarations used (1/1)"
        , " 100% top-level declarations used (1/1)"
        ]
      rpt = parseCoverage sample
  in pure (length (crMetrics rpt) >= 5
         && any (\m -> mPercent m == Just 92) (crMetrics rpt))

-- | Phase 11b F-04 part A: GHC 9.x emits a kind-signature line
-- (@type Run :: * -> *@) BEFORE the data decl in @:i@ output.
-- 'parseConstructors' previously bailed because @hasCtorHeader@
-- only checked the collapsed string's prefix. Pin the GHC 9.x layout
-- plus a record constructor with strict fields — must parse into a
-- 2-arg Constructor.
testCtorsRecordStrictWithKindHeader :: IO Bool
testCtorsRecordStrictWithKindHeader =
  let raw = T.unlines
        [ "type Run :: * -> *"
        , "data Run a = Run {runLen :: !Int, runVal :: !a}"
        , "  \t-- Defined at src/DogfoodRle.hs:20:1"
        ]
  in case parseConstructors raw of
       [c] -> pure (cName c == "Run" && length (cArgs c) == 2)
       _   -> pure False

-- | Phase 11b F-04 part B: even absent the kind header, a record
-- constructor @Ctor {f1 :: T1, f2 :: T2}@ used to be mis-tokenised
-- because @groupTokens@ didn't treat @{}@ as grouping — fields got
-- split on every internal space, inflating 'cArgs' to 6 tokens.
testCtorsInlineRecord2Fields :: IO Bool
testCtorsInlineRecord2Fields =
  let raw = "data Run a = Run {runLen :: !Int, runVal :: !a}"
  in case parseConstructors raw of
       [c] -> pure (cName c == "Run" && length (cArgs c) == 2)
       _   -> pure False

-- | Phase 11b F-05: @ghc_suggest@ used to emit false laws for
-- @encode :: [a] -> [Run a]@ because @ruleListLengthPreserving@ and
-- @ruleListRoundtrip@ matched @([TyList _], TyList _)@ without
-- checking the inner types. Both @Self-inverse on lists@ and
-- @Length preserving@ are nonsense (don't even type-check) when
-- arg and return lists carry different element types. Pin the
-- invariant: for @[a] -> [SomeOther a]@, neither rule fires.
testSuggestEncodeShapeSkipsListRules :: IO Bool
testSuggestEncodeShapeSkipsListRules =
  case parseSignature "[a] -> [Run a]" of
    Nothing  -> pure False
    Just sig ->
      let laws = map sLaw (applyRules "encode" sig)
      in pure $ "Self-inverse on lists" `notElem` laws
             && "Length preserving / non-extending" `notElem` laws

--------------------------------------------------------------------------------
-- ghc_quickcheck: store-module resolution (the "persist with the right file"
-- UX fix). The dogfood of the expr-evaluator surfaced the bug: callers pass
-- the module of the /function under test/ ('src/Foo.hs'), but the property
-- itself lives in 'test/Spec.hs', and regression replay needs the latter to
-- put the identifier in scope. These tests pin the pure decision function
-- so the resolution rule can evolve without a live GHCi.
--------------------------------------------------------------------------------

-- | Wave-3: chooseStoreModule no longer consults ':info' output —
-- that plumbing sat on top of the subprocess ghci which has been
-- retired. Under the new contract it always returns the caller's
-- hint verbatim, regardless of what ':info' would have said.
testChooseStoreModuleIdentWithInfo :: IO Bool
testChooseStoreModuleIdentWithInfo = pure $
  QcTool.chooseStoreModule
    "prop_idempotent"
    (Just "src/Foo.hs")
    (Just ":info output\nprop_idempotent :: Expr -> Bool \
           \\t-- Defined at test/Spec.hs:12:1\n")
  == Just "src/Foo.hs"

-- | Identifier but no ':info' available (e.g. session busy) → fall back
-- to whatever the caller passed. We don't invent a path.
testChooseStoreModuleIdentNoInfo :: IO Bool
testChooseStoreModuleIdentNoInfo = pure $
  QcTool.chooseStoreModule
    "prop_idempotent"
    (Just "src/Foo.hs")
    Nothing
  == Just "src/Foo.hs"

-- | Lambda expression (not a simple identifier) → ':info' doesn't apply
-- even if we had it; use caller hint verbatim. Keeps backwards
-- compatibility for inline-property callers.
testChooseStoreModuleLambda :: IO Bool
testChooseStoreModuleLambda = pure $
  QcTool.chooseStoreModule
    "\\xs -> reverse (reverse xs) == xs"
    (Just "src/Foo.hs")
    (Just "anything") -- should be ignored because the expression isn't an ident
  == Just "src/Foo.hs"

-- | ':info' reports only a module ("Defined in 'Prelude'"), not a file
-- location. That's not actionable for regression replay, so we still
-- fall back to the caller hint. Prevents a regression where we'd
-- persist a module NAME where the store expects a file PATH.
testChooseStoreModuleModuleLoc :: IO Bool
testChooseStoreModuleModuleLoc = pure $
  QcTool.chooseStoreModule
    "prop_trivial"
    (Just "src/Foo.hs")
    (Just "prop_trivial :: Bool -- Defined in 'Prelude'")
  == Just "src/Foo.hs"

-- | Classifier: bare identifiers pass, qualified identifiers pass,
-- prefix operators and lambdas are rejected.
testIsSimpleIdentClassifier :: IO Bool
testIsSimpleIdentClassifier = pure $ and
  [       QcTool.isSimpleIdent "prop_x"
  ,       QcTool.isSimpleIdent "Spec.prop_x"
  ,       QcTool.isSimpleIdent "prop_x'"
  ,       QcTool.isSimpleIdent "Foo.Bar.baz"
  , not ( QcTool.isSimpleIdent "\\x -> x" )
  , not ( QcTool.isSimpleIdent "prop_x y" )           -- space → compound
  , not ( QcTool.isSimpleIdent "(prop_x)" )           -- parens rejected
  , not ( QcTool.isSimpleIdent "prop_x + 1" )
  , not ( QcTool.isSimpleIdent "" )
  , not ( QcTool.isSimpleIdent "42" )                 -- leading digit
  ]

--------------------------------------------------------------------------------
-- ghc_regression: parser for ':show modules' output. Used by the scope
-- snapshot/restore path so a regression run doesn't clobber the caller's
-- previously-loaded module set.
--------------------------------------------------------------------------------

-- | Single-module shape: the format GHCi emits for a project with
-- exactly one compiled module.
testParseShowModulesPathsSimple :: IO Bool
testParseShowModulesPathsSimple =
  let raw = T.pack "Foo              ( src/Foo.hs, interpreted )\n"
  in pure (RegTool.parseShowModulesPaths raw == ["src/Foo.hs"])

-- | Multi-module shape: library + test-suite layout. Order preserved;
-- paths extracted without picking up the module name or the 'kind'
-- trailing bit.
testParseShowModulesPathsMulti :: IO Bool
testParseShowModulesPathsMulti =
  let raw = T.unlines
        [ "Expr.Syntax     ( src/Expr/Syntax.hs, interpreted )"
        , "Expr.Eval       ( src/Expr/Eval.hs, interpreted )"
        , "Main            ( test/Spec.hs, interpreted )"
        ]
  in pure $
       RegTool.parseShowModulesPaths raw ==
         ["src/Expr/Syntax.hs", "src/Expr/Eval.hs", "test/Spec.hs"]

-- | Garbage / empty lines: skip. Parser is a best-effort tool, not a
-- strict validator; refusing to crash on unexpected input is the
-- important invariant.
testParseShowModulesPathsGarbage :: IO Bool
testParseShowModulesPathsGarbage = pure $ and
  [ null (RegTool.parseShowModulesPaths "")
  , null (RegTool.parseShowModulesPaths "random log output\n")
  , null (RegTool.parseShowModulesPaths "Foo  ( , interpreted )")
    -- real-looking line sandwiched between garbage: still extracted.
  , RegTool.parseShowModulesPaths
      ( T.unlines
          [ "random warning line"
          , "Bar  ( src/Bar.hs, interpreted )"
          , ""
          ]
      ) == ["src/Bar.hs"]
  ]
