-- | Unit tests for the QuickCheck-output parser, typed-hole parser,
-- 'Arbitrary' constructor helpers, 'parseHoogleLine', and the
-- 'augmentEvalContext' pure dedup logic (#86). All pure — no GhcSession.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.ParseQc
  ( testQcPassed
  , testQcFailed
  , testQcGaveUp
  , testQcGaveUpValidationKind
  , testQcException
  , testQcUnparsed
  , testClassifyMissingArbitrary
  , testExtractArbitraryType
  , testQcMissingArbitraryNextStep
  , testHoleOne
  , testHoleIgnored
  , testCtorsInline
  , testCtorsMultiline
  , testCtorsSynonym
  , testCtorsNoUniqueSuffix
  , testTemplate3
  , testHoogleHit
  , testHoogleEmpty
  , testEvalContextEmptyAddsAll
  , testEvalContextSkipsExistingPrelude
  , testEvalContextSecondCallNoop
  , testEvalContextSubsetExisting
  , testEvalContextIdempotent
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Parser.Hole
  ( TypedHole (..)
  , parseTypedHoles
  )
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
import HaskellFlows.Tool.Arbitrary
  ( Constructor (..)
  , renderTemplate
  , parseConstructors
  )
import HaskellFlows.Tool.Hoogle (HoogleHit (..), parseHoogleLine)
import qualified HaskellFlows.Tool.Eval as EvalTool
import qualified HaskellFlows.Tool.QuickCheck as QcTool

--------------------------------------------------------------------------------
-- Phase 4: QuickCheck output + typed-hole parsers
--------------------------------------------------------------------------------

testQcPassed :: IO Bool
testQcPassed =
  let raw = "+++ OK, passed 100 tests."
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcPassed _ 100 -> True
       _              -> False

testQcFailed :: IO Bool
testQcFailed =
  let raw = T.unlines
        [ "*** Failed! Falsifiable (after 3 tests and 2 shrinks):"
        , "[1,2,3]"
        , ""
        ]
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcFailed _ 2 2 cex -> cex == "[1,2,3]"
       _                  -> False

testQcGaveUp :: IO Bool
testQcGaveUp =
  let raw = "*** Gave up! Passed only 12 tests; 88 discarded."
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcGaveUp _ 12 88 -> True
       _                -> False

-- | Issue #211: QcGaveUp must produce kind='validation', not
-- kind='not_in_scope' (which wrongly implies "add an import").
testQcGaveUpValidationKind :: IO Bool
testQcGaveUpValidationKind =
  let result = QcTool.renderResult (QcGaveUp "\\x -> x > 0" 0 1000) Nothing
  in pure $ case Env.reError result of
       Just err -> Env.eeKind err == Env.Validation
       Nothing  -> False

testQcException :: IO Bool
testQcException =
  let raw = "*** Failed! Exception: 'divide by zero' (after 1 test):"
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcException _ exn -> "divide by zero" `T.isInfixOf` exn
       _                 -> False

testQcUnparsed :: IO Bool
testQcUnparsed =
  let raw = "something completely unexpected"
  in pure $ case parseQuickCheckOutput "prop" raw of
       QcUnparsed {} -> True
       _             -> False

-- | B-6: a missing-Arbitrary stderr must classify as 'MissingInstance',
-- NOT 'CompileError' (the project compiles; QuickCheck just can't
-- generate inputs). GHC uses typographic quotes around the constraint.
testClassifyMissingArbitrary :: IO Bool
testClassifyMissingArbitrary =
  let stderr =
        "<interactive>:6:11: error: [GHC-39999]\n\
        \    \x2022 No instance for \x2018\&Arbitrary Type\x2019\n\
        \        arising from a use of \x2018\&quickCheckWithResult\x2019"
  in pure $ QcTool.classifyStderrKind (Just stderr) == Env.MissingInstance
         && QcTool.isMissingArbitraryStderr stderr
         -- a genuine compile error must still classify as CompileError
         && QcTool.classifyStderrKind (Just "src/Foo.hs:3:1: error: parse error")
              == Env.CompileError

-- | B-6: extract the offending type from the diagnostic so 'ghc_arbitrary'
-- can be steered with 'type_name' pre-filled. Handles the bare type and a
-- single layer of wrapping parens.
testExtractArbitraryType :: IO Bool
testExtractArbitraryType =
  pure $ QcTool.extractArbitraryType
           "No instance for \x2018\&Arbitrary Type\x2019 arising"
           == Just "Type"
      && QcTool.extractArbitraryType
           "No instance for \x2018\&Arbitrary (Expr Int)\x2019 arising"
           == Just "Expr Int"
      && isNothing (QcTool.extractArbitraryType "unrelated error")

-- | B-6: the rendered envelope for a missing-Arbitrary QcUnparsed must
-- carry error_kind=missing_instance AND a nextStep pointing at
-- 'ghc_arbitrary' (not the misleading 'ghc_check_project' the generic
-- compile-error path emits).
testQcMissingArbitraryNextStep :: IO Bool
testQcMissingArbitraryNextStep =
  let stderr =
        "<interactive>:6:11: error: [GHC-39999]\n\
        \    \x2022 No instance for \x2018\&Arbitrary Type\x2019 arising from a use of \x2018\&quickCheckWithResult\x2019"
      result = QcTool.renderResult (QcUnparsed "\\(x :: Type) -> x == x" "") (Just stderr)
      kindOk = case Env.reError result of
                 Just err -> Env.eeKind err == Env.MissingInstance
                 Nothing  -> False
      nextOk = case Env.reNextStep result of
                 Just v  -> "ghc_arbitrary" `T.isInfixOf` T.pack (show v)
                 Nothing -> False
  in pure (kindOk && nextOk)

-- | A canonical GHC-88464 block. The whitespace before the indented
-- continuation lines is significant — GHC uses 4 spaces + bullet.
holeSampleOutput :: T.Text
holeSampleOutput = T.unlines
  [ "src/Foo.hs:12:5: warning: [GHC-88464] [-Wtyped-holes]"
  , "    \x2022 Found hole: _ :: Int -> Int"
  , "    \x2022 In the expression: _"
  , "      In an equation for 'bar': bar = _"
  , "    \x2022 Relevant bindings include"
  , "        x :: Int (bound at src/Foo.hs:12:1)"
  , "        bar :: Int -> Int (bound at src/Foo.hs:12:1)"
  ]

testHoleOne :: IO Bool
testHoleOne =
  pure $ case parseTypedHoles holeSampleOutput of
    [h] -> thHole h == "_"
        && thExpectedType h == "Int -> Int"
        && thFile h == "src/Foo.hs"
        && thLine h == 12
        && thColumn h == 5
        && length (thRelevantBindings h) == 2
    _   -> False

testHoleIgnored :: IO Bool
testHoleIgnored =
  let raw = "src/Foo.hs:3:1: error: Not in scope: 'blah'"
  in pure (null (parseTypedHoles raw))

--------------------------------------------------------------------------------
-- Phase 5: Arbitrary + Hoogle parsers
--------------------------------------------------------------------------------

-- | Inline @data T = A | B Int | C Int String@ should produce three
-- constructors with arities 0, 1, 2.
testCtorsInline :: IO Bool
testCtorsInline =
  let raw = T.unlines
        [ "data Foo = Bar | Baz Int | Qux Int String"
        , "  \t-- Defined at src/Foo.hs:5:1"
        ]
  in pure $ case parseConstructors raw of
       [a, b, c] -> cName a == "Bar" && null (cArgs a)
                 && cName b == "Baz" && length (cArgs b) == 1
                 && cName c == "Qux" && length (cArgs c) == 2
       _         -> False

-- | GHCi's multi-line form (one @|@ per line) must parse to the same
-- three constructors.
testCtorsMultiline :: IO Bool
testCtorsMultiline =
  let raw = T.unlines
        [ "data Foo"
        , "  = Bar"
        , "  | Baz Int"
        , "  | Qux Int String"
        , "  \t-- Defined at src/Foo.hs:5:1"
        ]
  in pure $ case parseConstructors raw of
       [a, b, c] -> cName a == "Bar" && cName b == "Baz" && cName c == "Qux"
                 && length (cArgs c) == 2
       _         -> False

-- | Type synonyms have no @=@ constructor list; parser must return an
-- empty list rather than invent ctors.
testCtorsSynonym :: IO Bool
testCtorsSynonym =
  let raw = "type Alias = Int"
  in pure (null (parseConstructors raw))

-- | #170: 'renderDataCon' used 'showPprUnsafe' which includes GHC's
-- internal unique suffix on type variable names (e.g. @a_ig1m@).
-- After the fix it uses @sdocSuppressUniques = True@ so arg names
-- are clean user-facing identifiers.
--
-- This test pins the contract on 'parseConstructors': a data decl
-- rendered with clean type-var names (as 'renderDataCon' now
-- produces) must produce @cArgs@ with exactly those clean names —
-- no underscore-hash suffix that would confuse downstream consumers.
testCtorsNoUniqueSuffix :: IO Bool
testCtorsNoUniqueSuffix =
  -- Simulate the clean output that renderDataCon now produces.
  -- If the unique suffix leaked, the decl would be "data Pair a_ig1m b_xyz = Pair a_ig1m b_xyz"
  -- and cArgs would contain "a_ig1m", "b_xyz".
  let clean  = "data Pair a b = Pair a b"
      leaky  = "data Pair a_ig1m b_xyz99 = Pair a_ig1m b_xyz99"
  in pure $
       -- Clean form: args should be exactly ["a", "b"]
       ( case parseConstructors clean of
           [c] -> cArgs c == ["a", "b"]
           _   -> False )
       -- Leaky form would produce unique-like args — confirm parser
       -- is transparent (does not strip them): the fix must be in
       -- renderDataCon, not in the parser.
       && ( case parseConstructors leaky of
              [c] -> cArgs c == ["a_ig1m", "b_xyz99"]
              _   -> False )

testTemplate3 :: IO Bool
testTemplate3 =
  let ctors = [ Constructor "Bar" []
              , Constructor "Baz" ["Int"]
              , Constructor "Qux" ["Int", "String"]
              ]
      out   = renderTemplate "Foo" [] ctors
  in pure $
       "instance Arbitrary Foo where"              `T.isInfixOf` out
    && "pure Bar"                                  `T.isInfixOf` out
    && "Baz <$> arbitrary"                         `T.isInfixOf` out
    && "Qux <$> arbitrary <*> arbitrary"           `T.isInfixOf` out

testHoogleHit :: IO Bool
testHoogleHit =
  let line = "Prelude filter :: (a -> Bool) -> [a] -> [a]"
  in pure $ case parseHoogleLine line of
       Just h -> hhSignature h == "(a -> Bool) -> [a] -> [a]"
              && hhModule h    == Just "Prelude"
       _      -> False

testHoogleEmpty :: IO Bool
testHoogleEmpty =
  pure (isNothing (parseHoogleLine "No results found"))

--------------------------------------------------------------------------------
-- Issue #86: augmentEvalContext dedup
--
-- The pure helper 'selectMissingExtras' is the load-bearing dedup
-- step. Direct GHC-session integration (calling 'augmentEvalContext'
-- twice and inspecting 'getContext') would require spinning up a
-- real GHC session inside the test runner, which is what the e2e
-- 'eval_context_dedup' scenario covers; here we just nail down the
-- pure arithmetic so a regression in the set logic surfaces in
-- ~1 ms instead of waiting on a 200 s e2e cycle.
--------------------------------------------------------------------------------

-- | Anchor: with no existing imports, every baseline extra is
-- missing and must be appended.
testEvalContextEmptyAddsAll :: IO Bool
testEvalContextEmptyAddsAll =
  let missing = EvalTool.selectMissingExtras Set.empty EvalTool.evalContextExtras
  in pure (missing == EvalTool.evalContextExtras
              && length missing == 5)

-- | Issue #86 — the bug shape: 'Prelude' already in the existing
-- context (because 'autoLoadProject' put it there) MUST suppress
-- the baseline 'Prelude' from being re-added. Pre-fix the result
-- contained 'Prelude' twice on the second eval call.
testEvalContextSkipsExistingPrelude :: IO Bool
testEvalContextSkipsExistingPrelude =
  let existing = Set.fromList ["Prelude"]
      missing  = EvalTool.selectMissingExtras existing EvalTool.evalContextExtras
  in pure (missing == ["System.IO", "Data.List", "Control.Monad", "Control.Concurrent"]
              && notElem "Prelude" missing)

-- | After the FIRST 'augmentEvalContext' call has run, every
-- baseline module is in the existing set. The SECOND call must
-- therefore append nothing — this is exactly the scenario that
-- produced the unbounded growth pre-fix.
testEvalContextSecondCallNoop :: IO Bool
testEvalContextSecondCallNoop =
  let existing = Set.fromList EvalTool.evalContextExtras
      missing  = EvalTool.selectMissingExtras existing EvalTool.evalContextExtras
  in pure (null missing)

-- | The dedup is purely on module name string. An existing context
-- with a SUBSET of the baseline (e.g. only 'Prelude' and
-- 'Data.List' from a project that imports them) leaves only the
-- complement to append.
testEvalContextSubsetExisting :: IO Bool
testEvalContextSubsetExisting =
  let existing = Set.fromList ["Prelude", "Data.List", "Foo.Bar"]
      missing  = EvalTool.selectMissingExtras existing EvalTool.evalContextExtras
  in pure (missing == ["System.IO", "Control.Monad", "Control.Concurrent"])

-- | Idempotence law: running the dedup against the result of a
-- previous run is a no-op. This is the clean property-style
-- statement of the bug — pre-fix, the operation was *not*
-- idempotent under the same input set.
testEvalContextIdempotent :: IO Bool
testEvalContextIdempotent =
  let step1     = EvalTool.selectMissingExtras Set.empty EvalTool.evalContextExtras
      afterStep = Set.fromList step1
      step2     = EvalTool.selectMissingExtras afterStep EvalTool.evalContextExtras
  in pure (null step2 && step1 == EvalTool.evalContextExtras)

--------------------------------------------------------------------------------
-- Issue #88: PermissiveJSON IntField + BoolField
--------------------------------------------------------------------------------

-- | The canonical wire shape — JSON number — must keep parsing.
