-- | Unit tests for Move.hs module-header parsing, QcSummariseStderr
-- filtering/capping, stderr classification, NIS extraction, and
-- wmhm stripping. All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.ParseHeaderUnit
  ( testParseHeaderSimple
  , testParseHeaderMultiSegment
  , testParseHeaderExportsMultiline
  , testParseHeaderSkipsLeading
  , testParseHeaderNoHeader
  , testParseHeaderInvalidName
  , testQcSummariseStderrFiltersNoise
  , testQcSummariseStderrCaps
  , testClassifyStderrNotInScope
  , testClassifyStderrGeneric
  , testClassifyStderrCompileError
  , testClassifyStderrCompileErrorCode
  , testIsCompileErrorStderrTrue
  , testIsCompileErrorStderrFalse
  , testExtractNisBareName
  , testExtractNisTypeSig
  , testExtractNisAbsent
  , testQcSummariseStripsWmhm
  ) where

import qualified Data.Text as T

import qualified HaskellFlows.Tool.Move as MoveTool
import qualified HaskellFlows.Tool.QuickCheck as QcTool

import Data.Maybe (isNothing)
import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.CheckModule as CheckModule

testParseHeaderSimple :: IO Bool
testParseHeaderSimple =
  pure $ CheckModule.parseModuleHeader "module Foo where" == Just "Foo"

-- | Issue #74: dotted module names round-trip exactly.
testParseHeaderMultiSegment :: IO Bool
testParseHeaderMultiSegment =
  pure $ CheckModule.parseModuleHeader "module Foo.Bar.Baz where"
       == Just "Foo.Bar.Baz"

-- | Issue #74: explicit export list — same line OR multi-line.
-- 'apply_exports' produces the same-line shape; the scaffold and
-- hand-edits often produce the multi-line variant. Both are valid.
testParseHeaderExportsMultiline :: IO Bool
testParseHeaderExportsMultiline = do
  let oneLine   = "module Foo (a, b, c) where"
      multiLine = T.unlines
        [ "module Foo"
        , "  ( a"
        , "  , b"
        , "  ) where"
        ]
  pure $ CheckModule.parseModuleHeader oneLine   == Just "Foo"
      && CheckModule.parseModuleHeader multiLine == Just "Foo"

-- | Issue #74: skip Haddock blurbs, pragmas, blank lines BEFORE
-- the module header. 'ghc_create_project' emits exactly this
-- shape: a Haddock comment, optional pragma, then `module … where`.
testParseHeaderSkipsLeading :: IO Bool
testParseHeaderSkipsLeading =
  let src = T.unlines
        [ "-- | Some Haddock blurb."
        , "{-# LANGUAGE OverloadedStrings #-}"
        , ""
        , "-- another comment"
        , "module DogfoodSuite.Math where"
        , ""
        , "square :: Int -> Int"
        ]
  in pure $ CheckModule.parseModuleHeader src == Just "DogfoodSuite.Math"

-- | Issue #74: a file without a `module … where` line is not a
-- regular Haskell source. Returning Nothing is the honest answer
-- — the caller falls back to path-only comparison.
testParseHeaderNoHeader :: IO Bool
testParseHeaderNoHeader =
  let src = T.unlines
        [ "-- just a comment"
        , "x = 1"
        ]
  in pure $ isNothing (CheckModule.parseModuleHeader src)

-- | Issue #74: defensive parsing — Haskell module names must
-- start uppercase. A misspelled or invalid header should not
-- be accepted as a valid name.
testParseHeaderInvalidName :: IO Bool
testParseHeaderInvalidName = do
  let lower    = "module foo where"
      digit    = "module 1Foo where"
  pure $ isNothing (CheckModule.parseModuleHeader lower)
      && isNothing (CheckModule.parseModuleHeader digit)

--------------------------------------------------------------------------------
-- BUG-PLUS-mediocre-2: summariseStderr cleans cabal noise, caps length
--------------------------------------------------------------------------------

-- | Real cabal stderr mixes signal ("Variable not in scope:
-- foo") with noise ("Resolving dependencies…",
-- "Build profile: -w ghc-9.12.2", cabal -W banner lines).
-- The summariser must keep the signal and drop the noise.
testQcSummariseStderrFiltersNoise :: IO Bool
testQcSummariseStderrFiltersNoise =
  let raw = T.unlines
        [ "Resolving dependencies..."
        , "Build profile: -w ghc-9.12.2 -O1"
        , "Warning: The package list for 'hackage' is 15 days old."
        , ""
        , "<interactive>:3:17: error: [GHC-76037]"
        , "    Variable not in scope: prop_trivial"
        ]
      summary = QcTool.summariseStderr raw
  in pure
      ( "prop_trivial" `T.isInfixOf` summary
        && "Variable not in scope" `T.isInfixOf` summary
        && not ("Resolving dependencies" `T.isInfixOf` summary)
        && not ("Build profile"          `T.isInfixOf` summary)
      )

-- | A pathological stderr (e.g. a dep-resolve megaflood) must
-- not blow the JSON-RPC envelope. summariseStderr caps at 1600
-- chars + appends a '…(truncated)' marker.
testQcSummariseStderrCaps :: IO Bool
testQcSummariseStderrCaps =
  let noisyLine = "<interactive>:1:1: error: [GHC-76037] not in scope — "
                  <> T.replicate 50 "blah blah "
      raw     = T.unlines (replicate 60 noisyLine)
      summary = QcTool.summariseStderr raw
  in pure
      ( T.length summary <= 1700  -- 1600 + "…(truncated)" slack
        && "…(truncated)" `T.isSuffixOf` summary
      )

--------------------------------------------------------------------------------
-- Issue #132 — classifyStderrKind + extractNotInScopeSymbol
--------------------------------------------------------------------------------

-- | #132: "Variable not in scope" in stderr → NotInScope kind.
testClassifyStderrNotInScope :: IO Bool
testClassifyStderrNotInScope =
  let hint = Just "Variable not in scope: sort :: [Int] -> [Int]"
      kind = QcTool.classifyStderrKind hint
  in pure (kind == Env.NotInScope)

-- | #132: generic stderr → SubprocessError kind (unchanged from before).
testClassifyStderrGeneric :: IO Bool
testClassifyStderrGeneric =
  let hint = Just "cabal repl exited with code 1"
      kind = QcTool.classifyStderrKind hint
  in pure (kind == Env.SubprocessError)

-- | #186: GHC ": error:" pattern in stderr → CompileError kind.
testClassifyStderrCompileError :: IO Bool
testClassifyStderrCompileError =
  let hint = Just "src/WithError.hs:3:10: error: No instance for IsString Int"
      kind = QcTool.classifyStderrKind hint
  in pure (kind == Env.CompileError)

-- | #186: GHC "error: [GHC-N]" pattern in stderr → CompileError kind.
testClassifyStderrCompileErrorCode :: IO Bool
testClassifyStderrCompileErrorCode =
  let hint = Just "src/Foo.hs:5:3: error: [GHC-39999] …"
      kind = QcTool.classifyStderrKind hint
  in pure (kind == Env.CompileError)

-- | #186: isCompileErrorStderr positive case.
testIsCompileErrorStderrTrue :: IO Bool
testIsCompileErrorStderrTrue =
  pure $ QcTool.isCompileErrorStderr "src/Foo.hs:5:3: error: type mismatch"

-- | #186: isCompileErrorStderr negative case — generic message without GHC patterns.
testIsCompileErrorStderrFalse :: IO Bool
testIsCompileErrorStderrFalse =
  pure $ not (QcTool.isCompileErrorStderr "cabal repl exited unexpectedly")

-- | #132: extract bare name from "Variable not in scope: sort".
testExtractNisBareName :: IO Bool
testExtractNisBareName =
  pure (QcTool.extractNotInScopeSymbol "Variable not in scope: sort" == Just "sort")

-- | #132: extract name before the type annotation (":: …").
testExtractNisTypeSig :: IO Bool
testExtractNisTypeSig =
  pure (QcTool.extractNotInScopeSymbol
          "Variable not in scope: sort :: [Int] -> [Int]" == Just "sort")

-- | #132: unrelated text yields Nothing.
testExtractNisAbsent :: IO Bool
testExtractNisAbsent =
  pure (isNothing (QcTool.extractNotInScopeSymbol "some other error"))

-- | #132: summariseStderr must strip -Wmissing-home-modules warning
-- lines (and their continuation "Modules listed as … but not compiled:" line).
testQcSummariseStripsWmhm :: IO Bool
testQcSummariseStripsWmhm =
  let noise = T.unlines
        [ "<no location info>: warning: [-Wmissing-home-modules]"
        , "    Modules listed as 'other-modules' but not compiled: HaskellFlows.Tool.Batch HaskellFlows.Tool.Eval"
        , "<interactive>:1:1: error:"
        , "    Variable not in scope: sort"
        ]
      result = QcTool.summariseStderr noise
  in pure $ not ("missing-home-modules" `T.isInfixOf` result)
         && not ("Modules listed as" `T.isInfixOf` result)
         && "Variable not in scope: sort" `T.isInfixOf` result
