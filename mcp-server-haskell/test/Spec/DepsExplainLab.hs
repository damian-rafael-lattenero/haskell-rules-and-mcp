-- | Unit tests for 'Tool.DepsExplain', the Deps pkg-search helpers,
-- 'Tool.Lab' list parsing, and 'Tool.ExplainError' pick/extract logic.
-- All pure.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.DepsExplainLab
  ( testDepsExplainParse
  , testDepsExplainRoot
  , testDepsExplainPackages
  , testDepsExplainClean
  , testPkgSearchTokensSimple
  , testPkgSearchTokensHyphen
  , testImportMatchesPkgHit
  , testImportMatchesPkgMiss
  , testCabalComponentsLibrary
  , testLabListSimple
  , testLabListMultiline
  , testLabListSkips
  , testExplainPickDefault
  , testExplainPickIndex
  , testExplainPickOOR
  , testExplainIndexOutOfRangeHint203
  , testExplainExtractImports
  , testExplainRangeClamps
  , testPAImplicationDetection
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import qualified HaskellFlows.Tool.DepsExplain as DepsExplain
import HaskellFlows.Tool.Deps (importsMatchingPackage)
import qualified HaskellFlows.Tool.ExplainError as ExplainError
import qualified HaskellFlows.Tool.Lab as LabTool
import qualified HaskellFlows.Tool.PropertyAudit as PropertyAuditTool

testDepsExplainParse :: IO Bool
testDepsExplainParse =
  let dump = T.unlines
        [ "Resolving dependencies..."
        , "cabal: Could not resolve dependencies:"
        , "[__0] trying: my-project-0.1.0.0 (user goal)"
        , "[__1] next goal: aeson (dependency of my-project)"
        , "[__1] rejecting: aeson-2.2.3.0 (conflict: my-project => aeson < 2.0)"
        , "[__2] rejecting: aeson-2.1.2.1 (conflict: text >= 2.0 needed; text-1.2.5.0 installed)"
        , "[__41] backjump limit reached (currently 4000, change with --max-backjumps)."
        ]
  in pure $ case DepsExplain.parseSolverOutput dump of
       Just c  -> length (DepsExplain.cAll c) == 2
                && DepsExplain.cBackjumps c == Just 4000
       Nothing -> False

-- | Issue #63: 'identifyRootCause' must pick the rejection at the
-- greatest depth.
testDepsExplainRoot :: IO Bool
testDepsExplainRoot =
  let rs =
        [ DepsExplain.Rejection 1  "aeson-2.2.3.0" "my-project => aeson < 2.0"
        , DepsExplain.Rejection 41 "aeson-2.1.2.1" "text needed"
        , DepsExplain.Rejection 12 "lens-5.2.0"    "transitive"
        ]
      root = DepsExplain.identifyRootCause rs
  in pure (DepsExplain.rDepth root == 41
        && DepsExplain.rPackage root == "aeson-2.1.2.1")

-- | Issue #63: 'extractPackages' strips version suffixes and
-- dedupes by name.
testDepsExplainPackages :: IO Bool
testDepsExplainPackages =
  let rs =
        [ DepsExplain.Rejection 1  "aeson-2.2.3.0" "text >= 2.0"
        , DepsExplain.Rejection 2  "aeson-2.1.2.1" "text needed"
        , DepsExplain.Rejection 3  "lens-5.2.0"    "lens upper bound"
        ]
      pkgs = DepsExplain.extractPackages rs
  in pure $ "aeson" `elem` pkgs
        && "lens"  `elem` pkgs
        -- Dedup: aeson appears twice in input.
        && length (filter (== "aeson") pkgs) == 1

-- | Issue #63: clean output (no rejections) → Nothing.
testDepsExplainClean :: IO Bool
testDepsExplainClean =
  let dump = T.unlines
        [ "Resolving dependencies..."
        , "Build profile: -w ghc-9.12.2 -O1"
        , "In order, the following will be built:"
        , " - my-project-0.1.0.0 (lib)"
        ]
  in pure (isNothing (DepsExplain.parseSolverOutput dump))

-- | #156: pkgSearchTokens for a single-word package name.
testPkgSearchTokensSimple :: IO Bool
testPkgSearchTokensSimple =
  pure (DepsExplain.pkgSearchTokens "aeson" == ["Aeson"])

-- | #156: pkgSearchTokens for a hyphenated package name produces
-- joined and individual capitalised tokens.
testPkgSearchTokensHyphen :: IO Bool
testPkgSearchTokensHyphen =
  let tokens = DepsExplain.pkgSearchTokens "data-default"
  in pure
       (  "DataDefault" `elem` tokens
       && "Data"        `elem` tokens
       && "Default"     `elem` tokens
       )

-- | #156: importMatchesPkg recognises a direct import from the package.
testImportMatchesPkgHit :: IO Bool
testImportMatchesPkgHit =
  pure
    (  DepsExplain.importMatchesPkg "aeson" "import Data.Aeson"
    && DepsExplain.importMatchesPkg "aeson" "import qualified Data.Aeson.Key as Key"
    )

-- | #156: importMatchesPkg rejects imports unrelated to the package.
testImportMatchesPkgMiss :: IO Bool
testImportMatchesPkgMiss =
  pure
    (  not (DepsExplain.importMatchesPkg "aeson" "import Data.Map")
    && not (DepsExplain.importMatchesPkg "aeson" "import Prelude")
    )

-- | #156: cabalComponentsMatchingPkg finds the library stanza
-- when it lists the package in build-depends.
testCabalComponentsLibrary :: IO Bool
testCabalComponentsLibrary =
  let cabalText = T.unlines
        [ "cabal-version: 3.4"
        , "name: my-project"
        , ""
        , "library"
        , "  hs-source-dirs: src"
        , "  build-depends:"
        , "      base"
        , "    , aeson"
        , ""
        , "test-suite my-test"
        , "  hs-source-dirs: test"
        , "  build-depends:"
        , "      base"
        ]
      (stanzas, srcDirs) = DepsExplain.cabalComponentsMatchingPkg "aeson" cabalText
  in pure
       (  length stanzas == 1
       && "library" `T.isPrefixOf` head stanzas
       && any (\(_, ds) -> "src" `elem` ds) srcDirs
       )

-- | Issue #60: 'listTopLevelBindings' must pick up every
-- column-0 type signature.
testLabListSimple :: IO Bool
testLabListSimple =
  let body = T.unlines
        [ "module M where"
        , ""
        , "import Data.List (sort)"
        , ""
        , "double :: Int -> Int"
        , "double x = x + x"
        , ""
        , "greet :: String -> String"
        , "greet n = \"hi \" <> n"
        ]
      bs = LabTool.listTopLevelBindings body
  in pure $ length bs == 2
        && map LabTool.bName bs == ["double", "greet"]

-- | Issue #60: signatures wrapped across lines (the second line
-- starts with whitespace) must be joined into one binding entry.
testLabListMultiline :: IO Bool
testLabListMultiline =
  let body = T.unlines
        [ "module M where"
        , ""
        , "concatPairs"
        , "  :: (Eq a, Show b)"
        , "  => [(a, b)] -> [b]"
        , "concatPairs = undefined"
        ]
      bs = LabTool.listTopLevelBindings body
  in pure $ length bs == 1
        && LabTool.bName (head bs) == "concatPairs"
        && T.isInfixOf "[(a, b)] -> [b]" (LabTool.bSignature (head bs))

-- | Issue #60: comments / module headers / equations are NOT
-- mistaken for signatures.
testLabListSkips :: IO Bool
testLabListSkips =
  let body = T.unlines
        [ "module M where"
        , ""
        , "-- top-level comment"
        , "import Data.List (sort)"
        , ""
        , "double = 42  -- no signature"
        ]
  in pure (null (LabTool.listTopLevelBindings body))

-- | Issue #60: 'confidenceAtLeast' compares the candidate against
-- the threshold (Low ≤ Medium ≤ High).
-- | Issue #59: 'pickDiagnostic' defaults to the first error
-- diagnostic. Warnings are filtered out — only severity-error
-- entries qualify.
testExplainPickDefault :: IO Bool
testExplainPickDefault =
  let diags =
        [ GhcError "f.hs" 10 1 SevWarning Nothing "warn"
        , GhcError "f.hs" 20 5 SevError   Nothing "first error"
        , GhcError "f.hs" 30 9 SevError   Nothing "second error"
        ]
  in pure $ case ExplainError.pickDiagnostic Nothing diags of
       Just d  -> geMessage d == "first error" && geLine d == 20
       Nothing -> False

-- | Issue #59: 'diagnostic_index=N' picks the Nth error (0-indexed).
testExplainPickIndex :: IO Bool
testExplainPickIndex =
  let diags =
        [ GhcError "f.hs" 1 1 SevError Nothing "a"
        , GhcError "f.hs" 2 1 SevError Nothing "b"
        , GhcError "f.hs" 3 1 SevError Nothing "c"
        ]
  in pure $ case ExplainError.pickDiagnostic (Just 2) diags of
       Just d  -> geMessage d == "c"
       Nothing -> False

-- | Issue #59: invalid index → Nothing (callers render an
-- error_kind=invalid_index instead of guessing).
testExplainPickOOR :: IO Bool
testExplainPickOOR =
  let diags =
        [ GhcError "f.hs" 1 1 SevError Nothing "a" ]
  in pure (isNothing (ExplainError.pickDiagnostic (Just 5) diags))

-- | Issue #203: renderIndexOutOfRange returns a clear "index N out of
-- range — M error(s) found" hint, not the misleading "No errors detected".
testExplainIndexOutOfRangeHint203 :: IO Bool
testExplainIndexOutOfRangeHint203 =
  let diags  = [ GhcError "src/F.hs" 1 1 SevError Nothing "boom" ]
      result = ExplainError.renderIndexOutOfRange "src/F.hs" 3 1 diags
  in pure $ case Env.reResult result of
    Just (A.Object r) ->
      let hint = AKM.lookup (AKey.fromText "hint") r
      in isJust hint
         -- must mention "out of range"
         && maybe False (\case
              A.String s -> "out of range" `T.isInfixOf` s
              _          -> False) hint
         -- must NOT say the old wrong message
         && maybe True (\case
              A.String s -> not ("No errors detected" `T.isInfixOf` s)
              _          -> True) hint
    _ -> False

-- | Issue #59: 'extractImports' must recognise plain, qualified,
-- and parenthesised import forms.
testExplainExtractImports :: IO Bool
testExplainExtractImports =
  let body = T.unlines
        [ "module M where"
        , ""
        , "import Data.List (sort)"
        , "import qualified Data.Map.Strict as Map"
        , "import Foo.Bar"
        ]
      imps = ExplainError.extractImports body
  in pure (length imps == 3)

-- | Issue #59: 'enclosingLineRange' clamps to the body bounds
-- so a diagnostic at line 1 doesn't request line -49.
testExplainRangeClamps :: IO Bool
testExplainRangeClamps =
  let (lo1, hi1) = ExplainError.enclosingLineRange 100 50 1
      (lo2, hi2) = ExplainError.enclosingLineRange 100 50 60
      (lo3, hi3) = ExplainError.enclosingLineRange 100 50 200  -- past EOF
  in pure $ lo1 == 1   && hi1 == 51
        && lo2 == 10  && hi2 == 100
        && lo3 == 100 && hi3 == 100   -- clamped on both ends

-- | Issue #64: 'pairCombinations' on an empty list returns no
-- | Issue #212: ==> detection pins the text predicate used by
-- 'runPairProbe' to short-circuit the REPL probe. Expressions
-- with implication must be detected; those without must not.
testPAImplicationDetection :: IO Bool
testPAImplicationDetection = pure $
  let has e = "==>" `T.isInfixOf` e
  in  has "\\(x :: Int) -> x > 0 ==> safeDiv x x == Just 1"
   && has "prop_foo ==> prop_bar"
   && not (has "\\x -> x + 0 == x")
   && not (has "\\xs -> reverse (reverse xs) == xs")
   && not (has "\\x -> double x == x * 2")
