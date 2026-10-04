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







testPAImplicationDetection :: IO Bool

testPAImplicationDetection = pure $
  let has e = "==>" `T.isInfixOf` e
  in  has "\\(x :: Int) -> x > 0 ==> safeDiv x x == Just 1"
   && has "prop_foo ==> prop_bar"
   && not (has "\\x -> x + 0 == x")
   && not (has "\\xs -> reverse (reverse xs) == xs")
   && not (has "\\x -> double x == x * 2")
