-- | Unit tests for Load tool helpers (#278): non-library stanza detection,
-- GHC-32850 suppression, cross-stanza dep matching, and GHC-76037 import fix.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.LoadUnit
  ( testArbitraryPathToModule
  , testArbitraryModuleRender
  , testDepsCrossStanzaMatch
  , testSuggestedImportGhc76037
  , testLoadNonLibStanzaPath
  , testLoadLibStanzaPath
  , testLoadFailureMessageGate
  , testLoad32850Suppressed
  , testLoad32850RetainedWhenMissing
  , testLoadNon32850Retained
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Set as Set
import qualified Data.Text as T

import HaskellFlows.Tool.Deps (importsMatchingPackage)
import qualified HaskellFlows.Tool.Load as LoadTool
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import HaskellFlows.Tool.Arbitrary
  ( pathToModule
  , renderArbitraryModule
  )

-- | #261: pathToModule derives the module name from a target path,
-- dropping a leading conventional source dir.
testArbitraryPathToModule :: IO Bool
testArbitraryPathToModule = pure $
  pathToModule "src/Expr/Syntax/Arbitrary.hs" == "Expr.Syntax.Arbitrary"
    && pathToModule "test/Foo.hs" == "Foo"

-- | #261: renderArbitraryModule emits a -Wno-orphans module importing
-- QuickCheck + the type's defining module.
testArbitraryModuleRender :: IO Bool
testArbitraryModuleRender =
  let out = renderArbitraryModule "Expr.Syntax.Arbitrary" (Just "Expr.Syntax")
              "instance Arbitrary Expr where arbitrary = pure undefined"
  in pure $ T.isInfixOf "{-# OPTIONS_GHC -Wno-orphans #-}" out
         && T.isInfixOf "module Expr.Syntax.Arbitrary where" out
         && T.isInfixOf "import Test.QuickCheck" out
         && T.isInfixOf "import Expr.Syntax" out

-- | #260: importsMatchingPackage finds sibling-stanza imports of a
-- module the added package owns (curated map), ignoring unrelated ones.
testDepsCrossStanzaMatch :: IO Bool
testDepsCrossStanzaMatch =
  let body = T.unlines
        [ "module Spec where"
        , "import Data.Map.Strict (Map)"
        , "import Data.Maybe (fromMaybe)"
        , "import Test.QuickCheck"
        ]
  in pure $ importsMatchingPackage "containers" body == ["import Data.Map.Strict (Map)"]
         && null (importsMatchingPackage "aeson" body)

-- | #259: a GHC-76037 (qualified-name not exported) error matching the
-- curated submodule table surfaces the corrected import.
testSuggestedImportGhc76037 :: IO Bool
testSuggestedImportGhc76037 =
  let e = GhcError "Pretty.hs" 7 14 SevError (Just "GHC-76037")
            "Not in scope: type constructor or class 'P.Parser'\n\
            \The module 'Text.Parsec' does not export 'Parser'."
  in pure $ case LoadTool.suggestedImportsFor [e] of
       (A.Object o : _) ->
         AKM.lookup "suggested_import" o
           == Just (A.String "import Text.Parsec.String (Parser)")
       _ -> False

-- | #278: a test-suite path is recognised as a non-library stanza so the
-- opaque "GHC reported failure" becomes an actionable ghc_gate hint.
testLoadNonLibStanzaPath :: IO Bool
testLoadNonLibStanzaPath = pure $
     LoadTool.isNonLibraryStanzaPath "test/Spec.hs"
  && LoadTool.isNonLibraryStanzaPath "/abs/proj/test/Foo/Bar.hs"
  && LoadTool.isNonLibraryStanzaPath "app/Main.hs"
  && LoadTool.isNonLibraryStanzaPath "bench/Bench.hs"

-- | #278: a library path under src/ is NOT flagged (the load should proceed
-- normally and any failure has a different cause).
testLoadLibStanzaPath :: IO Bool
testLoadLibStanzaPath = pure $
     not (LoadTool.isNonLibraryStanzaPath "src/Expr/Eval.hs")
  && not (LoadTool.isNonLibraryStanzaPath "/abs/proj/src/Foo.hs")

-- | #278: the failure message for a test-stanza module names ghc_gate, instead
-- of the old opaque "Compilation produced no errors but GHC reported failure."
testLoadFailureMessageGate :: IO Bool
testLoadFailureMessageGate =
  let msg = LoadTool.loadFailureMessage (Just "test/Spec.hs")
  in pure (T.isInfixOf "ghc_gate" msg && not (T.isInfixOf "no errors but GHC" msg))

-- | #258: a GHC-32850 warning whose modules are all registered in the
-- cabal (exposed-modules) is suppressed as the known false positive.
testLoad32850Suppressed :: IO Bool
testLoad32850Suppressed =
  let w = GhcError "X.hs" 0 0 SevWarning (Just "GHC-32850")
            "These modules are needed for compilation but not listed\n\
            \in your .cabal file's other-modules for 'pkg':\n\
            \    Expr.Eval Expr.Pretty"
      cabalMods = Set.fromList ["Expr.Eval", "Expr.Pretty", "Expr.Syntax"]
  in pure $ null (LoadTool.dropCoveredModuleWarnings cabalMods [w])

-- | #258: a GHC-32850 warning naming an unregistered module is retained.
testLoad32850RetainedWhenMissing :: IO Bool
testLoad32850RetainedWhenMissing =
  let w = GhcError "X.hs" 0 0 SevWarning (Just "GHC-32850")
            "These modules are needed for compilation but not listed\n\
            \in your .cabal file's other-modules for 'pkg':\n\
            \    Expr.Missing"
      cabalMods = Set.fromList ["Expr.Eval"]
  in pure $ length (LoadTool.dropCoveredModuleWarnings cabalMods [w]) == 1

-- | #258 (CI regression, 2026-05-30): a warning whose code is NOT
-- GHC-32850 must NEVER be dropped — even when its message text happens
-- to name a registered module. This is the invariant FlowDogfoodReplay
-- leaned on: a genuine -Wtype-defaults hint has to survive so 'ghc_load'
-- still classifies the module as LWFixable and routes nextStep to
-- ghc_fix_warning. Guards the filter against widening past GHC-32850.
testLoadNon32850Retained :: IO Bool
testLoadNon32850Retained =
  let w = GhcError "X.hs" 0 0 SevWarning (Just "GHC-18042")
            "Defaulting the type variable to type 'Integer'\n\
            \    Expr.Pretty"
      cabalMods = Set.fromList ["Expr.Eval", "Expr.Pretty", "Expr.Syntax"]
  in pure $ length (LoadTool.dropCoveredModuleWarnings cabalMods [w]) == 1
