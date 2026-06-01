-- | Unit tests for nextStep routing on load (clean/typed-hole-warn/
-- fixable-warn) and GhcSession bootstrap invariants.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepLoad
  ( testNextStepCleanLoad
  , testNextStepTypedHoleWarn
  , testNextStepFixableWarn
  , testCabalBootstrapLibrary
  , testGhcSessionPersists
  , testGhcSessionBoots
  ) where

import qualified Data.Aeson as A
import qualified Data.Text as T

import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession, captureStdout, withGhcSession)
import HaskellFlows.Ghc.CabalBootstrap (bootstrapProject, Target (..), StanzaFlags (..))
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Types (mkProjectDir)

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)
import HaskellFlows.Mcp.ToolName (ToolName (..))

import GHC
  ( InteractiveImport (IIDecl)
  , TcRnExprMode (TM_Inst)
  , exprType
  , mkModuleName
  , setContext
  , simpleImportDecl
  )
import GHC.Utils.Outputable (showPprUnsafe)

-- | When the 'warnings' array is empty, 'dispatch' proposes
-- 'ghc_suggest' — the clean-compile follow-up.
testNextStepCleanLoad :: IO Bool
testNextStepCleanLoad =
  let payload = A.object
        [ "success"  A..= True
        , "errors"   A..= ([] :: [Text])
        , "warnings" A..= ([] :: [Text])
        ]
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcSuggest
       Nothing -> False

-- | A typed-hole warning routes to 'ghc_hole' (which knows how
-- to surface expected types + in-scope fits).
testNextStepTypedHoleWarn :: IO Bool
testNextStepTypedHoleWarn =
  let payload = A.object
        [ "success"  A..= True
        , "errors"   A..= ([] :: [Text])
        , "warnings" A..=
            [ A.object
                [ "message" A..=
                    ("Found hole: _ :: Int\n  Valid hole fits include …"
                     :: Text)
                , "severity" A..= ("warning" :: Text)
                ]
            ]
        ]
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcHole
       Nothing -> False

-- | A non-hole warning (unused-imports, type-defaults, …) routes
-- to 'ghc_fix_warning' — the auto-patch tool.
testNextStepFixableWarn :: IO Bool
testNextStepFixableWarn =
  let payload = A.object
        [ "success"  A..= True
        , "errors"   A..= ([] :: [Text])
        , "warnings" A..=
            [ A.object
                [ "message" A..=
                    ("Defaulting the type variable 'a0' to type 'Integer'"
                     :: Text)
                , "severity" A..= ("warning" :: Text)
                ]
            ]
        ]
  in pure $ case suggestNext GhcLoad True payload of
       Just ns -> nsTool ns == GhcFixWarning
       Nothing -> False

-- | Wave-1 gate: drive cabal via the shim against a real project
-- and verify we get back a non-empty flag set that includes the
-- expected package-db paths. Uses '/tmp/bench-project' (created
-- during the Phase-2 benchmark work) as a minimal test fixture.
-- If that dir isn't there — e.g. on CI before the benchmark has
-- been run — we skip gracefully by returning True.
testCabalBootstrapLibrary :: IO Bool
testCabalBootstrapLibrary = case mkProjectDir "/tmp/bench-project" of
  Left _   -> pure True   -- malformed path shouldn't happen, skip
  Right pd -> do
    exists <- doesFileExist "/tmp/bench-project/bench-project.cabal"
    if not exists
      then pure True   -- fixture missing, skip (don't fail CI)
      else do
        stanzas <- bootstrapProject pd
        case Map.lookup TargetLibrary stanzas of
          Nothing ->
            pure False   -- bootstrap did not capture the library
          Just flags ->
            pure
              ( "--interactive" `elem` sfArgs flags
              && any ("-package-db" `isPrefix`) (sfArgs flags)
              && any ("-this-unit-id" `isPrefix`) (sfArgs flags)
              )
  where
    isPrefix p s = take (length p) s == p

-- | Phase-2 derisk: verify the interactive context set in one
-- 'withGhcSession' call survives into the next call. This is the
-- invariant the 22 read-only tool migrations rely on — each tool
-- call is its own 'withGhcSession', so if 'setSession' + 'getSession'
-- doesn't round-trip the HscEnv faithfully, we'd have to redo the
-- context every single call (which defeats the "1s cold-start" benefit).
--
-- If this ever starts failing, the fix is to host GHC in a
-- dedicated thread (HLS/ghcid pattern) rather than invoking 'runGhc'
-- per call. Better to discover that here than 6 tools into Phase 2.
testGhcSessionPersists :: IO Bool
testGhcSessionPersists = case mkProjectDir "/tmp" of
  Left _   -> pure False
  Right pd -> do
    sess <- startGhcSession pd
    -- Call 1: seed the interactive context with Prelude.
    withGhcSession sess $
      setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
    -- Call 2: depend on call 1's side effect. If Prelude is gone,
    -- 'exprType "map"' throws a SourceError ("not in scope") and
    -- the test fails by exception.
    result <- withGhcSession sess $ do
      ty <- exprType TM_Inst "map"
      pure (showPprUnsafe ty)
    killGhcSession sess
    pure (not (null result) && "->" `T.isInfixOf` T.pack result)

-- | Phase-1 gate for the GHC-API-in-process migration: can we boot a
-- 'GhcSession', round-trip an 'exprType' through 'withGhcSession', and
-- tear it down cleanly? The 'map' type string is checked for @->@ to
-- confirm the pretty-print path works, not just the compile path.
--
-- No modules are loaded here — Phase 2 will layer that in when real
-- tool handlers (type, info) migrate.
testGhcSessionBoots :: IO Bool
testGhcSessionBoots = case mkProjectDir "/tmp" of
  Left _   -> pure False
  Right pd -> do
    sess   <- startGhcSession pd
    result <- withGhcSession sess $ do
      setContext [IIDecl (simpleImportDecl (mkModuleName "Prelude"))]
      ty <- exprType TM_Inst "map"
      pure (showPprUnsafe ty)
    killGhcSession sess
    pure (not (null result) && "->" `T.isInfixOf` T.pack result)
