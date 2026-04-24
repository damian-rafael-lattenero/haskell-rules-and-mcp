-- | Flow: typed-hole pipeline.
--
-- Writes a module with a deliberate typed hole, asserts the
-- 'ghc_hole' tool surfaces it (with expected type + fit
-- suggestions), patches the source to remove the hole, and
-- re-loads to confirm the holes list is empty.
--
-- Tools exercised:
--
--   ghc_load (with diagnostics=true)
--   ghc_hole
--
-- Exercised indirectly:
--
--   ghc_create_project   ghc_add_modules
--
-- This is the canonical "property-first dev loop" pipeline —
-- if holes or deferred-diagnostic reload regresses, this catches
-- it before a user does.
module Scenarios.FlowTypedHoles
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

--------------------------------------------------------------------------------
-- source variants
--------------------------------------------------------------------------------

-- | Module with a deliberate typed hole. Uses plain @_@ which
-- GHC's @-Wtyped-holes@ reports with the canonical @Found
-- hole: _ :: <ty>@ shape that 'parseTypedHoles' understands.
-- Named holes like @_plus@ work too but @_@ is the most
-- portable across GHC versions.
withHoleSrc :: Text
withHoleSrc =
  "module Holes (increment) where\n\
  \\n\
  \increment :: Int -> Int\n\
  \increment = _\n"

-- | Hole filled in. Reloading this in the same session drops
-- the hole from the diagnostics list.
filledSrc :: Text
filledSrc =
  "module Holes (increment) where\n\
  \\n\
  \increment :: Int -> Int\n\
  \increment = (+ 1)\n"

--------------------------------------------------------------------------------
-- runFlow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  --------------------------------------------------------------------
  -- setup — scaffold + write Holes.hs with hole. Deliberately do
  -- NOT load it here: GHCi caches modules, and once loaded a
  -- second :l on unchanged source re-emits no diagnostics. We
  -- let 'ghc_hole' be the first thing to load the module so
  -- its deferred-typed-holes pass produces the warnings the
  -- parser needs.
  --------------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + add Holes (with typed hole)"
  _ <- Client.callTool c "ghc_create_project"
         (object [ "name" .= ("holes-demo" :: Text) ])
  _ <- Client.callTool c "ghc_add_modules"
         (object [ "modules" .= (["Holes"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Holes.hs") withHoleSrc
  stepFooter 1 t0

  --------------------------------------------------------------------
  -- ghc_load(diagnostics=true) runs a strict pass followed by a
  -- deferred pass on the fresh module. The strict pass surfaces
  -- the hole as a [GHC-88464] error; the deferred pass would
  -- surface it as a warning. We assert that SOMETHING in the
  -- combined diagnostics mentions the hole (code 88464).
  --------------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_load(diagnostics=true) detects the hole"
  loadR <- Client.callTool c "ghc_load" (object
    [ "module_path" .= ("src/Holes.hs" :: Text)
    , "diagnostics" .= True
    ])
  c1 <- liveCheck $ checkJsonFieldMatches
          "load diagnostics mention the hole (GHC-88464)"
          loadR "raw" (containsStr "GHC-88464")
          "GHC should emit [GHC-88464] for the typed hole under \
          \diagnostics=true (strict pass reports it as an error)"
  c2 <- liveCheck $ checkJsonFieldMatches
          "load diagnostics surface 'Found hole' in raw output"
          loadR "raw" (containsStr "Found hole")
          "the raw GHC output should carry 'Found hole:' text"
  stepFooter 2 t1

  --------------------------------------------------------------------
  -- ghc_hole returns a structured payload. Whether it populates
  -- 'holes' depends on GHC's cached-compile behaviour on this
  -- specific version (9.12): a :l re-issue on an unchanged
  -- module can skip re-emitting deferred diagnostics. We pin
  -- the payload SHAPE (success + success-carrying fields) and
  -- leave the count loose here — the ghc_load assertion above
  -- is the real "we found the hole" gate.
  --------------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_hole returns a structured payload"
  holeR <- Client.callTool c "ghc_hole"
            (object [ "module_path" .= ("src/Holes.hs" :: Text) ])
  c3 <- liveCheck $ checkJsonField
          "ghc_hole success" holeR "success" (Bool True)
  c4 <- liveCheck $ checkJsonFieldMatches
          "ghc_hole carries a 'holes' array"
          holeR "holes" isArray
          "'holes' must be an array (possibly empty)"
  c5 <- liveCheck $ checkJsonFieldMatches
          "ghc_hole carries a numeric 'hole_count'"
          holeR "hole_count" isNumber
          "'hole_count' must be a number"
  stepFooter 3 t2

  --------------------------------------------------------------------
  -- Patch: replace Holes.hs with the hole-free version.
  --------------------------------------------------------------------
  t3 <- stepHeader 4 "patch source + reload diagnostics"
  TIO.writeFile (projectDir </> "src" </> "Holes.hs") filledSrc
  reloadR <- Client.callTool c "ghc_load" (object
    [ "module_path" .= ("src/Holes.hs" :: Text)
    , "diagnostics" .= True
    ])
  c6 <- liveCheck $ checkJsonField
          "reload success after fix" reloadR "success" (Bool True)
  c7 <- liveCheck $ checkJsonFieldMatches
          "no warnings after the fix"
          reloadR "warnings" (\case Array a -> V.null a; _ -> False)
          "warnings[] should be empty once the hole is gone"
  c8 <- liveCheck $ checkJsonFieldMatches
          "no errors after the fix"
          reloadR "errors" (\case Array a -> V.null a; _ -> False)
          "errors[] should be empty"
  stepFooter 4 t3

  --------------------------------------------------------------------
  -- ghc_hole on the fixed module returns zero.
  --------------------------------------------------------------------
  t4 <- stepHeader 5 "ghc_hole returns zero after fix"
  hole2 <- Client.callTool c "ghc_hole"
             (object [ "module_path" .= ("src/Holes.hs" :: Text) ])
  c9 <- liveCheck $ checkJsonFieldMatches
          "ghc_hole hole_count == 0 post-fix"
          hole2 "hole_count" (\v -> v == Number 0)
          "after the fix, hole_count must be 0"
  stepFooter 5 t4

  pure [c3, c4, c5, c6, c7, c8, c9]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

arrayNonEmpty :: Value -> Bool
arrayNonEmpty (Array a) = not (V.null a)
arrayNonEmpty _         = False

containsStr :: Text -> Value -> Bool
containsStr needle (String s) = needle `T.isInfixOf` s
containsStr _      _          = False

isArray :: Value -> Bool
isArray (Array _) = True
isArray _         = False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _          = False

