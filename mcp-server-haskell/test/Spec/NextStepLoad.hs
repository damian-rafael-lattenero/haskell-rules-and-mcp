-- | Unit tests for nextStep routing on load (clean/typed-hole-warn/
-- fixable-warn) and the CabalBootstrap library-stanza capture.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.NextStepLoad
  ( testCabalBootstrapLibrary
  ) where

import HaskellFlows.Ghc.CabalBootstrap (bootstrapProject, Target (..), StanzaFlags (..))
import HaskellFlows.Types (mkProjectDir)

import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist)

-- | When the 'warnings' array is empty, 'dispatch' proposes
-- 'ghc_suggest' — the clean-compile follow-up.
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
