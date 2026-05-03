{-# LANGUAGE ScopedTypeVariables #-}

-- | Minimal placeholder module to make the fixture cabal-buildable.
--
-- Importing 'Test.QuickCheck' here forces cabal to resolve + build
-- the QuickCheck dependency tree during the CI pre-warm step. That
-- populates ~/.cabal/store/ghc-X.Y.Z/QuickCheck-N.N.N + transitive
-- closure (random, splitmix, etc.). Subsequent scenarios that scaffold
-- their own project + add QuickCheck see store hits and skip the
-- compile.
module Placeholder
  ( placeholder
  ) where

import qualified Test.QuickCheck as QC

placeholder :: IO ()
placeholder = QC.quickCheck (\(x :: Int) -> x == x)
