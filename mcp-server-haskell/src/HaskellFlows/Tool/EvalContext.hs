-- | Interactive-context extras shared by the GHC-API introspection
-- tools (moved out of the deleted subprocess-eval handler so the
-- helpers survive with their consumers).
module HaskellFlows.Tool.EvalContext
  ( evalContextExtras
  , selectMissingExtras
  , augmentEvalContext
  ) where

import Data.Set (Set)
import qualified Data.Set as Set

import GHC
  ( Ghc
  , InteractiveImport (IIDecl)
  , getContext
  , ideclName
  , mkModuleName
  , moduleNameString
  , setContext
  , simpleImportDecl
  , unLoc
  )

-- | Baseline modules appended to the interactive context.
evalContextExtras :: [String]
evalContextExtras =
  [ "Prelude"
  , "System.IO"
  , "Data.List"
  , "Control.Monad"
  , "Control.Concurrent"
  ]

-- | Issue #86: pure helper for the dedup arithmetic. Given the
-- module names already present in the interactive context and the
-- baseline 'evalContextExtras', returns the subset that is missing
-- and must therefore be appended.
selectMissingExtras
  :: Set String -- ^ existing context module names
  -> [String]   -- ^ candidate extras (typically 'evalContextExtras')
  -> [String]
selectMissingExtras existing = filter (`Set.notMember` existing)

-- | Append the missing 'evalContextExtras' to the current
-- interactive context (idempotent).
augmentEvalContext :: Ghc ()
augmentEvalContext = do
  existing <- getContext
  let existingNames = Set.fromList
        [ moduleNameString (unLoc (ideclName d)) | IIDecl d <- existing ]
      missing = selectMissingExtras existingNames evalContextExtras
      newImports =
        [ IIDecl (simpleImportDecl (mkModuleName m)) | m <- missing ]
  setContext (existing <> newImports)
