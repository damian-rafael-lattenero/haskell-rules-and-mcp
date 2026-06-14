-- | Unit tests for pure 'Tool.Deps' helpers: validatePackageName,
-- validateVersionConstraint, and extractErrorSummary (#48).
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.DepsUnit
  ( testPkgAccepts
  , testPkgRejectsSymbol
  , testPkgRejectsEmpty
  , testExtractErrorSummaryFindsPackage
  , testExtractErrorSummaryFallsBackOnNoMatch
  , testExtractErrorSummaryCaseInsensitive
  , testVerAccepts
  , testVerRejects
  , testExplainActionParses
  ) where

import Data.Aeson (Result (..), fromJSON, object, (.=))
import qualified Data.Text as T

import HaskellFlows.Tool.Deps
  ( Action (..)
  , DepsArgs (..)
  , extractErrorSummary
  , validatePackageName
  , validateVersionConstraint
  )

testPkgAccepts :: IO Bool
testPkgAccepts = pure $ case validatePackageName "haskell-flows-mcp" of
  Right _ -> True
  _       -> False

testPkgRejectsSymbol :: IO Bool
testPkgRejectsSymbol = pure $ case validatePackageName "foo; rm -rf /" of
  Left _ -> True
  _      -> False

testPkgRejectsEmpty :: IO Bool
testPkgRejectsEmpty = pure $ case validatePackageName "" of
  Left _ -> True
  _      -> False

-- | #48 — extractErrorSummary picks the cabal lines that mention
-- the package by name. Synthetic input: a typical "could not
-- resolve" verdict that mentions the package on its own line.
testExtractErrorSummaryFindsPackage :: IO Bool
testExtractErrorSummaryFindsPackage = do
  let stderr = T.unlines
        [ "Resolving dependencies..."
        , "cabal-3.14.2.0: Could not resolve dependencies:"
        , "[__0] trying: my-project-0.1.0.0 (user goal)"
        , "[__1] unknown package: this-package-does-not-exist (dependency of my-project-0.1.0.0)"
        , "[__1] fail (backjumping, conflict set: this-package-does-not-exist, my-project)"
        , "After searching the rest of the dependency tree exhaustively,"
        , "these were the goals I've had most trouble fulfilling: my-project, this-package-does-not-exist"
        ]
      summary = extractErrorSummary "this-package-does-not-exist" stderr
  pure ( "this-package-does-not-exist" `T.isInfixOf` summary
      && "unknown package"             `T.isInfixOf` T.toLower summary )

-- | #48 — when no line matches the package name or solver verdicts,
-- extractErrorSummary falls back to a truncated raw output instead
-- of emitting an empty summary that would lose information.
testExtractErrorSummaryFallsBackOnNoMatch :: IO Bool
testExtractErrorSummaryFallsBackOnNoMatch = do
  let stderr = T.replicate 200 "x"
      summary = extractErrorSummary "irrelevant-pkg" stderr
  pure (not (T.null summary) && T.length summary <= 800)

-- | #48 — extractErrorSummary is case-insensitive on the package
-- name, since cabal output often lowercases verdicts ("rejecting:
-- Aeson..." vs "rejecting: aeson..." between versions).
testExtractErrorSummaryCaseInsensitive :: IO Bool
testExtractErrorSummaryCaseInsensitive = do
  let stderr = T.unlines
        [ "[__1] rejecting: AESON-2.2.3.0 (constraint from user target requires <2.0)"
        , "[__1] fail"
        ]
      summary = extractErrorSummary "aeson" stderr
  pure ("AESON" `T.isInfixOf` summary)

testVerAccepts :: IO Bool
testVerAccepts = pure $ case validateVersionConstraint ">= 2.14 && < 2.16" of
  Right _ -> True
  _       -> False

testVerRejects :: IO Bool
testVerRejects = pure $ case validateVersionConstraint "; rm -rf" of
  Left _ -> True
  _      -> False

-- | #292 — 'explain' must parse through the Action ADT (not via the
-- stringly-typed pre-check that was removed). Verifies that
-- 'FromJSON DepsArgs' accepts {action:"explain"} and produces
-- 'ActExplain', closing the dispatch path cleanly.
testExplainActionParses :: IO Bool
testExplainActionParses =
  pure $ case fromJSON (object ["action" .= ("explain" :: T.Text)]) of
    Success DepsArgs { daAction = ActExplain } -> True
    _                                          -> False
