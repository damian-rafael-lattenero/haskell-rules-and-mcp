-- | Unit tests for the ghc_deps tool: discriminated-schema parse
-- validation (#92B). All pure — no GhcSession, no cabal invocation.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Deps
  ( testDepsListBareParses
  , testDepsAddMissingPackage
  , testDepsRemoveMissingPackage
  , testDepsAddCompleteParses
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM

import HaskellFlows.Mcp.Protocol (ToolDescriptor (..))
import qualified HaskellFlows.Tool.Deps as DepsTool

-- | 'list' has no extra required fields — bare {action: list} parses.
testDepsListBareParses :: IO Bool

testDepsListBareParses = do
  let raw = "{\"action\":\"list\"}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result DepsTool.DepsArgs of
      A.Success _ -> True
      _           -> False
    _      -> False

-- | 'add' without 'package' must FAIL at parse time — the
-- bug-class anchor that #92 closes.

testDepsAddMissingPackage :: IO Bool

testDepsAddMissingPackage = do
  let raw = "{\"action\":\"add\"}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result DepsTool.DepsArgs of
      A.Error _ -> True
      _         -> False
    _      -> False

-- | 'remove' without 'package' must FAIL at parse time.

testDepsRemoveMissingPackage :: IO Bool

testDepsRemoveMissingPackage = do
  let raw = "{\"action\":\"remove\"}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result DepsTool.DepsArgs of
      A.Error _ -> True
      _         -> False
    _      -> False

-- | 'add' with 'package' parses cleanly. 'version' is optional
-- (constraint-as-cabal-text), 'stanza' is optional (defaults to
-- the first build-depends block). Anchor for the positive path.

testDepsAddCompleteParses :: IO Bool

testDepsAddCompleteParses = do
  let raw = "{\"action\":\"add\",\"package\":\"text\",\"version\":\">= 2.0\"}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result DepsTool.DepsArgs of
      A.Success _ -> True
      _           -> False
    _      -> False

-- | The published 'tdInputSchema' for ghc_deps must publish the
-- discriminant as an 'enum' over every action — list / add /
-- remove / explain.

