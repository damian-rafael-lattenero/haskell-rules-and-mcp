-- | Unit tests for the @ghc_toolchain@ status/warmup tool — the Phase B
-- envelope-shape oracles and the back-compat field checks.
--
-- Extracted verbatim from the Spec.hs monolith (#271) via the
-- function-export shape: the driver keeps their (multi-line)
-- registrations and imports the moved functions. The shared
-- 'runToolEnvelope' helper lives in 'Spec.Helpers'.
module Spec.Toolchain
  ( testToolchainStatusEnvelopeShape
  , testToolchainStatusBackcompatFields
  , testToolchainStatusFailedIncludesInventory
  , testToolchainWarmupEnvelopeShape
  , testToolchainWarmupPartialWarnings
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.ToolchainStatus as ToolchainStatusTool
import qualified HaskellFlows.Tool.ToolchainWarmup as ToolchainWarmupTool
import HaskellFlows.Config (defaultLimits)

import Spec.Helpers (runToolEnvelope)

-- | Phase B oracle: 'ghc_toolchain_status' emits an envelope-shaped
-- response whose status is one of @ok | partial | failed@. The exact
-- status depends on the host's installed binaries — on a dev box
-- with cabal/ghc/hlint present and (typically) fourmolu/hoogle
-- absent, we'd see @partial@. CI may have different binaries; the
-- test stays host-independent by accepting any of the three valid
-- statuses.
testToolchainStatusEnvelopeShape :: IO Bool
testToolchainStatusEnvelopeShape = do
  decoded <- runToolEnvelope (ToolchainStatusTool.handle defaultLimits) (A.object [])
  pure $ case decoded of
    Right env ->
      Env.reStatus env
        `elem` [Env.StatusOk, Env.StatusPartial, Env.StatusFailed]
    Left _ -> False

-- | The migrated tool keeps the @tools@ + @blocking_gates@ + @summary@
-- fields inside @result@ so any consumer keying on them via the
-- legacy shape continues to function during the dual-shape window.
testToolchainStatusBackcompatFields :: IO Bool
testToolchainStatusBackcompatFields = do
  decoded <- runToolEnvelope (ToolchainStatusTool.handle defaultLimits) (A.object [])
  pure $ case decoded of
    Right env -> case Env.reResult env of
      Just (A.Object payload) ->
        AKM.member (AKey.fromText "tools")          payload
          && AKM.member (AKey.fromText "blocking_gates") payload
          && AKM.member (AKey.fromText "summary")        payload
      _ -> False
    Left _ -> False

-- | Pure regression guard: 'renderResult' must include @tools@,
-- @blocking_gates@, and @summary@ in the @result@ object even when
-- @status=failed@ (i.e. when a blocking gate binary is absent).
-- Without this guarantee the backcompat contract is only tested on dev
-- boxes where all gates are present; CI coverage jobs do not install
-- hlint so they hit the @status=failed@ branch.
testToolchainStatusFailedIncludesInventory :: IO Bool
testToolchainStatusFailedIncludesInventory =
  let entries =
        [ ToolchainStatusTool.Entry "cabal" "gate" True (Just "/usr/bin/cabal") (Just "3.12")
        , ToolchainStatusTool.Entry "ghc"   "gate" True (Just "/usr/bin/ghc")   (Just "9.12")
        , ToolchainStatusTool.Entry "hlint" "gate" False Nothing Nothing  -- simulates CI without hlint
        ]
      resp = ToolchainStatusTool.renderResult entries
   in pure $ case Env.reResult resp of
        Just (A.Object p) ->
          AKM.member (AKey.fromText "tools")          p
            && AKM.member (AKey.fromText "blocking_gates") p
            && AKM.member (AKey.fromText "summary")        p
        _ -> False

-- | 'ghc_toolchain_warmup' is the simpler analogue of toolchain_status —
-- it only probes optional binaries. After Phase B the response is
-- 'ok' when every probed binary is present, 'partial' when one or
-- more are missing. The host-independent assertion: the response
-- decodes as an envelope with status ∈ {ok, partial}.
testToolchainWarmupEnvelopeShape :: IO Bool
testToolchainWarmupEnvelopeShape = do
  decoded <- runToolEnvelope ToolchainWarmupTool.handle (A.object [])
  pure $ case decoded of
    Right env -> Env.reStatus env `elem` [Env.StatusOk, Env.StatusPartial]
              && case Env.reResult env of
                   Just (A.Object payload) ->
                     AKM.member (AKey.fromText "tools") payload
                   _ -> False
    Left _ -> False

-- | When the warmup status is 'partial' (i.e. ≥1 optional binary is
-- missing), the response MUST carry a non-empty 'warnings' array
-- with one entry per missing binary. This is the contract that
-- lets an agent know *which* downstream tool surfaces are about to
-- start returning status='unavailable'.
testToolchainWarmupPartialWarnings :: IO Bool
testToolchainWarmupPartialWarnings = do
  decoded <- runToolEnvelope ToolchainWarmupTool.handle (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusPartial ->
          not (null (Env.reWarnings env))
            && all (\w -> Env.wKind w == Env.SlowPath) (Env.reWarnings env)
      | Env.reStatus env == Env.StatusOk ->
          null (Env.reWarnings env)  -- ok ⇒ no missing binaries ⇒ no warnings
      | otherwise -> False  -- only ok or partial expected
    Left _ -> False
