-- | Unit tests for 'Tool.Bootstrap' (#90 Phase B) and 'Tool.Imports'
-- envelope shapes.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.Bootstrap
  ( testBootstrapClaudeCodePreviewEnvelope
  , testBootstrapGenericPreviewEnvelope
  , testBootstrapRejectsUnknownHost
  , testBootstrapRejectsMissingHost
  , testBootstrapMissingHostFriendlyMessage
  , testImportsEnvelopeShape
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Types (mkProjectDir)
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Tool.Bootstrap as BootstrapTool
import qualified HaskellFlows.Tool.Imports as ImportsTool
import Spec.ToolEnvFixture (sessionEnv)

-- | Phase B helper: build a fresh tmpdir-based ProjectDir + drive
-- 'BootstrapTool.handle' with the given args. Returns the parsed
-- envelope and cleans up the tmpdir on exit.
runBootstrap :: A.Value -> IO (Either String Env.ToolResponse)
runBootstrap args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-bootstrap-test"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      tr <- BootstrapTool.handle pd [] args
      pure (Right tr)
  removePathForcibly dir
  pure result

-- | 'ghc_bootstrap host=claude-code write=false' emits status='ok' with
-- the rules content + the canonical claude-code target path inside 'result'.
-- Issue #193: pass write=false explicitly; the default is now write=true (write to disk).
testBootstrapClaudeCodePreviewEnvelope :: IO Bool
testBootstrapClaudeCodePreviewEnvelope = do
  decoded <- runBootstrap (A.object [ "host" A..= ("claude-code" :: T.Text)
                                    , "write" A..= False ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "host")    payload == Just (A.String "claude-code")
            && AKM.lookup (AKey.fromText "mode")    payload == Just (A.String "preview")
            && AKM.member (AKey.fromText "content") payload
            && AKM.member (AKey.fromText "target")  payload  -- non-generic ⇒ target path is set
    _ -> False

-- | 'ghc_bootstrap host=generic' emits status='ok' with content but
-- no 'target' field (per the existing contract: generic mode has no
-- canonical target path).
testBootstrapGenericPreviewEnvelope :: IO Bool
testBootstrapGenericPreviewEnvelope = do
  decoded <- runBootstrap (A.object [ "host" A..= ("generic" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "host") payload == Just (A.String "generic")
            && AKM.lookup (AKey.fromText "mode") payload == Just (A.String "preview")
            && AKM.member (AKey.fromText "content") payload
            && not (AKM.member (AKey.fromText "target") payload)
    _ -> False

-- | An unknown host lands as status='failed' with
-- error.kind='validation' (the value was structurally a string,
-- just outside the closed Host enum).
testBootstrapRejectsUnknownHost :: IO Bool
testBootstrapRejectsUnknownHost = do
  decoded <- runBootstrap (A.object [ "host" A..= ("orbital-station" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.Validation
    _ -> False

-- | A request with no 'host' lands as status='failed' with
-- error.kind='missing_arg'. Catches the case where the FromJSON
-- 'fail' string format changes and the discriminator regresses.
testBootstrapRejectsMissingHost :: IO Bool
testBootstrapRejectsMissingHost = do
  decoded <- runBootstrap (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.MissingArg
    _ -> False

-- | #165: when 'host' is missing the error message must mention the
-- accepted values, not leak a raw aeson key name.
testBootstrapMissingHostFriendlyMessage :: IO Bool
testBootstrapMissingHostFriendlyMessage = do
  decoded <- runBootstrap (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env
      , Env.eeKind err == Env.MissingArg ->
          let msg = Env.eeMessage err
          in  "claude-code" `T.isInfixOf` msg
           && "cursor"      `T.isInfixOf` msg
           && "generic"     `T.isInfixOf` msg
    _ -> False

-- | 'ghc_imports' returns the interactive context's import list.
-- Phase B: status='ok' with result carrying the legacy 'count' +
-- 'imports' fields (preserved during the dual-shape window). The
-- absolute *contents* of the imports list depend on whatever
-- autoLoadProject + augmentEvalContext settled on (Prelude + a few
-- stdlib modules); we don't pin specific names — only the contract.
testImportsEnvelopeShape :: IO Bool
testImportsEnvelopeShape = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-imports-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- ImportsTool.handle (sessionEnv sess) (A.object [])
      killGhcSession sess
      pure (Right tr)
  removePathForcibly dir
  pure $ case result of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.member (AKey.fromText "count")   payload
            && AKM.member (AKey.fromText "imports") payload
    _ -> False
