-- | Unit tests for the @ghc_browse@ tool — project-module ok, external
-- no_match, package-env fallback (#168), descriptor mention, and
-- missing-arg guard.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions.
module Spec.Browse
  ( testBrowseProjectModuleOk
  , testBrowseExternalModuleNoMatch
  , testBrowseFallbackOk
  , testBrowseDescriptorMentionsSession
  , testBrowseRejectsMissingArg
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (killGhcSession, startGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (tdDescription)
import qualified HaskellFlows.Tool.Browse as BrowseTool
import HaskellFlows.Types (mkProjectDir)
import Spec.ToolEnvFixture (sessionEnv)

-- ---------------------------------------------------------------------------
-- Helper
-- ---------------------------------------------------------------------------

runBrowse :: A.Value -> IO (Either String Env.ToolResponse)
runBrowse args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-browse-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo (foo, bar) where\nfoo :: Int\nfoo = 1\n\
            \bar :: String -> String\nbar s = s ++ \"!\"\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- BrowseTool.handle (sessionEnv sess) args
      killGhcSession sess
      pure (Right tr)
  removePathForcibly dir
  pure result

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

-- | Browsing a project module → status='ok' with module / count / entries.
testBrowseProjectModuleOk :: IO Bool
testBrowseProjectModuleOk = do
  decoded <- runBrowse (A.object [ "module" A..= ("Foo" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "module") payload == Just (A.String "Foo")
            && AKM.member (AKey.fromText "count")   payload
            && AKM.member (AKey.fromText "entries") payload
    _ -> False

-- | Browsing a module not in the project graph and not in the
-- package environment → status='no_match' with remediation + nextStep.
testBrowseExternalModuleNoMatch :: IO Bool
testBrowseExternalModuleNoMatch = do
  decoded <- runBrowse (A.object [ "module" A..= ("NonExistent.Module.XYZ999" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "module") payload
            == Just (A.String "NonExistent.Module.XYZ999")
            && AKM.member (AKey.fromText "remediation") payload
            && case Env.reNextStep env of
                 Just _  -> True
                 Nothing -> False
    _ -> False

-- | #168: package-env modules (e.g. Data.Maybe from base) are
-- browseable via the fallback path even without a project compile graph.
testBrowseFallbackOk :: IO Bool
testBrowseFallbackOk = do
  decoded <- runBrowse (A.object [ "module" A..= ("Data.Maybe" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "module") payload == Just (A.String "Data.Maybe")
            && AKM.member (AKey.fromText "count") payload
            && AKM.member (AKey.fromText "entries") payload
    _ -> False

-- | The descriptor must mention "session" or "ghc_add_import" so
-- agents know they can browse session-preloaded modules.
testBrowseDescriptorMentionsSession :: IO Bool
testBrowseDescriptorMentionsSession =
  let desc = BrowseTool.descriptor
  in pure ( "session" `T.isInfixOf` tdDescription desc
         || "ghc_add_import" `T.isInfixOf` tdDescription desc )

-- | Empty args (missing 'module') → status='failed' kind='missing_arg'.
testBrowseRejectsMissingArg :: IO Bool
testBrowseRejectsMissingArg = do
  decoded <- runBrowse (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.MissingArg
    _ -> False
