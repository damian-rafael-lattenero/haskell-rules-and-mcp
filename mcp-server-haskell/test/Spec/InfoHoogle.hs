-- | Unit tests for 'Tool.Info' (#90 Phase B), 'Tool.Hoogle', and
-- 'Tool.AddImport' envelope shapes and guard cases.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.InfoHoogle
  ( testInfoRealSymbolOk
  , testInfoUnknownNameNoMatch
  , testInfoRefusesNewline
  , testHoogleRejectsEmpty
  , testHoogleUnavailable
  , testAddImportUnavailable
  , testAddImportRejectsMissingArg
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (isJust)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Control.Exception (bracket_)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Types (mkProjectDir)
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Tool.Info as InfoTool
import qualified HaskellFlows.Tool.Hoogle as HoogleTool
import qualified HaskellFlows.Tool.AddImport as AddImportTool

import Spec.Helpers (decodeToolResult, runToolEnvelope)

-- | Phase B helper: drive 'InfoTool.handle' against a fresh
-- session with a tiny project loaded.
runInfo :: A.Value -> IO (Either String Env.ToolResponse)
runInfo args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-info-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- InfoTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | 'ghc_info' on a real symbol resolves to a structured definition.
-- Status='ok' with result.{name, kind, definition, instances}.
testInfoRealSymbolOk :: IO Bool
testInfoRealSymbolOk = do
  decoded <- runInfo (A.object [ "name" A..= ("foo" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload == Just (A.String "foo")
            && AKM.member (AKey.fromText "kind") payload
            && AKM.member (AKey.fromText "definition") payload
    _ -> False

-- | Issue #87 closure: 'ghc_info' on a name not in scope MUST emit
-- status='no_match' (the question was well-formed; the answer is
-- the empty set), NOT a fabricated 'data X' definition. This is
-- the load-bearing test for #87 — the previous behaviour was
-- success=true with a synthesised definition that didn't exist
-- in the project, in base, or anywhere reachable. Post-#90 the
-- definition field is gone (no fabrication), result.searched_in
-- documents where we looked, result.remediation suggests the
-- next move.
testInfoUnknownNameNoMatch :: IO Bool
testInfoUnknownNameNoMatch = do
  decoded <- runInfo
    (A.object [ "name" A..= ("DoesNotExistName123" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload
            == Just (A.String "DoesNotExistName123")
            && AKM.member (AKey.fromText "searched_in") payload
            && AKM.member (AKey.fromText "remediation") payload
            -- The fabricated 'data DoesNotExistName123' definition
            -- is GONE — that was the #87 bug.
            && not (AKM.member (AKey.fromText "definition") payload)
    _ -> False

-- | Newline in name → status='refused' with kind='newline_injection'.
testInfoRefusesNewline :: IO Bool
testInfoRefusesNewline = do
  decoded <- runInfo (A.object [ "name" A..= ("foo\n:quit" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "name"
    _ -> False

-- | Phase B helper: drive 'HoogleTool.handle' / 'AddImportTool.handle'.
-- These tools don't need a GhcSession.
runHoogle :: A.Value -> IO (Either String Env.ToolResponse)
runHoogle args = do
  tr <- HoogleTool.handle args
  case trContent tr of
    [TextContent body] ->
      pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
    _ -> pure (Left "expected exactly one TextContent")

-- | #146: AddImportTool.handle now requires a GhcSession. For tests
-- that short-circuit before the session is touched (hoogle missing,
-- parse error), a stub session on an empty tmpdir is sufficient.
runAddImport :: A.Value -> IO (Either String Env.ToolResponse)
runAddImport args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-add-import-stub"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _  -> pure (Left "could not build ProjectDir for stub session")
    Right pd -> do
      sess <- startGhcSession pd
      tr <- AddImportTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")

-- | An empty hoogle query → status='refused' with
-- kind='empty_input' + field='query'.
testHoogleRejectsEmpty :: IO Bool
testHoogleRejectsEmpty = do
  decoded <- runHoogle (A.object [ "query" A..= ("" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.EmptyInput
            && Env.eeField err == Just "query"
    _ -> False

-- | When the hoogle binary isn't on PATH, the status is
-- 'unavailable' (NOT 'failed'). Distinct discriminator: an
-- environment-binary issue is structurally different from a
-- runtime failure. The test scrubs PATH around the call to
-- guarantee the missing-binary code path fires regardless of
-- the host's actual hoogle install.
testHoogleUnavailable :: IO Bool
testHoogleUnavailable = do
  origPath <- lookupEnv "PATH"
  let scrubbed = "/var/empty-haskell-flows-no-hoogle"
  decoded <- bracket_
    (setEnv "PATH" scrubbed)
    (case origPath of
       Just p  -> setEnv "PATH" p
       Nothing -> unsetEnv "PATH")
    (runHoogle (A.object [ "query" A..= ("filter" :: T.Text) ]))
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusUnavailable
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.BinaryUnavailable
            && isJust (Env.eeRemediation err)
    _ -> False

-- | ghc_add_import shares the unavailable contract with hoogle_search.
testAddImportUnavailable :: IO Bool
testAddImportUnavailable = do
  origPath <- lookupEnv "PATH"
  let scrubbed = "/var/empty-haskell-flows-no-hoogle"
  decoded <- bracket_
    (setEnv "PATH" scrubbed)
    (case origPath of
       Just p  -> setEnv "PATH" p
       Nothing -> unsetEnv "PATH")
    (runAddImport (A.object [ "name" A..= ("fromMaybe" :: T.Text) ]))
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusUnavailable
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.BinaryUnavailable
    _ -> False

-- | Empty args (missing 'name') → status='failed' with
-- error.kind='missing_arg'.
testAddImportRejectsMissingArg :: IO Bool
testAddImportRejectsMissingArg = do
  decoded <- runAddImport (A.object [])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.MissingArg
    _ -> False
