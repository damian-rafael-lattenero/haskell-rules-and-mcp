-- | Unit tests for 'Tool.Hole' (#90 Phase B): ok/no_match/non-existent
-- file/traversal-guard cases. Uses a real GhcSession.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.HoleTool
  ( testHoleWithHoleOk
  , testHoleNoHoleMatch
  , testHoleNonExistentFile
  , testHoleRejectsTraversal
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Types (mkProjectDir)
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Tool.Hole as HoleTool

import Spec.Helpers (decodeToolResult, isTraversalRefused, runToolEnvelope)

runHole :: T.Text -> A.Value -> IO (Either String Env.ToolResponse)
runHole stagedSource args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-hole-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs") stagedSource
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- HoleTool.handle sess pd args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- | A module with an explicit '_' hole produces status='ok' with
-- result.holes carrying ≥ 1 entry. Anchors the happy-path
-- contract: ghc_hole IS the right tool when there are holes.
testHoleWithHoleOk :: IO Bool
testHoleWithHoleOk = do
  let src = T.pack "module Foo where\nfoo :: Int -> Int\nfoo x = _\n"
  decoded <- runHole src (A.object [ "module_path" A..= ("src/Foo.hs" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          case AKM.lookup (AKey.fromText "hole_count") payload of
            Just (A.Number n) -> n >= 1
            _                 -> False
    _ -> False

-- | A module with no holes produces status='no_match' (the
-- question — \"where are the typed holes?\" — was well-formed;
-- the answer is the empty set). Pre-#90 this returned
-- success=true with hole_count=0 — the same anti-pattern #87
-- generalises.
testHoleNoHoleMatch :: IO Bool
testHoleNoHoleMatch = do
  let src = T.pack "module Foo where\nfoo :: Int -> Int\nfoo x = x + 1\n"
  decoded <- runHole src (A.object [ "module_path" A..= ("src/Foo.hs" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "hole_count") payload == Just (A.Number 0)
    _ -> False

-- | #148: a module_path that does not exist on disk → status='failed'
-- with kind='module_path_does_not_exist'. Before the fix the tool
-- returned status='no_match' (hole_count=0) — indistinguishable from
-- a valid hole-free file.
testHoleNonExistentFile :: IO Bool
testHoleNonExistentFile = do
  let src = T.pack "module Foo where\nfoo :: Int\nfoo = 1\n"
  decoded <- runHole src
    (A.object [ "module_path" A..= ("src/DoesNotExist.hs" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusFailed
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.ModulePathDoesNotExist
            && Env.eeField err == Just "module_path"
    _ -> False

-- | A path that escapes the project root is refused via the
-- mkModulePath gate → status='refused' with kind='path_traversal'.
testHoleRejectsTraversal :: IO Bool
testHoleRejectsTraversal = do
  let src = T.pack "module Foo where\nfoo :: Int\nfoo = 1\n"
  decoded <- runHole src
    (A.object [ "module_path" A..= ("../../etc/passwd" :: T.Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.PathTraversal
            && Env.eeField err == Just "module_path"
    _ -> False
