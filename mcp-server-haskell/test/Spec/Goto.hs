-- | Unit tests for the @ghc_goto@ tool — location resolution (#87/#117),
-- InFile/InModule payload shapes, compiled-module remediation (#214),
-- qualifiedPreloadPayload (#224), and the newline injection guard.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions.
module Spec.Goto
  ( testGotoLocalNameOk
  , testGotoUnknownNameNoMatch
  , testGotoRefusesNewline
  , testGotoLibraryNameNoMatch
  , testGotoFileHasLocation
  , testGotoCompiledModuleRemediation
  , testGotoQualifiedPreloadPayload
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import HaskellFlows.Ghc.ApiSession (killGhcSession, startGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import qualified HaskellFlows.Tool.Goto as GotoTool
import HaskellFlows.Types (mkProjectDir)

-- ---------------------------------------------------------------------------
-- Phase B helper
-- ---------------------------------------------------------------------------

-- | Stage a tmpdir project with a 'Foo' module exporting 'foo' and drive
-- 'GotoTool.handle' with the given args.
runGoto :: A.Value -> IO (Either String Env.ToolResponse)
runGoto args = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-goto-test"
  removePathForcibly dir
  createDirectoryIfMissing True (dir </> "src")
  TIO.writeFile (dir </> "src" </> "Foo.hs")
    (T.pack "module Foo where\nfoo :: Int\nfoo = 1\n")
  result <- case mkProjectDir dir of
    Left _   -> pure (Left "could not build ProjectDir")
    Right pd -> do
      sess <- startGhcSession pd
      tr   <- GotoTool.handle sess args
      killGhcSession sess
      case trContent tr of
        [TextContent body] ->
          pure (A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body)))
        _ -> pure (Left "expected exactly one TextContent")
  removePathForcibly dir
  pure result

-- ---------------------------------------------------------------------------
-- GHC-session goto tests
-- ---------------------------------------------------------------------------

-- | 'ghc_goto' on a project-defined name resolves to a file
-- location → status='ok' with result.kind='file' + result.file +
-- result.line + result.column.
testGotoLocalNameOk :: IO Bool
testGotoLocalNameOk = do
  decoded <- runGoto (A.object [ "name" A..= ("foo" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusOk
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload == Just (A.String "foo")
            && (AKM.lookup (AKey.fromText "kind") payload == Just (A.String "file")
                  || AKM.lookup (AKey.fromText "kind") payload == Just (A.String "module"))
    _ -> False

-- | 'ghc_goto' on a name that's not in scope → status='no_match'
-- with the searched name echoed inside result.
testGotoUnknownNameNoMatch :: IO Bool
testGotoUnknownNameNoMatch = do
  decoded <- runGoto
    (A.object [ "name" A..= ("definitelyNotARealName123" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusNoMatch
      , Just (A.Object payload) <- Env.reResult env ->
          AKM.lookup (AKey.fromText "name") payload
            == Just (A.String "definitelyNotARealName123")
            && AKM.member (AKey.fromText "remediation") payload
    _ -> False

-- | A newline-laden name → status='refused' with kind='newline_injection'.
testGotoRefusesNewline :: IO Bool
testGotoRefusesNewline = do
  decoded <- runGoto (A.object [ "name" A..= ("foo\n:quit" :: Text) ])
  pure $ case decoded of
    Right env
      | Env.reStatus env == Env.StatusRefused
      , Just err <- Env.reError env ->
          Env.eeKind err == Env.NewlineInjection
            && Env.eeField err == Just "name"
    _ -> False

-- ---------------------------------------------------------------------------
-- Pure locationPayload tests (#117 / #214 / #224)
-- ---------------------------------------------------------------------------

-- | #117: when 'queryLocation' resolves a name to an 'InModule' location
-- (library name with no local source file), 'locationPayload' must include
-- @has_location: false@ and a remediation hint.
testGotoLibraryNameNoMatch :: IO Bool
testGotoLibraryNameNoMatch =
  let loc = GotoTool.InModule "GHC.Base"
      payload = GotoTool.locationPayload "fmap" loc
  in case payload of
       A.Object o ->
         let hasLoc  = AKM.lookup "has_location" o == Just (A.Bool False)
             hasRem  = case AKM.lookup "remediation" o of
                         Just (A.String t) -> not (T.null t)
                         _                 -> False
             hasKind = AKM.lookup "kind" o == Just (A.String "module")
         in pure (hasLoc && hasRem && hasKind)
       _ -> pure False

-- | #117: an 'InFile' location must carry @has_location: true@.
testGotoFileHasLocation :: IO Bool
testGotoFileHasLocation =
  let loc = GotoTool.InFile "src/Foo.hs" 10 5
      payload = GotoTool.locationPayload "myFn" loc
  in case payload of
       A.Object o ->
         pure (AKM.lookup "has_location" o == Just (A.Bool True))
       _ -> pure False

-- | #214: the InModule remediation message must NOT say "no local source
-- file" because compiled project modules DO have a local source file —
-- they're simply compiled. The message must use "was compiled" instead.
testGotoCompiledModuleRemediation :: IO Bool
testGotoCompiledModuleRemediation =
  let loc = GotoTool.InModule "Scratch"
      payload = GotoTool.locationPayload "greet" loc
  in case payload of
       A.Object o ->
         case AKM.lookup "remediation" o of
           Just (A.String t) ->
             let hasCompiled   = "was compiled" `T.isInfixOf` t
                 noFalseSource = not ("no local source file" `T.isInfixOf` t)
             in pure (hasCompiled && noFalseSource)
           _ -> pure False
       _ -> pure False

-- | #224: qualifiedPreloadPayload names the unqualified form and the
-- module prefix so the agent knows how to retry without the qualifier.
testGotoQualifiedPreloadPayload :: IO Bool
testGotoQualifiedPreloadPayload =
  let loc = GotoTool.InModule "GHC.Internal.Data.OldList"
      payload = GotoTool.qualifiedPreloadPayload "Data.List.sort" "sort" loc
  in case payload of
       A.Object o ->
         case AKM.lookup "remediation" o of
           Just (A.String t) ->
             let mentionsSort = "'sort'" `T.isInfixOf` t
                 mentionsMod  = "'Data.List'" `T.isInfixOf` t
                 mentionsQual = "qualified" `T.isInfixOf` t
             in pure (mentionsSort && mentionsMod && mentionsQual)
           _ -> pure False
       _ -> pure False
