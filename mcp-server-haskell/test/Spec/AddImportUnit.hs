-- | Unit tests for 'Tool.AddImport' missing-hoogle handling, session
-- injection, and nextStep hints when no import is found.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.AddImportUnit
  ( testAddImportMissingHoogle
  , testAddImportToSessionInvalid
  , testAddImportToSessionValid
  , testNextStepAddImportZero
  , testNextStepAddImportNonZero
  , testAddModulesPath
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.NextStep
import qualified HaskellFlows.Mcp.NextStep as NextStep
import HaskellFlows.Mcp.ToolName (ToolName (..))
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Types (mkProjectDir)
import HaskellFlows.Ghc.ApiSession (startGhcSession, killGhcSession)
import qualified HaskellFlows.Tool.AddImport as AddImport
import qualified HaskellFlows.Tool.AddModules as AddModules

import Spec.Helpers (decodeToolResult, runToolEnvelope)

testAddImportMissingHoogle :: IO Bool
testAddImportMissingHoogle = do
  origPath <- lookupEnv "PATH"
  setEnv "PATH" "/nonexistent/path-for-test-only"
  -- Use a dedicated tempdir as PATH so hoogle is guaranteed missing.
  -- #146: AddImportTool.handle now requires a GhcSession; the stub
  -- session on an empty dir is sufficient since the tool fails at
  -- the hoogle-availability gate, before touching the session.
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-add-import-nohoogle"
  createDirectoryIfMissing True dir
  let args = A.object [ "name" A..= ("fromMaybe" :: T.Text) ]
  result <- case mkProjectDir dir of
    Left _  -> pure (ToolResult { trContent = [TextContent "{}"], trIsError = False })
    Right pd -> do
      sess <- startGhcSession pd
      r <- AddImport.handle sess args
      killGhcSession sess
      pure r
  -- Restore PATH so other tests aren't affected.
  case origPath of
    Just p  -> setEnv "PATH" p
    Nothing -> unsetEnv "PATH"
  case trContent result of
    [TextContent t] ->
      -- Issue #90 Phase D step 2: legacy 'success: false' is gone.
      -- Branch on @status@ and the structured @error.kind@ instead.
      let parsed = A.decode (TLE.encodeUtf8 (TL.fromStrict t)) :: Maybe A.Value
      in pure $ case parsed of
           Just v -> fieldText "status" v == Just "unavailable"
                  && trIsError result
                  && case lookupField "error" v of
                       Just (A.Object errObj) ->
                         let msg = AKM.lookup (AKey.fromText "message") errObj
                             rem_ = AKM.lookup (AKey.fromText "remediation") errObj
                             msgOk = case msg of
                               Just (A.String m) -> "hoogle" `T.isInfixOf` T.toLower m
                               _ -> False
                             remOk = case rem_ of
                               Just (A.String _) -> True
                               _ -> False
                         in msgOk && remOk
                       _ -> False
           Nothing -> False
    _ -> pure False
  where
    fieldText k (A.Object o) = case AKM.lookup (AKey.fromText k) o of
      Just (A.String s) -> Just s
      _                 -> Nothing
    fieldText _ _ = Nothing
    lookupField k (A.Object o) = AKM.lookup (AKey.fromText k) o
    lookupField _ _            = Nothing

-- | #146: addImportToSession returns (False, msg) when the import
-- line is syntactically invalid — GHC rejects it during parseImportDecl
-- and the exception is caught gracefully.
testAddImportToSessionInvalid :: IO Bool
testAddImportToSessionInvalid = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-add-import-invalid"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      (ok, msg) <- AddImport.addImportToSession sess
                     "this is not a valid import at all"
      killGhcSession sess
      pure (not ok && not (T.null msg))

-- | #146: addImportToSession returns (True, importLine) for a
-- well-formed import of a GHC base module.
testAddImportToSessionValid :: IO Bool
testAddImportToSessionValid = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-add-import-valid"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      let importLine = "import Data.Maybe"
      (ok, added) <- AddImport.addImportToSession sess importLine
      killGhcSession sess
      pure (ok && added == importLine)

-- | Issue #53: nextStep dispatch on a ghc_add_import payload
-- with @count: 0@ must return 'Nothing' (no \"reload to confirm\"
-- nudge), since nothing was added.
testNextStepAddImportZero :: IO Bool
testNextStepAddImportZero =
  let payload = A.object
        [ "success" A..= True
        , "name"    A..= ("ghostFn" :: T.Text)
        , "count"   A..= (0 :: Int)
        , "imports" A..= ([] :: [T.Text])
        ]
  in pure (isNothing (suggestNext GhcAddImport True payload))

-- | Issue #53: nextStep dispatch on a ghc_add_import payload
-- with @count: 3@ must return 'Just (...GhcLoad...)' so the
-- reload nudge fires when there's something to reload.
testNextStepAddImportNonZero :: IO Bool
testNextStepAddImportNonZero =
  let payload = A.object
        [ "success" A..= True
        , "name"    A..= ("fromMaybe" :: T.Text)
        , "count"   A..= (3 :: Int)
        , "imports" A..= (["import Data.Maybe"] :: [T.Text])
        ]
  in pure $ case suggestNext GhcAddImport True payload of
       Just ns -> nsTool ns == GhcLoad
       Nothing -> False

testAddModulesPath :: IO Bool
testAddModulesPath = pure $
     AddModules.moduleToPath "Expr.Syntax"  == "src/Expr/Syntax.hs"
  && AddModules.moduleToPath "Main"         == "src/Main.hs"
