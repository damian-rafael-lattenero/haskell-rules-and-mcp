-- | Unit tests for the @ghc_format@ tool — path-traversal guard (#100C) and
-- the missing-file clean-error contract (#246).
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape:
-- the driver keeps the registrations and imports these functions. Shared
-- helpers ('decodeToolResult', 'isTraversalRefused') live in 'Spec.Helpers'.
module Spec.Format
  ( testFormatRejectsTraversal
  , testFormatMissingFile
  ) where

import qualified Data.Aeson as A
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import qualified HaskellFlows.Tool.Format as FormatTool
import HaskellFlows.Types (mkProjectDir)

import Spec.Helpers (decodeToolResult, isTraversalRefused)
import Spec.ToolEnvFixture (pdEnv)

-- | #100C: 'ghc_format' must refuse traversal paths.
testFormatRejectsTraversal :: IO Bool
testFormatRejectsTraversal =
  case mkProjectDir "/tmp/project" of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("../../etc/passwd" :: Text) ]
      tr <- FormatTool.handle (pdEnv pd) args
      pure (isTraversalRefused (decodeToolResult tr))

-- | Issue #246: 'ghc_format' on a non-existent file must return
-- status='failed' with kind='module_path_does_not_exist', not a raw
-- subprocess backtrace from fourmolu/ormolu.
testFormatMissingFile :: IO Bool
testFormatMissingFile = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-format-missing"
  removePathForcibly dir
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _ -> pure False
    Right pd -> do
      let args = A.object
            [ "module_path" A..= ("src/DoesNotExist.hs" :: Text) ]
      tr <- FormatTool.handle (pdEnv pd) args
      let result = decodeToolResult tr
      pure $ case result of
        Right env ->
             Env.reStatus env == Env.StatusFailed
          && maybe False
               (\e -> Env.eeKind e == Env.ModulePathDoesNotExist)
               (Env.reError env)
        Left _ -> False
