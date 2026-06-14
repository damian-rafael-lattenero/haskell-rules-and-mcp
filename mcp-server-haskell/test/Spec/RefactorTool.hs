-- | Unit tests for the ghc_refactor tool: PermissiveJSON integration (#88),
-- discriminated-schema parse validation (#92B), and list_actions (#154).
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.RefactorTool
  ( testRefactorPermissiveLineRange
  , testRemoveModulesPermissiveBool
  , testFixWarningPermissiveLine
  , testRefactorRenameLocalCompleteParses
  , testRefactorRenameLocalMissingOldName
  , testRefactorRenameLocalMissingScopeStart
  , testRefactorExtractBindingNoOldName
  , testRefactorExtractBindingMissingScope
  , testRefactorSchemaIsDiscriminated
  , testRefactorListActions
  , testRefactorListActionsHasRequired
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as Vector
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..), ToolDescriptor (..))
import qualified HaskellFlows.Tool.FixWarning as FixWarning
import qualified HaskellFlows.Tool.Refactor as RefactorTool
import qualified HaskellFlows.Tool.RemoveModules as RM
import HaskellFlows.Ghc.ApiSession (killGhcSession, startGhcSession)
import HaskellFlows.Types (mkProjectDir)

import Spec.Helpers (decodeToolResult, runToolEnvelope)
import Spec.ToolEnvFixture (sessionPdEnv)

-- | Refactor.scope_line_start / scope_line_end accept stringified
-- numbers (the most user-visible win — rename_local was completely
-- unusable from stringifying clients pre-fix).
testRefactorPermissiveLineRange :: IO Bool
testRefactorPermissiveLineRange = do
  -- Issue #92 Phase B note: old_name is now a parse-time
  -- requirement for rename_local (the schema declares it,
  -- the FromJSON enforces it). The payload here includes it
  -- so the test exercises the *stringified primitive* axis
  -- without colliding with the per-action required-field axis.
  let nativeJson =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"old_name\":\"oldSym\",\"new_name\":\"newSym\",\
        \\"scope_line_start\":17,\"scope_line_end\":42}"
      stringJson =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"old_name\":\"oldSym\",\"new_name\":\"newSym\",\
        \\"scope_line_start\":\"17\",\"scope_line_end\":\"42\"}"
  -- Both must decode (no parse error). The pre-fix behaviour was:
  -- nativeJson decoded, stringJson rejected with an Aeson error
  -- string. Post-fix both succeed.
  let na = A.decode nativeJson :: Maybe A.Value
      st = A.decode stringJson :: Maybe A.Value
  case (na, st) of
    (Just nv, Just sv) -> do
      let nDecoded = A.fromJSON nv :: A.Result RefactorTool.RefactorArgs
          sDecoded = A.fromJSON sv :: A.Result RefactorTool.RefactorArgs
      case (nDecoded, sDecoded) of
        (A.Success _, A.Success _) -> pure True
        _                                   -> pure False
    _ -> pure False

-- | RemoveModules.delete_files / force accept stringified
-- "true"/"false" — the shape that triggered the issue's
-- @\"true\"@ wire form.
testRemoveModulesPermissiveBool :: IO Bool
testRemoveModulesPermissiveBool = do
  let nativeJson =
        "{\"modules\":[\"Foo\"],\"delete_files\":true,\"force\":false}"
      stringJson =
        "{\"modules\":[\"Foo\"],\"delete_files\":\"true\",\"force\":\"false\"}"
      decode raw = A.fromJSON <$> (A.decode raw :: Maybe A.Value)
  case (decode nativeJson, decode stringJson) of
    (Just (A.Success (a :: RM.RemoveModulesArgs)),
     Just (A.Success (b :: RM.RemoveModulesArgs))) ->
       -- Both decoded; if their fields agree then the permissive
       -- path is round-trip-equivalent to the native path.
       pure ( show a == show b )
    _ -> pure False

-- | FixWarning.line and apply: line is REQUIRED, and apply has a
-- default. Both must accept stringified primitives.
testFixWarningPermissiveLine :: IO Bool
testFixWarningPermissiveLine = do
  let nativeJson =
        "{\"module_path\":\"src/X.hs\",\"line\":3,\"code\":\"GHC-66111\",\
        \\"apply\":true}"
      stringJson =
        "{\"module_path\":\"src/X.hs\",\"line\":\"3\",\"code\":\"GHC-66111\",\
        \\"apply\":\"true\"}"
      decode raw = A.fromJSON <$> (A.decode raw :: Maybe A.Value)
  case (decode nativeJson, decode stringJson) of
    (Just (A.Success (a :: FixWarning.FixWarningArgs)),
     Just (A.Success (b :: FixWarning.FixWarningArgs))) ->
       pure ( show a == show b )
    _ -> pure False

-- Issue #92 Phase B: ghc_refactor migration anchors
--
-- These pin the per-action contract that #92 Phase A's
-- discriminatedSchema helper now expresses on the request side.
-- Pre-fix, the schema declared required = [action, module_path,
-- new_name] and the runtime emitted "scope_line_start is required
-- for rename_local" — the schema lied. Post-fix, the parser
-- enforces per-action requirements and a host that reads
-- 'tools/list' learns the right shape from the schema's
-- per-branch required list.
--------------------------------------------------------------------------------

-- | rename_local with the FULL required set must parse cleanly.
testRefactorRenameLocalCompleteParses :: IO Bool
testRefactorRenameLocalCompleteParses = do
  let raw =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"old_name\":\"oldSym\",\"new_name\":\"newSym\",\
        \\"scope_line_start\":17,\"scope_line_end\":42}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Success _ -> True
      _           -> False
    _      -> False

-- | rename_local without old_name must FAIL at parse time
-- (post-#92). Pre-fix this parsed and the handler returned
-- "'old_name' is required for rename_local" at runtime — the
-- schema-vs-runtime contract drift this issue closes.
testRefactorRenameLocalMissingOldName :: IO Bool
testRefactorRenameLocalMissingOldName = do
  let raw =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"new_name\":\"newSym\",\
        \\"scope_line_start\":17,\"scope_line_end\":42}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Error _ -> True   -- expected: parser rejects
      _         -> False
    _      -> False

-- | rename_local without scope_line_start must FAIL at parse
-- time. Same contract: schema declares it required, parser must
-- enforce.
testRefactorRenameLocalMissingScopeStart :: IO Bool
testRefactorRenameLocalMissingScopeStart = do
  let raw =
        "{\"action\":\"rename_local\",\"module_path\":\"src/X.hs\",\
        \\"old_name\":\"oldSym\",\"new_name\":\"newSym\",\
        \\"scope_line_end\":42}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Error _ -> True
      _         -> False
    _      -> False

-- | extract_binding does NOT need old_name — it only renames the
-- extracted binding's name, not an existing identifier. Anchor:
-- the parser must ACCEPT extract_binding without old_name.
testRefactorExtractBindingNoOldName :: IO Bool
testRefactorExtractBindingNoOldName = do
  let raw =
        "{\"action\":\"extract_binding\",\"module_path\":\"src/X.hs\",\
        \\"new_name\":\"helperFn\",\
        \\"scope_line_start\":10,\"scope_line_end\":20}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Success _ -> True
      _           -> False
    _      -> False

-- | extract_binding STILL requires both scope lines. The
-- per-action contract: dropping just one of the scope lines fails
-- at parse time (Aeson 'fromJSON' returns 'Error').
testRefactorExtractBindingMissingScope :: IO Bool
testRefactorExtractBindingMissingScope = do
  let raw =
        "{\"action\":\"extract_binding\",\"module_path\":\"src/X.hs\",\
        \\"new_name\":\"helperFn\",\"scope_line_start\":10}"
  pure $ case A.decode raw :: Maybe A.Value of
    Just v -> case A.fromJSON v :: A.Result RefactorTool.RefactorArgs of
      A.Error _ -> True
      _         -> False
    _      -> False

-- | The published 'tdInputSchema' for ghc_refactor must publish
-- the action discriminant as an 'enum' over every branch.
-- Anchor: a future drift back to top-level 'oneOf' (which Claude
-- rejects) would fail this.
testRefactorSchemaIsDiscriminated :: IO Bool
testRefactorSchemaIsDiscriminated =
  -- #94 Phase C: THREE action branches + #154 adds list_actions → FOUR.
  -- Post-flat-schema fix (Claude API top-level oneOf rejection): we
  -- anchor on the discriminant 'enum' instead of a per-branch 'oneOf'.
  let s = tdInputSchema RefactorTool.descriptor
  in pure $ case s of
       A.Object km -> case AKM.lookup "properties" km of
         Just (A.Object props) -> case AKM.lookup "action" props of
           Just (A.Object actObj) -> case AKM.lookup "enum" actObj of
             Just (A.Array xs) -> length xs == 4
             _                 -> False
           _ -> False
         _ -> False
       _ -> False

-- | #154: list_actions with no other args returns status=ok and
-- an 'actions' list (no module_path / new_name required).
testRefactorListActions :: IO Bool
testRefactorListActions = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-refactor-list-actions"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      let rawArgs = A.object [ "action" A..= ("list_actions" :: T.Text) ]
      tr <- RefactorTool.handle (sessionPdEnv sess pd) rawArgs
      killGhcSession sess
      case trContent tr of
        [TextContent t] ->
          case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) :: Maybe A.Value of
            Just (A.Object env) ->
              case AKM.lookup (AKey.fromText "status") env of
                Just (A.String "ok") -> pure True
                _                    -> pure False
            _ -> pure False
        _ -> pure False

-- | #154: list_actions response carries 'actions' array with an entry
-- for 'move_symbol' that lists the correct field names ('symbol','from','to').
testRefactorListActionsHasRequired :: IO Bool
testRefactorListActionsHasRequired = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "haskell-flows-refactor-list-actions-req"
  createDirectoryIfMissing True dir
  case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> do
      sess <- startGhcSession pd
      let rawArgs = A.object [ "action" A..= ("list_actions" :: T.Text) ]
      tr <- RefactorTool.handle (sessionPdEnv sess pd) rawArgs
      killGhcSession sess
      case trContent tr of
        [TextContent t] ->
          case A.decode (TLE.encodeUtf8 (TL.fromStrict t)) :: Maybe A.Value of
            Just (A.Object env) ->
              case AKM.lookup (AKey.fromText "result") env of
                Just (A.Object res) ->
                  case AKM.lookup (AKey.fromText "actions") res of
                    Just (A.Array arr) ->
                      -- Check that 'move_symbol' has 'symbol','from','to'
                      let moveEntry = [ o | A.Object o <- Vector.toList arr
                                      , AKM.lookup (AKey.fromText "action") o
                                          == Just (A.String "move_symbol")
                                      ]
                      in case moveEntry of
                           [o] -> case AKM.lookup (AKey.fromText "required") o of
                             Just (A.Array req) ->
                               let reqStrs = [ s | A.String s <- Vector.toList req ]
                               in pure (  "symbol" `elem` reqStrs
                                       && "from"   `elem` reqStrs
                                       && "to"     `elem` reqStrs)
                             _ -> pure False
                           _ -> pure False
                    _ -> pure False
                _ -> pure False
            _ -> pure False
        _ -> pure False
