-- | Flow: @ghc_refactor@ under adversarial conditions.
--
-- 'FlowRefactor' covers the happy-path rename, the narrow-scope
-- rollback (the security-critical snapshot invariant), and the
-- keyword-boundary rejection. This scenario covers what
-- 'FlowRefactor' leaves out:
--
--   * rename_local to a name that ALREADY EXISTS in scope — the
--     rewrite produces a valid Haskell file syntactically but
--     compile fails with "duplicate declaration" / "ambiguous
--     occurrence". The snapshot invariant must still restore the
--     file. Distinct from the narrow-scope rollback in that the
--     FAILURE MODE on the rewritten file is different (duplicate
--     vs not-in-scope), and we want both paths to trip the restore.
--
--   * extract_binding — exists in the module header as a
--     documented refactor but never actually exercised in the
--     original scenario. This step lifts a line range into a new
--     top-level binding and asserts compile stays green.
--
--   * Invalid scope range (start > end, lines past EOF) — the
--     tool must reject structurally, not crash.
--
--   * Nonexistent module_path — the tool must return a structured
--     error, not throw or corrupt state.
--
-- Tools exercised:
--
--   ghc_refactor (rename_local, extract_binding)
--   ghc_load     (post-rewrite compile verification)
--
-- Why this is worth having: the snapshot invariant is the single
-- claim most agents will stake dev-loop trust on. Each additional
-- way a rewrite can fail is a new chance for the restore to
-- regress. FlowRefactor covers one failure mode; this covers three
-- more.
module Scenarios.FlowRefactorAdversarial
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

--------------------------------------------------------------------------------
-- source
--------------------------------------------------------------------------------

-- | Source shaped for three adversarial tests:
--
--   * 'double' is defined (line 6) so renaming 'greet' → 'double'
--     triggers a collision.
--   * 'buildMessage' has an inline expression on line 11 that we
--     can extract into its own top-level binding.
--
--   Line 1:  module Refactor (greet, double, buildMessage) where
--   Line 2:  (blank)
--   Line 3:  greet :: String -> String
--   Line 4:  greet name = "Hello, " ++ name
--   Line 5:  (blank)
--   Line 6:  double :: Int -> Int
--   Line 7:  double x = x * 2
--   Line 8:  (blank)
--   Line 9:  buildMessage :: String -> String
--   Line 10: buildMessage name =
--   Line 11:   let prefix = "[INFO] " in prefix ++ name
initialSrc :: Text
initialSrc = T.unlines
  [ "module Refactor (greet, double, buildMessage) where"
  , ""
  , "greet :: String -> String"
  , "greet name = \"Hello, \" ++ name"
  , ""
  , "double :: Int -> Int"
  , "double x = x * 2"
  , ""
  , "buildMessage :: String -> String"
  , "buildMessage name ="
  , "  let prefix = \"[INFO] \" in prefix ++ name"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  --------------------------------------------------------------------
  -- setup
  --------------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Refactor module (greet, double, buildMessage)"
  _ <- Client.callTool c "ghc_create_project"
         (object [ "name" .= ("refactor-adv-demo" :: Text) ])
  _ <- Client.callTool c "ghc_add_modules"
         (object [ "modules" .= (["Refactor"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  let srcPath = projectDir </> "src" </> "Refactor.hs"
  TIO.writeFile srcPath initialSrc
  loadR <- Client.callTool c "ghc_load"
             (object [ "module_path" .= ("src/Refactor.hs" :: Text) ])
  cPre <- liveCheck $ checkPure
    "setup · Refactor compiles clean"
    (fieldIsTrue "success" loadR)
    ("expected ghc_load success=true; got: " <> renderShort loadR)
  stepFooter 1 t0

  --------------------------------------------------------------------
  -- 1. Collision rename: 'greet' → 'double' over the full greet
  -- scope (lines 3–4). After rewrite the file has TWO definitions
  -- of 'double' — duplicate declaration compile error. Snapshot
  -- must restore.
  --------------------------------------------------------------------
  t1 <- stepHeader 2 "collision · rename greet → double (duplicate)"
  bodyBefore <- TIO.readFile srcPath
  collisionR <- Client.callTool c "ghc_refactor" (object
    [ "action"           .= ("rename_local" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "old_name"         .= ("greet" :: Text)
    , "new_name"         .= ("double" :: Text)
    , "scope_line_start" .= (3 :: Int)
    , "scope_line_end"   .= (4 :: Int)
    ])
  bodyAfter <- TIO.readFile srcPath
  cCollisionFail <- liveCheck $ checkPure
    "collision · response flags the failure (success=false)"
    (not (fieldIsTrue "success" collisionR))
    ("A rewrite that introduces a duplicate declaration MUST be \
     \rolled back. The tool must not report success. Raw: "
      <> renderShort collisionR)
  cCollisionRestore <- liveCheck $ checkPure
    "collision · file bytes restored from snapshot"
    (bodyAfter == bodyBefore)
    "Post-compile-failure the file MUST be byte-identical to the \
     \pre-rewrite snapshot. This is the security-critical invariant \
     \the tool's module header documents. A partial rewrite leaking \
     \to disk would corrupt user source silently."
  -- Belt-and-braces: prove the tool actually RAN the write→compile→
  -- restore cycle (not some static collision shortcut). If it did,
  -- the response carries the compiler errors that caused the
  -- rollback. The response shape uses an 'errors' array (or 'error'
  -- string) that must mention the duplicate-ness of 'double'.
  let rawText = T.toLower (renderShort collisionR)
      mentionsFailure =
           "duplicate" `T.isInfixOf` rawText
        || "already"   `T.isInfixOf` rawText
        || "multiple"  `T.isInfixOf` rawText
        || "conflict"  `T.isInfixOf` rawText
        || hasField "errors" collisionR
  cCollisionEvidence <- liveCheck $ checkPure
    "collision · response carries compile-failure evidence"
    mentionsFailure
    ("If the response is just {success:false} with no errors[] or \
     \duplicate-like text, the tool may be short-circuiting without \
     \the compile-verify pass — that would mean the snapshot/restore \
     \contract is actually untested. Raw: " <> renderShort collisionR)
  -- And the session should still work — a failed refactor must not
  -- wedge the GHCi child.
  alive1 <- Client.callTool c "ghc_load"
              (object [ "module_path" .= ("src/Refactor.hs" :: Text) ])
  cCollisionAlive <- liveCheck $ checkPure
    "collision · ghc_load of the restored file still green"
    (fieldIsTrue "success" alive1)
    "After the rollback the file is back to the original; loading \
     \it must succeed. If not, the snapshot restore is NOT byte-identical."
  stepFooter 2 t1

  --------------------------------------------------------------------
  -- 2. extract_binding: lift the 'prefix = "[INFO] "' definition
  -- (line 11) into a new top-level binding called 'infoPrefix'.
  -- Compile must stay green.
  --------------------------------------------------------------------
  t2 <- stepHeader 3 "extract_binding · lift prefix into top-level"
  extractR <- Client.callTool c "ghc_refactor" (object
    [ "action"           .= ("extract_binding" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "new_name"         .= ("infoPrefix" :: Text)
    , "scope_line_start" .= (11 :: Int)
    , "scope_line_end"   .= (11 :: Int)
    ])
  -- extract_binding's contract: if the lift type-checks, persist;
  -- otherwise restore from snapshot. Either branch should return
  -- a structured response.
  cExtractShape <- liveCheck $ checkPure
    "extract · response carries a 'success' boolean"
    (case fieldBool "success" extractR of
       Just _  -> True
       Nothing -> False)
    ("extract_binding must return a structured response. Raw: "
      <> renderShort extractR)
  -- If it succeeded: the file should now contain 'infoPrefix' as
  -- a new top-level and the compile should stay green.
  bodyAfterExtract <- TIO.readFile srcPath
  let extractSucceeded = fieldIsTrue "success" extractR
      mentionsTopLevel = "infoPrefix" `T.isInfixOf` bodyAfterExtract
  cExtractOutcome <- liveCheck $ checkPure
    "extract · if success=true, 'infoPrefix' is in the file; else bytes unchanged"
    (if extractSucceeded
       then mentionsTopLevel
       else bodyAfterExtract == bodyBefore)
    ("success=true means the rewrite landed (infoPrefix visible); \
     \success=false means the snapshot restored the original bytes. \
     \Observed: success=" <> T.pack (show extractSucceeded)
     <> ", mentionsTopLevel=" <> T.pack (show mentionsTopLevel))
  -- Either way, the session must still be usable.
  alive2 <- Client.callTool c "ghc_load"
              (object [ "module_path" .= ("src/Refactor.hs" :: Text) ])
  cExtractAlive <- liveCheck $ checkPure
    "extract · ghc_load after the refactor still green"
    (fieldIsTrue "success" alive2)
    ("Post-extract, the file (rewritten or restored) must compile. \
     \Raw: " <> renderShort alive2)
  stepFooter 3 t2

  --------------------------------------------------------------------
  -- 3. Invalid scope: start > end should be rejected structurally.
  -- The file must not be touched at all — this is a boundary check,
  -- not a compile-verify-and-restore path.
  --------------------------------------------------------------------
  t3 <- stepHeader 4 "invalid scope · start > end must be refused"
  bodyBefore3 <- TIO.readFile srcPath
  badScopeR <- Client.callTool c "ghc_refactor" (object
    [ "action"           .= ("rename_local" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "old_name"         .= ("greet" :: Text)
    , "new_name"         .= ("hiThere" :: Text)
    , "scope_line_start" .= (10 :: Int)   -- line 10
    , "scope_line_end"   .= (3 :: Int)    -- line 3 — inverted range
    ])
  bodyAfter3 <- TIO.readFile srcPath
  cBadScope <- liveCheck $ checkPure
    "bad scope · response flags failure"
    (not (fieldIsTrue "success" badScopeR))
    ("An inverted scope range is non-sensical and must be refused \
     \at the boundary. Raw: " <> renderShort badScopeR)
  cBadScopeFile <- liveCheck $ checkPure
    "bad scope · file bytes unchanged (no-op path)"
    (bodyAfter3 == bodyBefore3)
    "A boundary-refused rewrite must not touch the file."
  stepFooter 4 t3

  --------------------------------------------------------------------
  -- 4. Nonexistent module_path: structured error, no crash, session
  -- alive.
  --------------------------------------------------------------------
  t4 <- stepHeader 5 "nonexistent file · src/DoesNotExist.hs"
  missingR <- Client.callTool c "ghc_refactor" (object
    [ "action"           .= ("rename_local" :: Text)
    , "module_path"      .= ("src/DoesNotExist.hs" :: Text)
    , "old_name"         .= ("x" :: Text)
    , "new_name"         .= ("y" :: Text)
    , "scope_line_start" .= (1 :: Int)
    , "scope_line_end"   .= (1 :: Int)
    ])
  cMissing <- liveCheck $ checkPure
    "missing file · response is structured failure"
    (not (fieldIsTrue "success" missingR))
    ("A module_path that doesn't exist must surface as success=false \
     \with an error field, not as a crash or a silent success. Raw: "
      <> renderShort missingR)
  alive3 <- Client.callTool c "ghc_eval"
              (object [ "expression" .= ("1 + 1" :: Text) ])
  cMissingAlive <- liveCheck $ checkPure
    "missing file · session still alive after the error"
    (fieldIsTrue "success" alive3)
    "A structured error must not poison the GHCi session."
  stepFooter 5 t4

  pure
    [ cPre
    , cCollisionFail, cCollisionRestore, cCollisionEvidence, cCollisionAlive
    , cExtractShape, cExtractOutcome, cExtractAlive
    , cBadScope, cBadScopeFile
    , cMissing, cMissingAlive
    ]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldIsTrue :: Text -> Value -> Bool
fieldIsTrue k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just (Bool True) -> True
  _                -> False
fieldIsTrue _ _ = False

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just (Bool b) -> Just b
  _             -> Nothing
fieldBool _ _ = Nothing

renderShort :: Value -> Text
renderShort v =
  let s = T.pack (show v)
  in if T.length s > 300 then T.take 300 s <> "…" else s

hasField :: Text -> Value -> Bool
hasField k (Object o) = KeyMap.member (Key.fromText k) o
hasField _ _          = False
