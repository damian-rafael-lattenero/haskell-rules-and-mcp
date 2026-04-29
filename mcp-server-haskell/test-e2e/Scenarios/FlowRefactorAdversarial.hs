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
  --   * extract_binding pointed at a whole top-level equation (issue
--     #46) — the line-based cut would otherwise produce a bare-name
--     call site and a binding with two @=@s. The fix refuses such
--     ranges up-front; the file must remain byte-identical. Covered
--     in five flavours so a regression in any one wire shape trips:
--     (a) single-line equation, (b) type signature only, (c)
--     multi-line range covering signature+body, (d) the same single-
--     line equation but with @dry_run=true@ (the refusal must
--     short-circuit BEFORE the dry-run preview path), (e) post-
--     refusal an indented body still extracts cleanly so the guard
--     hasn't regressed the documented success path.
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
import qualified E2E.Envelope as Env
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

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
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("refactor-adv-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Refactor"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  let srcPath = projectDir </> "src" </> "Refactor.hs"
  TIO.writeFile srcPath initialSrc
  loadR <- Client.callTool c GhcLoad
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
  collisionR <- Client.callTool c GhcRefactor (object
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
  alive1 <- Client.callTool c GhcLoad
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
  extractR <- Client.callTool c GhcRefactor (object
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
    (case statusOk extractR of
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
  alive2 <- Client.callTool c GhcLoad
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
  badScopeR <- Client.callTool c GhcRefactor (object
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
  missingR <- Client.callTool c GhcRefactor (object
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
  alive3 <- Client.callTool c GhcEval
              (object [ "expression" .= ("1 + 1" :: Text) ])
  cMissingAlive <- liveCheck $ checkPure
    "missing file · session still alive after the error"
    (fieldIsTrue "success" alive3)
    "A structured error must not poison the GHCi session."
  stepFooter 5 t4

  --------------------------------------------------------------------
  -- 5. Whole-equation extract refusal (issue #46). Pointing
  -- extract_binding at a top-level equation line used to land a
  -- textual cut at column 0 — call site became a bare name with no
  -- '=', appended binding got the original equation as its RHS
  -- (so '<name> = <name> args = body' — two '=' = parse error).
  -- The fix refuses such ranges up-front; nothing touches the file
  -- and nothing reaches the GHC parser. line 7 is "double x = x * 2"
  -- which is the canonical column-0 equation in this fixture.
  --------------------------------------------------------------------
  t5 <- stepHeader 6 "extract_binding · top-level equation refused (#46)"
  bodyBefore5 <- TIO.readFile srcPath
  topLevelR <- Client.callTool c GhcRefactor (object
    [ "action"           .= ("extract_binding" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "new_name"         .= ("doubledImpl" :: Text)
    , "scope_line_start" .= (7 :: Int)   -- "double x = x * 2"
    , "scope_line_end"   .= (7 :: Int)
    ])
  bodyAfter5 <- TIO.readFile srcPath
  cTopLevelFail <- liveCheck $ checkPure
    "top-level extract · response flags failure (success=false)"
    (not (fieldIsTrue "success" topLevelR))
    ("A range covering a whole top-level equation is not an \
     \expression; lifting it would produce broken Haskell. Tool \
     \must refuse with success=false. Raw: " <> renderShort topLevelR)
  cTopLevelBytes <- liveCheck $ checkPure
    "top-level extract · file bytes byte-identical to pre-call"
    (bodyAfter5 == bodyBefore5)
    "A pre-flight refusal must not touch disk. If bytes diverged, \
     \either the guard ran AFTER the write, or the snapshot/restore \
     \is masking a write that should never have happened."
  let topLevelMsg = T.toLower (renderShort topLevelR)
      mentionsExpressionLanguage =
           "expression range" `T.isInfixOf` topLevelMsg
        || "expression"       `T.isInfixOf` topLevelMsg
        || "column 0"         `T.isInfixOf` topLevelMsg
  cTopLevelMessage <- liveCheck $ checkPure
    "top-level extract · refusal explains the wrong-shape problem"
    mentionsExpressionLanguage
    ("The refusal must tell the agent how to recover (point at the \
     \indented body expression, not the whole equation). A bare \
     \success=false with no guidance leaves the agent guessing. \
     \Raw: " <> renderShort topLevelR)
  -- Belt-and-braces: a refused refactor must not poison the session.
  alive4 <- Client.callTool c GhcLoad
              (object [ "module_path" .= ("src/Refactor.hs" :: Text) ])
  cTopLevelAlive <- liveCheck $ checkPure
    "top-level extract · ghc_load still green after the refusal"
    (fieldIsTrue "success" alive4)
    "A refused refactor must leave the session usable."
  stepFooter 6 t5

  --------------------------------------------------------------------
  -- 6. Type-signature refusal (issue #46, second wire shape).
  -- Line 9 is "buildMessage :: String -> String" — sits at column 0,
  -- isn't even an equation. Lifting it would produce truly absurd
  -- output (the call site would be a name with no '=' AND there'd
  -- be a top-level "<new_name> = buildMessage :: String -> String"
  -- which doesn't parse as a binding at all). Same column-0 guard
  -- catches it.
  --------------------------------------------------------------------
  t6 <- stepHeader 7 "extract_binding · type signature refused (#46)"
  bodyBefore6 <- TIO.readFile srcPath
  sigR <- Client.callTool c GhcRefactor (object
    [ "action"           .= ("extract_binding" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "new_name"         .= ("sigCopy" :: Text)
    , "scope_line_start" .= (9 :: Int)   -- "buildMessage :: ..."
    , "scope_line_end"   .= (9 :: Int)
    ])
  bodyAfter6 <- TIO.readFile srcPath
  cSigFail <- liveCheck $ checkPure
    "type-sig extract · success=false"
    (not (fieldIsTrue "success" sigR))
    ("A type signature is not an expression. The tool must refuse. \
     \Raw: " <> renderShort sigR)
  cSigBytes <- liveCheck $ checkPure
    "type-sig extract · file bytes unchanged"
    (bodyAfter6 == bodyBefore6)
    "Refusing a type-signature extract must not touch disk."
  stepFooter 7 t6

  --------------------------------------------------------------------
  -- 7. Multi-line whole-equation refusal (issue #46, third wire
  -- shape). Lines 6–7 are "double :: Int -> Int" and
  -- "double x = x * 2" — the agent might think it's selecting "the
  -- definition of double" but extract_binding would still cut at
  -- column 0 (commonIndent == 0 because both lines start there) and
  -- emit garbage. The guard catches it because the FIRST non-blank
  -- line in the range still sits at column 0.
  --------------------------------------------------------------------
  t7 <- stepHeader 8 "extract_binding · multi-line equation refused (#46)"
  bodyBefore7 <- TIO.readFile srcPath
  multiR <- Client.callTool c GhcRefactor (object
    [ "action"           .= ("extract_binding" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "new_name"         .= ("doubleAll" :: Text)
    , "scope_line_start" .= (6 :: Int)   -- signature
    , "scope_line_end"   .= (7 :: Int)   -- body
    ])
  bodyAfter7 <- TIO.readFile srcPath
  cMultiFail <- liveCheck $ checkPure
    "multi-line equation extract · success=false"
    (not (fieldIsTrue "success" multiR))
    ("A multi-line cut over a whole top-level equation is just as \
     \broken as a single-line cut. The guard must catch both. \
     \Raw: " <> renderShort multiR)
  cMultiBytes <- liveCheck $ checkPure
    "multi-line equation extract · file bytes unchanged"
    (bodyAfter7 == bodyBefore7)
    "Refusing a multi-line top-level cut must not touch disk."
  stepFooter 8 t7

  --------------------------------------------------------------------
  -- 8. dry_run=true on a top-level equation (issue #46, fourth wire
  -- shape). dry_run is the "preview before commit" path; it must
  -- NOT mask a structural refusal. The guard fires inside
  -- 'extractBinding' which runs BEFORE the dry_run branch in
  -- 'withSnapshot', so the response must still flag success=false
  -- and never reach the preview-rendering code (no 'preview' field
  -- in the payload).
  --------------------------------------------------------------------
  t8 <- stepHeader 9 "extract_binding · dry_run=true top-level still refused (#46)"
  bodyBefore8 <- TIO.readFile srcPath
  dryR <- Client.callTool c GhcRefactor (object
    [ "action"           .= ("extract_binding" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "new_name"         .= ("doubledImpl" :: Text)
    , "scope_line_start" .= (7 :: Int)
    , "scope_line_end"   .= (7 :: Int)
    , "dry_run"          .= True
    ])
  bodyAfter8 <- TIO.readFile srcPath
  cDryFail <- liveCheck $ checkPure
    "dry_run extract top-level · success=false"
    (not (fieldIsTrue "success" dryR))
    ("dry_run must not paper over a structural refusal — that would \
     \be the worst possible UX (preview shows broken Haskell, then \
     \the agent commits it). Raw: " <> renderShort dryR)
  cDryNoPreview <- liveCheck $ checkPure
    "dry_run extract top-level · no 'preview' field in payload"
    (not (hasField "preview" dryR))
    ("If the response carries a 'preview' field, the guard ran AFTER \
     \the dry_run path and is therefore in the wrong place. Raw: "
      <> renderShort dryR)
  cDryBytes <- liveCheck $ checkPure
    "dry_run extract top-level · file bytes unchanged"
    (bodyAfter8 == bodyBefore8)
    "dry_run never touches the file regardless; this just pins it."
  stepFooter 9 t8

  --------------------------------------------------------------------
  -- 9. Regression: an indented body expression must STILL extract
  -- cleanly after the guard. Line 11 is
  --   "  let prefix = \"[INFO] \" in prefix ++ name"
  -- (the same line we already exercised in step 3 — but step 3 was
  -- before any of the col-0 refusals, so re-running it here proves
  -- the guard hasn't poisoned the session or the source layout
  -- across the four refusal hops above). The success path sits on
  -- line 11 in the post-refactor file; we recompute the line by
  -- scanning the live source so the test stays robust if step 3
  -- mutated the file.
  --------------------------------------------------------------------
  t9 <- stepHeader 10 "extract_binding · indented body still works (regression)"
  liveSrc <- TIO.readFile srcPath
  let liveLines = T.lines liveSrc
      letLineIx =
        case [ i | (i, ln) <- zip [1 :: Int ..] liveLines
                 , "let prefix" `T.isInfixOf` ln
             ] of
          (i:_) -> i
          []    -> -1
  okR <- if letLineIx < 1
           then pure (object [ "success" .= False
                             , "error"   .= ("fixture lost the let-prefix line"
                                              :: Text) ])
           else Client.callTool c GhcRefactor (object
             [ "action"           .= ("extract_binding" :: Text)
             , "module_path"      .= ("src/Refactor.hs" :: Text)
             , "new_name"         .= ("infoPrefix2" :: Text)
             , "scope_line_start" .= letLineIx
             , "scope_line_end"   .= letLineIx
             , "dry_run"          .= True   -- preview-only, leave file pristine
             ])
  cRegressionLine <- liveCheck $ checkPure
    "regression · fixture still has the let-prefix line"
    (letLineIx >= 1)
    "Earlier steps must not have mutated the indented expression line."
  cRegressionShape <- liveCheck $ checkPure
    "regression · response carries a 'success' boolean"
    (case statusOk okR of
       Just _  -> True
       Nothing -> False)
    ("extract_binding on an indented expression must produce a \
     \structured response. Raw: " <> renderShort okR)
  -- Sanity: the session is alive (its job is to support the next
  -- agent step in the same flow, not to die on the way out).
  alive5 <- Client.callTool c GhcEval
              (object [ "expression" .= ("1 + 1" :: Text) ])
  cRegressionAlive <- liveCheck $ checkPure
    "regression · session alive after the full sequence"
    (fieldIsTrue "success" alive5)
    "Four refusals + one dry_run preview must leave the GHCi child usable."
  stepFooter 10 t9

  pure
    [ cPre
    , cCollisionFail, cCollisionRestore, cCollisionEvidence, cCollisionAlive
    , cExtractShape, cExtractOutcome, cExtractAlive
    , cBadScope, cBadScopeFile
    , cMissing, cMissingAlive
    , cTopLevelFail, cTopLevelBytes, cTopLevelMessage, cTopLevelAlive
    , cSigFail, cSigBytes
    , cMultiFail, cMultiBytes
    , cDryFail, cDryNoPreview, cDryBytes
    , cRegressionLine, cRegressionShape, cRegressionAlive
    ]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- Issue #90 Phase D step 2: route through E2E.Envelope's
-- envelope-aware lookupField so 'success' resolves to the
-- synthesized projection of 'status' even though the legacy
-- top-level field has been dropped.
fieldIsTrue :: Text -> Value -> Bool
fieldIsTrue k v = case Env.lookupField k v of
  Just (Bool True) -> True
  _                -> False

renderShort :: Value -> Text
renderShort v =
  let s = T.pack (show v)
  in if T.length s > 300 then T.take 300 s <> "…" else s

hasField :: Text -> Value -> Bool
hasField k v = case lookupField k v of
  Just _  -> True
  Nothing -> False
