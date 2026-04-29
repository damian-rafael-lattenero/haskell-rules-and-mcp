-- | Flow: refactoring tools with snapshot-and-compile-verify.
--
-- Exercises three paths through 'ghc_refactor':
--
--   * rename_local happy path: compile stays green → rewrite
--     persists.
--   * rename_local rollback: scope is too narrow so the rewrite
--     creates a "name not in scope" elsewhere → compile fails →
--     file is restored from the snapshot (security-critical
--     invariant).
--   * extract_binding: lift a line range into a new top-level
--     binding; compile stays green.
--
-- Tools exercised:
--
--   ghc_refactor (rename_local, extract_binding)
--   ghc_load (to verify compile post-rewrite)
--
-- This is the single most security-relevant test in the E2E
-- suite: the snapshot invariant keeps an AST-unaware rewrite
-- from ever leaving the repo in a broken state.
module Scenarios.FlowRefactor
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
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- source
--------------------------------------------------------------------------------

-- | Source written in a shape that makes both a happy rename and
-- a scope-narrow-rollback rename easy to construct. Line numbers
-- matter because rename_local takes an explicit scope range.
--
--   Line 1: module Refactor (greet) where
--   Line 2: (blank)
--   Line 3: greet :: String -> String
--   Line 4: greet name =
--   Line 5:   let msg = \"Hello, \" ++ name
--   Line 6:       longer = msg ++ \"!\"
--   Line 7:   in longer
initialSrc :: Text
initialSrc = T.unlines
  [ "module Refactor (greet) where"
  , ""
  , "greet :: String -> String"
  , "greet name ="
  , "  let msg    = \"Hello, \" ++ name"
  , "      longer = msg ++ \"!\""
  , "  in longer"
  ]

--------------------------------------------------------------------------------
-- runFlow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + add Refactor module"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("refactor-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Refactor"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  let srcPath = projectDir </> "src" </> "Refactor.hs"
  TIO.writeFile srcPath initialSrc
  loadR <- Client.callTool c GhcLoad
             (object [ "module_path" .= ("src/Refactor.hs" :: Text) ])
  c1 <- liveCheck $ Check
    { cName   = "setup · Refactor compiles clean"
    , cOk     = fieldIsTrue "success" loadR
    , cDetail = "expected success=true from ghc_load"
    }
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- 1. Happy rename: rename 'msg' → 'greeting' across both its
  --    binding AND its use site (scope lines 5..6 cover both).
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "rename_local(msg → greeting) — happy path"
  renameOk <- Client.callTool c GhcRefactor (object
    [ "action"           .= ("rename_local" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "old_name"         .= ("msg" :: Text)
    , "new_name"         .= ("greeting" :: Text)
    , "scope_line_start" .= (4 :: Int)
    , "scope_line_end"   .= (7 :: Int)
    ])
  c2 <- liveCheck $ Check
    { cName   = "rename_local success"
    , cOk     = fieldIsTrue "success" renameOk
    , cDetail = "expected rename_local response success=true; raw: " <> renderShort renameOk
    }
  bodyAfterRename <- TIO.readFile srcPath
  c3 <- liveCheck $ checkPure
    "file now contains 'greeting' not 'msg'"
    ("greeting" `T.isInfixOf` bodyAfterRename
       && not ("msg " `T.isInfixOf` bodyAfterRename)
       && not (" msg" `T.isInfixOf` bodyAfterRename))
    "happy rename should have swapped msg → greeting in the scope"
  reloadOk <- Client.callTool c GhcLoad
                (object [ "module_path" .= ("src/Refactor.hs" :: Text) ])
  c4 <- liveCheck $ Check
    { cName   = "post-rename compile is still clean"
    , cOk     = fieldIsTrue "success" reloadOk
    , cDetail = "compile MUST pass after a happy rename"
    }
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- 2. Rollback: attempt to rename 'greeting' → 'noScope' with a
  --    NARROW scope that only covers the binding line (5), NOT
  --    the use line (6). That produces a broken source in which
  --    'greeting' is out of scope, compile fails, snapshot
  --    restores.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "rename_local with narrow scope → rollback"
  -- Capture the body BEFORE the rollback attempt so we can
  -- compare byte-for-byte after.
  bodyBefore <- TIO.readFile srcPath
  rollbackR <- Client.callTool c GhcRefactor (object
    [ "action"           .= ("rename_local" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "old_name"         .= ("greeting" :: Text)
    , "new_name"         .= ("noScope" :: Text)
    , "scope_line_start" .= (5 :: Int)
    , "scope_line_end"   .= (5 :: Int)
    ])
  bodyAfterRollback <- TIO.readFile srcPath
  c5 <- liveCheck $ checkPure
    "rollback · file bytes unchanged after failing rewrite"
    (bodyAfterRollback == bodyBefore)
    "snapshot-and-compile-verify MUST restore the file on \
    \compile failure (security-critical invariant)"
  c6 <- liveCheck $ checkPure
    "rollback · response flags the failure"
    (not (fieldIsTrue "success" rollbackR))
    "response should NOT claim success on a rolled-back rewrite"
  stepFooter 3 t2

  ----------------------------------------------------------------
  -- 3. Validator-rejection: renaming to a Haskell keyword MUST
  --    be rejected upfront by 'validateIdentifier', without
  --    touching the file. Distinct from rollback (which runs
  --    when compile fails): this one short-circuits at the
  --    input boundary.
  ----------------------------------------------------------------
  t3 <- stepHeader 4 "rename_local → Haskell keyword (boundary reject)"
  bodyBefore2 <- TIO.readFile srcPath
  keywordR <- Client.callTool c GhcRefactor (object
    [ "action"           .= ("rename_local" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "old_name"         .= ("greeting" :: Text)
    , "new_name"         .= ("class" :: Text)   -- Haskell keyword
    , "scope_line_start" .= (5 :: Int)
    , "scope_line_end"   .= (6 :: Int)
    ])
  bodyAfterKeyword <- TIO.readFile srcPath
  c7 <- liveCheck $ checkPure
    "keyword rename · response rejects the input"
    (not (fieldIsTrue "success" keywordR))
    "renaming to 'class' must return success=false (boundary check)"
  c8 <- liveCheck $ checkPure
    "keyword rename · file bytes unchanged"
    (bodyAfterKeyword == bodyBefore2)
    "a boundary-rejected rewrite must never touch the disk"
  stepFooter 4 t3

  pure [c1, c2, c3, c4, c5, c6, c7, c8]

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
  in if T.length s > 200 then T.take 200 s <> "…" else s
