-- | Flow: rename a binding that does NOT exist — the bug-finding
-- oracle for 'ghc_refactor'.
--
-- Real user flow:
--
--   * Dev wants to rename 'fooBar' but mistypes it as 'fooBarz'.
--   * Dev asks @ghc_refactor(action="rename_local")@ with the typo.
--   * Dev expects the MCP to /refuse/ — "target not found" — so
--     they can fix the typo without any file on disk being touched.
--
-- The wrong answers (all bugs we want to catch):
--
--   (a) success=true with zero rewrites — the MCP pretends to have
--       renamed something it could not find. Silent success is the
--       worst UX; it lets the typo ship.
--   (b) success=true and the file is modified anyway (e.g. by a
--       regex that accidentally matched a substring).
--   (c) success=false but the file is still mutated (partial write
--       slipped past the snapshot invariant).
--   (d) exception bubble — the dispatcher lets an internal error
--       escape as an MCP protocol error instead of returning a
--       structured tool result.
--
-- Correct behavior: success=false, compile key absent OR false,
-- file byte-for-byte identical to pre-call.
module Scenarios.FlowRefactorOutOfScope
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
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | Module with exactly one local binding we /could/ rename. The
-- scenario asks for a different name, so the rewrite must no-op.
srcModule :: Text
srcModule = T.unlines
  [ "module Foo where"
  , ""
  , "fooBar :: Int -> Int"
  , "fooBar x = x + 1"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- (1) scaffold + write a module with a known binding
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Foo.hs (fooBar is the only local)"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("oos-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Foo"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  let srcPath = projectDir </> "src" </> "Foo.hs"
  TIO.writeFile srcPath srcModule
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/Foo.hs" :: Text) ])
  stepFooter 1 t0

  -- Snapshot the file content for the byte-equality oracle.
  before <- TIO.readFile srcPath

  ----------------------------------------------------------------
  -- (2) ask rename_local for a name that does NOT exist
  --
  -- Scope lines 3..4 deliberately span the type signature + body
  -- of 'fooBar' so the request is well-formed — the only thing
  -- wrong is that 'fooBarz' is not a binding in that scope.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "rename_local(fooBarz → fooBarzzz) — target missing"
  r <- Client.callTool c GhcRefactor (object
    [ "action"            .= ("rename_local" :: Text)
    , "module_path"       .= ("src/Foo.hs"   :: Text)
    , "old_name"          .= ("fooBarz"      :: Text)
    , "new_name"          .= ("fooBarzzz"    :: Text)
    , "scope_line_start"  .= (3 :: Int)
    , "scope_line_end"    .= (4 :: Int)
    ])

  ----------------------------------------------------------------
  -- (3) oracle — refusal + file untouched
  ----------------------------------------------------------------
  after <- TIO.readFile srcPath

  let succ_ = fieldBool "success" r
      refused = succ_ == Just False
      untouched = after == before

  cRefused <- liveCheck $ checkPure
    "rename of missing binding · MCP returns success=false (not a silent no-op)"
    refused
    ("A rename whose target does not exist in the scoped window MUST \
     \return success=false. Any other shape (success=true + 0 \
     \changes, or success missing) lets typos ship. Raw: "
     <> truncRender r)

  cUntouched <- liveCheck $ checkPure
    "rename of missing binding · file is byte-for-byte unchanged"
    untouched
    "Even on a refused rename, snapshot-and-compile-verify must leave \
    \the source identical to pre-call. Any mutation here is a \
    \violation of the refactor's rollback invariant."
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (4) sanity check — a SECOND rename that DOES match the binding
  --     must still succeed (the earlier refusal left no sticky
  --     failure state behind).
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "sanity · rename_local(fooBar → foobaz) after refusal works"
  r2 <- Client.callTool c GhcRefactor (object
    [ "action"            .= ("rename_local" :: Text)
    , "module_path"       .= ("src/Foo.hs"   :: Text)
    , "old_name"          .= ("fooBar"       :: Text)
    , "new_name"          .= ("foobaz"       :: Text)
    , "scope_line_start"  .= (3 :: Int)
    , "scope_line_end"    .= (4 :: Int)
    ])
  let happy = fieldBool "success" r2 == Just True
  cHappy <- liveCheck $ checkPure
    "sanity · the happy-path rename still succeeds after the refusal"
    happy
    ("If this fails, the refusal mutated some hidden state (GHCi \
     \session, workflow tracker) that broke subsequent refactors. \
     \Raw: " <> truncRender r2)
  stepFooter 3 t2

  pure [cRefused, cUntouched, cHappy]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
