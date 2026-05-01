-- | Flow: rename_local accepts a clean rewrite even when the
-- module has unrelated pre-existing errors (#50).
--
-- Pre-fix behaviour
-- -----------------
-- 'commitWithVerify' rolled the rewrite back if the post-edit
-- diagnostic set was non-empty. If the module had an unrelated
-- typed hole that was already there before the rename, the post-
-- edit set still contained it (because the rename was scoped to
-- a different range), so the verify treated it as evidence the
-- rewrite \"broke something\". The user had to fix every pre-
-- existing diagnostic before any refactor would land — a much
-- weaker contract than the docs implied.
--
-- New contract
-- ------------
-- The verify compares post-edit and pre-edit error sets by
-- (file, line, col, message) key. The rewrite is rejected only
-- when at least one error appears post-edit that wasn't present
-- pre-edit. The response carries 'pre_existing_errors' so the
-- agent sees the pre-existing diagnostics it inherited.
module Scenarios.FlowRefactorPreExistingError
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
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | The repro module from issue #50: a clean binding 'greet' /
-- 'msg' to be renamed, plus an unrelated 'combineSorted' with a
-- typed hole that was there before the rename and should stay.
preExistingErrorSrc :: Text
preExistingErrorSrc = T.unlines
  [ "module Refactor where"
  , ""
  , "import Data.List (sort)"
  , ""
  , "greet :: String -> String"
  , "greet name ="
  , "  let msg = \"Hello, \" ++ name ++ \"!\""
  , "  in msg"
  , ""
  , "double :: Int -> Int"
  , "double x = x * 2"
  , ""
  , "combineSorted :: Ord a => [a] -> [a] -> [a]"
  , "combineSorted xs ys = sort (xs ++ _holeArg)"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + register Refactor + write the source
  -- file with both a clean binding to rename and the unrelated
  -- typed hole that used to block the refactor.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("ref-prerr" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Refactor"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Refactor.hs")
                preExistingErrorSrc

  -- Step 2 — invoke rename_local on lines 6..8 (the body of
  -- 'greet'). Pre-#50 this was rolled back because the load
  -- captured the typed hole on line 14. Post-fix, the diff shows
  -- the hole was there before AND after, so the rewrite is
  -- accepted.
  t0 <- stepHeader 1 "rename_local accepted despite unrelated hole (#50)"
  r <- Client.callTool c GhcRefactor (object
    [ "action"           .= ("rename_local" :: Text)
    , "module_path"      .= ("src/Refactor.hs" :: Text)
    , "old_name"         .= ("msg" :: Text)
    , "new_name"         .= ("greeting" :: Text)
    , "scope_line_start" .= (6 :: Int)
    , "scope_line_end"   .= (8 :: Int)
    ])
  let renameOk = statusOk r == Just True
  cAccept <- liveCheck $ checkPure
    "rename_local returns success=true (was: rolled back by hole)"
    renameOk
    ("Expected success=true; raw: " <> truncRender r)
  stepFooter 1 t0

  -- Step 3 — the rename actually wrote the new content to disk.
  t1 <- stepHeader 2 "file content swapped msg → greeting"
  body <- TIO.readFile (projectDir </> "src" </> "Refactor.hs")
  let renamed = T.isInfixOf "let greeting = " body
              && T.isInfixOf "in greeting" body
              && not (T.isInfixOf "let msg = "  body)
  cContent <- liveCheck $ checkPure
    "Refactor.hs contains 'greeting' and not 'let msg ='"
    renamed
    ("File body did not show the rename. Body:\n" <> body)
  stepFooter 2 t1

  -- Step 4 — the unrelated typed hole is STILL in place. The
  -- rewrite must not have touched lines outside its scope.
  t2 <- stepHeader 3 "unrelated hole survives the rename"
  let holeIntact = T.isInfixOf "_holeArg" body
  cHole <- liveCheck $ checkPure
    "src/Refactor.hs still contains '_holeArg' (unrelated hole)"
    holeIntact
    ("The unrelated hole was lost. Body:\n" <> body)
  stepFooter 3 t2

  -- Step 5 — response shape: pre_existing_errors should be
  -- present and non-empty (the hole), and the compile field
  -- should reflect the diff state.
  t3 <- stepHeader 4 "response surfaces pre_existing_errors (#50)"
  let hasPreExisting = case lookupField "pre_existing_errors" r of
        Just (Array xs) -> not (null xs)
        _               -> False
  cShape <- liveCheck $ checkPure
    "response.pre_existing_errors non-empty (hole was carried over)"
    hasPreExisting
    ( "Expected 'pre_existing_errors' array with the hole. \
      \Got: " <> truncRender r )
  stepFooter 4 t3

  pure [cAccept, cContent, cHole, cShape]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
