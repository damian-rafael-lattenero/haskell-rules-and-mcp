-- | Flow: 'ghc_fix_warning' applies a concrete patch for
-- GHC-40910 (unused binding) when the caller supplies the
-- binding 'name' (#55).
--
-- Pre-fix behaviour
-- -----------------
-- The tool returned 'success: true' with 'applied: false' and a
-- prose hint asking the agent to underscore-prefix the binding.
-- Agents had no machine-readable signal that GHC-40910 was
-- advice-only — the tool's NAME promised an action it didn't
-- deliver.
--
-- New contract
-- ------------
-- * Without 'name' → fixable=false, applied=false, hint asks for
--   the name.
-- * With 'name' + apply=true → fixable=true, applied=true, the
--   binding is rewritten in place (`x` → `_x`), and the next
--   ghc_check_module no longer reports GHC-40910.
module Scenarios.FlowFixWarningUnusedBinding
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
import E2E.Envelope (fieldBool)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | A function with one unused parameter. GHC reports
-- @[GHC-40910] Defined but not used: 'ys'@ on the binding line.
unusedBindingSrc :: Text
unusedBindingSrc = T.unlines
  [ "module FixDemo where"
  , ""
  , "import Data.List (sort)"
  , ""
  , "combineSorted :: Ord a => [a] -> [a] -> [a]"
  , "combineSorted xs ys = sort xs"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + write the source. The unused 'ys' is on
  -- line 6 (1-indexed) of FixDemo.hs.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("fix-warning-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["FixDemo"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "FixDemo.hs") unusedBindingSrc

  -- Step 2 — call WITHOUT name. fixable=false, no patch.
  t0 <- stepHeader 1 "ghc_fix_warning(40910) sin name → fixable=false (#55)"
  rNoName <- Client.callTool c GhcFixWarning (object
    [ "module_path" .= ("src/FixDemo.hs" :: Text)
    , "line"        .= (6 :: Int)
    , "code"        .= ("GHC-40910" :: Text)
    ])
  let fixableNo = fieldBool "fixable" rNoName == Just False
      appliedNo = fieldBool "applied" rNoName == Just False
  cNoName <- liveCheck $ checkPure
    "no-name call: fixable=false, applied=false"
    (fixableNo && appliedNo)
    ("Expected fixable=false, applied=false. Got: " <> truncRender rNoName)
  stepFooter 1 t0

  -- Step 3 — call WITH name + apply=true. fixable=true, applied=true.
  t1 <- stepHeader 2 "ghc_fix_warning(40910, name=ys, apply) → patched (#55)"
  rApply <- Client.callTool c GhcFixWarning (object
    [ "module_path" .= ("src/FixDemo.hs" :: Text)
    , "line"        .= (6 :: Int)
    , "code"        .= ("GHC-40910" :: Text)
    , "name"        .= ("ys" :: Text)
    , "apply"       .= True
    ])
  let fixableYes = fieldBool "fixable" rApply == Just True
      appliedYes = fieldBool "applied" rApply == Just True
  cApplied <- liveCheck $ checkPure
    "with-name+apply: fixable=true, applied=true"
    (fixableYes && appliedYes)
    ("Expected fixable=true, applied=true. Got: " <> truncRender rApply)
  stepFooter 2 t1

  -- Step 4 — file content was rewritten with '_ys'.
  t2 <- stepHeader 3 "FixDemo.hs binding now reads '_ys' (#55)"
  body <- TIO.readFile (projectDir </> "src" </> "FixDemo.hs")
  let renamed = "combineSorted xs _ys = sort xs" `T.isInfixOf` body
              && not ("combineSorted xs ys = " `T.isInfixOf` body)
  cFile <- liveCheck $ checkPure
    "source line patched in place"
    renamed
    ("File body did not show the rename. Body:\n" <> body)
  stepFooter 3 t2

  pure [cNoName, cApplied, cFile]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
