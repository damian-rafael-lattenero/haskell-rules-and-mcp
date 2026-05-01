-- | Flow: @ghc_fix_warning@ — propose patches for common GHC
-- warning codes.
--
-- Loads a module with a deliberate unused-import warning, asks
-- the fixer for a patch (apply=false → preview), asserts the
-- response shape. Does not apply — the patch write path is
-- unit-tested.
module Scenarios.FlowFixWarning
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkJsonFieldMatches
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

unusedImportSrc :: Text
unusedImportSrc =
  "module Warn where\n\
  \\n\
  \import Data.Map    -- UNUSED: GHC warns with -Wunused-imports\n\
  \\n\
  \greet :: String -> String\n\
  \greet n = \"Hello, \" ++ n\n"

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup — write source with unused import, add to cabal,
  -- add 'containers' dep so Data.Map is reachable.
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + Warn module (unused import)"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("fixwarn-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Warn"] :: [Text]) ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("containers" :: Text)
         , "stanza"  .= ("library" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Warn.hs") unusedImportSrc
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- ghc_fix_warning — ask for a patch, apply=false.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_fix_warning(Warn.hs, line=3, GHC-66111)"
  r <- Client.callTool c GhcFixWarning (object
    [ "module_path" .= ("src/Warn.hs" :: Text)
    , "line"        .= (3 :: Int)
    , "code"        .= ("GHC-66111" :: Text)   -- -Wunused-imports
    , "apply"       .= False
    ])
  c1 <- liveCheck $ checkJsonFieldMatches
          "fix_warning returns a structured response"
          r "success" (\case Bool _ -> True; _ -> False)
          "success must be a Bool (true if fixable, false otherwise)"
  c2 <- liveCheck $ checkPure
          "fix_warning · response carries patch / plan / hint"
          (hasPatchSignals r)
          "the payload should carry at least one of: 'patch', \
          \'plan', 'hint' — any of those surfaces give an agent \
          \something to act on"
  stepFooter 2 t1

  pure [c1, c2]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | A marker predicate: payload is any object that has at least
-- one of the user-facing output fields. Routes through
-- 'lookupField' from 'E2E.Envelope' so the auto-drill kicks in
-- for envelope-wrapped responses (post-#90 these fields live
-- under 'result').
hasPatchSignals :: Value -> Bool
hasPatchSignals v =
  any (\k -> case lookupField k v of
              Just _  -> True
              Nothing -> False)
      ["patch", "plan", "hint", "diff", "new_content"]
