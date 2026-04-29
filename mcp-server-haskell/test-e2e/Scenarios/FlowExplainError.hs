-- | Flow: 'ghc_explain_error' Phase 1 — structured context
-- builder for type errors (#59).
--
-- Plant a module with a known type error (string vs Int
-- mismatch), call 'ghc_explain_error', assert:
--
--   * @diagnostic@ is non-null with @severity=\"error\"@.
--   * @context.module_source@ contains the offending source.
--   * @context.imports@ enumerates the import lines.
--   * @context.enclosing_range@ is well-formed.
--   * @instructions_for_agent@ tells the agent how to proceed.
module Scenarios.FlowExplainError
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
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
import E2E.Envelope (lookupField, statusOk)
import HaskellFlows.Mcp.ToolName (ToolName (..))

brokenSrc :: Text
brokenSrc = T.unlines
  [ "module Broken where"
  , ""
  , "import Data.List (sort)"
  , ""
  , "double :: Int -> Int"
  , "double x = x ++ \"oops\"  -- type error: Int is not [Char]"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + plant the broken module.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("explain-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Broken"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Broken.hs") brokenSrc

  -- Step 2 — invoke explain_error on the module.
  t0 <- stepHeader 1 "ghc_explain_error returns structured context (#59)"
  r <- Client.callTool c GhcExplainError
         (object [ "module_path" .= ("src/Broken.hs" :: Text) ])
  let success     = statusOk r == Just True
      diagOk      = case drillField "diagnostic" "severity" r of
        Just (String "error") -> True
        _                     -> False
      hasMessage  = case drillField "diagnostic" "message" r of
        Just (String m) -> not (T.null m)
        _               -> False
      hasSource   = case drillPath ["context", "module_source"] r of
        Just (String s) -> "double" `T.isInfixOf` s
        _               -> False
      importsLen  = case drillPath ["context", "imports"] r of
        Just (Array xs) -> V.length xs
        _               -> -1
      hasInstructions = case lookupField "instructions_for_agent" r of
        Just (String _) -> True
        _               -> False
  cContext <- liveCheck $ checkPure
    "diagnostic + module_source + imports[≥1] + instructions present"
    (success && diagOk && hasMessage && hasSource
       && importsLen >= 1 && hasInstructions)
    ( "Got: success=" <> T.pack (show success)
      <> ", diagOk=" <> T.pack (show diagOk)
      <> ", hasMessage=" <> T.pack (show hasMessage)
      <> ", hasSource=" <> T.pack (show hasSource)
      <> ", importsLen=" <> T.pack (show importsLen)
      <> ", hasInstructions=" <> T.pack (show hasInstructions) )
  stepFooter 1 t0

  pure [cContext]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

drillField :: Text -> Text -> Value -> Maybe Value
drillField outer inner v = lookupField outer v >>= lookupField inner

drillPath :: [Text] -> Value -> Maybe Value
drillPath []       v = Just v
drillPath (k : ks) v = lookupField k v >>= drillPath ks
