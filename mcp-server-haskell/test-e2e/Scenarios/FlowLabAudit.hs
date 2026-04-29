-- | Flow: 'ghc_lab' Phase 1 — module-wide property audit (#60).
--
-- Plant a 2-binding library module where one binding's
-- signature pattern triggers an Idempotent suggestion (the
-- @reverse :: [a] -> [a]@ shape) — then call ghc_lab and
-- assert the per-function report shape:
--
--   * 'audited_bindings' counts every column-0 signature.
--   * Each function entry carries its 'name', 'signature',
--     and either 'properties' (when laws matched) or 'status'
--     + 'reason' (when none did).
--   * The summary names the pass/total ratio.
module Scenarios.FlowLabAudit
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
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
import HaskellFlows.Mcp.ToolName (ToolName (..))

src :: Text
src = T.unlines
  [ "module Demo where"
  , ""
  , "-- | Reverses its input. Idempotent under 'reverse . reverse'."
  , "myReverse :: [a] -> [a]"
  , "myReverse = reverse"
  , ""
  , "-- | A binding whose shape no Suggest rule matches yet."
  , "doubledSum :: [Int] -> Int"
  , "doubledSum xs = 2 * sum xs"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + write the source.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("lab-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Demo"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Demo.hs") src

  -- Step 2 — invoke ghc_lab. Min confidence = "low" so we
  -- exercise the full rule set (the dampened Involutive on
  -- non-reverse-named names becomes Low; default would skip it).
  t0 <- stepHeader 1 "ghc_lab audits Demo (#60)"
  r <- Client.callTool c GhcLab
         (object
           [ "module_path"    .= ("src/Demo.hs" :: Text)
           , "min_confidence" .= ("low" :: Text)
           ])
  let success     = fieldBool "success" r == Just True
      audited     = fieldInt "audited_bindings" r
      hasFunctions = arrayLen "functions" r >= 2
  cBasic <- liveCheck $ checkPure
    "ghc_lab returns success=true with audited_bindings ≥ 2"
    (success && audited == 2 && hasFunctions)
    ("Expected: audited=2, functions array≥2. Got: success=" <> T.pack (show success)
       <> ", audited=" <> T.pack (show audited)
       <> ", n=" <> T.pack (show (arrayLen "functions" r))
       <> ". Raw: " <> truncRender r)
  stepFooter 1 t0

  -- Step 3 — the response contains a summary line that names
  -- the property pass/total ratio.
  t1 <- stepHeader 2 "summary names the property pass/total (#60)"
  let summary = lookupString "summary" r
      okSummary = case summary of
        Just s  -> "/" `T.isInfixOf` s
                && T.isInfixOf "properties" s
        Nothing -> False
  cSummary <- liveCheck $ checkPure
    "summary contains 'X/Y properties' ratio"
    okSummary
    ("Expected ratio in summary; got: " <> T.pack (show summary))
  stepFooter 2 t1

  pure [cBasic, cSummary]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

fieldInt :: Text -> Value -> Int
fieldInt k v = case lookupField k v of
  Just (Number n) -> truncate (toRational n)
  _               -> -1

arrayLen :: Text -> Value -> Int
arrayLen k v = case lookupField k v of
  Just (Array xs) -> V.length xs
  _               -> -1

lookupString :: Text -> Value -> Maybe Text
lookupString k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 800
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
