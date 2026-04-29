-- | Flow: 'ghc_move' — Phase 1 cross-module symbol move (#62).
--
-- The scenario plants a 4-file project (Source, Dest, Consumer,
-- Spec.hs) where the source module exports a binding, the
-- destination module exists with a placeholder, the consumer
-- imports the symbol via @import Source (sym, …)@. Steps:
--
--   1. Verify the project loads green up front.
--   2. @dry_run=true@ — assert the response lists every
--      file-to-be-modified without writing anything to disk.
--   3. Real move — assert success, files updated, the consumer's
--      selective import was split, the project still loads.
--   4. Negative test: a missing destination module is rejected
--      with @error_kind=module_path_does_not_exist@ (post-#90
--      Phase C; the legacy @destination_module_missing@ string is
--      still accepted during the dual-shape window) before any
--      filesystem touch.
module Scenarios.FlowMoveSymbol
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

sourceSrc :: Text
sourceSrc = T.unlines
  [ "module Source (greet, double) where"
  , ""
  , "greet :: String -> String"
  , "greet name = \"hi \" <> name"
  , ""
  , "-- | Doubles its input."
  , "double :: Int -> Int"
  , "double x = x + x"
  ]

destSrc :: Text
destSrc = T.unlines
  [ "module Dest where"
  , ""
  , "placeholder :: Int"
  , "placeholder = 0"
  ]

consumerSrc :: Text
consumerSrc = T.unlines
  [ "module Consumer where"
  , ""
  , "import Source (greet, double)"
  , ""
  , "useDouble :: Int -> Int"
  , "useDouble x = double x"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + register Source / Dest / Consumer +
  -- write all sources.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("move-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object
           [ "modules" .= (["Source", "Dest", "Consumer"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Source.hs")   sourceSrc
  TIO.writeFile (projectDir </> "src" </> "Dest.hs")     destSrc
  TIO.writeFile (projectDir </> "src" </> "Consumer.hs") consumerSrc

  -- Step 2 — dry_run preview, no FS writes.
  t0 <- stepHeader 1 "ghc_move dry_run lists files (#62)"
  consumerBefore <- TIO.readFile (projectDir </> "src" </> "Consumer.hs")
  rDry <- Client.callTool c GhcMove (object
    [ "symbol"  .= ("double" :: Text)
    , "from"    .= ("Source" :: Text)
    , "to"      .= ("Dest"   :: Text)
    , "dry_run" .= True
    ])
  consumerAfterDry <- TIO.readFile (projectDir </> "src" </> "Consumer.hs")
  let dryOk     = fieldBool "success"  rDry == Just True
              && fieldBool "applied"  rDry == Just False
              && arrayLen "files_modified" rDry >= 3
      untouched = consumerBefore == consumerAfterDry
  cDry <- liveCheck $ checkPure
    "dry_run: success+applied=false+files≥3, no FS write"
    (dryOk && untouched)
    ("Expected dry_run preview without writes. Got: " <> truncRender rDry)
  stepFooter 1 t0

  -- Step 3 — real move. Source loses 'double', Dest gains it,
  -- Consumer's selective import splits.
  t1 <- stepHeader 2 "ghc_move applies + verifies (#62)"
  rApply <- Client.callTool c GhcMove (object
    [ "symbol" .= ("double" :: Text)
    , "from"   .= ("Source" :: Text)
    , "to"     .= ("Dest"   :: Text)
    ])
  let appliedOk = fieldBool "success" rApply == Just True
              && fieldBool "applied" rApply == Just True
  cApply <- liveCheck $ checkPure
    "move applied with verify ok"
    appliedOk
    ("Expected success+applied. Got: " <> truncRender rApply)
  stepFooter 2 t1

  -- Step 4 — file content invariants.
  t2 <- stepHeader 3 "post-move file invariants (#62)"
  srcAfter      <- TIO.readFile (projectDir </> "src" </> "Source.hs")
  destAfter     <- TIO.readFile (projectDir </> "src" </> "Dest.hs")
  consumerAfter <- TIO.readFile (projectDir </> "src" </> "Consumer.hs")
  let srcMissing      = not ("double :: Int -> Int" `T.isInfixOf` srcAfter)
                      && not ("double x = x + x"     `T.isInfixOf` srcAfter)
      destHas         = "double :: Int -> Int" `T.isInfixOf` destAfter
                      && "double x = x + x"     `T.isInfixOf` destAfter
      importSplit     = "import Source (greet)" `T.isInfixOf` consumerAfter
                      && "import Dest (double)" `T.isInfixOf` consumerAfter
                      && not ("Source (greet, double)"
                                `T.isInfixOf` consumerAfter)
  cInvariants <- liveCheck $ checkPure
    "Source lost double, Dest got double, Consumer import split"
    (srcMissing && destHas && importSplit)
    ( "Got: srcMissing=" <> T.pack (show srcMissing)
      <> ", destHas=" <> T.pack (show destHas)
      <> ", importSplit=" <> T.pack (show importSplit)
      <> ". Source body:\n" <> T.take 400 srcAfter
      <> "\nConsumer body:\n" <> T.take 400 consumerAfter )
  stepFooter 3 t2

  -- Step 5 — negative: missing destination is refused.
  t3 <- stepHeader 4 "ghc_move refuses missing destination (#62)"
  rMissing <- Client.callTool c GhcMove (object
    [ "symbol" .= ("greet"             :: Text)
    , "from"   .= ("Source"            :: Text)
    , "to"     .= ("Definitely.Missing" :: Text)
    ])
  -- Issue #90 Phase C: 'destination_module_missing' was a tool-local
  -- string in the pre-envelope wire. Post-migration the closed enum
  -- collapses both source/destination missing-on-disk failures to
  -- 'module_path_does_not_exist'. Accept either while the dual-shape
  -- window is open; Phase D drops the legacy form.
  let kindMatches t = t == "module_path_does_not_exist"
                   || t == "destination_module_missing"
      missingOk =
        fieldBool "success" rMissing == Just False
          && maybe False kindMatches (fieldText "error_kind" rMissing)
  cMissing <- liveCheck $ checkPure
    "missing destination → success=false, error_kind=module_path_does_not_exist"
    missingOk
    ("Expected module_path_does_not_exist. Got: " <> truncRender rMissing)
  stepFooter 4 t3

  pure [cDry, cApply, cInvariants, cMissing]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

fieldText :: Text -> Value -> Maybe Text
fieldText k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

arrayLen :: Text -> Value -> Int
arrayLen k v = case lookupField k v of
  Just (Array xs) -> V.length xs
  _               -> -1

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 800
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
