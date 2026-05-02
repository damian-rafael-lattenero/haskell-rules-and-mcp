-- | Flow: 'ghc_load(diagnostics=true)' on a module with a typed
-- hole returns exactly ONE error, not two (#57).
--
-- Pre-fix behaviour
-- -----------------
-- The deferred-pass GHC runs under @diagnostics=true@ emits both
-- the real typed-hole diagnostic AND a follow-up
-- @"<interactive>:1:1: error: [GHC-58427] ... is not loaded"@
-- as a side-effect of trying to compute downstream context for
-- a module that didn't fully load. The second entry has no
-- location, no code, and no information value — it's an
-- internal artifact of the deferred pass.
--
-- New contract
-- ------------
-- 'filterArtifacts' drops the GHC-58427 entry whenever the load
-- produced at least one other diagnostic, so the response has
-- one error per real problem.
module Scenarios.FlowLoadHoleDiagnostics
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
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | A module with a single typed hole — the canonical
-- repro from the bug report.
holeSrc :: Text
holeSrc = T.unlines
  [ "module HoleArtifact where"
  , ""
  , "import Data.List (sort)"
  , ""
  , "combineSorted :: Ord a => [a] -> [a] -> [a]"
  , "combineSorted xs ys = sort (xs ++ _holeArg)"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + register the module so the cabal-aware
  -- bootstrap finds it under the library stanza.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("hole-artifact-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["HoleArtifact"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "HoleArtifact.hs") holeSrc

  -- Step 2 — load with diagnostics=true.  The response used to put
  -- the hole into 'errors' alongside the GHC-58427 'is not loaded'
  -- artifact.  Post F-23, mergeDiags dedups by (file, line, col)
  -- and prefers the deferred pass — typed holes correctly land in
  -- 'warnings' (severity SevWarning under -fdefer-typed-holes), and
  -- 'errors' should be empty (no GHC-58427 artifact, no shadow-error).
  t0 <- stepHeader 1 "ghc_load(diagnostics=true) returns one warning per hole (#57, F-23)"
  r <- Client.callTool c GhcLoad (object
         [ "module_path" .= ("src/HoleArtifact.hs" :: Text)
         , "diagnostics" .= True
         ])
  let errs           = errorsArray r
      warns          = warningsArray r
      hasHoleWarn    = any (T.isInfixOf "GHC-88464")  warns
                    || any (T.isInfixOf "Found hole") warns
      hasErrArtifact = any (T.isInfixOf "GHC-58427") errs
  cFiltered <- liveCheck $ checkPure
    "warnings carry the hole; errors has no GHC-58427 artifact"
    (hasHoleWarn && not hasErrArtifact)
    ( "Expected: warnings=[hole], errors free of GHC-58427. Got: "
      <> "errors=" <> T.pack (show errs)
      <> ", warnings=" <> T.pack (show warns)
      <> ". Raw: " <> truncRender r )
  stepFooter 1 t0

  pure [cFiltered]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Pull every @errors[].message@ string from the load response.
errorsArray :: Value -> [Text]
errorsArray = diagsArrayBy "errors"

-- | Same shape extractor for the @warnings@ field. F-23 moved typed
-- holes from @errors@ → @warnings@ when @diagnostics=true@.
warningsArray :: Value -> [Text]
warningsArray = diagsArrayBy "warnings"

diagsArrayBy :: Text -> Value -> [Text]
diagsArrayBy key v = case lookupField key v of
  Just (Array xs) ->
    [ msg | Object o <- V.toList xs
          , Just (String msg) <- [KeyMap.lookup (Key.fromText "message") o]
    ]
  _ -> []

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 800
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
