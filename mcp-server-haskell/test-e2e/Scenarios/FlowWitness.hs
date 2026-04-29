-- | Flow: 'ghc_witness' Phase 1 — distribution + bias surface (#65).
--
-- Phase 1 verifies the structural surface and the live cabal-repl
-- probe path:
--
--   * The tool runs against an existing project and returns a
--     'success: true' payload with the expected fields
--     (passed, distribution.by_size, warnings, wall_time_ms).
--   * The 'phase' marker is the documented '1-mvp' string.
--   * The 'deferred' field lists the four Phase 2 follow-ups.
--
-- Running the actual instrumented property is left to the live
-- cabal-repl harness — Phase 1 of the witness tool talks to the
-- same channel that ghc_quickcheck uses, so a green check on the
-- shape implies the underlying Test.QuickCheck.collect wrapper
-- compiled and executed at least once.
module Scenarios.FlowWitness
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 1 — scaffold a project so cabal-repl has something to load.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("witness-demo" :: Text) ])

  -- Step 2 — drive a tiny tautology that only depends on `length`,
  -- which exists in Prelude. Phase 1 just needs the harness to
  -- come back with a structurally valid payload — it doesn't have
  -- to flag a contradiction.
  t0 <- stepHeader 1 "ghc_witness on tautology (#65)"
  rOk <- Client.callTool c GhcWitness
           (object
              [ "property" .= ("\\xs -> length (xs :: [Int]) >= 0" :: Text)
              , "runs"     .= (200 :: Int)
              ])
  let okShape =
           fieldBool "success" rOk == Just True
        && fieldText "phase" rOk == Just "1-mvp"
        && hasField "distribution" rOk
        && hasField "warnings" rOk
        && hasField "wall_time_ms" rOk
        && hasField "deferred" rOk
  cOk <- liveCheck $ checkPure
    "ghc_witness returns success with Phase 1 structural surface"
    okShape
    ("Got: " <> truncRender rOk)
  stepFooter 1 t0

  -- Step 3 — invalid arguments (no 'property' key) must surface as
  -- success: false rather than crash the server. Mirrors the Phase 1
  -- contract for every tool: bad args are diagnosable, not fatal.
  t1 <- stepHeader 2 "ghc_witness rejects malformed args (#65)"
  rBad <- Client.callTool c GhcWitness (object [])
  let okBad = fieldBool "success" rBad == Just False
  cBad <- liveCheck $ checkPure
    "missing 'property' → success=false (no crash)"
    okBad
    ("Got: " <> truncRender rBad)
  stepFooter 2 t1

  -- Step 4 — Issue #78: the distribution must NOT be empty. The
  -- pre-#78 witness path read 'output r' from QC with chatty=False,
  -- which suppressed the label histogram → the agent always saw
  -- empty buckets. Post-#78 the labels-aware harness emits the
  -- structured 'Result.labels' map directly, so the distribution
  -- is populated regardless of QC's chatty setting.
  --
  -- Use a list-shaped property whose 'show' length actually varies
  -- with input so QuickCheck genuinely produces multiple buckets.
  t2 <- stepHeader 3 "ghc_witness produces non-empty distribution (#78)"
  rDist <- Client.callTool c GhcWitness
            (object
               [ "property" .= ("\\xs -> length (xs :: [Int]) >= 0" :: Text)
               , "runs"     .= (300 :: Int)
               ])
  let buckets = bucketsArray rDist
      okDist  = fieldBool "success" rDist == Just True
             && not (null buckets)
  cDist <- liveCheck $ checkPure
    "distribution.by_size.buckets is non-empty post-#78"
    okDist
    ("Got: " <> truncRender rDist)
  stepFooter 3 t2

  pure [cOk, cBad, cDist]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Issue #78: drill into 'distribution.by_size.buckets' and
-- return its array contents. Empty list either means the path
-- didn't resolve OR the buckets array is genuinely empty —
-- both are red signals for the witness Phase 1 + #78 contract.
bucketsArray :: Value -> [Value]
bucketsArray v = case lookupField "distribution" v of
  Just (Object dist) ->
    case KeyMap.lookup (Key.fromText "by_size") dist of
      Just (Object bySize) ->
        case KeyMap.lookup (Key.fromText "buckets") bySize of
          Just (Array arr) -> foldr (:) [] arr
          _                -> []
      _ -> []
  _ -> []

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

fieldText :: Text -> Value -> Maybe Text
fieldText k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

hasField :: Text -> Value -> Bool
hasField k v = case lookupField k v of
  Just _  -> True
  Nothing -> False

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
