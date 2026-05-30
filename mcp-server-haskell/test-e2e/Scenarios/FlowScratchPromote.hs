-- | Flow: 'ghc_scratch' write → check → promote round-trip (#253, #272).
--
-- The scratchpad lifecycle had unit coverage only for its FAILURE
-- branches (promote/check with a missing id or target_module). This
-- scenario exercises the SUCCESS path end-to-end against a live MCP +
-- GHC session — the gap #272 calls out:
--
--   * write    — persist a Prelude-only declaration under an auto id,
--   * check    — type-check it (→ status 'verified', kind 'type_ok'),
--   * promote  — splice it into the scaffolded module with target_line
--                OMITTED (→ appended to end; snapshot-and-compile-verify
--                commits because the binding keeps the module compiling),
--   * reload   — prove the spliced binding actually compiles, so promote
--                wrote valid code rather than merely reporting success,
--   * list     — confirm the entry's status flipped to 'promoted'.
module Scenarios.FlowScratchPromote
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe, isJust)
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
import E2E.Envelope (fieldText, lookupField, statusOk)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 0 — scaffold a project so promote has a real module + GHC
  -- context to splice into and recompile. The stub exports 'greet'.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text)
                 , "name"   .= ("scratch-promote-demo" :: Text)
                 ])
  _ <- Client.callTool c GhcLoad (object [ "module_path" .= modPath ])

  -- Step 1 — write a Prelude-only declaration; expect an Open entry id.
  t0 <- stepHeader 1 "ghc_scratch write persists an entry"
  wR <- Client.callTool c GhcScratch
          (object [ "action" .= ("write" :: Text)
                  , "code"   .= declCode
                  , "kind"   .= ("hypothesis" :: Text)
                  ])
  let mEntryId = fieldText "id" wR
      okWrite  = statusOk wR == Just True && isJust mEntryId
  cWrite <- liveCheck $ checkPure
    "write → success + an entry id"
    okWrite
    ("Got: " <> truncRender wR)
  stepFooter 1 t0

  let entryId = fromMaybe "scratch-1" mEntryId

  -- Step 2 — check it: the declaration must type-check.
  t1 <- stepHeader 2 "ghc_scratch check verifies the declaration"
  kR <- Client.callTool c GhcScratch
          (object [ "action" .= ("check" :: Text)
                  , "id"     .= entryId
                  ])
  let okCheck = statusOk kR == Just True
             && fieldText "kind" kR == Just "type_ok"
  cCheck <- liveCheck $ checkPure
    "check → kind=type_ok (verified)"
    okCheck
    ("Got: " <> truncRender kR)
  stepFooter 2 t1

  -- Step 3 — promote into the scaffolded module. target_line is omitted,
  -- so the code is appended to the end of the file; snapshot-and-compile-
  -- verify commits because the Prelude-only binding keeps it compiling.
  t2 <- stepHeader 3 "ghc_scratch promote splices + verifies"
  pR <- Client.callTool c GhcScratch
          (object [ "action"        .= ("promote" :: Text)
                  , "id"            .= entryId
                  , "target_module" .= modPath
                  ])
  let okPromote = statusOk pR == Just True
               && fieldText "kind" pR == Just "promoted"
  cPromote <- liveCheck $ checkPure
    "promote → success, kind=promoted (snapshot committed)"
    okPromote
    ("Got: " <> truncRender pR)
  stepFooter 3 t2

  -- Step 4 — reload: the spliced binding must compile, proving promote
  -- wrote valid code (not just that it reported success).
  t3 <- stepHeader 4 "promoted module reloads clean"
  rl <- Client.callTool c GhcLoad (object [ "module_path" .= modPath ])
  let okReload = statusOk rl == Just True
  cReload <- liveCheck $ checkPure
    "ghc_load on the promoted module succeeds"
    okReload
    ("Got: " <> truncRender rl)
  stepFooter 4 t3

  -- Step 5 — the entry's status must now be 'promoted' in the store.
  t4 <- stepHeader 5 "scratch list shows the entry promoted"
  lR <- Client.callTool c GhcScratch (object [ "action" .= ("list" :: Text) ])
  let okList = statusOk lR == Just True && promotedCount lR >= 1
  cList <- liveCheck $ checkPure
    "list → counts.promoted >= 1"
    okList
    ("Got: " <> truncRender lR)
  stepFooter 5 t4

  pure [cWrite, cCheck, cPromote, cReload, cList]

--------------------------------------------------------------------------------
-- fixtures + helpers
--------------------------------------------------------------------------------

-- | The module 'ghc_project create' scaffolds for package
-- "scratch-promote-demo" (PascalCased), relative to the project root.
modPath :: Text
modPath = "src/ScratchPromoteDemo.hs"

-- | A Prelude-only multi-line declaration: type-checks in isolation AND
-- compiles when appended to the scaffold (needs no extra imports). The
-- unused-binding warning it triggers under -Wall is a warning, not an
-- error, so promote's snapshot-and-compile-verify commits it.
declCode :: Text
declCode = "triple :: Int -> Int\ntriple x = x * 3"

-- | Drill into 'counts.promoted' from a scratch list response. Any
-- non-Number / missing path reads as 0 — a red signal for the assertion.
promotedCount :: Value -> Int
promotedCount v = case lookupField "counts" v of
  Just (Object cs) -> case KeyMap.lookup (Key.fromText "promoted") cs of
    Just (Number n) -> round n
    _               -> 0
  _ -> 0

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
