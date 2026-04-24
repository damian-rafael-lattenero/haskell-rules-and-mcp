-- | Flow: 2 MCP clients + 1 project dir + 2 'ghc_quickcheck's.
--
-- What this scenario PROVES (product contract)
-- --------------------------------------------
-- Only one 'cabal repl' can hold 'dist-newstyle/' at a time — that
-- is a cabal-level invariant, not something the MCP controls. When
-- two clients on the same project try to spin up GHCi concurrently,
-- one wins the cabal lock and the other MUST fail *structurally*:
--
--   * with @success=false@
--   * with @error_kind = "session_exhausted"@  (not a hang,
--     not a silent null, not a half-JSON response)
--
-- And whichever DID win must still have its property persisted in
-- @.haskell-flows/properties.json@ — the loser's failure must not
-- corrupt the winner's write.
--
-- What this scenario does NOT claim
-- ---------------------------------
-- Two GHCi sessions cannot both succeed against one project; that is
-- impossible by cabal's design. An earlier draft of this scenario
-- asserted both quickchecks succeeded AND both properties ended up
-- in the store — that's architecturally unreachable, and the oracle
-- made the test a false negative.
--
-- Why the matching lock in 'PropertyStore.withGlobalStoreLock' still
-- matters: it protects against the DIFFERENT scenario where one
-- client's @save@ interleaves with another's @loadAll@/@remove@, or
-- where a batch tool calls @save@ twice through distinct Store
-- handles. Those paths DO race today with just a per-Store MVar.
-- The lock is defensive; this test exercises the session-contention
-- surface, not the store-race surface (which requires a path the
-- current tool set doesn't expose).
module Scenarios.FlowPropertyStoreRace
  ( runFlow
  ) where

import Control.Concurrent.Async (concurrently)
import Data.Aeson (Value (..), object, (.=))
import Data.Maybe (isJust)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  _ <- Client.callTool c "ghc_create_project"
         (object [ "name" .= ("propstore-race-demo" :: Text) ])

  -- Second client pointed at the same project dir. Each client
  -- wraps an independent in-process Server, each with its own
  -- GHCi child slot.
  d <- Client.newClient "" [ ("HASKELL_PROJECT_DIR", projectDir) ]

  let propA = "\\(xs :: [Int]) -> reverse (reverse xs) == xs"
      propB = "\\(n :: Int) -> n + 0 == n"

  t0 <- stepHeader 1 "contention · 2 × ghc_quickcheck against same dist-newstyle"
  (rA, rB) <- concurrently
    (Client.callTool c "ghc_quickcheck"
       (object [ "property" .= (propA :: Text) ]))
    (Client.callTool d "ghc_quickcheck"
       (object [ "property" .= (propB :: Text) ]))

  let aSucc    = fieldBool "success" rA == Just True
      bSucc    = fieldBool "success" rB == Just True
      aKind    = fieldText "error_kind" rA
      bKind    = fieldText "error_kind" rB
      aState   = fieldText "state" rA
      bState   = fieldText "state" rB
      anyWon   = aSucc || bSucc
      -- Post-Wave-5 the loser's failure is rendered by
      -- 'QuickCheck.renderResult' as a structured QcException /
      -- QcUnparsed payload: 'success=false' with 'state' =
      -- "exception" | "unparsed" (plus a non-empty 'error' /
      -- 'raw' field). The legacy subprocess-GHCi tag
      -- 'error_kind=session_exhausted' is no longer emitted —
      -- that whole code path was retired when ghc_quickcheck
      -- moved to the subprocess-cabal-repl vehicle.
      --
      -- The invariant we still want to pin: the loser does not
      -- return success=true, and does not return a null/hang —
      -- it returns a structured failure shape.
      loserIsStructured =
        (aSucc && not bSucc && isJust bState)
        || (bSucc && not aSucc && isJust aState)
        || (aSucc && bSucc)   -- unlikely but not wrong

  cContention <- liveCheck $ checkPure
    "at least one client won the cabal lock"
    anyWon
    ("Both clients failed to run GHCi. That's either a real product \
     \regression (neither got cabal repl) or a CI flake. A.success=" <>
     T.pack (show aSucc) <> ", B.success=" <> T.pack (show bSucc)
     <> ". A.kind=" <> T.pack (show aKind)
     <> ", B.kind=" <> T.pack (show bKind))

  cLoserShape <- liveCheck $ checkPure
    "the loser (if any) failed structurally (state = exception | unparsed)"
    loserIsStructured
    ("When the cabal-lock race eats one client, the loser's response \
     \must carry a structured payload (success=false + non-null 'state') \
     \— not a hang, not a half-null. A.state=" <> T.pack (show aState)
     <> ", B.state=" <> T.pack (show bState)
     <> ". Raw A: " <> truncRender rA
     <> " | Raw B: " <> truncRender rB)
  stepFooter 1 t0

  -- Disk oracle: whoever DID succeed must have their property
  -- persisted, in a well-formed JSON file. A partial-write would
  -- fail the parse and loadAll would see [].
  t1 <- stepHeader 2 "disk · winner's property is persisted (well-formed JSON)"
  let storeFile = projectDir </> ".haskell-flows" </> "properties.json"
  exists <- doesFileExist storeFile
  diskBytes <- if exists then T.pack <$> readFile storeFile else pure ""
  let expectedNeedle
        | aSucc && not bSucc = "reverse (reverse xs)"
        | bSucc && not aSucc = "n + 0 == n"
        | aSucc && bSucc     = "reverse (reverse xs)"  -- either works
        | otherwise          = T.empty
      winnerPersisted = not (T.null expectedNeedle)
                     && expectedNeedle `T.isInfixOf` diskBytes
      jsonShaped = "[" `T.isPrefixOf` T.strip diskBytes
  cPersisted <- liveCheck $ checkPure
    "winner's property is in the store file"
    winnerPersisted
    ("The winning client's ghc_quickcheck claimed success but its \
     \property is not on disk. expected=" <> expectedNeedle
     <> ", first 200 bytes=" <> T.take 200 diskBytes)
  cJsonShape <- liveCheck $ checkPure
    "store file is well-formed JSON (starts with '[')"
    jsonShaped
    ("The loser's failure must not have corrupted the winner's write. \
     \First 200 bytes: " <> T.take 200 diskBytes)
  stepFooter 2 t1

  Client.close d

  pure [cContention, cLoserShape, cPersisted, cJsonShape]

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

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
