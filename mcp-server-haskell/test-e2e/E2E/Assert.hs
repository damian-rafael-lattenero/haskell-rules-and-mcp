-- | Tiny assertion + result-aggregation layer for the E2E suite.
--
-- The scenario builds up a @[Check]@ list by calling 'checkThat'
-- on each step's result. 'renderReport' prints pass/fail per
-- check and 'allPassed' folds them into a final exit code.
--
-- No dependency on hspec or tasty — the E2E suite is a single
-- linear narrative, not a nested tree of groups, and the simpler
-- the framework the fewer moving parts can break mid-scenario.
module E2E.Assert
  ( Check (..)
  , checkThat
  , checkPure
  , checkJsonField
  , checkJsonFieldMatches
  , renderReport
  , allPassed
    -- * Streaming progress (hang-debug friendly)
  , stepHeader
  , stepFooter
  , liveCheck
  , beginSection
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.IO (hFlush, stdout)

-- | One step's outcome. 'cDetail' carries the reason for a failure
-- so the report points the reader straight at what broke.
data Check = Check
  { cName   :: !Text
  , cOk     :: !Bool
  , cDetail :: !Text
  }
  deriving stock (Show)

-- | Wrap a boolean predicate over an IO-produced value.
checkThat :: Text -> IO Value -> (Value -> Bool) -> Text -> IO Check
checkThat name iov pred_ detailOnFail = do
  v <- iov
  pure Check
    { cName   = name
    , cOk     = pred_ v
    , cDetail = if pred_ v then "" else detailOnFail <> " — got: " <> renderValue v
    }

-- | Pure predicate check, no IO. Useful for filesystem assertions.
checkPure :: Text -> Bool -> Text -> Check
checkPure name ok detail = Check
  { cName   = name
  , cOk     = ok
  , cDetail = if ok then "" else detail
  }

-- | Convenience: assert a top-level JSON field equals an expected
-- value (as a 'Value' literal so the caller can check bools,
-- strings, numbers).
checkJsonField :: Text -> Value -> Text -> Value -> Check
checkJsonField name payload key expected = Check
  { cName = name
  , cOk   = lookupField key payload == Just expected
  , cDetail = case lookupField key payload of
      Just actual -> "expected " <> renderValue expected
                   <> ", got " <> renderValue actual
      Nothing     -> "field " <> key <> " not present in " <> renderValue payload
  }

-- | Convenience: assert a top-level JSON field matches an arbitrary
-- predicate. Useful for 'state' = "passed" / "failed" unions, or
-- for count fields where only a lower bound matters.
checkJsonFieldMatches
  :: Text -> Value -> Text -> (Value -> Bool) -> Text -> Check
checkJsonFieldMatches name payload key pred_ detail = Check
  { cName   = name
  , cOk     = maybe False pred_ (lookupField key payload)
  , cDetail = case lookupField key payload of
      Just actual -> detail <> " — got: " <> renderValue actual
      Nothing     -> "field " <> key <> " not present"
  }

-- | Look up a field with two backwards-compat layers (issue #90
-- Phase D step 2):
--
-- 1. Top-level lookup (the original pre-envelope behaviour).
-- 2. Auto-drill through @result@ for tool-specific payload
--    fields (@type@, @output@, @file@, @raw@, @holes@, etc.)
--    that moved under @result@ post-#90 envelope.
-- 3. Synthesise the deprecated top-level @success@ and
--    @error_kind@ keys from the canonical @status@ /
--    @error.kind@ when an oracle still queries them by name.
--    This is purely a TEST-side shim so the 60+ pre-existing
--    @checkJsonField "..." "success" (Bool True)@ assertions
--    keep working without a 60-file mechanical sweep — the
--    wire format itself no longer emits those fields (see
--    'HaskellFlows.Mcp.Envelope' Phase D step 2 final).
--
-- Mirrors 'E2E.Envelope.lookupField' for the auto-drill half.
lookupField :: Text -> Value -> Maybe Value
lookupField k v@(Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just inner -> Just inner
  Nothing -> case k of
    "success"    -> synthesizeSuccess v
    "error_kind" -> synthesizeErrorKind v
    _ -> case KeyMap.lookup (Key.fromText "result") o of
      Just (Object r) -> KeyMap.lookup (Key.fromText k) r
      _               -> Nothing
lookupField _ _ = Nothing

-- | Project the envelope's @status@ discriminator into the
-- pre-envelope @success :: Bool@ shape: @ok@ / @partial@ → True;
-- everything else → False. Returns 'Nothing' if the response
-- has no @status@ field (i.e. it's not an envelope at all).
synthesizeSuccess :: Value -> Maybe Value
synthesizeSuccess (Object o) = case KeyMap.lookup (Key.fromText "status") o of
  Just (String s)
    | s == "ok"      -> Just (Bool True)
    | s == "partial" -> Just (Bool True)
    | otherwise      -> Just (Bool False)
  _                  -> Nothing
synthesizeSuccess _ = Nothing

-- | Project the envelope's nested @error.kind@ into the
-- pre-envelope top-level @error_kind :: Text@ shape. Returns
-- 'Nothing' if the response has no @error@ object (i.e. a
-- successful response).
synthesizeErrorKind :: Value -> Maybe Value
synthesizeErrorKind (Object o) = case KeyMap.lookup (Key.fromText "error") o of
  Just (Object errObj) -> KeyMap.lookup (Key.fromText "kind") errObj
  _                    -> Nothing
synthesizeErrorKind _ = Nothing

-- | Short, single-line JSON rendering for failure messages.
-- Truncates at ~200 chars so a multi-KB payload doesn't drown
-- the report.
renderValue :: Value -> Text
renderValue v =
  let raw = T.pack (show v)
      cap = 200
  in if T.length raw > cap
       then T.take cap raw <> "…(truncated)"
       else raw

--------------------------------------------------------------------------------
-- report
--------------------------------------------------------------------------------

-- | Print one line per check (PASS/FAIL + name + detail) and
-- return the full text for optional external capture.
renderReport :: [Check] -> IO ()
renderReport = mapM_ printOne
  where
    printOne c =
      putStrLn $
        (if cOk c then "PASS  " else "FAIL  ")
        <> T.unpack (cName c)
        <> (if cOk c then "" else "\n       " <> T.unpack (cDetail c))

-- | @True@ iff every check passed.
allPassed :: [Check] -> Bool
allPassed = all cOk

--------------------------------------------------------------------------------
-- streaming progress
--
-- Each step prints a banner, each check prints as it's recorded,
-- each step footer prints its duration. This turns "the e2e is
-- stuck somewhere" into "the e2e is 9 seconds into step 13" —
-- you can see in real time which MCP call is hanging.
--------------------------------------------------------------------------------

-- | Emit a top-level section banner (e.g. "Scenario: expr-evaluator").
beginSection :: Text -> IO ()
beginSection label = do
  putStrLn ""
  putStrLn (T.unpack ("═══ " <> label <> " ═══"))
  hFlush stdout

-- | Print the step banner and return a POSIX timestamp the caller
-- passes back into 'stepFooter' to compute duration.
stepHeader :: Int -> Text -> IO POSIXTime
stepHeader n title = do
  putStrLn ""
  putStrLn (T.unpack ("▶ step " <> tshow n <> " · " <> title))
  hFlush stdout
  getPOSIXTime

-- | Print step completion + duration. Pairs 1:1 with 'stepHeader'.
stepFooter :: Int -> POSIXTime -> IO ()
stepFooter n t0 = do
  t1 <- getPOSIXTime
  let ms = round ((realToFrac (t1 - t0) :: Double) * 1000) :: Int
  putStrLn (T.unpack ("◼ step " <> tshow n <> " done in " <> tshow ms <> " ms"))
  hFlush stdout

-- | Print a single check line as soon as it's recorded, and
-- return the check unchanged so the scenario can keep
-- aggregating. This is what lets the user see PASS / FAIL
-- streaming past instead of waiting for the whole scenario.
liveCheck :: Check -> IO Check
liveCheck c = do
  putStrLn (format c)
  hFlush stdout
  pure c
  where
    format k =
      "  " <> (if cOk k then "PASS  " else "FAIL  ") <> T.unpack (cName k)
      <> (if cOk k then "" else "\n       → " <> T.unpack (cDetail k))

tshow :: Show a => a -> Text
tshow = T.pack . show
