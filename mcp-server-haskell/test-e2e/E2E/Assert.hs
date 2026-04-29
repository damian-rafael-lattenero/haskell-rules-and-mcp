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

-- | Look up a field, auto-drilling through the @result@ envelope
-- when the field isn't at the top level (issue #90 Phase D step 2).
--
-- The pre-#90 wire format put tool-specific fields at the top
-- level (@type@, @output@, @file@, @raw@, @holes@, etc.); the
-- post-#90 envelope nests them under @result@. To keep oracles
-- ergonomic across the migration window, this helper checks
-- BOTH: top-level first (so envelope discriminators @status@ /
-- @error@ / @nextStep@ resolve directly), then under @result@
-- (so tool-specific payload fields resolve transparently).
--
-- Mirrors the auto-drilling 'lookupField' in 'E2E.Envelope';
-- having both makes the migration survive whether a scenario
-- imports its helpers from Assert ('checkJsonField',
-- 'checkJsonFieldMatches') or from Envelope.
lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = case KeyMap.lookup (Key.fromText k) o of
  Just inner -> Just inner
  Nothing    -> case KeyMap.lookup (Key.fromText "result") o of
    Just (Object r) -> KeyMap.lookup (Key.fromText k) r
    _               -> Nothing
lookupField _ _ = Nothing

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
