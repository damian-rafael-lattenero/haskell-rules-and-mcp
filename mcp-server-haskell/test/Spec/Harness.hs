-- | Minimal test-harness primitives shared by the unit-test driver
-- ('test/Spec.hs') and the per-domain test modules under @test/Spec/@.
--
-- The suite uses a deliberately tiny bespoke harness rather than hspec:
-- a test is just an @IO Bool@, and 'test' runs one under a defensive
-- timeout, prints a @PASS/FAIL/TIMEOUT@ line, and returns the verdict.
-- The driver @sequence@s a big list of these and exits non-zero if any
-- returned False.
--
-- Extracted from the Spec.hs monolith (#271) so domain modules can
-- import 'test' without a cycle back through the driver.
module Spec.Harness
  ( test
  , testTimeoutMicros
  ) where

import Data.Maybe (fromMaybe)
import System.Timeout (timeout)

-- | Per-test defensive timeout. Any unit test that doesn't complete in
-- 60 s is reported as a hard failure with a TIMEOUT prefix rather than
-- hanging the whole suite. Protects CI from the class of hazards that
-- land when a test uses forkIO/takeMVar or spawns a subprocess that
-- stops emitting without closing its pipe.
testTimeoutMicros :: Int
testTimeoutMicros = 60 * 1000 * 1000

-- | Run one named test under 'testTimeoutMicros', print its verdict
-- (@PASS@/@FAIL@/@TIMEOUT@), and return whether it passed.
test :: String -> IO Bool -> IO Bool
test name action = do
  mok <- timeout testTimeoutMicros action
  let ok = fromMaybe False mok
      prefix
        | Nothing <- mok = "TIMEOUT "
        | ok             = "PASS    "
        | otherwise      = "FAIL    "
  putStrLn (prefix <> name)
  pure ok
