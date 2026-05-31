-- | Cross-domain fixture helpers shared by the unit-test driver
-- ('test/Spec.hs') and the per-domain test modules under @test/Spec/@.
--
-- Extracted from the Spec.hs monolith (#271) so domain modules can reuse
-- 'withTempProject' (used by ~38 tests) without a cycle back through the
-- driver. Keep this module to genuinely SHARED helpers only — anything
-- used by a single domain belongs in that domain's module.
module Spec.Helpers
  ( withTempProject
  , getTestTimestamp
  , decodeToolResult
  ) where

import qualified Data.Aeson as A
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Types (ProjectDir, mkProjectDir)

-- | Create a fresh temp directory and delete it after the test.
-- Passes a validated 'ProjectDir' (absolute + normalised) to the body.
withTempProject :: (ProjectDir -> IO Bool) -> IO Bool
withTempProject k = do
  tmp <- getTemporaryDirectory
  ts  <- show <$> getTestTimestamp
  let dir = tmp </> ("haskell-flows-test-" <> ts)
  createDirectoryIfMissing True dir
  res <- case mkProjectDir dir of
    Left _   -> pure False
    Right pd -> k pd
  removePathForcibly dir
  pure res

-- | Microsecond POSIX timestamp, used to name unique temp dirs.
getTestTimestamp :: IO Int
getTestTimestamp = do
  t <- getPOSIXTime
  pure (floor (t * 1_000_000))

-- | Decode the JSON body inside a wire-level 'ToolResult' into the
-- structured 'Env.ToolResponse' envelope (or a string-shaped failure).
-- Shared by ~16 tests across domains.
decodeToolResult :: ToolResult -> Either String Env.ToolResponse
decodeToolResult tr = case trContent tr of
  [TextContent body] ->
    A.eitherDecode (TLE.encodeUtf8 (TL.fromStrict body))
  _ -> Left "expected exactly one TextContent"
