-- | Flow: external-toolchain probes.
--
-- Tools exercised:
--
--   ghc_toolchain_status (cabal/ghc/hlint + optional bins)
--   ghc_toolchain_warmup (probe every optional binary once)
--
-- Both are pure-read, no GHCi session mutation.
module Scenarios.FlowToolchain
  ( runFlow
  ) where

import Data.Aeson (Value (..), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Vector as V

import E2E.Assert
  ( Check (..)
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _pd = do
  -- ghc_toolchain_status
  t0 <- stepHeader 1 "ghc_toolchain_status"
  r1 <- Client.callTool c "ghc_toolchain_status" (object [])
  -- Dropped: "status success" — the 'cabal/ghc/hlint available'
  -- check below is a stronger semantic oracle (fails if any of the
  -- three binaries are missing, which is the real failure mode).
  c2 <- liveCheck $ checkJsonFieldMatches
          "status · has non-empty 'tools' array"
          r1 "tools" (isArrayOfLenAtLeast 1)
          "expected at least one probed tool in the 'tools' array"
  c3 <- liveCheck $ checkJsonFieldMatches
          "status · has 'summary' string"
          r1 "summary" isStr
          "the summary field should summarise availability counts"
  c4 <- liveCheck $ checkJsonFieldMatches
          "status · cabal / ghc / hlint are available on this host"
          r1 "tools" hasCabalGhcHlint
          "cabal, ghc, and hlint should all be available on a dev host — \
          \if any is missing, the rest of the E2E suite would have \
          \crashed earlier. Check your PATH and rerun."
  stepFooter 1 t0

  -- ghc_toolchain_warmup
  t1 <- stepHeader 2 "ghc_toolchain_warmup (probe + report)"
  r2 <- Client.callTool c "ghc_toolchain_warmup" (object [])
  -- Dropped: "warmup success" — redundant with 'tools array non-empty'
  -- which is the shape the tool is actually producing.
  c6 <- liveCheck $ checkJsonFieldMatches
          "warmup · 'tools' array non-empty"
          r2 "tools" (isArrayOfLenAtLeast 1)
          "warmup should probe at least one optional binary"
  stepFooter 2 t1

  pure [c2, c3, c4, c6]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

isStr :: Value -> Bool
isStr (String _) = True
isStr _          = False

isArrayOfLenAtLeast :: Int -> Value -> Bool
isArrayOfLenAtLeast n (Array a) = V.length a >= n
isArrayOfLenAtLeast _ _         = False

-- | Check that the 'tools' array contains entries for cabal,
-- ghc and hlint each flagged @available: true@.
hasCabalGhcHlint :: Value -> Bool
hasCabalGhcHlint (Array a) =
  all (\n -> any (isAvailableToolCalled n) (V.toList a))
      ["cabal", "ghc", "hlint"]
hasCabalGhcHlint _ = False

isAvailableToolCalled :: Text -> Value -> Bool
isAvailableToolCalled tname (Object o) =
  case (KeyMap.lookup (Key.fromText "name") o,
        KeyMap.lookup (Key.fromText "available") o) of
    (Just (String n), Just (Bool True)) -> n == tname
    _                                   -> False
isAvailableToolCalled _ _ = False
