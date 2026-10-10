-- | @--doctor@ — first-run verification for humans (W5).
--
-- The bootstrap story in one binary-resident command: probe the same
-- toolchain 'ghc_session(action=\"toolchain\")' probes at runtime,
-- render a sober ✓/✗ table with one-line remediation, and exit
-- 0 / 1. Because the probes are shared with the live tool, the
-- doctor can never disagree with what the server actually needs —
-- the "initialization file" is the binary itself.
--
-- Exit contract:
--
--   * 0 — every hard requirement (@gate@ category) is present.
--     Missing optional binaries only warn (the corresponding
--     features degrade honestly to @status=\"unavailable\"@).
--   * 1 — at least one hard requirement is missing.
module HaskellFlows.Mcp.Doctor
  ( runDoctor
    -- * Pure render — unit-tested
  , renderDoctor
  , doctorOk
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import HaskellFlows.Config (defaultLimits)
import HaskellFlows.Mcp.PathBootstrap (augmentPath)
import HaskellFlows.Tool.ToolchainStatus
  ( Entry (..)
  , installHintFor
  , probeOne
  , probeTargets
  )

-- | Probe everything the server can delegate to and print the
-- verdict table. Returns the process exit code (0 = healthy).
--
-- 'augmentPath' first: the live server prepends the standard
-- toolchain dirs (~/.ghcup/bin, ~/.cabal/bin, ~/.local/bin,
-- homebrew) to PATH at boot — the doctor must see the same view or
-- it could disagree with the very server it is diagnosing.
runDoctor :: IO Int
runDoctor = do
  _ <- augmentPath
  entries <- mapM (probeOne defaultLimits) probeTargets
  TIO.putStr (T.pack (renderDoctor entries))
  pure (doctorExitCode entries)

-- | True iff every @gate@-category binary is available. Optional
-- binaries do not affect the verdict — a missing formatter or
-- hoogle only disables that feature.
doctorOk :: [Entry] -> Bool
doctorOk = all eAvailable . gates

doctorExitCode :: [Entry] -> Int
doctorExitCode es = if doctorOk es then 0 else 1

gates :: [Entry] -> [Entry]
gates = filter ((== "gate") . eCategory)

optionals :: [Entry] -> [Entry]
optionals = filter ((/= "gate") . eCategory)

-- | Sober aligned table:
--
-- > Hard requirements (the server cannot work without them):
-- >   ✓ ghc      9.12.2    /root/.ghcup/bin/ghc
-- >   ✗ hlint    —         fix: cabal install hlint
-- > Optional (per-feature):
-- >   ✓ fourmolu 0.18.0.0  ...
--
-- A missing binary carries its install hint inline, so the fix is
-- copy-pasteable from the terminal scrollback.
renderDoctor :: [Entry] -> String
renderDoctor entries = T.unpack body
  where
    body =
      T.unlines $
        [ "haskell-flows-mcp doctor — toolchain verification"
        , ""
        , "Hard requirements (the server cannot work without them):"
        ]
          ++ concatMap row (gates entries)
          ++ [ ""
             , "Optional (a missing one only disables its feature —"
             , "the tool reports status=\"unavailable\"):"
             ]
          ++ concatMap row (optionals entries)
          ++ [ "", verdict ]
    nameW = maximum (8 : map (T.length . eName) entries)
    verW = 10 :: Int
    row e =
      [ "  "
          <> mark e
          <> T.justifyLeft nameW ' ' (eName e)
          <> " "
          <> T.justifyLeft verW ' ' (versionOf e)
          <> rowTail e
      ]
    rowTail e
      | eAvailable e = maybe "" (("  " <>) . T.pack) (ePath e)
      | otherwise = "fix: " <> installHintFor (eName e)
    mark e = if eAvailable e then "✓ " else "✗ "
    versionOf e = fromMaybe "—" (eVersion e)
    verdict
      | doctorOk entries =
          "OK — hard requirements met ("
            <> T.pack (show presentOpt)
            <> "/"
            <> T.pack (show totalOpt)
            <> " optional present). Next: wire the binary into your MCP"
            <> "\nclient config and verify end-to-end with"
            <> " ghc_session(action=\"toolchain\")."
      | otherwise =
          "FAIL — missing hard requirements: "
            <> T.intercalate ", " (map eName (filter (not . eAvailable) (gates entries)))
            <> ".\nInstall them and re-run --doctor."
    presentOpt = length (filter eAvailable (optionals entries))
    totalOpt = length (optionals entries)
