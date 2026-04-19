-- | The sentinel protocol used to delimit GHCi command responses.
--
-- The TypeScript server introduced this trick (see @mcp-server/src/ghci-session.ts:6@):
-- instead of parsing GHCi's prompt (which varies across versions, extensions,
-- and module contexts), we set the prompt to a fixed magic string and
-- treat that string as an explicit end-of-output marker.
--
-- The exact token is kept wire-compatible with the TS version so either
-- implementation can drive the same GHCi invariants if we ever run them
-- side by side during migration.
module HaskellFlows.Ghci.Sentinel
  ( sentinel
  , initScript
  ) where

import Data.Text (Text)

-- | The fixed end-of-output marker. Must match the TS server verbatim.
sentinel :: Text
sentinel = "<<<GHCi-DONE-7f3a2b>>>"

-- | GHCi commands sent once, right after startup, to install the sentinel
-- prompt and enable extensions the TS server also enables so behaviour is
-- comparable.
--
-- Each line here produces exactly one sentinel in the output stream — the
-- session loop relies on that 1:1 relationship to stay synchronised.
initScript :: [Text]
initScript =
  [ ":set prompt \"\\n" <> sentinel <> "\\n\""
  , ":set prompt-cont \"\""
  , ":set -XScopedTypeVariables"
  , ":set -XTypeApplications"
  , ":set -XOverloadedStrings"
  , ":set -w"
  ]
