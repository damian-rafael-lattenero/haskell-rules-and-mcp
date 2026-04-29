-- | Flow: 'ghc_bootstrap' rules content reflects the in-process
-- GHC API session model, NOT the retired subprocess GHCi
-- vocabulary (#56).
--
-- Pre-fix behaviour
-- -----------------
-- The bake-source for the rules markdown ('Mcp.Guidance') still
-- documented 'SessionStatus = Alive | Overflowed | Dead',
-- 'executeNoLock', 'registerDelay', and 'GHCi death' — concepts
-- from the subprocess REPL retired in Wave 5. Agents reading
-- the bootstrap output to debug timeouts or session crashes
-- looked for invariants that didn't exist in the running binary.
--
-- New contract
-- ------------
-- The emitted markdown drops the retired vocabulary and names
-- the actual model: in-process 'HscEnv' guarded by 'MVar',
-- exception-evicted sessions, 'resetHscEnvInPlace', and
-- 'Server.runTool' as the 10-min outer ceiling.
module Scenarios.FlowBootstrapDocs
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
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- The 'generic' host returns the markdown WITHOUT writing
  -- anything to disk — perfect for a content assertion.
  t0 <- stepHeader 1 "ghc_bootstrap(generic) drops retired vocab (#56)"
  r <- Client.callTool c GhcBootstrap
         (object [ "host" .= ("generic" :: Text), "write" .= True ])
  let body = case lookupField "content" r of
        Just (String s) -> s
        _               -> ""
      retired =
        [ "SessionStatus"
        , "executeNoLock"
        , "registerDelay"
        , "GHCi death"
        ]
      stillThere = filter (`T.isInfixOf` body) retired
  cRetired <- liveCheck $ checkPure
    "rules markdown contains none of the retired-subprocess terms"
    (null stillThere)
    ( "Expected no retired terms; found: " <> T.pack (show stillThere) )
  stepFooter 1 t0

  -- Step 2 — same body must affirmatively name the new model.
  t1 <- stepHeader 2 "ghc_bootstrap(generic) names in-process GHC API (#56)"
  let bodyLower = T.toLower body
      required  = map T.toLower
        [ "in-process"
        , "HscEnv"
        , "MVar"
        , "resetHscEnvInPlace"
        ]
      missing = filter (\w -> not (w `T.isInfixOf` bodyLower)) required
  cNew <- liveCheck $ checkPure
    "rules markdown names the in-process model (HscEnv/MVar/resetHscEnvInPlace)"
    (null missing)
    ( "Expected all new terms; missing: " <> T.pack (show missing) )
  stepFooter 2 t1

  pure [cRetired, cNew]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

