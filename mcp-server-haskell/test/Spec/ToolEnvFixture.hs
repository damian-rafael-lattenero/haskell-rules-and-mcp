-- | Minimal 'ToolEnv' constructors for unit tests.
--
-- Most tool tests only need a subset of 'ToolEnv' fields. The helpers
-- here build a 'stubEnv' with safe error/no-op defaults and let callers
-- fill in only what they need.  Any field that is accessed but not
-- set will throw a descriptive error message at runtime.
module Spec.ToolEnvFixture
  ( stubEnv
  , sessionEnv
  , pdEnv
  , sessionPdEnv
  , storeSessionPdSinkEnv
  ) where

import HaskellFlows.Config (defaultLimits)
import HaskellFlows.Mcp.Progress (ProgressSink, noopSink)
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Data.PropertyStore (Store)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Types (ProjectDir)

-- | A 'ToolEnv' with all fields set to safe error/no-op defaults.
-- Override individual fields as needed.
stubEnv :: ToolEnv
stubEnv = ToolEnv
  { teSession         = pure (error "stubEnv.teSession: not configured for this test")
  , teProjectDir      = pure (error "stubEnv.teProjectDir: not configured for this test")
  , teStore           = pure (error "stubEnv.teStore: not configured for this test")
  , teScratchpad      = pure (error "stubEnv.teScratchpad: not configured for this test")
  , teWorkflowState   = pure (error "stubEnv.teWorkflowState: not configured for this test")
  , teStaleness       = pure (error "stubEnv.teStaleness: not configured for this test")
  , teIsSelf          = pure (error "stubEnv.teIsSelf: not configured for this test")
  , teLimits          = defaultLimits
  , teSink            = noopSink
  , teDescriptors     = []
  , teToolNames       = []
  , teSessionRef      = error "stubEnv.teSessionRef: not configured for this test"
  , teProjectDirRef   = error "stubEnv.teProjectDirRef: not configured for this test"
  , teStoreRef        = error "stubEnv.teStoreRef: not configured for this test"
  , teScratchpadRef   = error "stubEnv.teScratchpadRef: not configured for this test"
  , teIsSelfRef       = error "stubEnv.teIsSelfRef: not configured for this test"
  , teDispatch        = error "stubEnv.teDispatch: not configured for this test"
  , teInvalidateSession = pure ()
  , teInvalidateStanza  = pure ()
  }

-- | Env with only 'teSession' set — for tools that need just a GHC session.
sessionEnv :: GhcSession -> ToolEnv
sessionEnv sess = stubEnv { teSession = pure sess }

-- | Env with only 'teProjectDir' set — for traversal-guard tests.
pdEnv :: ProjectDir -> ToolEnv
pdEnv pd = stubEnv { teProjectDir = pure pd }

-- | Env with 'teSession' + 'teProjectDir' set.
sessionPdEnv :: GhcSession -> ProjectDir -> ToolEnv
sessionPdEnv sess pd = stubEnv
  { teSession    = pure sess
  , teProjectDir = pure pd
  }

-- | Env with store, session, project-dir, and sink — for Gate tests.
storeSessionPdSinkEnv :: Store -> GhcSession -> ProjectDir -> ProgressSink -> ToolEnv
storeSessionPdSinkEnv store sess pd sink = stubEnv
  { teStore      = pure store
  , teSession    = pure sess
  , teProjectDir = pure pd
  , teSink       = sink
  }
