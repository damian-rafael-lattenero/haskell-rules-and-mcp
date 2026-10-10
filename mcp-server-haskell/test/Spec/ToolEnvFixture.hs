-- | Minimal 'ToolEnv' constructors for unit tests.
--
-- Most tool tests only need a subset of 'ToolEnv' fields. The helpers
-- here build a 'stubEnv' with safe error/no-op defaults and let callers
-- fill in only what they need.  Any field that is accessed but not
-- set will throw a descriptive error message at runtime.
module Spec.ToolEnvFixture
  ( stubEnv
  , pdEnv
  , storePdSinkEnv
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import System.IO.Unsafe (unsafePerformIO)

import HaskellFlows.Config (defaultLimits)
import HaskellFlows.Mcp.Progress (ProgressSink, noopSink)
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Data.PropertyStore (Store)
import HaskellFlows.Ghc.IdeSession (IdeSession)
import HaskellFlows.Types (ProjectDir)

-- | Shared empty IdeSession slot: handlers that invalidate the
-- session (Deps/Modules/Project via 'IdeBacked.dropIdeSession') can
-- safely run against stubEnv — the drop is a no-op on Nothing.
{-# NOINLINE stubIdeRef #-}
stubIdeRef :: MVar (Maybe IdeSession)
stubIdeRef = unsafePerformIO (newMVar Nothing)

-- | A 'ToolEnv' with all fields set to safe error/no-op defaults.
-- Override individual fields as needed.
stubEnv :: ToolEnv
stubEnv = ToolEnv
  { teProjectDir      = pure (error "stubEnv.teProjectDir: not configured for this test")
  , teStore           = pure (error "stubEnv.teStore: not configured for this test")
  , teScratchpad      = pure (error "stubEnv.teScratchpad: not configured for this test")
  , teWorkflowState   = pure (error "stubEnv.teWorkflowState: not configured for this test")
  , teStaleness       = pure (error "stubEnv.teStaleness: not configured for this test")
  , teIsSelf          = pure (error "stubEnv.teIsSelf: not configured for this test")
  , teLimits          = defaultLimits
  , teSink            = noopSink
  , teDescriptors     = []
  , teToolNames       = []
  , teIdeSessionRef   = stubIdeRef
  , teProjectDirRef   = error "stubEnv.teProjectDirRef: not configured for this test"
  , teStoreRef        = error "stubEnv.teStoreRef: not configured for this test"
  , teScratchpadRef   = error "stubEnv.teScratchpadRef: not configured for this test"
  , teIsSelfRef       = error "stubEnv.teIsSelfRef: not configured for this test"
  , teDispatch        = error "stubEnv.teDispatch: not configured for this test"
  }

-- | Env with only 'teProjectDir' set — for traversal-guard tests.
pdEnv :: ProjectDir -> ToolEnv
pdEnv pd = stubEnv { teProjectDir = pure pd }

-- | Env with store, project-dir, and sink — for Gate tests.
storePdSinkEnv :: Store -> ProjectDir -> ProgressSink -> ToolEnv
storePdSinkEnv store pd sink = stubEnv
  { teStore      = pure store
  , teProjectDir = pure pd
  , teSink       = sink
  }
