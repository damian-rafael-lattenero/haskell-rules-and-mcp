-- | Uniform tool-handler interface — issue #285.
--
-- 'ToolEnv' captures every server-level dependency a tool may need.
-- IO-valued fields resolve lazily (most tools use only a subset).
-- Handlers return ToolResponse; ToolResult wrapping happens once in dispatchByName (#290).
-- Raw-ref fields are exposed for the tools that mutate shared state
--
-- NoStrictData: the global StrictData default would force all fields on
-- construction, defeating the lazy-error design of IO thunks and test
-- stubs.  Explicit ! on teLimits/teSink/teDescriptors/teToolNames is
-- preserved.
{-# LANGUAGE NoStrictData #-}
-- (Project, PropertyStore, Workflow).
--
-- 'ToolHandler' is the single wire type for all tool dispatches, letting
-- 'dispatchByName' collapse to a lookup table and eliminating the 36-arm
-- inline dep-wiring in the old dispatcher.
module HaskellFlows.Tool.Env
  ( ToolEnv (..)
  , ToolHandler
  ) where

import Control.Concurrent.MVar (MVar)
import Data.Aeson (Value)
import Data.IORef (IORef)
import Data.Text (Text)

import HaskellFlows.Config (Limits)
import HaskellFlows.Data.PropertyStore (Store)
import qualified HaskellFlows.Data.Scratchpad as Scratchpad
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Progress (ProgressSink)
import HaskellFlows.Mcp.Protocol (ToolCall, ToolDescriptor, ToolResult)
import HaskellFlows.Mcp.Envelope (ToolResponse)
import HaskellFlows.Mcp.Staleness (StalenessReport)
import HaskellFlows.Mcp.WorkflowState (WorkflowState)
import HaskellFlows.Types (ProjectDir)

-- | All server-level dependencies a tool handler may need.
--
-- IO-valued fields are thunks: most tools call only a subset, paying
-- only for what they use. The raw-ref fields (ending in @Ref@) are
-- exposed for the handful of tools that must mutate or pass mutable
-- state to sub-handlers (Project, PropertyStore, Workflow).
data ToolEnv = ToolEnv
  { -- IO thunks — resolve on demand
    teSession       :: IO GhcSession
  , teProjectDir    :: IO ProjectDir
  , teStore         :: IO Store
  , teScratchpad    :: IO Scratchpad.Store
  , teWorkflowState :: IO WorkflowState
  , teStaleness     :: IO StalenessReport
  , teIsSelf        :: IO Bool
    -- Pure values
  , teLimits        :: !Limits
  , teSink          :: !ProgressSink
  , teDescriptors   :: ![ToolDescriptor]
  , teToolNames     :: ![Text]
    -- Raw refs for tools that mutate shared state
  , teSessionRef    :: MVar (Maybe GhcSession)
  , teProjectDirRef :: IORef ProjectDir
  , teStoreRef      :: IORef Store
  , teScratchpadRef :: IORef Scratchpad.Store
  , teIsSelfRef     :: IORef Bool
    -- Side-effect callbacks
  , teDispatch          :: ToolCall -> IO ToolResult
  , teInvalidateSession :: IO ()
  , teInvalidateStanza  :: IO ()
  }

-- | The uniform signature for all tool handlers after #285.
type ToolHandler = ToolEnv -> Value -> IO ToolResponse
