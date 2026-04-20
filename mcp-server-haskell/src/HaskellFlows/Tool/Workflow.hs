-- | @ghci_workflow@ — meta-tool that summarises the state of the server
-- and suggests the next action.
--
-- The TS port has a stateful workflow engine that tracks per-module
-- progress, which functions have tests, which gates are pending, etc.
-- This Haskell Phase-5 port ships the /observable/ subset: actions that
-- can be answered purely from the session's current state (is a GHCi
-- child alive? is the project dir set? etc.). Per-module workflow
-- state will grow in Phase 6 once the property-store is ported and we
-- can persist per-function facts.
--
-- This is intentionally read-only. It never spawns GHCi, never mutates
-- the session — safe to call at any time, including from an agent that
-- just errored and wants to know what's reachable.
module HaskellFlows.Tool.Workflow
  ( descriptor
  , handle
  , WorkflowArgs (..)
  , Action (..)
  ) where

import Control.Concurrent.MVar (MVar, readMVar)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.IORef (IORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session (Session)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_workflow"
    , tdDescription =
        "Query the server's workflow state. Actions: 'status' (server "
          <> "inventory), 'help' (what to do next, context-aware), 'next' "
          <> "(single most likely next tool call). Read-only; never spawns "
          <> "or mutates a GHCi session."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["status", "help", "next"] :: [Text])
                  , "description" .=
                      ("Which view to return. Default: 'status'." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data Action = ActStatus | ActHelp | ActNext
  deriving stock (Eq, Show)

newtype WorkflowArgs = WorkflowArgs
  { waAction :: Action
  }
  deriving stock (Show)

instance FromJSON WorkflowArgs where
  parseJSON = withObject "WorkflowArgs" $ \o -> do
    mAct <- o .:? "action"
    a    <- case mAct :: Maybe Text of
      Nothing        -> pure ActStatus
      Just "status"  -> pure ActStatus
      Just "help"    -> pure ActHelp
      Just "next"    -> pure ActNext
      Just other     -> fail ("unknown action: " <> T.unpack other)
    pure (WorkflowArgs a)

-- | The @toolNames@ argument is the canonical tool list provided by
-- 'HaskellFlows.Mcp.Server' — the same list that feeds @tools/list@.
-- Passing it in keeps this module free of a dependency on Server
-- (no import cycle) while guaranteeing the @toolsActive@ view can
-- never drift from the @tools/list@ surface again.
handle
  :: IORef ProjectDir
  -> MVar (Maybe Session)
  -> [Text]
  -> Value
  -> IO ToolResult
handle pdRef sessMVar toolNames rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right (WorkflowArgs a) -> do
    pd       <- readIORef pdRef
    sessAlive <- isAlive sessMVar
    pure (render a pd sessAlive toolNames)

isAlive :: MVar (Maybe Session) -> IO Bool
isAlive sessMVar = do
  m <- readMVar sessMVar
  pure (case m of Nothing -> False; Just _ -> True)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Render the workflow view. The three branches share the same
-- top-level shape so agents can treat the tool's output polymorphically.
render :: Action -> ProjectDir -> Bool -> [Text] -> ToolResult
render a pd alive toolNames =
  let payload = case a of
        ActStatus -> statusPayload pd alive toolNames
        ActHelp   -> helpPayload pd alive
        ActNext   -> nextPayload pd alive
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

statusPayload :: ProjectDir -> Bool -> [Text] -> Value
statusPayload pd alive toolNames =
  object
    [ "view"        .= ("status" :: Text)
    , "projectDir"  .= T.pack (unProjectDir pd)
    , "ghciAlive"   .= alive
    , "toolsActive" .= toolNames
    ]

helpPayload :: ProjectDir -> Bool -> Value
helpPayload _pd alive =
  object
    [ "view"     .= ("help" :: Text)
    , "ghciAlive".= alive
    , "steps"    .= steps
    , "reasoning".= reasoning
    ]
  where
    steps :: [Text]
    steps
      | not alive =
          [ "1. Call ghci_load with your entry module to boot GHCi."
          , "2. For data types you'll test: ghci_arbitrary (type_name=...)."
          , "3. For stubs with _ holes: ghci_hole (module_path=...)."
          , "4. For properties: ghci_quickcheck (property=...)."
          ]
      | otherwise =
          [ "1. ghci_load (diagnostics=true) to catch holes + errors."
          , "2. ghci_hole if holes surfaced."
          , "3. ghci_type to confirm subexpressions compose."
          , "4. ghci_quickcheck once a law is testable."
          , "5. hoogle_search when stuck on which library function fits."
          ]

    reasoning :: Text
    reasoning =
      if alive
        then "GHCi is alive, so the property-first loop is open: keep \
             \the compile/type/quickcheck triangle tight before touching \
             \external tools."
        else "No active GHCi session. Start by loading the module you \
             \want to work on — every other tool will auto-boot on first \
             \use anyway, but ghci_load gives you the cleanest error \
             \surface."

nextPayload :: ProjectDir -> Bool -> Value
nextPayload _pd alive =
  object
    [ "view"   .= ("next" :: Text)
    , "tool"   .= tool
    , "why"    .= why
    ]
  where
    (tool :: Text, why :: Text) =
      if alive
        then ( "ghci_load"
             , "Re-check with diagnostics=true to surface any new holes \
               \or warnings introduced by the last edit." )
        else ( "ghci_load"
             , "No active session. ghci_load boots GHCi and is the \
               \cheapest way to learn the project's current compile state." )

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False
        , "error"   .= msg
        ]))
      ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
