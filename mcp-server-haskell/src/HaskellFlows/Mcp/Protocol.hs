-- | MCP protocol envelope types — JSON-RPC 2.0 plus MCP-specific payloads.
--
-- We intentionally do not pull in a third-party MCP SDK: the protocol
-- surface we need for Phase 1 is small (initialize, tools/list, tools/call,
-- plus notifications) and keeping it in-tree means the dependency graph
-- stays flat and auditable. If we later outgrow this we can swap to a
-- published package without changing the tool handlers — they only see
-- already-decoded 'ToolCall' values.
module HaskellFlows.Mcp.Protocol
  ( -- * JSON-RPC envelopes
    Request (..)
  , Response (..)
  , RpcError (..)
  , RequestId
    -- * MCP payloads
  , InitializeResult (..)
  , ServerInfo (..)
  , ToolDescriptor (..)
  , ToolCall (..)
  , ToolResult (..)
  , ToolContent (..)
    -- * Error helpers
  , parseErr
  , methodNotFoundErr
  , invalidParamsErr
  , internalErr
  ) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | JSON-RPC permits numeric or string ids; null is reserved for notifications.
type RequestId = Value

data Request = Request
  { reqJsonrpc :: !Text
  , reqMethod  :: !Text
  , reqParams  :: !(Maybe Value)
  , reqId      :: !(Maybe RequestId)
  }
  deriving stock (Show, Generic)

instance FromJSON Request where
  parseJSON = withObject "Request" $ \o ->
    Request
      <$> o .:  "jsonrpc"
      <*> o .:  "method"
      <*> o .:? "params"
      <*> o .:? "id"

-- | Either a success payload or an error payload, carried alongside the id.
data Response = Response
  { respId      :: !RequestId
  , respPayload :: !(Either RpcError Value)
  }
  deriving stock (Show)

instance ToJSON Response where
  toJSON r =
    let base = [ "jsonrpc" .= ("2.0" :: Text), "id" .= respId r ]
        tail_ = case respPayload r of
          Right v -> [ "result" .= v ]
          Left e  -> [ "error"  .= e ]
    in object (base <> tail_)

data RpcError = RpcError
  { errCode    :: !Int
  , errMessage :: !Text
  , errData    :: !(Maybe Value)
  }
  deriving stock (Show)

instance ToJSON RpcError where
  toJSON (RpcError c m d) =
    object $ [ "code" .= c, "message" .= m ]
          <> maybe [] (\v -> ["data" .= v]) d

parseErr, methodNotFoundErr, invalidParamsErr, internalErr :: Text -> RpcError
parseErr         msg = RpcError (-32700) msg Nothing
methodNotFoundErr m  = RpcError (-32601) ("Method not found: " <> m) Nothing
invalidParamsErr msg = RpcError (-32602) msg Nothing
internalErr      msg = RpcError (-32603) msg Nothing

--------------------------------------------------------------------------------
-- MCP payloads
--------------------------------------------------------------------------------

data InitializeResult = InitializeResult
  { irProtocolVersion :: !Text
  , irServerInfo      :: !ServerInfo
  , irInstructions    :: !(Maybe Text)
    -- ^ Optional free-form guidance the client surfaces to the LLM at
    -- session start. MCP spec: @InitializeResult.instructions: string?@.
    -- Populated here with the tool-tier / dogfood-fix / liveness
    -- guarantee summary so an agent that only reads the MCP handshake
    -- (no project-level CLAUDE.md) still knows what tools exist, which
    -- situation picks which tool, and that the session layer is
    -- liveness-safe.
  }
  deriving stock (Show)

instance ToJSON InitializeResult where
  toJSON ir =
    object $
      [ "protocolVersion" .= irProtocolVersion ir
      , "capabilities"    .= object [ "tools" .= object [] ]
      , "serverInfo"      .= irServerInfo ir
      ]
      <> maybe [] (\t -> ["instructions" .= t]) (irInstructions ir)

data ServerInfo = ServerInfo
  { siName    :: !Text
  , siVersion :: !Text
  }
  deriving stock (Show)

instance ToJSON ServerInfo where
  toJSON si = object [ "name" .= siName si, "version" .= siVersion si ]

data ToolDescriptor = ToolDescriptor
  { tdName        :: !Text
  , tdDescription :: !Text
  , tdInputSchema :: !Value
  }
  deriving stock (Show)

instance ToJSON ToolDescriptor where
  toJSON td =
    object
      [ "name"        .= tdName td
      , "description" .= tdDescription td
      , "inputSchema" .= tdInputSchema td
      ]

-- | A decoded @tools/call@ params body.
data ToolCall = ToolCall
  { tcName      :: !Text
  , tcArguments :: !Value
  }
  deriving stock (Show)

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o ->
    ToolCall
      <$> o .:  "name"
      <*> (o .:? "arguments" .!= object [])

-- | MCP spec: tool results are a list of content blocks plus an isError flag.
data ToolResult = ToolResult
  { trContent :: ![ToolContent]
  , trIsError :: !Bool
  }
  deriving stock (Show)

instance ToJSON ToolResult where
  toJSON tr =
    object
      [ "content" .= trContent tr
      , "isError" .= trIsError tr
      ]

newtype ToolContent
  = TextContent Text
  deriving stock (Show)

instance ToJSON ToolContent where
  toJSON (TextContent t) =
    object [ "type" .= ("text" :: Text), "text" .= t ]
