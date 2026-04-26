-- | Closed enumeration of every JSON-RPC method our server understands.
--
-- The MCP spec carves the request surface into a small, fixed set of method
-- strings (initialize, tools/list, tools/call, resources/list,
-- resources/read) plus a handful of notifications (initialized,
-- notifications/cancelled). Treating those as bare 'Text' values forces
-- every dispatcher into a defensive @case@ over string literals — typos
-- silently degrade to "method not found" and the compiler can not flag a
-- missing handler when we add a new method.
--
-- This module reifies the catalogue as a sum type. The wire format is
-- preserved by 'rpcMethodText' / 'parseRpcMethod' (the bijection between
-- constructors and the literals the spec mandates), and downstream
-- dispatchers ('Server.handleRequest', 'Server.dispatch') get exhaustive
-- pattern matching for free.
--
-- Adding a new method = add a constructor here, extend the two bijection
-- functions, then watch GHC's @-Wincomplete-patterns@ list every dispatcher
-- that needs an update. No grep, no missed call sites.
module HaskellFlows.Mcp.RpcMethod
  ( RpcMethod (..)
  , rpcMethodText
  , parseRpcMethod
  , isNotification
  , allRpcMethods
  , allRpcMethodTexts
  ) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Every JSON-RPC method (request or notification) our server accepts.
--
-- Order is irrelevant on the wire (we map by 'rpcMethodText'); the
-- 'Enum'/'Bounded' instances are used by 'allRpcMethods' so tests can
-- pin the inventory.
data RpcMethod
  = Initialize
  | Initialized
  | ToolsList
  | ToolsCall
  | ResourcesList
  | ResourcesRead
  | NotificationsCancelled
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Render a method to the exact wire literal MCP/JSON-RPC expect.
--
-- This is the only place where the canonical strings live — every other
-- dispatch site goes through 'parseRpcMethod' or matches on the ADT.
rpcMethodText :: RpcMethod -> Text
rpcMethodText = \case
  Initialize             -> "initialize"
  Initialized            -> "initialized"
  ToolsList              -> "tools/list"
  ToolsCall              -> "tools/call"
  ResourcesList          -> "resources/list"
  ResourcesRead          -> "resources/read"
  NotificationsCancelled -> "notifications/cancelled"

-- | Inverse of 'rpcMethodText'. Returns 'Nothing' for any string outside
-- the closed set so 'Server.handleRequest' can short-circuit to
-- @methodNotFoundErr@ instead of accidentally matching a typo.
parseRpcMethod :: Text -> Maybe RpcMethod
parseRpcMethod = flip Map.lookup reverseMap
  where
    reverseMap = Map.fromList [ (rpcMethodText m, m) | m <- allRpcMethods ]

-- | True for methods that are notifications (no @id@, no response). MCP
-- declares this membership explicitly per method; we keep it co-located
-- with the constructor list so adding a new notification can not desync
-- from 'Server.handleRequest'\'s notification short-circuit.
isNotification :: RpcMethod -> Bool
isNotification = \case
  Initialized            -> True
  NotificationsCancelled -> True
  Initialize             -> False
  ToolsList              -> False
  ToolsCall              -> False
  ResourcesList          -> False
  ResourcesRead          -> False

-- | Every method in declaration order. Used by parity tests that pin
-- the wire-format inventory against the constructor set.
allRpcMethods :: [RpcMethod]
allRpcMethods = [minBound .. maxBound]

-- | Convenience: 'allRpcMethods' rendered as 'Text', so list-shape
-- assertions in tests stay one call away.
allRpcMethodTexts :: [Text]
allRpcMethodTexts = map rpcMethodText allRpcMethods
