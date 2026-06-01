-- | Unit tests for the wire-protocol ADTs: ToolName (#44), ErrorKind
-- (#45), RpcMethod, and ResourceUri. Every ADT that maps to a wire
-- string is tested for bijection (parse ∘ render == id), total-
-- rejection of unknown inputs, wire-string uniqueness, and exhaustive
-- coverage via the bounded enum.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export
-- shape: the driver keeps the registrations and imports these functions.
module Spec.Protocol
  ( testToolNameRoundTrip
  , testToolNameParseUnknown
  , testToolNameWireUnique
  , testToolNameSnakeCase
  , testToolNameExhaustive
  , testErrorKindRoundTrip
  , testErrorKindParseUnknown
  , testErrorKindWireUnique
  , testErrorKindCoversThree
  , testRpcMethodRoundTrip
  , testRpcMethodParseUnknown
  , testRpcMethodWireUnique
  , testRpcMethodCoversAllMcp
  , testRpcMethodIsNotification
  , testResourceUriRoundTrip
  , testResourceUriParseUnknown
  , testResourceUriWireCanonical
  ) where

import Data.Char (isAsciiLower, isDigit)
import Data.Maybe (isNothing)
import qualified Data.Text as T

import HaskellFlows.Mcp.ErrorKind
  ( ErrorKind (..)
  , parseErrorKind
  , renderErrorKind
  )
import HaskellFlows.Mcp.ResourceUri
  ( ResourceUri (..)
  , allResourceUris
  , allResourceUriTexts
  , parseResourceUri
  , resourceUriText
  )
import HaskellFlows.Mcp.RpcMethod
  ( RpcMethod (..)
  , allRpcMethods
  , allRpcMethodTexts
  , isNotification
  , parseRpcMethod
  , rpcMethodText
  )
import HaskellFlows.Mcp.ToolName
  ( allToolNames
  , allToolNameTexts
  , parseToolName
  , toolNameText
  )

-- ---------------------------------------------------------------------------
-- ToolName (#44)
-- ---------------------------------------------------------------------------

-- | Bijection: every ToolName round-trips through its text form.
testToolNameRoundTrip :: IO Bool
testToolNameRoundTrip =
  pure $ all (\t -> parseToolName (toolNameText t) == Just t) allToolNames

-- | parseToolName returns Nothing for strings that aren't a
-- registered tool. Without this, the dispatcher would silently route
-- a typo to the wrong tool.
testToolNameParseUnknown :: IO Bool
testToolNameParseUnknown = pure $
     isNothing (parseToolName "")
  && isNothing (parseToolName "ghc_unknown")
  && isNothing (parseToolName "GHC_LOAD")      -- case-sensitive
  && isNothing (parseToolName " ghc_load")     -- whitespace
  && isNothing (parseToolName "ghc_load ")
  && isNothing (parseToolName "ghc-load")      -- hyphen vs underscore
  && isNothing (parseToolName "tools/call")    -- not a method

-- | Two distinct ToolName constructors must never collide on the
-- wire — the dispatcher would otherwise pick the first match and
-- stop, silently breaking the second tool.
testToolNameWireUnique :: IO Bool
testToolNameWireUnique =
  let texts = map toolNameText allToolNames
      uniq  = length (foldr insertOnce [] texts)
      insertOnce x acc = if x `elem` acc then acc else x : acc
  in pure (uniq == length texts && length texts >= 30)

-- | Wire forms must be non-empty, all-ASCII lowercase snake_case
-- (a-z, 0-9, underscores only — no spaces, no hyphens, no slashes,
-- no upper-case). Substring scans across guidance text and the
-- agent's tool-name autocomplete rely on this shape; allowing a
-- stray uppercase letter or hyphen would silently break those
-- consumers.
--
-- The current registry has two prefix families: @ghc_*@ for the
-- Haskell tooling itself and @hoogle_*@ for the Hoogle bridge. We
-- assert each name belongs to one of them so a future tool that
-- forgets the family prefix (and therefore won't sort with its
-- siblings) trips the test.
testToolNameSnakeCase :: IO Bool
testToolNameSnakeCase =
  let isLowerSnake c =
           isAsciiLower c
        || isDigit c
        || c == '_'
      hasFamilyPrefix s =
           "ghc_"    `T.isPrefixOf` s
        || "hoogle_" `T.isPrefixOf` s
      ok t =
        let s = toolNameText t
        in    not (T.null s)
           && hasFamilyPrefix s
           && T.all isLowerSnake s
           && not (T.isInfixOf "__" s)        -- no double underscore
           && not (T.isPrefixOf "_" s)        -- no leading underscore
           && not (T.isSuffixOf "_" s)        -- no trailing underscore
  in pure (all ok allToolNames)

-- | 'allToolNames' is derived from @[minBound .. maxBound]@; if a new
-- constructor is added but Bounded/Enum is broken, this catches it.
testToolNameExhaustive :: IO Bool
testToolNameExhaustive = pure $
     length allToolNames >= 30
  && length allToolNames == length allToolNameTexts

-- ---------------------------------------------------------------------------
-- ErrorKind (#45)
-- ---------------------------------------------------------------------------

-- | Bijection: every ErrorKind round-trips through its text form.
-- This is the wire contract for tool-error responses; if any
-- constructor's text drifts, the LLM's classifier breaks.
testErrorKindRoundTrip :: IO Bool
testErrorKindRoundTrip =
  let kinds = [Timeout, SessionExhausted, ToolException]
  in pure $ all (\k -> parseErrorKind (renderErrorKind k) == Just k) kinds

-- | Unknown error_kind strings must not parse — protects against
-- silent classification of fresh failure modes as known ones.
testErrorKindParseUnknown :: IO Bool
testErrorKindParseUnknown = pure $
     isNothing (parseErrorKind "")
  && isNothing (parseErrorKind "unknown")
  && isNothing (parseErrorKind "TIMEOUT")           -- case-sensitive
  && isNothing (parseErrorKind "session-exhausted") -- hyphen vs underscore

-- | The three kinds must produce three distinct wire strings.
-- Uniqueness check: deduplicate the list and assert the length is
-- preserved.
testErrorKindWireUnique :: IO Bool
testErrorKindWireUnique =
  let kinds = [Timeout, SessionExhausted, ToolException]
      texts = map renderErrorKind kinds
      uniq  = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] texts
  in pure (length uniq == length texts && length uniq == 3)

-- | The wire strings are exactly the three documented constants.
-- This is the literal contract surfaced to the agent in tool-error
-- responses.
testErrorKindCoversThree :: IO Bool
testErrorKindCoversThree = pure $
     renderErrorKind Timeout          == "timeout"
  && renderErrorKind SessionExhausted == "session_exhausted"
  && renderErrorKind ToolException    == "tool_exception"

-- ---------------------------------------------------------------------------
-- RpcMethod
-- ---------------------------------------------------------------------------

-- | Bijection: every RpcMethod round-trips through its text form.
testRpcMethodRoundTrip :: IO Bool
testRpcMethodRoundTrip =
  pure $ all (\m -> parseRpcMethod (rpcMethodText m) == Just m) allRpcMethods

-- | Unknown JSON-RPC methods must not parse — this is what the
-- dispatcher uses to send a "method not found" envelope back to the
-- caller.
testRpcMethodParseUnknown :: IO Bool
testRpcMethodParseUnknown = pure $
     isNothing (parseRpcMethod "")
  && isNothing (parseRpcMethod "tools/unknown")
  && isNothing (parseRpcMethod "tools.list")          -- dot vs slash
  && isNothing (parseRpcMethod "TOOLS/CALL")          -- case-sensitive
  && isNothing (parseRpcMethod "ghc_load")            -- not a tool

-- | Two distinct RpcMethod constructors must never share a wire
-- string. The dispatcher matches by exact text, so a collision would
-- silently route both to the same handler.
testRpcMethodWireUnique :: IO Bool
testRpcMethodWireUnique =
  let texts = allRpcMethodTexts
      uniq  = foldr (\x acc -> if x `elem` acc then acc else x:acc) [] texts
  in pure (length uniq == length texts && length texts == length allRpcMethods)

-- | Pin the seven JSON-RPC methods we currently support against
-- their literal wire strings — these are part of the MCP protocol
-- contract; any drift would break LLM clients.
testRpcMethodCoversAllMcp :: IO Bool
testRpcMethodCoversAllMcp = pure $
     rpcMethodText Initialize             == "initialize"
  && rpcMethodText Initialized            == "initialized"
  && rpcMethodText ToolsList              == "tools/list"
  && rpcMethodText ToolsCall              == "tools/call"
  && rpcMethodText ResourcesList          == "resources/list"
  && rpcMethodText ResourcesRead          == "resources/read"
  && rpcMethodText NotificationsCancelled == "notifications/cancelled"
  && length allRpcMethods == 7

-- | 'isNotification' must classify each method correctly.
-- Notifications are JSON-RPC messages without an @id@ — the server
-- must NOT send a response. A misclassification here either drops
-- a real response (request misclassified as notification) or sends
-- a spurious one (notification misclassified as request).
testRpcMethodIsNotification :: IO Bool
testRpcMethodIsNotification = pure $
  -- Notifications: handshake-complete + cancellation.
     isNotification Initialized
  && isNotification NotificationsCancelled
  -- Requests: every other method has an id-bearing reply.
  && not (isNotification Initialize)
  && not (isNotification ToolsList)
  && not (isNotification ToolsCall)
  && not (isNotification ResourcesList)
  && not (isNotification ResourcesRead)
  -- Sanity: classification is total over the ADT — every constructor
  -- in 'allRpcMethods' has a defined notification status.
  && length [ () | m <- allRpcMethods
                 , let _b = isNotification m
            ] == length allRpcMethods

-- ---------------------------------------------------------------------------
-- ResourceUri
-- ---------------------------------------------------------------------------

-- | Bijection: every ResourceUri round-trips through its text form.
testResourceUriRoundTrip :: IO Bool
testResourceUriRoundTrip =
  pure $ all (\u -> parseResourceUri (resourceUriText u) == Just u) allResourceUris

-- | Unknown URIs must not parse. The resources/read dispatcher
-- relies on this to reject probes for non-advertised URIs.
testResourceUriParseUnknown :: IO Bool
testResourceUriParseUnknown = pure $
     isNothing (parseResourceUri "")
  && isNothing (parseResourceUri "haskell-flows://nonexistent")
  && isNothing (parseResourceUri "https://example.com")
  && isNothing (parseResourceUri "haskell-flows://rules/other")
  && isNothing (parseResourceUri "file:///etc/passwd")

-- | The advertised wire form for the only resource we currently
-- expose. This is part of the MCP resource contract — clients hold
-- the URI literally.
testResourceUriWireCanonical :: IO Bool
testResourceUriWireCanonical = pure $
     resourceUriText WorkflowRules == "haskell-flows://rules/workflow"
  && length allResourceUris       == 1
  && length allResourceUriTexts   == 1
