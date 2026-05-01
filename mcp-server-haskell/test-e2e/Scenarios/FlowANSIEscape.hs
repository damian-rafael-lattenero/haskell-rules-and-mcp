-- | Flow: GHC error messages that carry ANSI colour escape codes
-- must not corrupt the MCP's JSON response envelope.
--
-- The threat model
-- ----------------
-- When stdout is attached to a TTY, modern GHC emits type errors
-- wrapped in SGR escape sequences (e.g. @\\x1b[31merror:\\x1b[0m@).
-- The MCP pipes the GHCi child's output through a Text-valued
-- JSON field; if those escape bytes land in a JSON string without
-- proper escaping, some JSON parsers reject the whole response,
-- and even those that accept it may render the field as control
-- characters the LLM host treats as part of the message.
--
-- We force the issue by setting @TERM=xterm-256color@ on the
-- child and asking GHC to emit a type error. The MCP's response
-- MUST:
--
--   (a) parse as valid JSON (the harness already decoded it —
--       so reaching our scenario code at all is half the
--       assertion).
--   (b) either strip the escape codes, or keep them escaped
--       as valid JSON string chars (@\\u001b[31m…@).
--   (c) NOT contain raw 0x1B bytes mid-string in the rendered
--       response — those break terminals downstream when the
--       agent re-prints the message.
--
-- Why this is worth a scenario
-- ----------------------------
-- The 'FlowCorpusTransport' scenario fuzzes the TRANSPORT (malformed
-- JSON-RPC lines INTO the server). FlowANSIEscape tests the opposite
-- direction: the server's OUTPUT correctness when GHC emits bytes
-- that are legal stdout but hostile-adjacent for JSON.
module Scenarios.FlowANSIEscape
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv, unsetEnv, lookupEnv)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | A type error GHC will happily colourise on a TTY.
brokenSrc :: Text
brokenSrc =
  "module Broken (bad) where\n\
  \\n\
  \bad :: Int -> Int\n\
  \bad x = x + \"totally not an Int\"\n"

-- | Raw ESC byte — the prefix of every SGR sequence GHC emits
-- under colour. If this appears UN-escaped in the response, the
-- MCP is piping bytes that break downstream terminals and some
-- strict JSON parsers.
esc :: Char
esc = '\ESC'

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Force colour output from GHC by pretending our stderr is a
  -- 256-colour TTY. GHC reads TERM + checks isatty; since we're
  -- in-process there's no real TTY, but many error-formatting code
  -- paths key off TERM alone, which is enough to stress the MCP's
  -- text-encoding path.
  oldTerm <- lookupEnv "TERM"
  setEnv "TERM" "xterm-256color"

  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("ansi-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Broken"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Broken.hs") brokenSrc

  t0 <- stepHeader 1 "load · trigger a type error (potential SGR output)"
  loadR <- Client.callTool c GhcLoad
            (object [ "module_path" .= ("src/Broken.hs" :: Text) ])
  -- Restore TERM immediately after the call so later scenarios
  -- don't inherit our override.
  case oldTerm of
    Just t  -> setEnv "TERM" t
    Nothing -> unsetEnv "TERM"

  let failed        = statusOk loadR == Just False
  cFailed <- liveCheck $ checkPure
    "load · type error was reported (success=false)"
    failed
    ("If load succeeded, GHC did not surface the type error at all \
     \and the ANSI assertion below is vacuous. Raw: "
      <> truncRender loadR)
  stepFooter 1 t0

  -- Now the real oracle: flatten the whole JSON response to Text
  -- and ensure no raw ESC bytes leaked through. A properly-encoded
  -- response either strips ANSI or represents ESC as its escape
  -- form \u001b, which is ASCII-safe and present only as the
  -- 6-char escape sequence, not the raw byte.
  t1 <- stepHeader 2 "ANSI oracle · no raw 0x1B bytes in response"
  let flat        = flattenStrings loadR
      hasRawEsc   = T.any (== esc) flat
      rawEscCount = T.length (T.filter (== esc) flat)
  cNoEsc <- liveCheck $ checkPure
    ("no raw ESC byte in flattened response · count=" <>
     T.pack (show rawEscCount))
    (not hasRawEsc)
    ("The response contains raw 0x1B byte(s). Either GHC's SGR \
     \sequences are reaching the agent un-stripped, or the MCP's \
     \encoder preserved them as literal chars. Either way, \
     \downstream terminals and strict JSON parsers can break. \
     \First chars: " <> T.take 200 flat)
  stepFooter 2 t1

  -- And a positive anchor: SOME diagnostic text should be visible
  -- so we know the path exercised the error-formatter at all. The
  -- error should mention 'Int' and 'String' regardless of colour.
  t2 <- stepHeader 3 "anchor · error mentions 'Int' and '[Char]'"
  let mentionsInt  = "Int"   `T.isInfixOf` flat
      mentionsChar = "[Char]" `T.isInfixOf` flat
                  || "String" `T.isInfixOf` flat
  cAnchor <- liveCheck $ checkPure
    "error text mentions both types involved in the mismatch"
    (mentionsInt && mentionsChar)
    ("Neither 'Int' nor '[Char]/String' showed up in the flattened \
     \response, so the test did not actually trigger a type error. \
     \Flat response head: " <> T.take 200 flat)
  stepFooter 3 t2

  -- Last: the session must still be alive.
  t3 <- stepHeader 4 "session alive · ghc_eval(1+1) after the bad load"
  alive <- Client.callTool c GhcEval
             (object [ "expression" .= ("1 + 1" :: Text) ])
  cAlive <- liveCheck $ checkPure
    "session alive · ghc_eval(1+1) returns 2"
    (statusOk alive == Just True)
    ("Raw: " <> truncRender alive)
  stepFooter 4 t3

  pure [cFailed, cNoEsc, cAnchor, cAlive]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Walk the Value and concatenate every String leaf it contains,
-- separated by '\n'. Lets us do one substring check on the whole
-- response without caring which nested path the SGR escape ended
-- up on.
flattenStrings :: Value -> Text
flattenStrings = go
  where
    go (String s) = s
    go (Array xs) = T.intercalate "\n" (map go (foldr (:) [] xs))
    go (Object o) = T.intercalate "\n" (map go (KeyMap.elems o))
    go _          = T.empty

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
