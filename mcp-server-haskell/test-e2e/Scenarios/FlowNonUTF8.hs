-- | Flow: .hs files with invalid UTF-8 must surface a structured
-- error, not crash the loader or smuggle garbled text back to the
-- client.
--
-- The MCP's 'ghc_load' drives @cabal repl@ which invokes GHC; GHC
-- assumes UTF-8 source files. A pathological file (byte 0xFF somewhere
-- mid-module) can make three different things happen, and only one
-- of them is right:
--
--   (a) GHC errors with a clean "invalid UTF-8" message that the MCP
--       parses into its normal errors[] array — success=false with
--       a diagnostic. This is the contract we want.
--   (b) GHC panics, the process dies, the MCP detects Dead and
--       evicts the session — recovery succeeds but the raw panic
--       text may leak into the response.
--   (c) The MCP's parser chokes on the non-UTF-8 byte while
--       building its Text-valued JSON response, throws an
--       encoding exception, and runTool catches it as a
--       tool_exception. Also survivable, less informative.
--
-- Failure modes the oracle catches:
--
--   * The tool response becomes an invalid JSON envelope (the
--     non-UTF-8 byte in an error field breaks the encoding path).
--   * The response claims success=true with empty errors[] — GHC
--     silently swallowed the file and the MCP didn't notice.
--   * The server hangs waiting for a sentinel because a non-UTF-8
--     byte desynced the framing read.
module Scenarios.FlowNonUTF8
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

-- | Haskell source with raw 0xFF bytes planted OUTSIDE any comment
-- or string literal — specifically in the middle of a numeric
-- expression. A 0xFF byte inside a comment is silently accepted
-- by GHC (comments are not re-tokenised), so an earlier version of
-- this scenario reported green for the wrong reason. Putting the
-- bytes between the '=' and the literal forces UTF-8 decoding of
-- the token stream and triggers a real parse/decode error.
evilSourceBytes :: BS.ByteString
evilSourceBytes = BS.concat
  [ "module Evil (x) where\n\nx :: Int\nx = 4"
  , BS.pack [0xFF, 0xFE, 0xFF]  -- in the middle of the RHS
  , "2\n"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  _ <- Client.callTool c "ghc_create_project"
         (object [ "name" .= ("nonutf8-demo" :: Text) ])

  -- Plant the evil file directly on disk — we explicitly do not go
  -- through ghc_add_modules because that scaffolds with a clean
  -- stub. We want the raw bytes under the MCP's feet.
  let evilPath = projectDir </> "src" </> "Evil.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  BS.writeFile evilPath evilSourceBytes

  -- Also register the module in the cabal file so ghc_load tries
  -- to compile it. Uses the public MCP surface so we're still
  -- black-box.
  _ <- Client.callTool c "ghc_add_modules"
         (object [ "modules" .= (["Evil"] :: [Text]) ])

  t0 <- stepHeader 1 "load · ghc_load on a non-UTF-8 module"
  r  <- Client.callTool c "ghc_load"
          (object [ "module_path" .= ("src/Evil.hs" :: Text) ])
  let ok        = fieldBool "success" r
      errsField = lookupField "errors" r
      hasErrors = case errsField of
        Just (Array xs) -> not (null xs)
        _               -> False
      -- Any of these shapes is acceptable: the MCP surfaced SOME
      -- failure. The WRONG outcome is success=true with no errors.
      rejectedCleanly = ok == Just False || hasErrors
  cReject <- liveCheck $ checkPure
    "rejected cleanly · success=false OR errors[] non-empty"
    rejectedCleanly
    ("A .hs file with raw 0xFF bytes must not load as if it were \
     \valid. Got success=" <> T.pack (show ok)
     <> ", errors.isEmpty=" <> T.pack (show (not hasErrors))
     <> ". If success=true and errors=[], GHC silently accepted the \
        \garbage file. Raw: " <> truncRender r)

  -- Also prove the error surface is JSON-safe. If a 0xFF byte bled
  -- into an error field, the response wouldn't have parsed at all
  -- at the transport layer — but a more subtle corruption (UTF-8
  -- replacement characters, truncated text) is worth sanity-checking.
  cShape <- liveCheck $ checkPure
    "response is a JSON object (no transport corruption)"
    (case r of Object _ -> True; _ -> False)
    ("Non-UTF-8 bytes in the error path must not corrupt the JSON \
     \envelope. Raw: " <> truncRender r)
  stepFooter 1 t0

  -- Session must still be alive. A parser desync from the 0xFF byte
  -- would make the next call hang.
  t1 <- stepHeader 2 "session alive · next ghc_eval(1+1) works"
  alive <- Client.callTool c "ghc_eval"
             (object [ "expression" .= ("1 + 1" :: Text) ])
  cAlive <- liveCheck $ checkPure
    "session alive · ghc_eval(1+1) returns 2"
    (fieldBool "success" alive == Just True
     && case lookupField "output" alive of
          Just (String s) -> "2" `T.isInfixOf` s
          _               -> False)
    ("If the session wedges after a non-UTF-8 load, the framing read \
     \picked up garbage bytes and is now desynced. Raw: "
      <> truncRender alive)
  stepFooter 2 t1

  pure [cReject, cShape, cAlive]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
