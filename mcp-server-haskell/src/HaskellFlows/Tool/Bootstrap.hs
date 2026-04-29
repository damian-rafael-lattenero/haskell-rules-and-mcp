-- | @ghc_bootstrap@ — host-agnostic self-install surface.
--
-- The BUG-10 problem: 'initialize.instructions' ships agent
-- guidance in the MCP handshake, but some hosts (Claude Code,
-- Cursor) expect project-level rules files on disk BEFORE the
-- MCP connects. Pre-BUG-10 the user was instructed to clone
-- the repo just to pick up @.claude/rules/use-haskell-flows-mcp.md@
-- — breaking the "install the MCP, done" promise.
--
-- Fix: a tool that, at the agent's request, writes the canonical
-- rules file directly from content baked into the binary. The
-- content is derived from 'HaskellFlows.Mcp.Guidance.workflowRulesMarkdown'
-- so it always matches the live tool registry. No external rules
-- file needed on the user's machine.
--
-- Dry-run by default ('write=false' returns the content so the
-- agent can preview). 'write=true' opt-in writes to disk — and
-- the output path is routed through 'mkModulePath' so a
-- path-traversal attempt is rejected by the existing boundary
-- validator (CWE-22 defence). Hosts are a closed enum: the set
-- of target paths cannot be influenced by agent input beyond
-- picking one of the enumerated values.
module HaskellFlows.Tool.Bootstrap
  ( descriptor
  , handle
  , BootstrapArgs (..)
  , Host (..)
  , pathForHost
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Guidance (workflowRulesMarkdown)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath)

-- | Supported host targets. The enum is closed — adding a new
-- one requires an explicit code change + a test entry, so no
-- agent request can route writes to an unvetted path.
data Host
  = HostClaudeCode
  | HostCursor
  | HostGeneric
  deriving stock (Eq, Show)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcBootstrap
    , tdDescription =
        "Self-install host-specific guidance files from content baked "
          <> "into the MCP binary. Hosts: \"claude-code\" (writes "
          <> ".claude/rules/haskell-flows-mcp.md), \"cursor\" "
          <> "(.cursor/rules/haskell-flows-mcp.md), or \"generic\" "
          <> "(returns the text without writing). Dry-run by default; "
          <> "pass write=true to actually write the file. Content is "
          <> "dynamically derived from the live tool registry — never "
          <> "stale vs the running binary."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "host" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .=
                      (["claude-code", "cursor", "generic"] :: [Text])
                  , "description" .= ("Target host convention." :: Text)
                  ]
              , "write" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("If true, write the file to disk under the \
                       \project dir. If false (default), return the \
                       \content so the agent can preview before \
                       \committing." :: Text)
                  ]
              ]
          , "required"             .= ["host" :: Text]
          , "additionalProperties" .= False
          ]
    }

data BootstrapArgs = BootstrapArgs
  { baHost  :: !Host
  , baWrite :: !Bool
  }
  deriving stock (Show)

instance FromJSON BootstrapArgs where
  parseJSON = withObject "BootstrapArgs" $ \o -> do
    hTxt <- o .:  "host"
    w    <- o .:? "write" .!= False
    h <- case hTxt :: Text of
      "claude-code" -> pure HostClaudeCode
      "cursor"      -> pure HostCursor
      "generic"     -> pure HostGeneric
      other         -> fail ("unknown host: " <> T.unpack other)
    pure BootstrapArgs { baHost = h, baWrite = w }

handle :: ProjectDir -> [ToolDescriptor] -> Value -> IO ToolResult
handle pd descriptors rawArgs = case parseEither parseJSON rawArgs of
  Left err ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind err)
          (T.pack ("Invalid arguments: " <> err)))
            { Env.eeCause = Just (T.pack err) })))
  Right (BootstrapArgs host write) -> do
    let body = workflowRulesMarkdown descriptors
    case host of
      HostGeneric -> pure (previewResult host body Nothing)
      _ ->
        let relPath = pathForHost host
        in case mkModulePath pd relPath of
             Left e ->
               pure (Env.toolResponseToResult (Env.mkFailed
                 ((Env.mkErrorEnvelope Env.PathTraversal
                     (T.pack ("Invalid output path: " <> show e)))
                       { Env.eeCause = Just (T.pack (show e)) })))
             Right mp -> do
               let full = unModulePath mp
               if not write
                 then pure (previewResult host body (Just (T.pack full)))
                 else do
                   w <- try (do
                     createDirectoryIfMissing True (takeDirectory full)
                     TIO.writeFile full body) :: IO (Either SomeException ())
                   case w of
                     Left e ->
                       pure (Env.toolResponseToResult (Env.mkFailed
                         ((Env.mkErrorEnvelope Env.SubprocessError
                             (T.pack ("Could not write: " <> show e)))
                               { Env.eeCause = Just (T.pack (show e)) })))
                     Right _ -> pure (writeResult host full)

-- | Discriminate the FromJSON failure shape — same heuristic as
-- 'HaskellFlows.Tool.Workflow.parseErrorKind'. \"unknown host\"
-- maps to 'Validation' (the value was a string, just outside the
-- closed enum); a missing required key maps to 'MissingArg';
-- everything else falls back to 'TypeMismatch'.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "unknown host" `isInfixOfStr` err = Env.Validation
  | "key" `isInfixOfStr` err          = Env.MissingArg
  | otherwise                         = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Canonical on-disk location per host. Closed mapping; no
-- agent input flows into this.
pathForHost :: Host -> FilePath
pathForHost = \case
  HostClaudeCode -> ".claude/rules/haskell-flows-mcp.md"
  HostCursor     -> ".cursor/rules/haskell-flows-mcp.md"
  HostGeneric    -> ""   -- unused: HostGeneric never writes.

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Preview path response. Issue #90 Phase B: status='ok' with the
-- preview body inside 'result'. Same field names as before
-- ('mode', 'host', 'content', 'hint', and optionally 'target')
-- so any consumer that read those fields directly continues to
-- work during the dual-shape window.
previewResult :: Host -> Text -> Maybe Text -> ToolResult
previewResult host body mTarget =
  let payload = object $
        [ "mode"     .= ("preview" :: Text)
        , "host"     .= hostLabel host
        , "content"  .= body
        , "hint"     .=
            ( "Preview only — re-run with write=true to persist. "
              <> (case mTarget of
                    Just t  -> "Target: " <> t
                    Nothing -> "Generic mode: no target path; paste the \
                               \content wherever your host expects it.")
                :: Text )
        ]
        <> maybe [] (\t -> ["target" .= t]) mTarget
  in Env.toolResponseToResult (Env.mkOk payload)

-- | Write-completed response. Issue #90 Phase B: status='ok' with
-- the write metadata inside 'result'.
writeResult :: Host -> FilePath -> ToolResult
writeResult host path =
  let payload = object
        [ "mode" .= ("written" :: Text)
        , "host" .= hostLabel host
        , "path" .= T.pack path
        , "hint" .=
            ( "Rules written. Your host should pick them up on the \
              \next session start — for Claude Code, that means the \
              \next /claude launch; for Cursor, the next reload. \
              \No external repo clone needed to get the canonical \
              \tool surface."
              :: Text )
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

hostLabel :: Host -> Text
hostLabel = \case
  HostClaudeCode -> "claude-code"
  HostCursor     -> "cursor"
  HostGeneric    -> "generic"
