-- | @ghc_switch_project@ — repoint the MCP at a different cabal
-- project at runtime.
--
-- Why this exists
-- ---------------
-- Until this tool landed, the only way to change 'HASKELL_PROJECT_DIR'
-- was to restart Claude Code (or whichever host spawned the MCP) —
-- every path-bound tool ('ghc_load', 'ghc_check_module',
-- 'ghc_lint', 'ghc_quickcheck', …) validated its argument against
-- the 'ProjectDir' captured at server boot and rejected siblings with
-- @"target path escapes project directory"@. That friction surfaced
-- during a real dogfood session where the operator wanted to build a
-- new playground project alongside the MCP repo in a single
-- conversation.
--
-- What it does
-- ------------
-- * Validates the supplied path as an absolute 'ProjectDir'.
-- * Confirms the directory exists and contains at least one @.cabal@
--   file — refuses to point at a non-project to avoid surprising
--   downstream tools that assume a cabal layout.
-- * Tears down the in-process 'GhcSession' (if any) so the next tool
--   call boots a fresh session against the new path.
-- * Atomically swaps 'srvProjectDir'. The MVar around
--   'srvGhcSession' serialises the swap against concurrent tool
--   handlers: in-flight reads finish against the old session; fresh
--   reads start against the new one.
--
-- Response shape
-- --------------
-- @
--   {
--     "success":  true,
--     "previous": "/Users/…/old-project",
--     "current":  "/Users/…/new-project",
--     "message":  "Project directory switched."
--   }
-- @
-- Error paths return @success: false@ with a human-readable @error@
-- field — no exceptions leak out to the JSON-RPC envelope.
module HaskellFlows.Tool.SwitchProject
  ( descriptor
  , handle
  , SwitchProjectArgs (..)
  , validateSwitchTarget
  , ValidationError (..)
  , renderValidationError
  ) where

-- We take the individual mutable refs ('IORef ProjectDir' +
-- 'MVar (Maybe GhcSession)') rather than the whole 'Server' value
-- so 'handle' can be unit-tested without constructing a full
-- transport stack.
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.IORef (IORef, atomicWriteIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension)

import HaskellFlows.Ghc.ApiSession (GhcSession, killGhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types
  ( PathError (..)
  , ProjectDir
  , mkProjectDir
  , unProjectDir
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghc_switch_project"
    , tdDescription =
        "Repoint the MCP at a different cabal project without "
          <> "restarting the host. The new path must be absolute, "
          <> "must exist, and must contain at least one .cabal file. "
          <> "Tears down the current in-process GhcSession (if any) "
          <> "so the next tool call boots fresh against the new path."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Absolute path to the target cabal project \
                       \directory. Example: \
                       \\"/Users/me/projects/new-app\"." :: Text)
                  ]
              ]
          , "required"             .= ["path" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype SwitchProjectArgs = SwitchProjectArgs
  { spaPath :: Text
  }
  deriving stock (Eq, Show)

instance FromJSON SwitchProjectArgs where
  parseJSON = withObject "SwitchProjectArgs" $ \o ->
    SwitchProjectArgs <$> o .: "path"

-- | Validation errors. Closed sum so a future consumer (e.g.
-- a batch operation) gets exhaustiveness-checked via
-- @-Wincomplete-patterns@.
data ValidationError
  = VEPathError !PathError
    -- ^ 'mkProjectDir' rejected the input (non-absolute, traversal, …).
  | VENotADirectory !FilePath
    -- ^ Path is absolute and sanitised but doesn't exist on disk.
  | VENoCabalFile !FilePath
    -- ^ Directory exists but has no @*.cabal@ entry at its root.
  deriving stock (Eq, Show)

renderValidationError :: ValidationError -> Text
renderValidationError = \case
  VEPathError (PathNotAbsolute p) ->
    "path must be absolute, got: " <> p
  VEPathError (PathEscapesProject a p _) ->
    "path escapes project scope: '" <> a
      <> "' is not under '" <> p <> "'"
  VENotADirectory fp ->
    "path is not a directory or does not exist: " <> T.pack fp
  VENoCabalFile fp ->
    "no .cabal file found in: " <> T.pack fp
      <> ". Run 'cabal init' first, or pick a directory that \
         \already contains a cabal package."

-- | Pure-ish pre-flight: validates the path without touching any
-- server state. Exposed for unit tests.
--
-- Validation rules:
--
--   * Path must parse as an absolute 'ProjectDir' — 'mkProjectDir'
--     does that.
--   * Directory must exist.
--   * Directory must EITHER contain at least one @.cabal@ file
--     OR be empty. An empty directory is a valid target because
--     the canonical next step is @ghc_create_project@ — the
--     previous "must have .cabal" gate forced callers to pre-
--     scaffold a stub just to unlock the tool, which is
--     unnecessary friction. A non-empty directory without a
--     @.cabal@ is still rejected to avoid accidentally pointing
--     at a random folder (e.g. @~/Downloads@) whose contents
--     would be interpreted as sources by subsequent tools.
validateSwitchTarget :: Text -> IO (Either ValidationError ProjectDir)
validateSwitchTarget raw =
  case mkProjectDir (T.unpack raw) of
    Left pe -> pure (Left (VEPathError pe))
    Right pd -> do
      let root = unProjectDir pd
      exists <- doesDirectoryExist root
      if not exists
        then pure (Left (VENotADirectory root))
        else do
          entries <- listDirectory root
          let hasCabal = any ((".cabal" ==) . takeExtension) entries
              isEmpty  = null entries
          if hasCabal || isEmpty
            then pure (Right pd)
            else pure (Left (VENoCabalFile root))

-- | Side-effecting handler. Takes the mutable refs the server
-- already owns — we don't import 'Server' here to keep
-- 'Tool.SwitchProject' free of a layering cycle.
handle
  :: IORef ProjectDir
  -> MVar (Maybe GhcSession)
  -> Value
  -> IO ToolResult
handle pdRef sessRef rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right (SwitchProjectArgs raw) -> do
    res <- validateSwitchTarget raw
    case res of
      Left ve -> pure (errorResult (renderValidationError ve))
      Right newPd -> do
        oldPd <- readIORef pdRef
        -- Re-check scaffold state AFTER validation accepted the path
        -- (empty-dir is allowed — see 'validateSwitchTarget'). The
        -- flag lets the NextStep router distinguish \"scaffolded
        -- project → run status\" from \"empty dir → run
        -- create_project\" instead of always pointing at status.
        entries <- listDirectory (unProjectDir newPd)
        let scaffolded = any ((".cabal" ==) . takeExtension) entries
        -- Serialise the swap against any in-flight tool call that
        -- acquired the session mutex. The kill happens under the
        -- same lock that 'getOrStartGhcSession' takes, so nobody
        -- observes a half-switched server.
        modifyMVar_ sessRef $ \mSess -> do
          mapM_ killGhcSession mSess
          atomicWriteIORef pdRef newPd
          pure Nothing
        pure (successResult oldPd newPd scaffolded)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: ProjectDir -> ProjectDir -> Bool -> ToolResult
successResult oldPd newPd scaffolded =
  let payload = object
        [ "success"    .= True
        , "previous"   .= T.pack (unProjectDir oldPd)
        , "current"    .= T.pack (unProjectDir newPd)
        , "scaffolded" .= scaffolded
        , "message"    .= ("Project directory switched. Next tool \
                           \call boots a fresh GhcSession." :: Text)
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent =
        [ TextContent (encodeUtf8Text (object
            [ "success" .= False
            , "error"   .= msg
            ]))
        ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
