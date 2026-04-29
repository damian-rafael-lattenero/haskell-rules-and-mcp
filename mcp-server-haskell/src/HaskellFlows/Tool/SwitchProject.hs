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
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension)

import HaskellFlows.Data.PropertyStore (Store, openStore)
import HaskellFlows.Ghc.ApiSession (GhcSession, killGhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types
  ( PathError (..)
  , ProjectDir
  , mkProjectDir
  , unProjectDir
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcSwitchProject
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
--
-- Issue #39: the third parameter ('IORef Store') is the property
-- store handle the server keeps in 'srvStore'. The handler
-- atomically opens a fresh 'Store' rooted at the new project's
-- @.haskell-flows/properties.json@ and swaps it in. Without
-- that, every subsequent @ghc_check_module@ / @ghc_regression@
-- after a switch would still consult the previous project's
-- stored properties — the surprising behaviour that motivated
-- the bug report.
handle
  :: IORef ProjectDir
  -> MVar (Maybe GhcSession)
  -> IORef Store
  -> Value
  -> IO ToolResult
handle pdRef sessRef storeRef rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (formatParseError err)
  Right (SwitchProjectArgs raw) -> do
    res <- validateSwitchTarget raw
    case res of
      Left ve -> pure (validationErrorResult ve)
      Right newPd -> do
        oldPd <- readIORef pdRef
        -- Re-check scaffold state AFTER validation accepted the path
        -- (empty-dir is allowed — see 'validateSwitchTarget'). The
        -- flag lets the NextStep router distinguish \"scaffolded
        -- project → run status\" from \"empty dir → run
        -- create_project\" instead of always pointing at status.
        entries <- listDirectory (unProjectDir newPd)
        let scaffolded = any ((".cabal" ==) . takeExtension) entries
        -- Open the new project's store BEFORE we take the session
        -- lock. 'openStore' is a stat + IORef new-MVar — quick and
        -- never throws — so doing it outside the critical section
        -- keeps the swap window tight. Then serialise the
        -- (kill-session, swap-pdRef, swap-storeRef) trio under the
        -- session MVar so concurrent tool handlers either see the
        -- complete old triple or the complete new one.
        newStore <- openStore newPd
        modifyMVar_ sessRef $ \mSess -> do
          mapM_ killGhcSession mSess
          atomicWriteIORef pdRef    newPd
          atomicWriteIORef storeRef newStore
          pure Nothing
        pure (successResult oldPd newPd scaffolded)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: switch landed → status='ok' with the
-- ('previous', 'current', 'scaffolded') fields under 'result'.
-- The boolean 'scaffolded' lets the NextStep router branch
-- between 'run status' (project ready) and 'run create_project'
-- (empty dir).
successResult :: ProjectDir -> ProjectDir -> Bool -> ToolResult
successResult oldPd newPd scaffolded =
  Env.toolResponseToResult (Env.mkOk (object
    [ "previous"   .= T.pack (unProjectDir oldPd)
    , "current"    .= T.pack (unProjectDir newPd)
    , "scaffolded" .= scaffolded
    , "message"    .= ("Project directory switched. Next tool \
                       \call boots a fresh GhcSession." :: Text)
    ]))


-- | Issue #90 Phase C: closed-enum dispatch over the validation
-- failure modes. Each maps to a typed kind:
--   * 'VEPathError'     → kind='path_traversal' (refused).
--   * 'VENotADirectory' → kind='module_path_does_not_exist'.
--   * 'VENoCabalFile'   → kind='module_not_in_graph' (no cabal
--                         project in the target — caller likely
--                         wants ghc_create_project).
validationErrorResult :: ValidationError -> ToolResult
validationErrorResult ve =
  let msg = renderValidationError ve
  in case ve of
       VEPathError _ ->
         Env.toolResponseToResult
           (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))
       VENotADirectory _ ->
         Env.toolResponseToResult
           (Env.mkFailed (Env.mkErrorEnvelope Env.ModulePathDoesNotExist msg))
       VENoCabalFile _ ->
         let payload  = object
               [ "remediation" .= ( "Run ghc_create_project to scaffold \
                                   \a cabal layout, then retry." :: Text )
               ]
             envErr   = Env.mkErrorEnvelope Env.ModuleNotInGraph msg
             response = (Env.mkNoMatch payload) { Env.reError = Just envErr }
         in Env.toolResponseToResult response
