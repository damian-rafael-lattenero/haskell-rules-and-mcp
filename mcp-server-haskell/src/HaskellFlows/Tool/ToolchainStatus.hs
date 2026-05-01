-- | Internal handler for the @status@ branch of @ghc_toolchain@.
--
-- Single-call availability inventory for every external binary the
-- MCP can delegate to. Returns, per binary:
--
-- * @available@ — boolean, from 'findExecutable'
-- * @path@ — absolute resolved path, when available
-- * @version@ — best-effort @tool --version@ first line (nil on
--   timeout / parse failure — not treated as \"unavailable\", just
--   \"version unknown\")
-- * @category@ — @gate@ (blocks CI), @workflow@ (optional), @query@
--
-- Pure query; does not mutate session state.
--
-- Issue #94 Phase C retired the @ghc_toolchain_status@ wire surface;
-- 'HaskellFlows.Tool.Toolchain' is the single externally-advertised
-- tool. This module's 'handle' is the implementation
-- 'Toolchain.handle' forwards to when @action="status"@.
module HaskellFlows.Tool.ToolchainStatus
  ( handle
    -- * Install-hint table (re-used by tools that report
    -- @status="unavailable"@ to populate their @remediation@ field)
  , installHintFor
    -- * Canonical list of optional (non-blocking) binaries — re-used
    -- by 'HaskellFlows.Tool.Workflow' to surface a session-start nudge
    -- without re-doing the per-binary version probe.
  , optionalBinaryNames
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hClose, hGetContents)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

-- | (binary name, version-flag, category).
--
-- Category convention:
--
-- * @gate@ — required for module-complete gates (cabal, ghc, hlint).
-- * @workflow@ — enables a named workflow (fourmolu/ormolu for
--   format, hls for refactor, hoogle for search).
-- * @query@ — optional improvement but not blocking.
probeTargets :: [(Text, String, Text)]
probeTargets =
  [ ("cabal",    "--numeric-version", "gate")
  , ("ghc",      "--numeric-version", "gate")
  , ("hlint",    "--version",         "gate")
  , ("fourmolu", "--version",         "workflow")
  , ("ormolu",   "--version",         "workflow")
  , ("hoogle",   "--version",         "query")
  , ("hls",      "--numeric-version", "workflow")
  , ("haskell-language-server", "--numeric-version", "workflow")
  ]

-- | Names of the optional (non-gate) binaries.  Single source of
-- truth — derived from 'probeTargets' so dropping a binary from the
-- probe set automatically updates every consumer.
--
-- Used by 'HaskellFlows.Tool.Workflow.statusPayload' to surface a
-- one-shot session-start nudge listing what is missing + the
-- copy-pasteable install commands, without paying for the per-binary
-- @--version@ probe (a single 'findExecutable' per name is enough
-- for the boolean answer the nudge needs).
optionalBinaryNames :: [Text]
optionalBinaryNames =
  [ name | (name, _, category) <- probeTargets, category /= "gate" ]

versionTimeoutMicros :: Int
versionTimeoutMicros = 3_000_000  -- 3s per binary

handle :: Value -> IO ToolResult
handle rawArgs = case parseEither parseJSON rawArgs :: Either String Value of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope Env.MissingArg
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right _ -> do
    entries <- mapM probeOne probeTargets
    pure (renderResult entries)

--------------------------------------------------------------------------------
-- probe
--------------------------------------------------------------------------------

data Entry = Entry
  { eName      :: !Text
  , eCategory  :: !Text
  , eAvailable :: !Bool
  , ePath      :: !(Maybe FilePath)
  , eVersion   :: !(Maybe Text)
  }

probeOne :: (Text, String, Text) -> IO Entry
probeOne (name, verFlag, category) = do
  mPath <- findExecutable (T.unpack name)
  case mPath of
    Nothing ->
      pure Entry
        { eName = name, eCategory = category
        , eAvailable = False, ePath = Nothing, eVersion = Nothing
        }
    Just p -> do
      mVer <- getVersion p verFlag
      pure Entry
        { eName = name, eCategory = category
        , eAvailable = True, ePath = Just p, eVersion = mVer
        }

-- | Capture a best-effort first line of @tool --version@ with a hard
-- timeout. Failure to parse returns 'Nothing' — the tool is still
-- available, we just didn't manage to extract a version string.
getVersion :: FilePath -> String -> IO (Maybe Text)
getVersion bin verFlag = do
  let cp = (proc bin [verFlag])
             { std_in  = NoStream
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  outVar <- newEmptyMVar
  _ <- forkIO (hGetContents hOut >>= putMVar outVar)
  _ <- forkIO (void (hGetContents hErr))
  exited <- timeout versionTimeoutMicros (waitForProcess ph)
  case exited of
    Nothing -> do
      terminateProcess ph
      hClose hOut
      hClose hErr
      pure Nothing
    Just ExitSuccess -> do
      o <- takeMVar outVar
      pure (firstLine (T.pack o))
    Just _ ->
      pure Nothing

firstLine :: Text -> Maybe Text
firstLine t = case T.lines (T.strip t) of
  (l:_) | not (T.null l) -> Just l
  _                      -> Nothing

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Build the response. After issue #90 Phase B, two outcomes are
-- distinguished structurally on the wire:
--
-- * Every blocking gate available → 'Env.StatusOk'.
-- * One or more optional binaries missing but every blocking gate
--   present → 'Env.StatusPartial' with a 'Env.SlowPath' warning per
--   missing optional. The result still carries the full @tools@
--   inventory so a consumer that doesn't care about the discriminator
--   keeps reading the same shape.
-- * Any blocking gate missing → 'Env.StatusFailed' with
--   'Env.BinaryUnavailable'. The dependency is structural; rerunning
--   without installing the binary will fail again.
--
-- The legacy @success@ field on the wire is auto-derived by
-- 'Env.isLegacySuccess'; clients that key on it keep working until
-- Phase D removes the field.
renderResult :: [Entry] -> ToolResult
renderResult entries =
  let blocking      = filter (\e -> eCategory e == "gate" && not (eAvailable e)) entries
      missingOpt    = filter (\e -> eCategory e /= "gate" && not (eAvailable e)) entries
      payload =
        object
          [ "tools"           .= map renderEntry entries
          , "blocking_gates"  .= map eName blocking
          , "summary"         .= summarise entries blocking
          ]
      response = case (blocking, missingOpt) of
        ([], [])   -> Env.mkOk payload
        ([], opts) -> Env.withWarnings (map missingOptionalWarning opts)
                        (Env.mkPartial payload)
        (bs, _)    -> Env.mkFailed
          ((Env.mkErrorEnvelope Env.BinaryUnavailable
              ("blocking gate(s) unavailable: " <> T.intercalate ", " (map eName bs)))
                { Env.eeRemediation =
                    Just ("Install the missing binaries via ghcup / cabal: "
                       <> T.intercalate ", " (map eName bs)) })
  in Env.toolResponseToResult response

missingOptionalWarning :: Entry -> Env.Warning
missingOptionalWarning e = Env.Warning
  { Env.wKind    = Env.SlowPath
  , Env.wMessage = "optional binary '" <> eName e
                <> "' is unavailable; tools that delegate to it will "
                <> "return status='unavailable'. Install: "
                <> installHintFor (eName e)
  , Env.wExtra   = Just (object
      [ "binary"       .= eName e
      , "category"     .= eCategory e
      , "install_hint" .= installHintFor (eName e)
      ])
  }

-- | Closed mapping: optional-binary name → copy-pasteable install
-- command.  Hardcoded by design — the canonical install path for each
-- binary is well-known and stable, and a closed lookup means agents
-- never see a wrong-package or curl|sh suggestion injected from the
-- environment.
--
-- Both 'hls' and 'haskell-language-server' route to the same
-- @ghcup install hls --set@ because that is the only supported HLS
-- distribution path — Hackage @cabal install haskell-language-server@
-- exists but breaks against bleeding-edge GHC and is documented as
-- not recommended.
installHintFor :: Text -> Text
installHintFor name = case name of
  "fourmolu"                -> "cabal install fourmolu"
  "ormolu"                  -> "cabal install ormolu"
  "hoogle"                  -> "cabal install hoogle && hoogle generate"
  "hls"                     -> "ghcup install hls --set"
  "haskell-language-server" -> "ghcup install hls --set"
  -- Defensive fallback — shouldn't fire while the closed enum in
  -- 'probeTargets' is the input set, but a tracking note for the
  -- agent is still better than a silent empty hint.
  other                     -> "(no install hint for '" <> other <> "')"

renderEntry :: Entry -> Value
renderEntry e =
  object
    [ "name"      .= eName e
    , "category"  .= eCategory e
    , "available" .= eAvailable e
    , "path"      .= ePath e
    , "version"   .= eVersion e
    ]

summarise :: [Entry] -> [Entry] -> Text
summarise entries blocking =
  let n    = length entries
      avail = length (filter eAvailable entries)
  in T.pack (show avail) <> " of " <> T.pack (show n)
     <> " tools available" <> case blocking of
          [] -> "."
          bs -> "; blocking gates: "
             <> T.intercalate ", " (map eName bs)

