-- | @ghc_format@ — run @fourmolu@ or @ormolu@ on a module.
--
-- Mirrors the TS tool's resolution policy: try @fourmolu@ first, fall
-- back to @ormolu@, report unavailable if neither is on @PATH@. With
-- @write=true@ the tool rewrites the file in-place; with @write=false@
-- (the default) it returns the formatted text without touching disk —
-- safer default, the agent inspects the diff first.
--
-- Boundary safety: the module path goes through 'mkModulePath', so a
-- caller cannot trick us into formatting (or rewriting!) a file
-- outside the project tree.
module HaskellFlows.Tool.Format
  ( descriptor
  , handle
  , FormatArgs (..)
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
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
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types
  ( ModulePath
  , PathError (..)
  , ProjectDir
  , canonicalModulePathCheck
  , mkModulePath
  , unModulePath
  , unProjectDir
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcFormat
    , tdDescription =
        "Format a Haskell module with fourmolu (preferred) or ormolu "
          <> "(fallback). Default is check-only — pass write=true to "
          <> "rewrite the file in-place. Reports availability when "
          <> "neither formatter is on PATH."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Relative path to the module." :: Text)
                  ]
              , "write" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("Rewrite the file in place. Default: false \
                       \(returns formatted text only)." :: Text)
                  ]
              ]
          , "required"             .= ["module_path" :: Text]
          , "additionalProperties" .= False
          ]
    }

data FormatArgs = FormatArgs
  { faModulePath :: !Text
  , faWrite      :: !Bool
  }
  deriving stock (Show)

instance FromJSON FormatArgs where
  parseJSON = withObject "FormatArgs" $ \o -> do
    mp <- o .:  "module_path"
    w  <- o .:? "write" .!= False
    pure FormatArgs { faModulePath = mp, faWrite = w }

formatTimeoutMicros :: Int
formatTimeoutMicros = 30 * 1_000_000  -- 30 s

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (formatParseError parseError)
  Right args -> case mkModulePath pd (T.unpack (faModulePath args)) of
    Left e -> pure (pathTraversalResult (formatPathError e))
    Right mp -> do
      -- Issue #100 Phase D: defence-in-depth canonical check before any
      -- file write. 'mkModulePath' is a pure lexical guard; this IO-level
      -- check canonicalises both paths (resolves symlinks) so a symlink
      -- pointing outside the project root is caught here even if the
      -- lexical guard passed.
      canonResult <- canonicalModulePathCheck pd mp
      case canonResult of
        Left e -> pure (pathTraversalResult (formatPathError e))
        Right () -> do
          mFormatter <- resolveFormatter
          case mFormatter of
            Nothing ->
              pure (unavailableResult "Neither fourmolu nor ormolu was found on PATH")
            Just f -> do
              outcome <- runFormatter pd f mp (faWrite args)
              pure (renderResult f mp (faWrite args) outcome)

-- | Issue #90 Phase C: caller-side parse failure → status='failed'
-- with kind='missing_arg' (missing key) or 'type_mismatch'.

-- | Issue #90 Phase C: 'mkModulePath' rejected the input → that's
-- a path-traversal refusal. Status='refused', kind='path_traversal'.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

-- | Which formatter we plan to invoke. 'fBinary' is the absolute path
-- resolved at the time of the call so a mid-call PATH change can't
-- make us spawn a different binary than we reported.
data Formatter = Formatter { fName :: !Text, fBinary :: !FilePath }
  deriving stock (Eq, Show)

resolveFormatter :: IO (Maybe Formatter)
resolveFormatter = do
  mFour <- findExecutable "fourmolu"
  case mFour of
    Just p  -> pure (Just Formatter { fName = "fourmolu", fBinary = p })
    Nothing -> do
      mOr <- findExecutable "ormolu"
      pure (fmap (\p -> Formatter { fName = "ormolu", fBinary = p }) mOr)

--------------------------------------------------------------------------------
-- subprocess
--------------------------------------------------------------------------------

data FmtOutcome
  = FmtOk !Text      -- output (or "" if we wrote in place)
  | FmtTimeout
  | FmtFailure !Int !Text
  deriving stock (Eq, Show)

runFormatter :: ProjectDir -> Formatter -> ModulePath -> Bool -> IO FmtOutcome
runFormatter pd f mp write = do
  -- We pass the traversal-checked absolute path from 'unModulePath'
  -- directly; --mode inplace rewrites in place, --mode stdout returns
  -- the formatted text without touching disk.
  let mode = if write then "inplace" else "stdout"
      args = ["--mode", mode, unModulePath mp]
      cp   = (proc (T.unpack (fName f)) args)
               { cwd     = Just (unProjectDir pd)
               , std_in  = NoStream
               , std_out = CreatePipe
               , std_err = CreatePipe
               }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  outVar <- newEmptyMVar
  errVar <- newEmptyMVar
  _ <- forkIO (hGetContents hOut >>= putMVar outVar)
  _ <- forkIO (hGetContents hErr >>= putMVar errVar)
  exited <- timeout formatTimeoutMicros (waitForProcess ph)
  case exited of
    Nothing -> do
      terminateProcess ph
      hClose hOut
      hClose hErr
      pure FmtTimeout
    Just ExitSuccess -> do
      o <- takeMVar outVar
      pure (FmtOk (T.pack o))
    Just (ExitFailure code) -> do
      e <- takeMVar errVar
      pure (FmtFailure code (T.pack e))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: every outcome of the formatter subprocess
-- maps to a typed envelope:
--
-- * 'FmtOk'      → status='ok' with the formatted text under
--                  'result.formatted'.
-- * 'FmtTimeout' → status='timeout' with kind='inner_timeout' and
--                  the 30 s budget surfaced in 'cause' so callers
--                  can distinguish from the 10-min outer guard.
-- * 'FmtFailure' → status='failed' with kind='subprocess_error',
--                  exit code under 'cause', stderr in the message.
renderResult :: Formatter -> ModulePath -> Bool -> FmtOutcome -> ToolResult
renderResult _ _ _ (FmtOk out) =
  Env.toolResponseToResult (Env.mkOk (object
    [ "formatted" .= out
    ]))
renderResult _ _ _ FmtTimeout =
  let envErr = (Env.mkErrorEnvelope Env.InnerTimeout
                  ("formatter timed out after 30 seconds" :: Text))
                 { Env.eeCause = Just "30s" }
  in Env.toolResponseToResult (Env.mkTimeout envErr)
renderResult _ _ _ (FmtFailure code err) =
  let msg    = "formatter exited with code " <> T.pack (show code)
                 <> ": " <> T.strip err
      envErr = (Env.mkErrorEnvelope Env.SubprocessError msg)
                 { Env.eeCause = Just (T.pack (show code)) }
  in Env.toolResponseToResult (Env.mkFailed envErr)

-- | Issue #90 Phase C: neither fourmolu nor ormolu on PATH →
-- status='unavailable', kind='binary_unavailable'. The
-- 'remediation' string lives under 'result' so it stays readable
-- even when the consumer is showing an error banner.
unavailableResult :: Text -> ToolResult
unavailableResult msg =
  let payload  = object
        [ "remediation" .= ( "Install fourmolu (`cabal install fourmolu`) \
                            \or ormolu and retry." :: Text )
        ]
      envErr   = Env.mkErrorEnvelope Env.BinaryUnavailable msg
      response = (Env.mkUnavailable envErr) { Env.reResult = Just payload }
  in Env.toolResponseToResult response

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p        -> "Project directory is not absolute: " <> p
  PathEscapesProject a p _ -> "module_path '" <> a <> "' escapes project directory " <> p
