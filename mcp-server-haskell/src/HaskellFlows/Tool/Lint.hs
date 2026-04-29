-- | @ghc_lint@ — HLint wrapper that matches CI semantics by default.
--
-- Diseñado con la lección de `docs/ts-mcp-retrospective.md` § B3
-- internalizada: el TS original sólo acepta un archivo individual,
-- entonces es natural lintear src/ y olvidar test/. CI corre
-- hlint sobre todo el directorio y atrapa el gap — autor-time ↔ CI
-- drift. Este tool acepta un directorio por default y recursa igual
-- que CI, eliminando la clase de fallos de raíz.
--
-- Accepts either @path=\"mcp-server-haskell/\"@ (recursive) or
-- @module_path=\"src/Foo.hs\"@ (single file). Both forms route through
-- the same hlint process; only argv differs.
--
-- Security:
--
-- * @path@ / @module_path@ goes through mkModulePath's flavour of
--   validation via 'resolveRelative' — the hlint invocation cannot
--   escape the project directory.
-- * argv-form spawn; no shell.
module HaskellFlows.Tool.Lint
  ( descriptor
  , handle
  , LintArgs (..)
  , Suggestion (..)
  , parseHlintJson
  , resolveTarget
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import System.FilePath
  ( equalFilePath
  , isAbsolute
  , normalise
  , pathSeparator
  , splitDirectories
  , (</>)
  )
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

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcLint
    , tdDescription =
        "Run HLint. Accepts either `path` (directory, recursive) or "
          <> "`module_path` (single file). `path` is the default form "
          <> "and matches CI's behaviour exactly — no more author/CI "
          <> "drift. Returns structured suggestions with severity and "
          <> "location."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Directory to lint (recursive). Example: "
                       <> "\"mcp-server-haskell/\" or \"src/\"." :: Text)
                  ]
              , "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Single file to lint. Faster for inner loops; "
                       <> "use `path` before pushing." :: Text)
                  ]
              , "fail_on" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .=
                      (["suggestion", "warning", "error"] :: [Text])
                  , "description" .=
                      ("Minimum severity that counts as failure. "
                       <> "Default: warning." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data LintArgs = LintArgs
  { laPath       :: !(Maybe Text)
  , laModulePath :: !(Maybe Text)
  , laFailOn     :: !Text
  }
  deriving stock (Show)

instance FromJSON LintArgs where
  parseJSON = withObject "LintArgs" $ \o -> do
    p  <- o .:? "path"
    mp <- o .:? "module_path"
    f  <- o .:? "fail_on" .!= ("warning" :: Text)
    pure LintArgs { laPath = p, laModulePath = mp, laFailOn = f }

hlintTimeoutMicros :: Int
hlintTimeoutMicros = 60 * 1_000_000  -- 60 s

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (formatParseError parseError)
  Right args -> case resolveTarget pd args of
    Left err     -> pure (pathTraversalResult err)
    Right target -> do
      mHlint <- findExecutable "hlint"
      case mHlint of
        Nothing ->
          pure (unavailableResult "hlint binary not found on PATH")
        Just _ -> do
          res <- runHlint pd target
          pure (renderResult target (laFailOn args) res)


-- | Issue #90 Phase C: 'resolveTarget' rejected the input as
-- escaping the project root → status='refused', kind='path_traversal'.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

-- | Resolve which path hlint should lint. Prefer `module_path`
-- (single file, fastest inner loop), fall back to `path` (directory
-- — matches CI). If neither, default to the project root itself.
--
-- Path-traversal guard mirrors 'HaskellFlows.Types.mkModulePath':
-- after joining and normalising, refuse any path whose directory
-- segments contain @..@, or whose normalised form does not live
-- under the project root. The previous string-prefix check
-- (issue #81 / CWE-22) accepted @"<root>/../.."@ because the
-- literal prefix matched, leaving hlint to run wherever the path
-- actually pointed (e.g. the parent directory of the project).
resolveTarget :: ProjectDir -> LintArgs -> Either Text FilePath
resolveTarget pd args =
  let raw = case (laPath args, laModulePath args) of
        (_, Just mp)        -> T.unpack mp
        (Just p,  Nothing)  -> T.unpack p
        (Nothing, Nothing)  -> ""
      root      = unProjectDir pd
      rootN     = normalise root
      joined
        | null raw       = rootN
        | isAbsolute raw = normalise raw
        | otherwise      = normalise (root </> raw)
      prefix    = rootN <> [pathSeparator]
      segments  = splitDirectories joined
      hasDotDot = ".." `elem` segments
      insidePrefix =
        joined `equalFilePath` rootN
          || take (length prefix) joined == prefix
  in if hasDotDot || not insidePrefix
       then Left ("target path escapes project directory: " <> T.pack raw)
       else Right joined

--------------------------------------------------------------------------------
-- subprocess
--------------------------------------------------------------------------------

data HlintOutcome
  = HlOk   !Text           -- raw JSON output
  | HlTimeout
  | HlFailure !Int !Text
  | HlMissingTarget
  deriving stock (Show)

runHlint :: ProjectDir -> FilePath -> IO HlintOutcome
runHlint pd target = do
  let cp = (proc "hlint" ["--json", target])
             { cwd     = Just (unProjectDir pd)
             , std_in  = NoStream
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  -- Pre-flight: hlint on a missing target prints an opaque error.
  -- Check existence up front for a cleaner message.
  existsDir  <- doesDirectoryExist target
  existsFile <- doesFileExist target
  if not (existsDir || existsFile)
    then pure HlMissingTarget
    else do
      (_, Just hOut, Just hErr, ph) <- createProcess cp
      outVar <- newEmptyMVar
      errVar <- newEmptyMVar
      _ <- forkIO (hGetContents hOut >>= putMVar outVar)
      _ <- forkIO (hGetContents hErr >>= putMVar errVar)
      exited <- timeout hlintTimeoutMicros (waitForProcess ph)
      case exited of
        Nothing -> do
          terminateProcess ph
          hClose hOut
          hClose hErr
          pure HlTimeout
        Just _ -> do
          -- hlint exits 1 when it finds hints (not an error for us).
          o <- takeMVar outVar
          e <- takeMVar errVar
          if null o && not (null e)
            then pure (HlFailure 1 (T.pack e))
            else pure (HlOk (T.pack o))

--------------------------------------------------------------------------------
-- parser
--------------------------------------------------------------------------------

-- | One hint from hlint's JSON output, in the subset we forward.
data Suggestion = Suggestion
  { sSeverity    :: !Text
  , sHint        :: !Text
  , sFile        :: !Text
  , sStartLine   :: !Int
  , sStartColumn :: !Int
  , sFrom        :: !Text
  , sTo          :: !Text
  }
  deriving stock (Eq, Show)

instance ToJSON Suggestion where
  toJSON s =
    object
      [ "severity"    .= sSeverity s
      , "hint"        .= sHint s
      , "file"        .= sFile s
      , "startLine"   .= sStartLine s
      , "startColumn" .= sStartColumn s
      , "from"        .= sFrom s
      , "to"          .= sTo s
      ]

instance FromJSON Suggestion where
  parseJSON = withObject "Suggestion" $ \o ->
    Suggestion
      <$> o .:  "severity"
      <*> o .:  "hint"
      <*> o .:  "file"
      <*> o .:  "startLine"
      <*> o .:  "startColumn"
      <*> (fromMaybe "" <$> o .:? "from")
      <*> (fromMaybe "" <$> o .:? "to")

parseHlintJson :: Text -> [Suggestion]
parseHlintJson raw =
  case eitherDecode (TLE.encodeUtf8 (TL.fromStrict raw)) of
    Right xs -> xs
    Left _   -> []

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: each hlint subprocess outcome maps to a
-- typed envelope. The success path carries the suggestions list
-- under 'result' so callers can iterate per-hint; offending hints
-- (>= fail_on) flip the status to 'failed' but the same payload
-- is preserved so the caller can still render and decide.
renderResult :: FilePath -> Text -> HlintOutcome -> ToolResult
renderResult target failOn (HlOk raw) =
  let suggestions = parseHlintJson raw
      offending   = filter (atOrAbove failOn . sSeverity) suggestions
      payload =
        object
          [ "target"       .= T.pack target
          , "fail_on"      .= failOn
          , "count"        .= length suggestions
          , "blocking"     .= length offending
          , "suggestions"  .= suggestions
          ]
  in if null offending
       then Env.toolResponseToResult (Env.mkOk payload)
       else
         let envErr   = Env.mkErrorEnvelope Env.Validation
                          ( T.pack (show (length offending))
                              <> " hint(s) at or above '"
                              <> failOn <> "' threshold" )
             response = (Env.mkFailed envErr) { Env.reResult = Just payload }
         in Env.toolResponseToResult response
renderResult _ _ HlTimeout =
  let envErr = (Env.mkErrorEnvelope Env.InnerTimeout
                  ("hlint timed out after 60 seconds" :: Text))
                 { Env.eeCause = Just "60s" }
  in Env.toolResponseToResult (Env.mkTimeout envErr)
renderResult _ _ (HlFailure code err) =
  let msg    = "hlint failed with exit code " <> T.pack (show code)
                 <> ": " <> T.strip err
      envErr = (Env.mkErrorEnvelope Env.SubprocessError msg)
                 { Env.eeCause = Just (T.pack (show code)) }
  in Env.toolResponseToResult (Env.mkFailed envErr)
renderResult target _ HlMissingTarget =
  Env.toolResponseToResult (Env.mkFailed
    (Env.mkErrorEnvelope Env.ModulePathDoesNotExist
       ("target path does not exist: " <> T.pack target)))

-- | severity ordering: ignore < suggestion < warning < error.
severityRank :: Text -> Int
severityRank = \case
  "Ignore"     -> 0
  "Suggestion" -> 1
  "Warning"    -> 2
  "Error"      -> 3
  _            -> 1   -- unknown → treat as suggestion (safe default)

atOrAbove :: Text -> Text -> Bool
atOrAbove threshold severity =
  severityRank severity >= severityRank (canonSeverity threshold)
  where
    canonSeverity "suggestion" = "Suggestion"
    canonSeverity "warning"    = "Warning"
    canonSeverity "error"      = "Error"
    canonSeverity x            = x

-- | Issue #90 Phase C: hlint binary missing → status='unavailable'
-- kind='binary_unavailable'. The 'remediation' string lives under
-- 'result' so it stays readable when the consumer is showing an
-- error banner.
unavailableResult :: Text -> ToolResult
unavailableResult msg =
  let payload  = object
        [ "remediation" .= ( "Install hlint: `cabal install hlint` or \
                            \`ghcup install hls` (bundles hlint)." :: Text )
        ]
      envErr   = Env.mkErrorEnvelope Env.BinaryUnavailable msg
      response = (Env.mkUnavailable envErr) { Env.reResult = Just payload }
  in Env.toolResponseToResult response
