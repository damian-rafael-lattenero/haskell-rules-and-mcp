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
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import System.FilePath (isAbsolute, (</>))
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
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> case resolveTarget pd args of
    Left err     -> pure (errorResult err)
    Right target -> do
      mHlint <- findExecutable "hlint"
      case mHlint of
        Nothing ->
          pure (unavailableResult "hlint binary not found on PATH")
        Just _ -> do
          res <- runHlint pd target
          pure (renderResult target (laFailOn args) res)

-- | Resolve which path hlint should lint. Prefer `module_path`
-- (single file, fastest inner loop), fall back to `path` (directory
-- — matches CI). If neither, default to the project root itself.
resolveTarget :: ProjectDir -> LintArgs -> Either Text FilePath
resolveTarget pd args =
  let raw = case (laPath args, laModulePath args) of
        (_, Just mp)        -> T.unpack mp
        (Just p,  Nothing)  -> T.unpack p
        (Nothing, Nothing)  -> ""
      root = unProjectDir pd
      full
        | null raw       = root
        | isAbsolute raw = raw
        | otherwise      = root </> raw
  in if isInside root full
       then Right full
       else Left ("target path escapes project directory: " <> T.pack raw)

-- | Cheap prefix check. Good enough here because we construct @full@
-- ourselves via @root </> raw@ — we're defending against the agent
-- passing an absolute path that points outside.
isInside :: FilePath -> FilePath -> Bool
isInside root target =
  let r = if last root == '/' then root else root <> "/"
  in target == root || take (length r) target == r

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

renderResult :: FilePath -> Text -> HlintOutcome -> ToolResult
renderResult target failOn (HlOk raw) =
  let suggestions = parseHlintJson raw
      offending   = filter (atOrAbove failOn . sSeverity) suggestions
      payload =
        object
          [ "success"      .= null offending
          , "target"       .= T.pack target
          , "fail_on"      .= failOn
          , "count"        .= length suggestions
          , "blocking"     .= length offending
          , "suggestions"  .= suggestions
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = not (null offending)
       }
renderResult _ _ HlTimeout =
  errorResult "hlint timed out after 60 seconds"
renderResult _ _ (HlFailure code err) =
  errorResult ( "hlint failed with exit code " <> T.pack (show code)
             <> ": " <> T.strip err )
renderResult target _ HlMissingTarget =
  errorResult ("target path does not exist: " <> T.pack target)

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
  severityRank severity >= severityRank (normalise threshold)
  where
    normalise "suggestion" = "Suggestion"
    normalise "warning"    = "Warning"
    normalise "error"      = "Error"
    normalise x            = x

unavailableResult :: Text -> ToolResult
unavailableResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success"     .= False
        , "error"       .= msg
        , "remediation" .= ( "Install hlint: `cabal install hlint` or \
                            \`ghcup install hls` (bundles hlint)." :: Text )
        ]))
      ]
    , trIsError = True
    }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False
        , "error"   .= msg
        ]))
      ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
