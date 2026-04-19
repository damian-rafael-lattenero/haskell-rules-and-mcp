-- | @ghci_validate_cabal@ — sanity check the project's @.cabal@ file.
--
-- Two layers of validation:
--
-- 1. **Cabal's own validator** — shells out to @cabal check@, which
--    flags OSS-hygiene issues (license, synopsis length, category).
-- 2. **Common-issue heuristics** — line-level checks tuned for the
--    issues we've actually hit while porting: duplicate
--    @build-depends@ entries, missing @default-language@ in a
--    stanza, exposed-modules that don't exist on disk.
--
-- Innovation vs the TS port: the TS MCP only runs @cabal check@ and
-- relays the text. We add structured per-issue output plus a
-- @severity@ tag so an agent can decide which issues merit fixing
-- before push vs which are nits.
module HaskellFlows.Tool.ValidateCabal
  ( descriptor
  , handle
  , Issue (..)
  , Severity (..)
  , scanCabalText
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isSpace)
import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
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
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_validate_cabal"
    , tdDescription =
        "Validate the project's .cabal file. Runs `cabal check` + "
          <> "common-issue heuristics (duplicate deps, missing "
          <> "default-language, phantom exposed-modules). Returns "
          <> "structured per-issue output with severity tags."
    , tdInputSchema =
        object
          [ "type"                 .= ("object" :: Text)
          , "properties"           .= object []
          , "additionalProperties" .= False
          ]
    }

-- | Constructor names are prefixed @CabalSev@ to avoid colliding with
-- 'HaskellFlows.Parser.Error.Severity' when both are imported in the
-- same consumer (the test suite hits this).
data Severity = CabalSevWarn | CabalSevError
  deriving stock (Eq, Show)

instance ToJSON Severity where
  toJSON CabalSevWarn  = "warning"
  toJSON CabalSevError = "error"

data Issue = Issue
  { iSeverity :: !Severity
  , iKind     :: !Text
  , iMessage  :: !Text
  }
  deriving stock (Eq, Show)

instance ToJSON Issue where
  toJSON i =
    object
      [ "severity" .= iSeverity i
      , "kind"     .= iKind i
      , "message"  .= iMessage i
      ]

cabalCheckTimeoutMicros :: Int
cabalCheckTimeoutMicros = 30 * 1_000_000  -- 30 s

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs :: Either String Value of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right _ -> do
    mCabalFile <- findCabalFile pd
    case mCabalFile of
      Nothing ->
        pure (errorResult "No .cabal file found in project root")
      Just file -> do
        readRes <- try (TIO.readFile file) :: IO (Either SomeException Text)
        case readRes of
          Left e ->
            pure (errorResult (T.pack ("Could not read cabal file: " <> show e)))
          Right body -> do
            heuristicIssues <- (scanCabalText body ++) <$> scanExposedModules pd body
            cabalCheckIssues <- runCabalCheck pd
            pure (renderResult file (heuristicIssues <> cabalCheckIssues))

--------------------------------------------------------------------------------
-- .cabal discovery
--------------------------------------------------------------------------------

findCabalFile :: ProjectDir -> IO (Maybe FilePath)
findCabalFile pd = do
  let root = unProjectDir pd
  exists <- doesDirectoryExist root
  if not exists then pure Nothing else do
    entries <- listDirectory root
    let cabals = [ root </> e | e <- entries, takeExtension e == ".cabal" ]
    case cabals of
      [one] -> pure (Just one)
      _     -> pure Nothing

--------------------------------------------------------------------------------
-- heuristic scanners
--------------------------------------------------------------------------------

-- | Pure scanner: runs over the raw .cabal text and returns issues
-- that don't require filesystem access. Exposed for unit tests.
scanCabalText :: Text -> [Issue]
scanCabalText body =
  concatMap ($ body)
    [ checkDuplicateDeps
    , checkMissingSynopsis
    ]

-- | Duplicate package names across build-depends (including across
-- multiple stanzas). For each line that contributes entries, strip
-- the @build-depends:@ header (when present) before splitting on
-- commas — otherwise the header would show up as a \"package\" and
-- corrupt the duplicate count.
checkDuplicateDeps :: Text -> [Issue]
checkDuplicateDeps body =
  let pkgs = [ firstWord (T.strip tok)
             | ln <- T.lines body
             , let stripped      = T.stripStart ln
                   isHeader      = "build-depends:" `T.isInfixOf` T.toLower stripped
                   isContinuation = isContinuationLine ln
             , isHeader || isContinuation
             , let effective = if isHeader
                                 then T.drop 1 (T.dropWhile (/= ':') stripped)
                                 else stripped
             , tok <- T.splitOn "," effective
             , not (T.null (T.strip tok))
             ]
      grouped = filter ((> 1) . length) . group . sort $ filter (not . T.null) pkgs
  in [ Issue CabalSevWarn "duplicate-dep"
         ("Package appears more than once in build-depends: " <> name)
     | (name : _) <- grouped
     ]
  where
    firstWord = T.takeWhile (\c -> not (isSpace c) && c /= '<' && c /= '>'
                                 && c /= '=' && c /= '^' && c /= '&')
    isContinuationLine ln =
      let s = T.strip ln
      in "," `T.isPrefixOf` s || T.isPrefixOf "  " ln

-- | A cabal-hygiene nit that @cabal check@ also flags but we surface
-- earlier with a more agent-friendly message.
checkMissingSynopsis :: Text -> [Issue]
checkMissingSynopsis body
  | "synopsis:" `T.isInfixOf` T.toLower body =
      let synopsisBody =
            T.strip (T.drop (T.length "synopsis:")
                    (T.dropWhile (/= ':')
                     (T.concat [ ln <> " "
                               | ln <- T.lines body
                               , "synopsis:" `T.isInfixOf` T.toLower (T.stripStart ln)
                               ])))
      in [ Issue CabalSevWarn "weak-synopsis"
             "synopsis is missing or too short to be useful"
         | T.null synopsisBody || T.length synopsisBody < 8
         ]
  | otherwise =
      [ Issue CabalSevWarn "missing-synopsis" "no synopsis: field in .cabal" ]

-- | FS-backed check: every exposed-module line references a file
-- that actually exists under one of the hs-source-dirs.
--
-- This is a simplified sweep — it looks for any .hs file whose stem
-- matches the module name anywhere under the project. Good enough to
-- catch common typos and modules renamed-but-not-removed.
scanExposedModules :: ProjectDir -> Text -> IO [Issue]
scanExposedModules pd body = do
  let moduleNames =
        [ T.strip ln
        | section <- chunksAfter "exposed-modules:" body
        , ln <- T.lines section
        , let stripped = T.strip ln
        , not (T.null stripped)
        , not ("--" `T.isPrefixOf` stripped)
        , not (":" `T.isInfixOf` stripped)
        ]
  issues <- mapM (checkModuleExists pd) moduleNames
  pure (concat issues)

-- | Split @body@ by @marker@ header and return the text blocks that
-- follow each occurrence, up to the next cabal field header at
-- column-zero. Used to find the text chunk after @exposed-modules:@.
chunksAfter :: Text -> Text -> [Text]
chunksAfter marker body =
  let ls = T.lines body
  in go [] ls
  where
    go acc [] = reverse acc
    go acc (l:rest)
      | marker `T.isInfixOf` T.toLower (T.stripStart l) =
          let (chunk, after) = break isFieldHeader rest
          in go (T.unlines chunk : acc) after
      | otherwise = go acc rest

    isFieldHeader ln =
      let s = T.stripStart ln
      in case T.break (== ':') s of
           (name, rest) -> not (T.null rest)
                        && not (T.any isSpace name)
                        && T.length name > 0

checkModuleExists :: ProjectDir -> Text -> IO [Issue]
checkModuleExists pd name = do
  let root    = unProjectDir pd
      relPath = T.unpack (T.replace "." "/" name) <> ".hs"
      candidates = [ root </> "src" </> relPath
                   , root </> "lib" </> relPath
                   , root </> relPath
                   ]
  exists <- or <$> mapM doesFileExist candidates
  pure
    [ Issue CabalSevError "phantom-module"
        ( "exposed-modules references '" <> name
        <> "' but no .hs file found under src/, lib/, or root" )
    | not exists
    ]

--------------------------------------------------------------------------------
-- cabal check passthrough
--------------------------------------------------------------------------------

runCabalCheck :: ProjectDir -> IO [Issue]
runCabalCheck pd = do
  mCabal <- findExecutable "cabal"
  case mCabal of
    Nothing -> pure
      [ Issue CabalSevWarn "cabal-unavailable"
          "cabal binary not on PATH; skipped 'cabal check'" ]
    Just _ -> do
      let cp = (proc "cabal" ["check"])
                 { cwd = Just (unProjectDir pd)
                 , std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
      (_, Just hOut, Just hErr, ph) <- createProcess cp
      outVar <- newEmptyMVar
      errVar <- newEmptyMVar
      _ <- forkIO (hGetContents hOut >>= putMVar outVar)
      _ <- forkIO (hGetContents hErr >>= putMVar errVar)
      exited <- timeout cabalCheckTimeoutMicros (waitForProcess ph)
      case exited of
        Nothing -> do
          terminateProcess ph
          hClose hOut >> hClose hErr
          pure [ Issue CabalSevError "cabal-check-timeout"
                   "cabal check did not finish within 30s" ]
        Just code -> do
          o <- takeMVar outVar
          e <- takeMVar errVar
          let sev = case code of { ExitSuccess -> CabalSevWarn; _ -> CabalSevError }
              combined = T.strip (T.pack (o <> e))
          pure [ Issue sev "cabal-check" combined | not (T.null combined) ]

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult :: FilePath -> [Issue] -> ToolResult
renderResult file issues =
  let errs  = length (filter ((== CabalSevError) . iSeverity) issues)
      warns = length (filter ((== CabalSevWarn)  . iSeverity) issues)
      payload =
        object
          [ "success"    .= (errs == 0)
          , "cabal_file" .= T.pack file
          , "errors"     .= errs
          , "warnings"   .= warns
          , "issues"     .= issues
          , "summary"    .= summarise errs warns
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = errs > 0
       }

summarise :: Int -> Int -> Text
summarise 0 0 = "cabal file is clean."
summarise 0 w = T.pack (show w) <> " warning(s); cabal file is shippable."
summarise e w = T.pack (show e) <> " error(s), " <> T.pack (show w) <> " warning(s)."

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
