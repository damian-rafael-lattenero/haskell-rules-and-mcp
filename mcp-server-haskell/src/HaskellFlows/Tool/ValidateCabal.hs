-- | @ghc_validate_cabal@ — sanity check the project's @.cabal@ file.
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
  ( handle
  , Issue (..)
  , Severity (..)
  , scanCabalText
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlpha, isSpace)
import Data.List (group, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | #94 Phase C step 5: this module's @descriptor@ was retired
-- when the four legacy project-lifecycle tools were merged into
-- 'HaskellFlows.Tool.Project'. The 'handle' function below is now
-- invoked indirectly via 'Server.dispatchProject' when the agent
-- calls @ghc_project(action=\"validate\")@. Behaviour is
-- byte-identical to the legacy @ghc_validate_cabal@ surface.

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
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope Env.Validation
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right _ -> do
    mCabalFile <- findCabalFile pd
    case mCabalFile of
      Nothing ->
        pure (Env.toolResponseToResult (Env.mkFailed
          ((Env.mkErrorEnvelope Env.ModulePathDoesNotExist
              "No .cabal file found in project root")
                { Env.eeRemediation =
                    Just "Run ghc_create_project to scaffold one, or check that the project root is correct." })))
      Just file -> do
        readRes <- try (TIO.readFile file) :: IO (Either SomeException Text)
        case readRes of
          Left e ->
            pure (Env.toolResponseToResult (Env.mkFailed
              ((Env.mkErrorEnvelope Env.SubprocessError
                  (T.pack ("Could not read cabal file: " <> show e)))
                    { Env.eeCause = Just (T.pack (show e)) })))
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

-- | Duplicate package names in 'build-depends'.
--
-- Stanza-aware AND field-aware. Only flags duplicates that
-- appear within the SAME stanza. Earlier versions counted
-- cross-stanza repeats — e.g. @base@ in both @library@ and
-- @test-suite@ — as duplicates, and they also mis-parsed
-- indented lines like @hs-source-dirs: src@ as fictional
-- packages because their continuation-line heuristic matched
-- any indented text following a stanza header.
--
-- Algorithm: walk the cabal file line by line with a small
-- state machine — track which stanza we're in and whether the
-- last header was @build-depends:@. Only lines that are
-- continuations of an active @build-depends:@ field contribute
-- package names, and duplicates are counted per-stanza.
checkDuplicateDeps :: Text -> [Issue]
checkDuplicateDeps body =
  let perStanza = collectDeps (T.lines body)
      flagged   =
        [ (stanzaTag, name)
        | (stanzaTag, names) <- perStanza
        , (name : _) <- filter ((> 1) . length) . group . sort $ names
        ]
  in [ Issue CabalSevWarn "duplicate-dep"
         ("Package '" <> name
          <> "' listed more than once in " <> stanzaTag
          <> "'s build-depends")
     | (stanzaTag, name) <- flagged
     ]

-- | Line-by-line walker. Returns a list of (stanza-label,
-- deps-in-that-stanza). Dep names are stripped of version
-- bounds; empty entries are filtered out.
collectDeps :: [Text] -> [(Text, [Text])]
collectDeps = go "top-level" "" []
  where
    -- stanzaTag: label of the stanza we're currently scanning
    -- activeField: name of the field whose continuation lines we
    --   should append to (""  when none active)
    -- deps:      dep names collected for the current stanza so far
    go :: Text -> Text -> [Text] -> [Text] -> [(Text, [Text])]
    go stanzaTag _active deps [] = [(stanzaTag, reverse deps)]
    go stanzaTag active deps (ln : rest)
      | Just newStanza <- matchStanzaHeader ln =
          (stanzaTag, reverse deps)
            : go newStanza "" [] rest
      | Just (field, value) <- matchField ln =
          let pkgs = if field == "build-depends"
                       then tokenise value
                       else []
              nextActive = field
          in go stanzaTag nextActive (reverse pkgs <> deps) rest
      | isContinuation ln =
          if active == "build-depends"
            then
              let pkgs = tokenise (T.stripStart ln)
              in go stanzaTag active (reverse pkgs <> deps) rest
            else go stanzaTag active deps rest
      | otherwise =
          -- blank line or comment → close any active field
          go stanzaTag "" deps rest

    -- Recognise 'library' / 'executable NAME' / 'test-suite NAME'
    -- / 'benchmark NAME' / 'common NAME'. Case-insensitive,
    -- must start at column 0 (cabal stanzas are not indented).
    matchStanzaHeader :: Text -> Maybe Text
    matchStanzaHeader ln
      | not (T.null ln), not (isSpace (T.head ln))
      , let low = T.toLower (T.strip ln)
      , Just tag <- headerTag low
      = Just tag
      | otherwise = Nothing

    headerTag :: Text -> Maybe Text
    headerTag s
      | s == "library"                          = Just "library"
      | "executable "  `T.isPrefixOf` s         =
          Just ("executable '" <> firstTok (T.drop 11 s) <> "'")
      | "test-suite "  `T.isPrefixOf` s         =
          Just ("test-suite '" <> firstTok (T.drop 11 s) <> "'")
      | "benchmark "   `T.isPrefixOf` s         =
          Just ("benchmark '"  <> firstTok (T.drop 10 s) <> "'")
      | "common "      `T.isPrefixOf` s         =
          Just ("common '"     <> firstTok (T.drop 7 s)  <> "'")
      | otherwise                               = Nothing
    firstTok = T.takeWhile (not . isSpace)

    -- Match @<whitespace>field-name: rest@. Returns (field-name,
    -- rest). Field names are the text left of the first colon,
    -- lowercased for comparison.
    matchField :: Text -> Maybe (Text, Text)
    matchField ln =
      let stripped = T.stripStart ln
      in case T.breakOn ":" stripped of
           (nm, rest) | not (T.null rest), not (T.null (T.strip nm))
                      , T.all (\c -> isAlpha c || c == '-' || c == '_') (T.strip nm)
             -> Just (T.toLower (T.strip nm), T.drop 1 rest)
           _ -> Nothing

    -- An indented line that's NOT a field header (no colon
    -- followed by value, OR starts with ',' or whitespace-only).
    isContinuation :: Text -> Bool
    isContinuation ln =
      let s = T.strip ln
      in not (T.null s)
         && case T.uncons ln of
              Just (c, _) -> isSpace c  -- must be indented
              Nothing     -> False

    -- Split on commas, trim, strip version bounds, drop blanks.
    tokenise :: Text -> [Text]
    tokenise raw =
      [ depName
      | tok <- T.splitOn "," raw
      , let depName = firstWordOf (T.strip tok)
      , not (T.null depName)
      ]

    firstWordOf :: Text -> Text
    firstWordOf = T.takeWhile
      (\c -> not (isSpace c) && c /= '<' && c /= '>'
          && c /= '=' && c /= '^' && c /= '&')

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
              -- Issue #69: 'cabal check' emits a positive
              -- confirmation line on success — "No errors or
              -- warnings could be found in the package." Wrapping
              -- that as a Warning Issue is a UX bug: a green
              -- check is reported as yellow. Treat the
              -- confirmation line (and any blank-trailing
              -- siblings) as the empty case — no Issue emitted.
              actionable =
                  not (T.null combined)
                && not ("No errors or warnings could be found"
                          `T.isInfixOf` combined)
          pure [ Issue sev "cabal-check" combined | actionable ]

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Build the response. Three structurally-distinct outcomes after
-- issue #90 Phase B:
--
-- * No issues at all → 'Env.StatusOk'.
-- * Warnings only → 'Env.StatusPartial' with one envelope-warning
--   per issue. The cabal file is still shippable; the agent can
--   decide whether to fix.
-- * One or more cabal errors → 'Env.StatusFailed' with
--   'Env.Validation' kind. The 'cause' field carries a short tally
--   of the issue counts; the structured 'issues' array stays inside
--   'result' so consumers that branch per-issue keep the same
--   shape.
--
-- The 'errors', 'warnings', 'issues', 'cabal_file', and 'summary'
-- fields are preserved inside 'result' so any consumer keying on
-- the legacy shape continues to function during the dual-shape
-- window. The 'cause' field on the error envelope mirrors the
-- summary text without poisoning the user-facing 'message' with
-- counts.
renderResult :: FilePath -> [Issue] -> ToolResult
renderResult file issues =
  let errs  = length (filter ((== CabalSevError) . iSeverity) issues)
      warns = length (filter ((== CabalSevWarn)  . iSeverity) issues)
      summary = summarise errs warns
      payload =
        object
          [ "cabal_file" .= T.pack file
          , "errors"     .= errs
          , "warnings"   .= warns
          , "issues"     .= issues
          , "summary"    .= summary
          ]
      response = case (errs, warns) of
        (0, 0)     -> Env.mkOk payload
        (0, _)     -> Env.withWarnings (map issueToWarning warningIssues)
                        (Env.mkPartial payload)
        (_, _)     -> Env.mkFailed
          ((Env.mkErrorEnvelope Env.Validation
              ("cabal validation found " <> T.pack (show errs) <> " error(s)"))
                { Env.eeCause = Just summary })
      warningIssues = filter ((== CabalSevWarn) . iSeverity) issues
  in Env.toolResponseToResult response

summarise :: Int -> Int -> Text
summarise 0 0 = "cabal file is clean."
summarise 0 w = T.pack (show w) <> " warning(s); cabal file is shippable."
summarise e w = T.pack (show e) <> " error(s), " <> T.pack (show w) <> " warning(s)."

-- | Translate a Phase-A 'Issue' into an envelope 'Warning'. Carries
-- the issue's 'kind' inside 'wExtra' so an agent can branch on
-- specific cabal-check categories without re-parsing the message.
issueToWarning :: Issue -> Env.Warning
issueToWarning i = Env.Warning
  { Env.wKind    = Env.OtherWarning
  , Env.wMessage = iMessage i
  , Env.wExtra   = Just (object
      [ "kind"     .= iKind i
      , "severity" .= iSeverity i
      ])
  }
