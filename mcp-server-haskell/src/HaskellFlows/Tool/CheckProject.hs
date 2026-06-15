-- | @ghc_check_project@ — enumerate every module declared in the
-- project's @.cabal@ file, run 'ghc_check_module' on each, and
-- return an aggregated report.
--
-- Innovation over 'ghc_check_module' (per-file): one call that
-- answers \"is the whole package green?\" without falling back to
-- @cabal test@ (which doesn't run hlint / format / property gates).
--
-- Execution model:
--
-- * Modules are checked sequentially under the existing GHCi session
--   — the STM lock already serialises GHCi commands, so parallel
--   wouldn't actually buy anything without a second session.
-- * @fail_fast=false@ by default: we want full coverage of which
--   modules are red, not just the first.
module HaskellFlows.Tool.CheckProject
  ( descriptor
  , handle
  , CheckProjectArgs (..)
  , parseExposedModules
    -- * Exposed for unit tests (#129)
  , ModuleOutcome (..)
  , renderResult
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as AKM
import Data.Char (isAlphaNum, isAsciiUpper, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import HaskellFlows.Config (Limits, checkProjectTimeout, unMicros)
import HaskellFlows.Data.PropertyStore (Store)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Progress (ProgressEvent (..), ProgressSink, emitProgress)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import HaskellFlows.Tool.Env (ToolEnv (..))
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcCheckProject
    , tdDescription =
        "PURPOSE: Run ghc_check_module on every exposed-module + "
          <> "other-module in the .cabal. "
          <> "WHEN: before pushing, to confirm the whole project is clean — "
          <> "not just the files you edited. "
          <> "WHEN NOT: ghc_check_module for a single file; ghc_gate for the "
          <> "tests + build pre-push composite. "
          <> "PREREQUISITES: a .cabal in the active project. "
          <> "OUTPUT: {modules:[{path, overall}], overall, timed_out}. "
          <> "SEE ALSO: ghc_check_module, ghc_gate."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "fail_fast" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("Stop on first red module. Default: false (full "
                       <> "coverage preferred over speed)." :: Text)
                  ]
              , "warnings_block" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("Forwarded verbatim to each 'ghc_check_module' \
                       \call. When false, warnings stay informational \
                       \— the project is considered green as long as \
                       \there are no compile errors, holes, or property \
                       \regressions. Default: true (pre-push strictness)." :: Text)
                  ]
              -- #129: overall wall-clock budget
              , "timeout_seconds" .= object
                  [ "type"        .= ("integer" :: Text)
                  , "description" .=
                      ("Overall wall-clock budget in seconds. Default: 120. \
                       \When the budget expires the tool returns partial \
                       \results with timed_out=true rather than hanging \
                       \the session." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data CheckProjectArgs = CheckProjectArgs
  { cpFailFast        :: !Bool
  , cpWarningsBlock   :: !Bool
    -- ^ #129: overall wall-clock budget. 'Nothing' = use 'checkProjectTimeout'
    -- from the 'Limits' passed to 'handle' (env-var overridable). When the
    -- deadline fires we return partial results rather than hanging.
  , cpTimeoutSeconds  :: !(Maybe Int)
  }
  deriving stock (Show)

instance FromJSON CheckProjectArgs where
  parseJSON = withObject "CheckProjectArgs" $ \o -> do
    ff <- o .:? "fail_fast"       .!= False
    wb <- o .:? "warnings_block"  .!= True
    ts <- o .:? "timeout_seconds"
    pure CheckProjectArgs { cpFailFast = ff, cpWarningsBlock = wb
                          , cpTimeoutSeconds = fmap (max 1) ts }

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = do
  ghcSess <- teSession env
  store   <- teStore env
  pd      <- teProjectDir env
  runHandle (teLimits env) (teSink env) ghcSess store pd rawArgs

runHandle :: Limits -> ProgressSink -> GhcSession -> Store -> ProjectDir -> Value -> IO ToolResponse
runHandle lim sink ghcSess store pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (formatParseError parseError)
  Right args -> do
    mCabalFile <- findCabalFile pd
    case mCabalFile of
      Nothing -> pure cabalNotFoundResult
      Just cabalPath -> do
        readRes <- try (TIO.readFile cabalPath)
                   :: IO (Either SomeException Text)
        case readRes of
          Left e ->
            pure (subprocessResult
                    (T.pack ("Could not read .cabal: " <> show e)))
          Right body -> do
            let moduleNames = parseExposedModules body
            modulePaths   <- resolveModulePaths pd moduleNames
            -- #129: compute an absolute deadline from the caller's budget
            -- (or the env-var-overridable Limits default) so each module
            -- checks the clock before starting — no async-exception surprises.
            start    <- realToFrac <$> getPOSIXTime
            let timeoutSecs = fromMaybe
                  (unMicros (checkProjectTimeout lim) `div` 1_000_000)
                  (cpTimeoutSeconds args)
                deadline = start + fromIntegral timeoutSecs
                total    = length modulePaths
            (outcomes, timedOut) <- runChecks sink start total ghcSess store pd
                                      (cpFailFast args) (cpWarningsBlock args)
                                      (Just deadline)
                                      1 modulePaths
            pure (renderResult outcomes timedOut)


-- | Issue #90 Phase C: no .cabal in project root → status='no_match'
-- with kind='module_not_in_graph' (the project layout doesn't
-- expose any modules to check).
cabalNotFoundResult :: ToolResponse
cabalNotFoundResult =
  let payload  = object
        [ "remediation" .= ( "Run ghc_create_project to scaffold a \
                            \cabal layout, then retry." :: Text )
        ]
      envErr   = Env.mkErrorEnvelope Env.ModuleNotInGraph
                   ("No .cabal file found in project root" :: Text)
      response = (Env.mkNoMatch payload) { Env.reError = Just envErr }
  in response

-- | Issue #90 Phase C: filesystem read of .cabal failed.
subprocessResult :: Text -> ToolResponse
subprocessResult msg =
  Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg)

--------------------------------------------------------------------------------
-- cabal parsing
--------------------------------------------------------------------------------

-- | Pull every module name from @exposed-modules:@ and
-- @other-modules:@ sections. Exposed for unit tests.
--
-- Strategy: walk lines, find field headers, for each one consume the
-- tail content (inline or on continuation lines indented deeper) and
-- extract anything that looks like a module name
-- (@[A-Z][A-Za-z0-9_.]*@).
parseExposedModules :: Text -> [Text]
parseExposedModules body = go (T.lines body) []
  where
    go []       acc = reverse acc
    go (ln:rest) acc
      | Just inlineTail <- stripFieldHeader ln =
          let (contLines, after) = span isContinuation rest
              payload = inlineTail : map T.strip contLines
              names   = concatMap modulesIn payload
          in go after (names <> acc)
      | otherwise = go rest acc

    -- | If @ln@ is an @exposed-modules:@ or @other-modules:@ header,
    -- return whatever followed on the same line. Nothing otherwise.
    stripFieldHeader ln =
      let lower = T.toLower (T.stripStart ln)
      in if "exposed-modules:" `T.isPrefixOf` lower
           then Just (inlineAfter "exposed-modules:" ln)
         else if "other-modules:" `T.isPrefixOf` lower
           then Just (inlineAfter "other-modules:" ln)
         else Nothing

    -- | Return text after @field:@ on the same line (may be empty
    -- for a header that only has modules on following lines).
    inlineAfter :: Text -> Text -> Text
    inlineAfter _ ln =
      let rest = T.dropWhile (/= ':') (T.stripStart ln)
      in T.strip (T.drop 1 rest)

    -- | A continuation of a field is an indented line; a column-0
    -- token with a colon starts a new field.
    -- Issue #109 note: whole-line @-- comment@ lines ARE continuations
    -- (they belong to the block) but their content is neutralised by
    -- 'modulesIn' via @T.breakOn "--"@.  Do NOT filter them here —
    -- stopping the continuation scan at a comment line would drop
    -- every module that follows the comment (regression observed in
    -- 'testParseExposedModulesStripsComments').
    isContinuation ln =
      let stripped = T.stripStart ln
      in not (T.null stripped)
         && (T.length (T.takeWhile isSpace ln) > 0)
         && not (T.any (== ':') (T.takeWhile (not . isSpace) stripped))

    -- | Pull every module-shaped token from a payload line. Accepts
    -- commas, whitespace, or mixed separators.
    -- Issue #109: strip inline @-- comment@ before tokenising so that
    -- words like @Bench@ or @Phase@ after a comment marker are not
    -- mistaken for module names. The previous approach only filtered
    -- @"--"@ itself (the marker token), leaving the comment words through.
    modulesIn :: Text -> [Text]
    modulesIn t =
      let noComment = T.strip (fst (T.breakOn "--" t))
      in [ tok
         | tok <- T.words (T.replace "," " " noComment)
         , isModuleName tok
         ]

    -- | Issue #109: require ALL characters to be alphanumeric or @.@
    -- (not just the first character). This rejects tokens like @"A)"@
    -- or @"Phase#1"@ that start with a capital but contain punctuation.
    isModuleName t =
      not (T.null t)
      && isAsciiUpper (T.head t)
      && T.all (\c -> isAlphaNum c || c == '.') t

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

-- | Walk the standard hs-source-dirs (src, lib, project-root) looking
-- for each module name. Modules we can't locate are returned with a
-- 'Nothing' path — the tool reports them as \"not found\" rather
-- than silently skipping.
resolveModulePaths :: ProjectDir -> [Text] -> IO [(Text, Maybe Text)]
resolveModulePaths pd = mapM locate
  where
    locate nm = do
      let root    = unProjectDir pd
          relPath = T.unpack (T.replace "." "/" nm) <> ".hs"
          -- Source-dir candidates in order of specificity. The
          -- first four match the conventional 'ghc_create_project'
          -- + 'ghc_add_modules stanza=…' layout; 'relPath' is the
          -- legacy fallback for projects that use the project root
          -- directly. Ordering matters: if a module happens to
          -- exist under more than one candidate (unusual), the
          -- library's 'src/' wins — that's the behaviour tests
          -- relied on before the test/app/bench extensions.
          candidates =
            [ "src"   </> relPath
            , "lib"   </> relPath
            , "test"  </> relPath
            , "app"   </> relPath
            , "bench" </> relPath
            , relPath
            ]
      found <- firstExisting root candidates
      pure (nm, fmap T.pack found)

    firstExisting _    []     = pure Nothing
    firstExisting root (p:ps) = do
      let full = root </> p
      e <- doesFileExist full
      if e then pure (Just p) else firstExisting root ps

--------------------------------------------------------------------------------
-- running the per-module checks
--------------------------------------------------------------------------------

data ModuleOutcome
  = MoChecked  !Text !ToolResponse
  | MoNotFound !Text
  | MoSkipped  !Text
  | MoTimedOut !Text   -- #129: budget expired before this module ran

-- | Run per-module checks with a deadline and progress reporting.
--
-- #129: Before processing each module we compare the current clock
-- against 'mDeadline'. On expiry, remaining modules are tagged
-- 'MoTimedOut' and we return @(outcomes, True)@ so the caller can
-- surface a @timed_out=true@ field in the response.
--
-- #265 extension: emits one 'ProgressEvent' per module via 'sink'
-- so long-running runs surface incremental feedback in the client.
runChecks
  :: ProgressSink
  -> Double                -- start POSIXTime (for elapsed_ms in events)
  -> Int                   -- total module count (for progress denominator)
  -> GhcSession
  -> Store
  -> ProjectDir
  -> Bool                  -- fail_fast
  -> Bool                  -- warnings_block — forwarded to ghc_check_module
  -> Maybe Double          -- absolute deadline (POSIXTime seconds)
  -> Int                   -- current 1-based index
  -> [(Text, Maybe Text)]
  -> IO ([ModuleOutcome], Bool)  -- (outcomes, timed_out)
runChecks _ _ _ _ _ _ _ _ _ _ [] = pure ([], False)
runChecks sink startTime total ghcSess store pd ff wb mDeadline idx ((nm, mp) : rest) = do
  -- #129: check the budget before each module.
  now <- realToFrac <$> getPOSIXTime
  let expired = case mDeadline of
        Nothing -> False
        Just dl -> now >= dl
  if expired
    then
      -- Budget exhausted — tag this module and all remaining ones.
      let timedOuts = map (MoTimedOut . fst) ((nm, mp) : rest)
      in pure (timedOuts, True)
    else do
      -- #265: emit progress before checking so the client sees "Checking X"
      -- while the module is being compiled, not after.
      let elapsedMs = round ((now - startTime) * 1000) :: Int
      emitProgress sink ProgressEvent
        { peStep        = "Checking " <> nm
                            <> " (" <> T.pack (show idx)
                            <> "/" <> T.pack (show total) <> ")"
        , peElapsedMs   = elapsedMs
        , peCurrentStep = Just idx
        , peTotalSteps  = Just total
        }
      case mp of
        Nothing -> do
          (cont, to) <- runChecks sink startTime total ghcSess store pd ff wb mDeadline (idx + 1) rest
          pure (MoNotFound nm : cont, to)
        Just relPath -> do
          tr <- CheckModule.runHandle ghcSess store pd
                  (object
                    [ "module_path"    .= relPath
                    , "warnings_block" .= wb
                    ])
          let this = MoChecked nm tr
              stop = ff && trIsError (Env.toolResponseToResult tr)
          if stop
            then pure (this : map (MoSkipped . fst) rest, False)
            else do
              (cont, to) <- runChecks sink startTime total ghcSess store pd ff wb mDeadline (idx + 1) rest
              pure (this : cont, to)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: every-module-passed → status='ok'. Any
-- module fails or 'not_found' → status='failed' (all fail) or
-- status='partial' (some pass, some fail) per issue #255.
--
-- #129: when 'timedOut=True', adds @timed_out=true@ and @checked@
-- count so the agent knows partial results were returned. Status is
-- still 'failed' when any checked module failed; 'ok' when all
-- checked modules passed (even if some were skipped by the budget).
renderResult :: [ModuleOutcome] -> Bool -> ToolResponse
renderResult outcomes timedOut =
  let checked   = [ (nm, tr) | MoChecked nm tr <- outcomes ]
      failing   = [ nm       | (nm, tr) <- checked, trIsError (Env.toolResponseToResult tr) ]
      notFound  = [ nm       | MoNotFound nm <- outcomes ]
      skipped   = [ nm       | MoSkipped nm <- outcomes ]
      timedOutMs= [ nm       | MoTimedOut nm <- outcomes ]
      overall   = null failing && null notFound && not timedOut
      total     = length outcomes
      nChecked  = length checked
      okCount   = length (filter (not . trIsError . Env.toolResponseToResult . snd) checked)
      errCount  = length failing + length notFound
      -- #255: true when some modules passed and some failed (mixed result).
      isMixed   = not overall && okCount > 0 && errCount > 0 && not timedOut
      -- #151: use a timeout-aware summary that only counts actually-
      -- evaluated modules. The old code passed 'total' to summarise
      -- even when only nChecked modules were examined, producing the
      -- contradictory "168/168 modules green. (1/168 checked)".
      summaryText
        | timedOut  = timeoutSummarise total nChecked (length failing) (length notFound)
        | otherwise = summarise total (length failing) (length notFound)
      payload =
        object $
          [ "overall"       .= overall
          , "total"         .= total
          , "checked"       .= nChecked
          , "passed"        .= okCount
          , "failed"        .= length failing
          , "not_found"     .= length notFound
          , "skipped"       .= length skipped
          , "per_module"    .= map renderOutcome outcomes
          , "summary"       .= summaryText
          ]
          -- #129: only include timed_out when it's true, keeping the
          -- common-case response shape unchanged.
          <> if timedOut
               then [ "timed_out"         .= True
                    , "timed_out_modules" .= timedOutMs ]
               else []
      envErr = Env.mkErrorEnvelope
                 (if timedOut then Env.InnerTimeout else Env.GateFailure)
                 summaryText
  in if overall
       then Env.mkOk payload
       else if isMixed
         -- #255: mixed results → status='partial' so callers can
         -- distinguish "project partially clean" from "project broken".
         then (Env.mkPartial payload) { Env.reError = Just envErr }
         else (Env.mkFailed envErr) { Env.reResult = Just payload }

renderOutcome :: ModuleOutcome -> Value
renderOutcome (MoChecked nm tr) =
  -- #119: avoid context-bombing the agent on large projects.
  -- For passing modules emit only status + terse summary.
  -- For failing modules include the summary + errors list so the
  -- agent knows exactly what to fix, without the full tool result.
  let moduleStatus = if trIsError (Env.toolResponseToResult tr) then "failed" :: Text else "ok"
      detail = extractModuleDetail tr
  in object
    [ "module"  .= nm
    , "status"  .= moduleStatus
    , "detail"  .= detail
    ]
renderOutcome (MoNotFound nm) =
  object
    [ "module" .= nm
    , "status" .= ("not_found" :: Text)
    , "reason" .= ("no .hs file under src/, lib/, test/, app/, bench/, or project root" :: Text)
    ]
renderOutcome (MoSkipped nm) =
  object
    [ "module" .= nm
    , "status" .= ("skipped" :: Text)
    , "reason" .= ("fail_fast tripped on an earlier module" :: Text)
    ]
renderOutcome (MoTimedOut nm) =
  object
    [ "module" .= nm
    , "status" .= ("timed_out" :: Text)
    , "reason" .= ("overall timeout_seconds budget exhausted" :: Text)
    ]

-- | #119: extract a terse summary from a per-module 'ToolResponse'.
-- Keep only the fields an agent needs to triage the outcome:
--   * summary (omit holes + property-gate details)
-- This prevents context-bombing the agent on large projects.
extractModuleDetail :: ToolResponse -> Value
extractModuleDetail tr =
  case Env.reResult tr of
    Just (Object r) ->
      let wantedKeys = ["summary", "errors", "warnings"]
          found = [ (fieldK, fieldV)
                  | fieldK <- wantedKeys
                  , Just fieldV <- [AKM.lookup fieldK r]
                  ]
      in object [ fieldK .= fieldV | (fieldK, fieldV) <- found ]
    _ -> object []

summarise :: Int -> Int -> Int -> Text
summarise total 0 0 =
  T.pack (show total) <> " / " <> T.pack (show total) <> " modules green."
summarise total failed notFound =
  T.pack (show (total - failed - notFound))
  <> " of " <> T.pack (show total) <> " modules pass"
  <> (if failed   > 0 then ", "    <> T.pack (show failed)   <> " failed"    else "")
  <> (if notFound > 0 then "; "    <> T.pack (show notFound) <> " not found" else "")
  <> "."

-- | #151: timeout-aware summary. Only mentions modules that were
-- actually evaluated ('nChecked'). The previous 'summarise' call with
-- 'total' produced "168/168 green. (1/168 checked)" which was
-- contradictory — the unchecked modules were never verified.
timeoutSummarise :: Int -> Int -> Int -> Int -> Text
timeoutSummarise total nChecked failed notFound =
  let notEvaluated = total - nChecked
  in T.pack (show nChecked) <> "/" <> T.pack (show total)
       <> " modules checked before timeout"
       <> (if failed   > 0 then ", " <> T.pack (show failed)   <> " failed"    else "")
       <> (if notFound > 0 then "; " <> T.pack (show notFound) <> " not found" else "")
       <> ". " <> T.pack (show notEvaluated) <> " not evaluated."

