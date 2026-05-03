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
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.KeyMap as AKM
import Data.Char (isAlphaNum, isAsciiUpper, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import HaskellFlows.Data.PropertyStore (Store)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcCheckProject
    , tdDescription =
        "Run ghc_check_module on every module in the project's "
          <> ".cabal exposed-modules + other-modules. Returns "
          <> "per-module pass/fail plus a single overall flag. Use "
          <> "before pushing to ensure the whole project is clean, not "
          <> "just the files you edited. For a single-module check use "
          <> "ghc_check_module instead; for the full pre-push gate "
          <> "(tests + build) use ghc_gate. SEE ALSO: ghc_check_module "
          <> "(single module), ghc_gate (pre-push composite)."
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
              ]
          , "additionalProperties" .= False
          ]
    }

data CheckProjectArgs = CheckProjectArgs
  { cpFailFast       :: !Bool
  , cpWarningsBlock  :: !Bool
  }
  deriving stock (Show)

instance FromJSON CheckProjectArgs where
  parseJSON = withObject "CheckProjectArgs" $ \o -> do
    ff <- o .:? "fail_fast"      .!= False
    wb <- o .:? "warnings_block" .!= True
    pure CheckProjectArgs { cpFailFast = ff, cpWarningsBlock = wb }

handle :: GhcSession -> Store -> ProjectDir -> Value -> IO ToolResult
handle ghcSess store pd rawArgs = case parseEither parseJSON rawArgs of
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
            results       <- runChecks ghcSess store pd
                               (cpFailFast args) (cpWarningsBlock args)
                               modulePaths
            pure (renderResult results)


-- | Issue #90 Phase C: no .cabal in project root → status='no_match'
-- with kind='module_not_in_graph' (the project layout doesn't
-- expose any modules to check).
cabalNotFoundResult :: ToolResult
cabalNotFoundResult =
  let payload  = object
        [ "remediation" .= ( "Run ghc_create_project to scaffold a \
                            \cabal layout, then retry." :: Text )
        ]
      envErr   = Env.mkErrorEnvelope Env.ModuleNotInGraph
                   ("No .cabal file found in project root" :: Text)
      response = (Env.mkNoMatch payload) { Env.reError = Just envErr }
  in Env.toolResponseToResult response

-- | Issue #90 Phase C: filesystem read of .cabal failed.
subprocessResult :: Text -> ToolResult
subprocessResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))

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
  = MoChecked !Text !ToolResult
  | MoNotFound !Text
  | MoSkipped !Text

runChecks
  :: GhcSession
  -> Store
  -> ProjectDir
  -> Bool                  -- fail_fast
  -> Bool                  -- warnings_block — forwarded to ghc_check_module
  -> [(Text, Maybe Text)]
  -> IO [ModuleOutcome]
runChecks _ _ _ _ _ [] = pure []
runChecks ghcSess store pd ff wb ((nm, mp) : rest) = case mp of
  Nothing ->
    (MoNotFound nm :) <$> runChecks ghcSess store pd ff wb rest
  Just relPath -> do
    tr <- CheckModule.handle ghcSess store pd
            (object
              [ "module_path"    .= relPath
              , "warnings_block" .= wb
              ])
    let this = MoChecked nm tr
        stop = ff && trIsError tr
    cont <-
      if stop
        then pure (map (MoSkipped . fst) rest)
        else runChecks ghcSess store pd ff wb rest
    pure (this : cont)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Issue #90 Phase C: every-module-passed → status='ok'. Any
-- module fails or 'not_found' → status='failed', kind='validation'
-- (compile errors are already aggregated through ghc_check_module's
-- envelope under 'per_module[i].result'; the project-level
-- envelope just summarises).
renderResult :: [ModuleOutcome] -> ToolResult
renderResult outcomes =
  let checked   = [ (nm, tr) | MoChecked nm tr <- outcomes ]
      failing   = [ nm       | (nm, tr) <- checked, trIsError tr ]
      notFound  = [ nm       | MoNotFound nm <- outcomes ]
      skipped   = [ nm       | MoSkipped nm <- outcomes ]
      overall   = null failing && null notFound
      total     = length outcomes
      payload =
        object
          [ "overall"       .= overall
          , "total"         .= total
          , "passed"        .= length (filter (not . trIsError . snd) checked)
          , "failed"        .= length failing
          , "not_found"     .= length notFound
          , "skipped"       .= length skipped
          , "per_module"    .= map renderOutcome outcomes
          , "summary"       .= summarise total (length failing) (length notFound)
          ]
  in if overall
       then Env.toolResponseToResult (Env.mkOk payload)
       else
         let envErr   = Env.mkErrorEnvelope Env.Validation
                          (summarise total (length failing) (length notFound))
             response = (Env.mkFailed envErr) { Env.reResult = Just payload }
         in Env.toolResponseToResult response

renderOutcome :: ModuleOutcome -> Value
renderOutcome (MoChecked nm tr) =
  -- #119: avoid context-bombing the agent on large projects.
  -- For passing modules emit only status + terse summary.
  -- For failing modules include the summary + errors list so the
  -- agent knows exactly what to fix, without the full tool result.
  let moduleStatus = if trIsError tr then "failed" :: Text else "ok"
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

-- | #119: extract a terse summary from a per-module 'ToolResult'.
-- Parse the first TextContent block as JSON and keep only the fields
-- an agent needs to triage the outcome:
--   * summary, errors, warnings  (omit holes + property-gate details)
-- This prevents context-bombing the agent on large projects.
extractModuleDetail :: ToolResult -> Value
extractModuleDetail tr =
  case trContent tr of
    (TextContent t : _) ->
      case decode (TLE.encodeUtf8 (TL.fromStrict t)) of
        Just (Object top) ->
          case AKM.lookup "result" top of
            Just (Object r) ->
              let wantedKeys = ["summary", "errors", "warnings"]
                  found = [ (fieldK, fieldV)
                          | fieldK <- wantedKeys
                          , Just fieldV <- [AKM.lookup fieldK r]
                          ]
              in object [ fieldK .= fieldV | (fieldK, fieldV) <- found ]
            _ -> object []
        _ -> object []
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

