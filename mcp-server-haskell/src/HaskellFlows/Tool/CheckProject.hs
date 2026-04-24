-- | @ghci_check_project@ — enumerate every module declared in the
-- project's @.cabal@ file, run 'ghci_check_module' on each, and
-- return an aggregated report.
--
-- Innovation over 'ghci_check_module' (per-file): one call that
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
import Data.Char (isAsciiUpper, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import HaskellFlows.Data.PropertyStore (Store)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Protocol
import qualified HaskellFlows.Tool.CheckModule as CheckModule
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_check_project"
    , tdDescription =
        "Run ghci_check_module on every module declared in the "
          <> "project's .cabal exposed-modules + other-modules. "
          <> "Returns per-module pass/fail + a single overall flag."
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
                      ("Forwarded verbatim to each 'ghci_check_module' \
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
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> do
    mCabalFile <- findCabalFile pd
    case mCabalFile of
      Nothing -> pure (errorResult "No .cabal file found in project root")
      Just cabalPath -> do
        readRes <- try (TIO.readFile cabalPath)
                   :: IO (Either SomeException Text)
        case readRes of
          Left e ->
            pure (errorResult (T.pack ("Could not read .cabal: " <> show e)))
          Right body -> do
            let moduleNames = parseExposedModules body
            modulePaths   <- resolveModulePaths pd moduleNames
            results       <- runChecks ghcSess store pd
                               (cpFailFast args) (cpWarningsBlock args)
                               modulePaths
            pure (renderResult results)

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
    isContinuation ln =
      let stripped = T.stripStart ln
      in not (T.null stripped)
         && (T.length (T.takeWhile isSpace ln) > 0)
         && not (T.any (== ':') (T.takeWhile (not . isSpace) stripped))

    -- | Pull every module-shaped token from a payload line. Accepts
    -- commas, whitespace, or mixed separators.
    modulesIn :: Text -> [Text]
    modulesIn t =
      [ tok
      | tok <- T.words (T.replace "," " " t)
      , isModuleName tok
      , not ("--" `T.isPrefixOf` tok)
      ]

    isModuleName t =
      not (T.null t) && isAsciiUpper (T.head t)

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
          -- first four match the conventional 'ghci_create_project'
          -- + 'ghci_add_modules stanza=…' layout; 'relPath' is the
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
  -> Bool                  -- warnings_block — forwarded to ghci_check_module
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

renderResult :: [ModuleOutcome] -> ToolResult
renderResult outcomes =
  let checked   = [ (nm, tr) | MoChecked nm tr <- outcomes ]
      failing   = [ nm       | (nm, tr) <- checked, trIsError tr ]
      notFound  = [ nm       | MoNotFound nm <- outcomes ]
      skipped   = [ nm       | MoSkipped nm <- outcomes ]
      overall   = null failing && null notFound
      payload =
        object
          [ "success"       .= overall
          , "overall"       .= overall
          , "total"         .= length outcomes
          , "passed"        .= length (filter (not . trIsError . snd) checked)
          , "failed"        .= length failing
          , "not_found"     .= length notFound
          , "skipped"       .= length skipped
          , "per_module"    .= map renderOutcome outcomes
          , "summary"       .= summarise (length outcomes) (length failing) (length notFound)
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = not overall
       }

renderOutcome :: ModuleOutcome -> Value
renderOutcome (MoChecked nm tr) =
  object
    [ "module" .= nm
    , "status" .= (if trIsError tr then "failed" :: Text else "ok")
    , "result" .= toJSON tr
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

summarise :: Int -> Int -> Int -> Text
summarise total 0 0 =
  T.pack (show total) <> " / " <> T.pack (show total) <> " modules green."
summarise total failed notFound =
  T.pack (show (total - failed - notFound))
  <> " of " <> T.pack (show total) <> " modules pass"
  <> (if failed   > 0 then ", "    <> T.pack (show failed)   <> " failed"    else "")
  <> (if notFound > 0 then "; "    <> T.pack (show notFound) <> " not found" else "")
  <> "."

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
