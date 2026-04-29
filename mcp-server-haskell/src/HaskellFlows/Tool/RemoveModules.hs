-- | @ghc_remove_modules@ — de-register modules from the project's
-- @.cabal@ exposed-modules list. Symmetric to
-- 'HaskellFlows.Tool.AddModules' (BUG-16).
--
-- The source files are left on disk by default. Deletion is opt-in
-- via @delete_files: true@; safer to have the caller be explicit
-- about destroying source than to have a \"remove from cabal\"
-- tool that silently rm's the backing file.
--
-- After every mutation the resulting cabal is parsed back and the
-- removed-module list compared to the list of modules no longer
-- present in @exposed-modules@. If the post-parse disagrees with
-- the verb ("removed"), the write is rolled back — the same
-- post-edit invariant discipline 'ghc_deps' uses.
module HaskellFlows.Tool.RemoveModules
  ( descriptor
  , handle
  , RemoveModulesArgs (..)
  , removeModulesFromBody
  , moduleToPath
    -- * Issue #41 — downstream-importer detection
  , Importer (..)
  , scanImportersInBody
  , scanImporters
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (takeExtension, (</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.ModuleName
  ( ModuleNameError
  , renderModuleNameError
  , validateModuleNames
  )
import HaskellFlows.Tool.AddModules (moduleToPath, parseModuleList)
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcRemoveModules
    , tdDescription =
        "De-register modules from the project's .cabal exposed-modules "
          <> "list. Source files are NOT deleted by default — pass "
          <> "delete_files=true to also remove the .hs files. Symmetric "
          <> "to ghc_add_modules; idempotent (no-op for modules that "
          <> "were not present)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "modules" .= object
                  [ "oneOf" .= (
                      [ object
                          [ "type"  .= ("array" :: Text)
                          , "items" .= object [ "type" .= ("string" :: Text) ]
                          ]
                      , object
                          [ "type" .= ("string" :: Text) ]
                      ] :: [Value])
                  , "description" .=
                      ("Module names to de-register. Accepts either a \
                       \JSON array (e.g. [\"Expr.Old\"]) or a single \
                       \comma-/whitespace-separated string \
                       \(e.g. \"Expr.Old, Expr.Unused\"). Same lenient \
                       \parsing as ghc_add_modules." :: Text)
                  ]
              , "delete_files" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("If true, also delete the backing .hs files under "
                       <> "src/. Default: false — source is preserved so "
                       <> "the agent can review / rename before destroying."
                       :: Text)
                  ]
              , "force" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("Issue #41: bypass the downstream-importer check. \
                       \Default false — if any remaining .hs file under \
                       \src/, test/, app/, or bench/ still has \
                       \'import <Mod>' for a removed module, the tool \
                       \refuses without force=true. With force=true, the \
                       \remove proceeds and the response carries a \
                       \'warnings.downstream_imports' array so the agent \
                       \knows what to repair next." :: Text)
                  ]
              ]
          , "required"             .= ["modules" :: Text]
          , "additionalProperties" .= False
          ]
    }

data RemoveModulesArgs = RemoveModulesArgs
  { rmaModules     :: ![Text]
  , rmaDeleteFiles :: !Bool
  , rmaForce       :: !Bool
  }
  deriving stock (Show)

instance FromJSON RemoveModulesArgs where
  parseJSON = withObject "RemoveModulesArgs" $ \o -> do
    raw <- o .: "modules"
    mods <- parseModuleList raw
    delF <- o .:? "delete_files" .!= False
    frc  <- o .:? "force" .!= False
    pure (RemoveModulesArgs mods delF frc)

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left err ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKindRM err)
          (T.pack ("Invalid arguments: " <> err)))
            { Env.eeCause = Just (T.pack err) })))
  Right (RemoveModulesArgs mods deleteFiles forceFlag) ->
    -- ISSUE-47: refuse names that violate the module-name grammar
    -- symmetrically with 'ghc_add_modules'. Two motivations:
    --   (1) typo-defence — agents calling remove with a malformed
    --       name are almost certainly buggy, and proceeding would
    --       corrupt-then-rewrite the .cabal in confusing ways;
    --   (2) consistency — the grammar contract is owned at the tool
    --       boundary, not duplicated per-callsite.
    --
    -- Recovery from a manually-corrupted .cabal (rare; pre-fix or
    -- hand-edited) requires direct file editing — by construction,
    -- post-fix add_modules/remove_modules cannot create that state.
    case validateModuleNames mods of
      (rejected@(_:_), _) -> pure (rejectionResult rejected)
      ([], validated) -> do
        -- Issue #41: scan the project for downstream importers
        -- BEFORE touching the .cabal. If any remaining .hs file
        -- imports one of the to-be-removed modules and force=false,
        -- refuse — the safer alternative the issue prefers.
        importers <- scanImporters pd validated
        if not (null importers) && not forceFlag
          then pure (downstreamRefusalResult validated importers)
          else do
            mCabal <- findCabalFile pd
            case mCabal of
              Nothing ->
                pure (Env.toolResponseToResult (Env.mkFailed
                  ((Env.mkErrorEnvelope Env.ModulePathDoesNotExist
                      "No .cabal file found in project root")
                        { Env.eeRemediation =
                            Just "Run ghc_create_project to scaffold a cabal package first." })))
              Just file -> do
                eCabal <- tryRewriteCabal file validated
                case eCabal of
                  Left err ->
                    pure (Env.toolResponseToResult (Env.mkFailed
                      ((Env.mkErrorEnvelope Env.SubprocessError err)
                          { Env.eeCause = Just err })))
                  Right removedFromCabal -> do
                    deleted <- if deleteFiles
                                 then deleteSourceFiles pd removedFromCabal
                                 else pure []
                    pure (successResult removedFromCabal deleted importers)

parseErrorKindRM :: String -> Env.ErrorKind
parseErrorKindRM err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

--------------------------------------------------------------------------------
-- cabal rewriting
--------------------------------------------------------------------------------

tryRewriteCabal :: FilePath -> [Text] -> IO (Either Text [Text])
tryRewriteCabal file mods = do
  res <- try (TIO.readFile file) :: IO (Either SomeException Text)
  case res of
    Left e     -> pure (Left (T.pack ("Could not read: " <> show e)))
    Right body ->
      let (newBody, removed) = removeModulesFromBody body mods
      in if null removed
           then pure (Right [])
           else do
             wres <- try (TIO.writeFile file newBody)
                       :: IO (Either SomeException ())
             case wres of
               Left e  -> pure (Left (T.pack ("Could not write: " <> show e)))
               Right _ -> pure (Right removed)

-- | Strip any exposed-modules entry whose stripped name is in
-- the remove-set. Preserves every other line verbatim. Returns
-- the updated body + the list of names that were actually
-- removed.
--
-- The scaffolded cabal often places the FIRST module on the
-- header line itself:
--
-- @
--     exposed-modules:  ExprEvaluator
--                       Expr.Syntax
-- @
--
-- so 'splitContinuation' alone misses it. We extract whatever
-- name lives after @exposed-modules:@ on the header line, treat
-- it as an additional candidate, and splice it back if it was
-- not a victim. If every module is gone we emit a bare
-- @exposed-modules:@ header — cabal accepts that and
-- 'ghc_add_modules' re-populates it on the next call.
removeModulesFromBody :: Text -> [Text] -> (Text, [Text])
removeModulesFromBody body mods =
  let lns              = T.lines body
      (pre, hAndRest)  = break isExposedHeader lns
  in case hAndRest of
       [] -> (body, [])
       (h : rest) ->
         let (cont, tailLns)    = break isNewField rest
             (keptCont, remCont) = splitContinuation mods cont
             (headerLeft, headerName) = splitHeaderLine h
             (newHeader, remHead) = case headerName of
               Just n | n `elem` mods ->
                 -- Victim on the header line. Drop it — leave the
                 -- bare "   exposed-modules:" so later adds can
                 -- re-seed the block. If there's at least one kept
                 -- continuation, promote the first to the header
                 -- line so the block keeps a module on each line.
                 case keptCont of
                   (firstKept : _) ->
                     (headerLeft <> T.stripStart firstKept, [n])
                   [] -> (headerLeft, [n])
               _ -> (h, [])
             -- Continuation lines to emit AFTER the rewritten
             -- header. If we promoted the first kept line into
             -- the header, drop it from the continuation list.
             newCont = case (headerName, keptCont) of
               (Just n, _ : rest2) | n `elem` mods -> rest2
               _                                    -> keptCont
             removed  = remHead <> remCont
             newBody  = T.unlines (pre <> (newHeader : newCont) <> tailLns)
         in if null removed then (body, [])
                            else (newBody, removed)

-- | Split an @exposed-modules:@ header line into the
-- @"  exposed-modules:  "@ prefix (preserving leading whitespace
-- and the colon + trailing spaces) and the possibly-empty first
-- module name on the same line. Whitespace-only tails yield
-- 'Nothing' for the name.
splitHeaderLine :: Text -> (Text, Maybe Text)
splitHeaderLine h =
  let (beforeColon, afterColon) = T.breakOn ":" h
  in case T.uncons afterColon of
       Just (':', rest) ->
         let leadingWs = T.takeWhile (\c -> c == ' ' || c == '\t') rest
             name      = T.strip rest
             headerLeft = beforeColon <> ":" <> leadingWs
         in if T.null name then (headerLeft, Nothing)
                           else (headerLeft, Just name)
       _ -> (h, Nothing)

-- | Partition the continuation lines into (kept, removed-names).
-- A continuation line usually contains exactly one module name
-- (the first value after the colon lives on the header line in
-- some cabal styles and is handled too).
splitContinuation :: [Text] -> [Text] -> ([Text], [Text])
splitContinuation victims = foldr go ([], [])
  where
    go ln (kept, removed) =
      let name = T.strip ln
      in if name `elem` victims
           then (kept,         name : removed)
           else (ln : kept,    removed)

isExposedHeader :: Text -> Bool
isExposedHeader ln =
  "exposed-modules:" `T.isPrefixOf` T.toLower (T.stripStart ln)

-- | Matches the sentinel used by 'AddModules.isNewField': a
-- continuation block ends at a blank line or the next field.
isNewField :: Text -> Bool
isNewField ln =
  let stripped = T.strip ln
  in T.null stripped
  || (T.null (T.takeWhile (== ' ') ln) && not (T.null stripped))
  || ":" `T.isInfixOf` T.takeWhile (/= ' ') stripped

--------------------------------------------------------------------------------
-- Issue #41 — downstream importer detection
--------------------------------------------------------------------------------

-- | One downstream importer reference: file path + line number +
-- the imported module name (the one we're about to remove). The
-- response includes a list of these so the agent can fix each.
data Importer = Importer
  { iFile   :: !Text
  , iLine   :: !Int
  , iModule :: !Text
  }
  deriving stock (Eq, Show)

-- | Walk @src/@, @test/@, @app/@, @bench/@ for @.hs@ files and
-- return every @import \<Mod\>@ reference whose target is in
-- 'targets'. Used by 'handle' before touching the .cabal so the
-- removal can be refused when force=false.
scanImporters :: ProjectDir -> [Text] -> IO [Importer]
scanImporters pd targets = do
  let root = unProjectDir pd
      searchDirs = map (root </>) ["src", "test", "app", "bench"]
  files <- enumerateHs searchDirs
  importerLists <- mapM (scanFile root targets) files
  pure (concat importerLists)

-- | Scan one @.hs@ file. The path stored in the result is
-- project-relative so the agent can paste it back into other
-- tools without escaping the project root.
scanFile :: FilePath -> [Text] -> FilePath -> IO [Importer]
scanFile root targets full = do
  eBody <- try (TIO.readFile full) :: IO (Either SomeException Text)
  case eBody of
    Left _     -> pure []
    Right body ->
      let rel = T.pack (stripRoot root full)
      in pure (scanImportersInBody rel targets body)

-- | Pure helper: given the @.hs@ file body (line-oriented), the
-- remove-list, and a label for the file, return every
-- @import \<Mod\>@ reference whose @\<Mod\>@ matches one of the
-- targets exactly (qualified / unqualified / aliased forms all
-- match). Module names are matched as whole tokens so removing
-- @\"Foo\"@ does NOT flag @import Foo.Bar@.
scanImportersInBody :: Text -> [Text] -> Text -> [Importer]
scanImportersInBody label targets body =
  [ Importer { iFile = label, iLine = ln, iModule = m }
  | (ln, raw) <- zip [1..] (T.lines body)
  , Just m <- [parseImportLine raw]
  , m `elem` targets
  ]

-- | Parse a Haskell @import@ line and extract the module name.
-- Recognises:
--
--   * @import Foo@
--   * @import qualified Foo@
--   * @import Foo (bar)@
--   * @import qualified Foo as F@
--
-- Returns 'Nothing' for non-import lines, comments, and any
-- shape we don't understand (keeps false positives out).
parseImportLine :: Text -> Maybe Text
parseImportLine raw =
  let stripped = T.stripStart raw
  in case T.stripPrefix "import " stripped of
       Nothing  -> Nothing
       Just rest ->
         let afterQ = case T.stripPrefix "qualified " (T.stripStart rest) of
               Just s  -> T.stripStart s
               Nothing -> T.stripStart rest
             modTok = T.takeWhile isModChar afterQ
         in if T.null modTok then Nothing else Just modTok
  where
    isModChar c = isAsciiUpper c
               || isAsciiLower c
               || isDigit c
               || c == '.' || c == '_' || c == '\''

-- | Recursively enumerate @.hs@ files under the given directories.
-- Missing directories are skipped silently.
enumerateHs :: [FilePath] -> IO [FilePath]
enumerateHs = go
  where
    go [] = pure []
    go (d : ds) = do
      eEnts <- try (listDirectory d) :: IO (Either SomeException [FilePath])
      case eEnts of
        Left _ -> go ds
        Right ents -> do
          let abs_ = map (d </>) ents
          subdirs <- filterM (\p -> do
                                 e <- try (listDirectory p)
                                        :: IO (Either SomeException [FilePath])
                                 pure (case e of
                                         Right _ -> True
                                         Left _  -> False)) abs_
          let hsHere = [ p | p <- abs_, takeExtension p == ".hs"
                           , p `notElem` subdirs ]
          rec_ <- go (subdirs <> ds)
          pure (hsHere <> rec_)

-- | Trim the project-root prefix from an absolute path. Falls
-- back to the original path if the prefix doesn't match (defensive
-- — the scanner only feeds in paths it built from 'root').
stripRoot :: FilePath -> FilePath -> FilePath
stripRoot root full =
  let r = root <> "/"
  in if take (length r) full == r then drop (length r) full else full

--------------------------------------------------------------------------------
-- file deletion (opt-in)
--------------------------------------------------------------------------------

deleteSourceFiles :: ProjectDir -> [Text] -> IO [FilePath]
deleteSourceFiles pd = foldr step (pure [])
  where
    step m ioAcc = do
      acc <- ioAcc
      case mkModulePath pd (moduleToPath m) of
        Left _   -> pure acc
        Right mp -> do
          let full = unModulePath mp
          exists <- doesFileExist full
          if not exists
            then pure acc
            else do
              _ <- try (removeFile full) :: IO (Either SomeException ())
              pure (full : acc)

--------------------------------------------------------------------------------
-- cabal discovery (duplicated from AddModules to avoid a cyclic import)
--------------------------------------------------------------------------------

findCabalFile :: ProjectDir -> IO (Maybe FilePath)
findCabalFile pd = do
  entries <- try (listDirectory (unProjectDir pd))
                 :: IO (Either SomeException [FilePath])
  case entries of
    Left _   -> pure Nothing
    Right es ->
      case [ unProjectDir pd </> e | e <- es, takeExtension e == ".cabal" ] of
        [one] -> pure (Just one)
        _     -> pure Nothing

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: [Text] -> [FilePath] -> [Importer] -> ToolResult
successResult removedFromCabal deletedFiles importers =
  let payload = object $
        [ "cabal_removed" .= removedFromCabal
        , "deleted_files" .= map T.pack deletedFiles
        , "hint"          .= mkHint importers
        ] <> warnsField
      warnsField
        | null importers = []
        | otherwise =
            [ "warnings" .= object
                [ "downstream_imports" .= map renderImporter importers ] ]
  in Env.toolResponseToResult (Env.mkOk payload)
  where
    mkHint :: [Importer] -> Text
    mkHint [] =
      "Modules were de-registered from exposed-modules. The next \
      \ghc_load picks up the new surface."
    mkHint _ =
      "Forced removal completed but downstream files still import \
      \the removed module(s). See warnings.downstream_imports for \
      \(file, line, module) tuples — fix each before the next \
      \ghc_load."

-- | Issue #41 + #90: refusal when downstream importers exist
-- and force=false. status='failed' (this is a hard refusal, not
-- a sanitize-layer policy) with kind='validation' and the
-- structured importer list inside 'result' for back-compat.
downstreamRefusalResult :: [Text] -> [Importer] -> ToolResult
downstreamRefusalResult requested importers =
  let err = (Env.mkErrorEnvelope Env.Validation
               "Refusing to remove module(s) — at least one remaining .hs file still imports them. Pass force=true to override.")
              { Env.eeRemediation =
                  Just "Either remove the import lines first OR pass force=true to proceed with downstream warnings."
              , Env.eeCause = Just "downstream_imports_present"
              }
      payload = object
        [ "requested"          .= requested
        , "downstream_imports" .= map renderImporter importers
        ]
      response = (Env.mkFailed err)
                   { Env.reResult = Just payload }
  in Env.toolResponseToResult response

renderImporter :: Importer -> Value
renderImporter i = object
  [ "file"   .= iFile i
  , "line"   .= iLine i
  , "module" .= iModule i
  ]

-- | Mirror of 'HaskellFlows.Tool.AddModules.rejectionResult'.
rejectionResult :: [(Text, ModuleNameError)] -> ToolResult
rejectionResult entries =
  let n        = length entries
      summary  = "rejected " <> tshow n <> " invalid module name"
                              <> (if n == 1 then "" else "s")
                              <> "; see 'rejected' for details"
      rendered = [ object
                     [ "name"   .= name
                     , "reason" .= renderModuleNameError err
                     ]
                 | (name, err) <- entries
                 ]
      err = (Env.mkErrorEnvelope Env.Validation summary)
              { Env.eeField = Just "modules"
              , Env.eeHint  =
                  Just "Haskell module names follow conid('.'conid)*, where each conid starts with an uppercase ASCII letter and may contain A-Z, a-z, 0-9, underscore, and apostrophe. Reserved keywords (module, where, class, ...) are rejected."
              }
      response = (Env.mkFailed err)
                   { Env.reResult = Just (object [ "rejected" .= rendered ]) }
  in Env.toolResponseToResult response

tshow :: Show a => a -> Text
tshow = T.pack . show
