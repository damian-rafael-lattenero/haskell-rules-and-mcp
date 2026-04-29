-- | @ghc_move@ — atomic cross-module symbol move with consumer
-- import-line rewrites and project-level verify (#62).
--
-- Phase 1 scope (the issue itself estimates 1.5–2 weeks split
-- across iterations; this commit lands the structural core):
--
--   * Move a single TOP-LEVEL binding from a source module to a
--     destination module that ALREADY EXISTS.
--   * Slice = optional leading Haddock comment + type signature +
--     body up to the next top-level binding.
--   * Append the slice at the end of the destination file.
--   * Rewrite consumer imports of shape @import From (sym, …)@ —
--     the symbol moves to a sibling @import To (sym)@ line, the
--     remaining selective imports stay on the original line.
--   * Multi-file snapshot + rollback on verify failure.
--   * @dry_run=true@ returns the planned change set without writing.
--
-- Phase 1 deferred (rejected with a remediation hint):
--
--   * Creating the destination module on the fly (call
--     @ghc_add_modules@ first).
--   * @import From hiding (…)@, @import qualified From as F@
--     rewrites — the rewriter leaves them untouched and verify
--     surfaces the real failure.
--   * Haddock cross-reference rewrites.
--   * Re-export modules (@module Foo (module Bar) where@).
module HaskellFlows.Tool.Move
  ( descriptor
  , handle
  , MoveArgs (..)
    -- * Pure slicing helpers (exported for unit tests)
  , SliceResult (..)
  , sliceTopLevelBinding
  , removeSliceFromBody
  , insertSliceAtEnd
  , rewriteImports
  , rewriteSelectiveImport
  , removeFromSourceExportList
  , moduleNameToPath
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (void)
import Data.Either (fromRight)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , firstLibraryOrTestSuite
  , invalidateLoadCache
  , loadForTarget
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Error (GhcError (..), Severity (..))
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcMove
    , tdDescription =
        "Atomic cross-module move of a top-level binding. Slices the "
          <> "type signature + Haddock + body out of the source module, "
          <> "appends to the destination, rewrites consumer 'import' "
          <> "lines, then verifies the project still loads. Any failure "
          <> "rolls back ALL touched files. Phase 1: destination module "
          <> "must already exist; 'import qualified' / 'import hiding' / "
          <> "Haddock refs are left untouched (verify will surface "
          <> "anything that breaks)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "symbol"  .= obj "string"
              , "from"    .= obj "string"
              , "to"      .= obj "string"
              , "dry_run" .= obj "boolean"
              ]
          , "required"             .= (["symbol", "from", "to"] :: [Text])
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data MoveArgs = MoveArgs
  { maSymbol :: !Text
  , maFrom   :: !Text
  , maTo     :: !Text
  , maDryRun :: !Bool
  }
  deriving stock (Show)

instance FromJSON MoveArgs where
  parseJSON = withObject "MoveArgs" $ \o ->
    MoveArgs
      <$> o .:  "symbol"
      <*> o .:  "from"
      <*> o .:  "to"
      <*> o .:? "dry_run" .!= False

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> runMove ghcSess pd args

--------------------------------------------------------------------------------
-- orchestration
--------------------------------------------------------------------------------

runMove :: GhcSession -> ProjectDir -> MoveArgs -> IO ToolResult
runMove sess pd args = do
  let root    = unProjectDir pd
      fromAbs = root </> moduleNameToPath (maFrom args)
      toAbs   = root </> moduleNameToPath (maTo   args)
  fromExists <- doesFileExist fromAbs
  toExists   <- doesFileExist toAbs
  if not fromExists
    then pure (kindError "source_module_missing"
                ("Could not locate source module on disk: "
                  <> T.pack fromAbs))
    else if not toExists
      then pure (kindError "destination_module_missing"
                  ("Phase 1 requires the destination module to exist. \
                   \Create it via ghc_add_modules first, then retry. \
                   \Looked for: " <> T.pack toAbs))
      else do
        eFromBody <- readBody fromAbs
        case eFromBody of
          Left e -> pure (errorResult
            (T.pack ("Could not read source: " <> show e)))
          Right fromBody ->
            case sliceTopLevelBinding (maSymbol args) fromBody of
              Nothing ->
                pure (kindError "symbol_not_found"
                       ("Could not locate top-level binding '"
                          <> maSymbol args <> "' in " <> maFrom args
                          <> ". Phase 1 keys on column-0 type-signatures \
                             \(`name :: …`) — names defined only via \
                             \pattern bindings or guards may not match yet."))
              Just sliced -> do
                eToBody <- readBody toAbs
                case eToBody of
                  Left e -> pure (errorResult
                    (T.pack ("Could not read destination: " <> show e)))
                  Right toBody ->
                    proceedMove sess pd args fromAbs toAbs
                      fromBody toBody sliced

readBody :: FilePath -> IO (Either SomeException Text)
readBody p = try (TIO.readFile p)

proceedMove
  :: GhcSession -> ProjectDir -> MoveArgs
  -> FilePath -> FilePath -> Text -> Text -> SliceResult
  -> IO ToolResult
proceedMove sess pd args fromAbs toAbs fromBody toBody sliced = do
  -- Phase 1 source-export update: if the source module's
  -- @module Foo (sym, …) where@ header carries an explicit export
  -- list with our symbol, drop the symbol from it. Otherwise the
  -- post-move load fails with \"Not in scope\" on the export list.
  -- An open export ('module Foo where') is left untouched.
  let fromExportStripped = removeFromSourceExportList (maSymbol args) fromBody
      fromBody'          = removeSliceFromBody sliced fromExportStripped
      toBody'            = insertSliceAtEnd sliced toBody
  consumerFiles <- enumerateConsumers (unProjectDir pd)
                     [fromAbs, toAbs]
  consumerSnapshots <- mapM (\p -> do
    e <- readBody p
    pure (p, fromRight "" e)) consumerFiles
  let plannedConsumers =
        [ (path, body, rewriteImports (maSymbol args)
                          (maFrom args) (maTo args) body)
        | (path, body) <- consumerSnapshots ]
      changedConsumers = filter (\(_, before, after) -> before /= after)
                           plannedConsumers
      allWrites :: [(FilePath, Text, Text)]
      allWrites =
        (fromAbs, fromBody, fromBody')
          : (toAbs, toBody, toBody')
          : changedConsumers
  if maDryRun args
    then pure (dryRunResult args allWrites)
    else doApply sess args allWrites

doApply
  :: GhcSession -> MoveArgs -> [(FilePath, Text, Text)] -> IO ToolResult
doApply sess args allWrites = do
  appliedRef <- newIORef ([] :: [FilePath])
  outcome    <- writeAll appliedRef allWrites
  case outcome of
    Left err -> do
      done <- readIORef appliedRef
      restoreAll allWrites done
      pure (kindError "write_failed"
             ("Could not write one of the files; rolled back: "
                <> T.pack err))
    Right () -> do
      invalidateLoadCache sess
      tgt <- firstLibraryOrTestSuite sess
      eLoad <- try (loadForTarget sess tgt Strict)
                 :: IO (Either SomeException (Bool, [GhcError]))
      done <- readIORef appliedRef
      case eLoad of
        Left ex -> do
          restoreAll allWrites done
          pure (verifyFailedResult args
                  [GhcError "" 0 0 SevError Nothing
                      (T.pack ("loadForTarget exception: " <> show ex))])
        Right (ok, diags) ->
          let errs = filter ((== SevError) . geSeverity) diags
          in if ok && null errs
               then pure (successResult args allWrites)
               else do
                 restoreAll allWrites done
                 pure (verifyFailedResult args errs)

writeAll
  :: IORef [FilePath]
  -> [(FilePath, Text, Text)]
  -> IO (Either String ())
writeAll _    [] = pure (Right ())
writeAll ref ((path, _, after) : rest) = do
  r <- try (TIO.writeFile path after) :: IO (Either SomeException ())
  case r of
    Left e  -> pure (Left (show e))
    Right _ -> do
      modifyIORef' ref (path :)
      writeAll ref rest

restoreAll :: [(FilePath, Text, Text)] -> [FilePath] -> IO ()
restoreAll allWrites =
  mapM_ (\p ->
    case find (\(q,_,_) -> q == p) allWrites of
      Just (_, before, _) ->
        void (try @SomeException (TIO.writeFile p before))
      Nothing -> pure ())

--------------------------------------------------------------------------------
-- slicing
--------------------------------------------------------------------------------

-- | Result of cutting a top-level binding out of a module body.
data SliceResult = SliceResult
  { srSliced     :: !Text   -- ^ binding text (sig + Haddock + body)
  , srStartLine  :: !Int    -- ^ 1-indexed start line of the slice
  , srEndLine    :: !Int    -- ^ 1-indexed end line (inclusive)
  }
  deriving stock (Eq, Show)

-- | Locate a top-level binding in the source body. Strategy:
-- find the first column-0 type-signature line of shape
-- @<symbol> :: …@, walk upward to absorb any Haddock comment
-- block, walk downward until the next column-0 non-comment
-- non-blank line.
sliceTopLevelBinding :: Text -> Text -> Maybe SliceResult
sliceTopLevelBinding symbol body =
  let indexed = zip [1 :: Int ..] (T.lines body)
  in case findSignatureLine symbol indexed of
       Nothing  -> Nothing
       Just sig ->
         let start    = absorbHaddockUp sig indexed
             end      = findEndOfBinding symbol sig indexed
             slicedTx = [ ln | (i, ln) <- indexed
                              , i >= start, i <= end ]
         in Just SliceResult
              { srSliced    = T.unlines slicedTx
              , srStartLine = start
              , srEndLine   = end
              }

findSignatureLine :: Text -> [(Int, Text)] -> Maybe Int
findSignatureLine symbol = fmap fst . find matches
  where
    needle = symbol <> " :: "
    matches (_, ln) =
      not (T.null ln)
        && T.takeWhile (== ' ') ln == ""
        && needle `T.isPrefixOf` ln

absorbHaddockUp :: Int -> [(Int, Text)] -> Int
absorbHaddockUp anchor indexed =
  let upTo = takeWhile ((< anchor) . fst) indexed
  in walk anchor (reverse upTo)
  where
    walk n []         = n
    walk n ((i, ln) : rest)
      | isComment ln          = walk i rest
      | T.null (T.strip ln)   = n
      | otherwise             = n
    isComment ln =
      let s = T.stripStart ln
      in "-- " `T.isPrefixOf` s
      || "--|" `T.isPrefixOf` s
      || "-- |" `T.isPrefixOf` s

-- | Walk DOWN from the signature line and stop at the first
-- column-0 line that starts a DIFFERENT top-level binding.
-- Lines that begin with the same symbol (the function's
-- equations / pattern matches) are part of THIS binding.
findEndOfBinding :: Text -> Int -> [(Int, Text)] -> Int
findEndOfBinding symbol start indexed =
  let after = dropWhile ((<= start) . fst) indexed
  in case find (isOtherTopLevel symbol . snd) after of
       Just (i, _) -> i - 1
       Nothing     -> case reverse indexed of
         ((i, _) : _) -> i
         []           -> start
  where
    isOtherTopLevel sym ln =
      let stripped = T.stripStart ln
      in not (T.null stripped)
         && T.takeWhile (== ' ') ln == ""
         && not ("-- " `T.isPrefixOf` ln)
         && not ("--|" `T.isPrefixOf` ln)
         && not ("--^" `T.isPrefixOf` ln)
         && not (sym `T.isPrefixOf` stripped
                   && hasIdentBoundaryAt (T.length sym) stripped)
    -- Same-symbol shape — header `sym :: …`, equation `sym x = …`,
    -- pattern `sym (Just x) = …`. After the symbol there's a
    -- non-identifier char (space, paren, equals, ::, etc).
    hasIdentBoundaryAt n s = case T.uncons (T.drop n s) of
      Just (c, _) -> not (isIdentChar c)
      Nothing     -> True
    isIdentChar c = isAsciiLower c
                 || isAsciiUpper c
                 || isDigit c
                 || c == '_' || c == '\''

-- | Drop the slice's line range from the source body. Collapses
-- the gap left by the cut so the post-move source stays readable.
removeSliceFromBody :: SliceResult -> Text -> Text
removeSliceFromBody slice body =
  let lns  = T.lines body
      kept = [ ln | (i, ln) <- zip [1 :: Int ..] lns
                  , i < srStartLine slice || i > srEndLine slice ]
  in T.unlines (collapseBlanks kept)

-- | Append the slice at the end of the destination body, with a
-- blank-line separator from any preceding content.
insertSliceAtEnd :: SliceResult -> Text -> Text
insertSliceAtEnd slice destBody =
  let trimmed = T.stripEnd destBody
      sep     = if T.null trimmed then "" else "\n\n"
  in trimmed <> sep <> srSliced slice

-- | Issue #62 Phase 1 — strip the moved symbol from the source
-- module's explicit export list. Handles the common shapes:
--
--   * @module Foo (sym, other) where@   → @module Foo (other) where@
--   * @module Foo (sym) where@           → @module Foo () where@
--   * @module Foo where@                 → unchanged (open export)
--   * Multi-line export lists            → not handled in Phase 1
--     (verify will surface the post-move \"not in scope\" error
--     and the user can fix manually).
--
-- Operates on the FULL body so the rest of the file is preserved
-- byte-for-byte.
removeFromSourceExportList :: Text -> Text -> Text
removeFromSourceExportList symbol body =
  case T.lines body of
    []       -> body
    (h : tl) ->
      case rewriteHeader symbol h of
        Nothing       -> body
        Just newHeader -> T.unlines (newHeader : tl)
  where
    rewriteHeader :: Text -> Text -> Maybe Text
    rewriteHeader sym ln =
      let leading  = T.takeWhile (== ' ') ln
          stripped = T.drop (T.length leading) ln
      in case T.stripPrefix "module " stripped of
           Nothing -> Nothing
           Just rest ->
             case T.breakOn "(" rest of
               (_, parenAndAfter) | T.null parenAndAfter -> Nothing
               (modPart, parenAndAfter) ->
                 let afterOpen = T.drop 1 parenAndAfter
                     (inside, closeAndAfter) = T.breakOn ")" afterOpen
                 in if T.null closeAndAfter then Nothing
                    else
                      let items   = map T.strip (T.splitOn "," inside)
                          kept    = filter (\t -> t /= sym && not (T.null t))
                                           items
                          afterClose = T.drop 1 closeAndAfter
                      in if sym `notElem` items then Nothing
                         else Just $ leading
                              <> "module " <> T.stripEnd modPart
                              <> " (" <> T.intercalate ", " kept <> ")"
                              <> afterClose

collapseBlanks :: [Text] -> [Text]
collapseBlanks = go False
  where
    go _ [] = []
    go prev (ln : rest)
      | T.null (T.strip ln) =
          if prev then go True rest else ln : go True rest
      | otherwise = ln : go False rest

--------------------------------------------------------------------------------
-- consumer rewriting
--------------------------------------------------------------------------------

-- | Rewrite a consumer body's @import@ lines so the moved symbol
-- comes from the destination instead of the source. Only the
-- selective shape @import From (sym, …)@ is touched in Phase 1
-- (qualified / hiding shapes are left alone — verify will surface
-- anything that breaks).
rewriteImports :: Text -> Text -> Text -> Text -> Text
rewriteImports symbol fromMod toMod body =
  T.unlines
    [ ln'
    | ln <- T.lines body
    , ln' <- expandLine ln
    ]
  where
    expandLine ln =
      case rewriteSelectiveImport symbol fromMod toMod ln of
        Nothing             -> [ln]
        Just (kept, addNew) ->
          let keptOut = [kept | not (T.null (T.strip kept))]
          in keptOut <> [addNew]

-- | Detect @import From (sym, …)@. If the symbol appears in the
-- list, returns @Just (keptLine, newLine)@. The rewriter
-- preserves leading whitespace so structured indentation isn't
-- lost.
rewriteSelectiveImport
  :: Text -> Text -> Text -> Text
  -> Maybe (Text, Text)
rewriteSelectiveImport symbol fromMod toMod ln =
  let leading  = T.takeWhile (== ' ') ln
      stripped = T.drop (T.length leading) ln
  in case T.stripPrefix "import " stripped of
       Nothing -> Nothing
       Just rest ->
         -- Bail on qualified imports — Phase 2.
         if "qualified " `T.isPrefixOf` rest then Nothing
         else
           let modTok = T.takeWhile isModChar rest
           in if modTok /= fromMod then Nothing
              else case stripImportList rest of
                Nothing -> Nothing
                Just (_, items, suffix) ->
                  let trimmed = map T.strip items
                      kept    = filter (\t -> t /= symbol && not (T.null t))
                                       trimmed
                  in if symbol `notElem` trimmed
                       then Nothing
                       else
                         let keptTxt
                               | null kept = leading <> "import " <> fromMod
                                                 <> " ()" <> suffix
                               | otherwise = leading <> "import " <> fromMod
                                                 <> " ("
                                                 <> T.intercalate ", " kept
                                                 <> ")" <> suffix
                             newTxt = leading <> "import " <> toMod
                                              <> " (" <> symbol <> ")"
                         in Just (keptTxt, newTxt)
  where
    isModChar c = isAsciiUpper c
               || isAsciiLower c
               || isDigit c
               || c == '.' || c == '_' || c == '\''

-- | Split an import line at the parenthesised import list.
-- Returns @Just (modName, items, suffix)@ or 'Nothing' when the
-- line lacks an explicit list.
stripImportList :: Text -> Maybe (Text, [Text], Text)
stripImportList input =
  let (lhs, parenAndAfter) = T.breakOn "(" input
  in if T.null parenAndAfter
       then Nothing
       else
         let afterOpen = T.drop 1 parenAndAfter
             (inside, closeAndAfter) = T.breakOn ")" afterOpen
         in if T.null closeAndAfter
              then Nothing
              else Just
                ( T.strip lhs
                , T.splitOn "," inside
                , T.drop 1 closeAndAfter
                )

--------------------------------------------------------------------------------
-- consumer enumeration
--------------------------------------------------------------------------------

-- | Recursively enumerate @.hs@ files under @src/@, @test/@,
-- @app/@, @bench/@. Skips the source + destination paths the
-- caller passes in (they're handled separately).
enumerateConsumers :: FilePath -> [FilePath] -> IO [FilePath]
enumerateConsumers root excluded = do
  let dirs = map (root </>) ["src", "test", "app", "bench"]
  hs <- mapM scanRec dirs
  pure [ p | p <- concat hs, p `notElem` excluded ]

scanRec :: FilePath -> IO [FilePath]
scanRec p = do
  isDir <- doesDirectoryExist p
  if not isDir
    then pure []
    else do
      eEnts <- try @SomeException (listDirectory p)
      case eEnts of
        Left _   -> pure []
        Right xs -> do
          let absChildren = map (p </>) xs
          deeper <- mapM scanRec absChildren
          let here = [ q | q <- absChildren, takeExtension q == ".hs" ]
          pure (here <> concat deeper)

--------------------------------------------------------------------------------
-- module-name → file-path
--------------------------------------------------------------------------------

moduleNameToPath :: Text -> FilePath
moduleNameToPath m =
  "src" </> T.unpack (T.replace "." "/" m) <> ".hs"

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

dryRunResult :: MoveArgs -> [(FilePath, Text, Text)] -> ToolResult
dryRunResult args allWrites =
  let payload = object
        [ "success"           .= True
        , "applied"           .= False
        , "dry_run"           .= True
        , "symbol"            .= maSymbol args
        , "from"              .= maFrom   args
        , "to"                .= maTo     args
        , "files_modified"    .=
            map (\(p,_,_) -> T.pack p) allWrites
        , "consumers_updated" .= max 0 (length allWrites - 2)
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

successResult :: MoveArgs -> [(FilePath, Text, Text)] -> ToolResult
successResult args allWrites =
  let payload = object
        [ "success"           .= True
        , "applied"           .= True
        , "dry_run"           .= False
        , "symbol"            .= maSymbol args
        , "from"              .= maFrom   args
        , "to"                .= maTo     args
        , "files_modified"    .=
            map (\(p,_,_) -> T.pack p) allWrites
        , "consumers_updated" .= max 0 (length allWrites - 2)
        , "verification"      .= object
            [ "compile_status" .= ("ok" :: Text)
            , "new_errors"     .= (0 :: Int)
            ]
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

verifyFailedResult :: MoveArgs -> [GhcError] -> ToolResult
verifyFailedResult args errs =
  let payload = object
        [ "success"     .= False
        , "applied"     .= False
        , "error_kind"  .= ("verify_failed" :: Text)
        , "error"       .=
            ( "Move rolled back — post-move project did not load. \
              \See 'errors' for the GHC diagnostics." :: Text )
        , "symbol"      .= maSymbol args
        , "from"        .= maFrom   args
        , "to"          .= maTo     args
        , "errors"      .= map renderErr errs
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = True
       }
  where
    renderErr e = object
      [ "file"    .= geFile e
      , "line"    .= geLine e
      , "column"  .= geColumn e
      , "message" .= geMessage e
      ]

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

kindError :: Text -> Text -> ToolResult
kindError kind msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success"    .= False
        , "error_kind" .= kind
        , "error"      .= msg
        ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
