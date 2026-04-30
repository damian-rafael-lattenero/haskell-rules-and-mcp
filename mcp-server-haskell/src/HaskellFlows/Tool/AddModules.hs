-- | @ghc_add_modules@ — register new modules in the project's
-- @.cabal@ (library's @exposed-modules@ by default; test-suite,
-- executable, or benchmark @other-modules@ when 'stanza' is
-- supplied) AND scaffold empty @.hs@ stubs under the matching
-- @hs-source-dirs@. Idempotent (skips names already present).
module HaskellFlows.Tool.AddModules
  ( descriptor
  , handle
  , AddModulesArgs (..)
  , moduleToPath
  , moduleToPathForStanza
  , parseModuleList
  , StanzaTarget (..)
  , resolveStanzaTarget
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.ModuleName
  ( ModuleNameError
  , renderModuleNameError
  , validateModuleNames
  )
import qualified HaskellFlows.Tool.Deps as Deps
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcAddModules
    , tdDescription =
        "Register new modules in the project's .cabal and scaffold \
        \their empty .hs stubs. Default target is the library's \
        \'exposed-modules' under 'src/'. Pass 'stanza' to target \
        \another stanza: 'test-suite' / 'test-suite:NAME' / \
        \'executable[:NAME]' / 'benchmark[:NAME]'. Non-library \
        \stanzas route to 'other-modules' and scaffold under the \
        \stanza's conventional source dir (test/, app/, bench/). \
        \Idempotent."
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
                      ("Module names. Accepts either a JSON array \
                       \(e.g. [\"Expr.Syntax\", \"Expr.Eval\"]) or \
                       \a single string with comma- or whitespace-\
                       \separated names (e.g. \"Expr.Syntax, \
                       \Expr.Eval\"). The string form is a fallback \
                       \for MCP clients whose deferred-tool wrapper \
                       \stringifies array arguments before dispatch." :: Text)
                  ]
              , "stanza" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional stanza selector. Omit or pass \
                       \'library' to target the main library \
                       \(exposed-modules + src/). Other valid \
                       \values: 'test-suite', 'test-suite:NAME', \
                       \'executable', 'executable:NAME', \
                       \'benchmark', 'benchmark:NAME'. Non-library \
                       \stanzas route to 'other-modules' and scaffold \
                       \stubs under test/, app/, or bench/." :: Text)
                  ]
              ]
          , "required"             .= ["modules" :: Text]
          , "additionalProperties" .= False
          ]
    }

data AddModulesArgs = AddModulesArgs
  { amModules :: ![Text]
  , amStanza  :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON AddModulesArgs where
  parseJSON = withObject "AddModulesArgs" $ \o -> do
    raw <- o .: "modules"
    mods <- parseModuleList raw
    st <- o .:? "stanza"
    pure AddModulesArgs { amModules = mods, amStanza = st }

-- | Parse the @modules@ field of an 'AddModulesArgs' payload.
--
-- Accepts three forms:
--
--   * JSON array of strings — the documented, canonical shape.
--     @[\"Expr.Syntax\", \"Expr.Eval\"]@.
--   * Single JSON string whose CONTENT is itself a rendered JSON
--     array — e.g. @\"[\\\"Expr.Syntax\\\", \\\"Expr.Eval\\\"]\"@.
--     Deferred-tool wrappers sometimes produce this shape by
--     JSON-encoding the array twice. We try 'Data.Aeson.decode'
--     on it first and recurse on the result — so the caller
--     gets the same answer whether the array was passed
--     natively or stringified.
--   * Single JSON string — split on commas and/or whitespace.
--     @\"Expr.Syntax, Expr.Eval\"@ or @\"Expr.Syntax Expr.Eval\"@
--     both normalise to the same two-module list. This is the
--     last-resort fallback for plain-text input.
--
-- Motivation: BUG-PLUS-08 — Claude for Desktop's deferred-tool
-- path serialises array arguments as the RENDERED JSON text
-- (with brackets AND quotes), not as a comma-separated list.
-- Earlier versions accepted strings via naive comma-split,
-- which kept the @[@, @]@ and @\"@ delimiters as part of the
-- "module names", producing files like @src/[\\\"Expr/Syntax\\\".hs@
-- and corrupting the @.cabal@. The aeson-first path recognises
-- and unwraps the legitimate shape cleanly.
--
-- Empty or whitespace-only entries are filtered. Empty input
-- produces an empty list — the handler decides whether that's an
-- error (it is, but the validation message is friendlier at the
-- handler layer than here).
parseModuleList :: Value -> Parser [Text]
parseModuleList (Array xs) =
  traverse parseString (foldr (:) [] xs)  -- Data.Vector.toList without the dep
  where
    parseString (String s) = pure (T.strip s)
    parseString other      =
      fail ("expected module-name string, got: " <> show other)
parseModuleList (String s) =
  let trimmed   = T.strip s
      looksJson = case T.uncons trimmed of
                    Just ('[', _) -> "]" `T.isSuffixOf` trimmed
                    _             -> False
  in if looksJson
       then case eitherDecodeStrict (encodeUtf8 trimmed) of
              Right (Array xs) ->
                -- Recurse via the Array branch to reuse the
                -- strict-string enforcement; anything else in
                -- the array falls through to 'fail'.
                parseModuleList (Array xs)
              Right _ ->
                -- Decoded to something that's not an array;
                -- fall back to the comma/whitespace splitter on
                -- the ORIGINAL string so we don't hide the
                -- operator's intent.
                pure (splitPlain s)
              Left _ ->
                -- Malformed inner JSON; same fallback as above.
                pure (splitPlain s)
       else pure (splitPlain s)
  where
    splitPlain =
        filter (not . T.null)
      . map T.strip
      . T.split (\c -> c == ',' || c == ' ' || c == '\t' || c == '\n')
parseModuleList other =
  fail ("modules must be an array of strings or a comma-/whitespace-\
        \separated string; got: " <> show other)

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left err ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKindAM err)
          (T.pack ("Invalid arguments: " <> err)))
            { Env.eeCause = Just (T.pack err) })))
  Right (AddModulesArgs mods mStanzaRaw) ->
    case validateModuleNames mods of
      (rejected@(_:_), _) -> pure (rejectionResult rejected)
      ([], validated) ->
        case resolveStanzaTarget mStanzaRaw of
          Left err ->
            pure (Env.toolResponseToResult (Env.mkFailed
              ((Env.mkErrorEnvelope Env.Validation err)
                  { Env.eeField = Just "stanza" })))
          Right tgt -> do
            mCabal <- findCabalFile pd
            case mCabal of
              Nothing ->
                pure (Env.toolResponseToResult (Env.mkFailed
                  ((Env.mkErrorEnvelope Env.ModulePathDoesNotExist
                      "No .cabal file found in project root")
                        { Env.eeRemediation =
                            Just "Run ghc_create_project to scaffold a cabal package first." })))
              Just file -> do
                (createdFiles, existingFiles) <- scaffoldFiles pd tgt validated
                eCabal <- tryRewriteCabal file tgt validated
                case eCabal of
                  Left err ->
                    pure (Env.toolResponseToResult (Env.mkFailed
                      ((Env.mkErrorEnvelope Env.SubprocessError err)
                          { Env.eeCause = Just err })))
                  Right addedToCabal ->
                    pure (successResult tgt createdFiles existingFiles addedToCabal)

parseErrorKindAM :: String -> Env.ErrorKind
parseErrorKindAM err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

--------------------------------------------------------------------------------
-- stanza target
--------------------------------------------------------------------------------

-- | Resolved stanza destination. Distinguishes the (sourceDir,
-- fieldHeader) pair so 'scaffoldFiles' and 'addModulesToBody'
-- don't each have to re-implement the stanza → directory +
-- stanza → field-keyword mapping.
data StanzaTarget = StanzaTarget
  { stSelector  :: !(Maybe (Text, Maybe Text))
    -- ^ 'Nothing' for the library's first @exposed-modules@;
    -- 'Just (kind, mName)' for a scoped stanza slice.
  , stSourceDir :: !FilePath
    -- ^ conventional @hs-source-dirs@ root for new stubs: "src",
    -- "test", "app", or "bench".
  , stFieldName :: !Text
    -- ^ which cabal field to edit: "exposed-modules" for library,
    -- "other-modules" for every other stanza kind.
  , stLabel     :: !Text
    -- ^ human-readable label for the success payload.
  }
  deriving stock (Show)

-- | Parse the optional @stanza@ argument into a 'StanzaTarget'.
-- 'Nothing' / @"library"@ / @"lib"@ all pick the library.
resolveStanzaTarget :: Maybe Text -> Either Text StanzaTarget
resolveStanzaTarget mRaw = case normalise mRaw of
  Nothing -> Right libTarget
  Just raw
    | raw == "library" || raw == "lib" -> Right libTarget
    | otherwise -> case Deps.parseStanzaSelector raw of
        Left err        -> Left err
        Right sel@(k,_) ->
          let sourceDir = case k of
                "test-suite"  -> "test"
                "executable"  -> "app"
                "benchmark"   -> "bench"
                _             -> "src"
          in Right StanzaTarget
               { stSelector  = Just sel
               , stSourceDir = sourceDir
               , stFieldName = "other-modules"
               , stLabel     = Deps.renderSelector sel
               }
  where
    normalise = fmap T.strip . mfilter (not . T.null . T.strip)

    libTarget = StanzaTarget
      { stSelector  = Nothing
      , stSourceDir = "src"
      , stFieldName = "exposed-modules"
      , stLabel     = "library"
      }

    mfilter p (Just x) | p x = Just x
    mfilter _ _              = Nothing

--------------------------------------------------------------------------------
-- scaffolding
--------------------------------------------------------------------------------

-- | Convert @Expr.Syntax@ to @src/Expr/Syntax.hs@. Library-first
-- default — kept for backwards compatibility with the previous
-- single-target API. New callers should prefer
-- 'moduleToPathForStanza' which honours the stanza's source dir.
moduleToPath :: Text -> FilePath
moduleToPath = moduleToPathForStanza "src"

-- | Like 'moduleToPath' but honours the stanza's source directory
-- (e.g. @test/@ for a test-suite module).
moduleToPathForStanza :: FilePath -> Text -> FilePath
moduleToPathForStanza srcDir m =
  let parts = T.splitOn "." m
      joined = T.intercalate "/" parts
  in srcDir <> "/" <> T.unpack joined <> ".hs"

scaffoldFiles :: ProjectDir -> StanzaTarget -> [Text] -> IO ([FilePath], [FilePath])
scaffoldFiles pd tgt = foldl' step (pure ([], []))
  where
    step ioAcc m = do
      (c, ex) <- ioAcc
      case mkModulePath pd (moduleToPathForStanza (stSourceDir tgt) m) of
        Left _   -> pure (c, ex)  -- skip malformed silently; caller sees count delta
        Right mp -> do
          let full = unModulePath mp
          exists <- doesFileExist full
          if exists
            then pure (c, full : ex)
            else do
              _ <- try (do
                createDirectoryIfMissing True (takeDirectory full)
                TIO.writeFile full (stubContent m))
                :: IO (Either SomeException ())
              pure (full : c, ex)

stubContent :: Text -> Text
stubContent m = T.unlines
  [ "-- | TODO: describe " <> m <> "."
  , "module " <> m <> " where"
  , ""
  ]

--------------------------------------------------------------------------------
-- cabal rewriting
--------------------------------------------------------------------------------

tryRewriteCabal :: FilePath -> StanzaTarget -> [Text] -> IO (Either Text [Text])
tryRewriteCabal file tgt mods = do
  res <- try (TIO.readFile file) :: IO (Either SomeException Text)
  case res of
    Left e     -> pure (Left (T.pack ("Could not read: " <> show e)))
    Right body -> case addModulesToBody tgt body mods of
      Left err -> pure (Left err)
      Right (newBody, added)
        | null added -> pure (Right [])
        | otherwise -> do
            wres <- try (TIO.writeFile file newBody) :: IO (Either SomeException ())
            case wres of
              Left e  -> pure (Left (T.pack ("Could not write: " <> show e)))
              Right _ -> pure (Right added)

-- | Insert each not-already-present module into the stanza's
-- target field ('exposed-modules' for library; 'other-modules'
-- otherwise). When the field doesn't exist in the targeted
-- stanza, it's synthesised at the top of the stanza body with a
-- 4-space indent.
--
-- Returns 'Left' on stanza-not-found — the caller surfaces that
-- to the agent; the file is not touched.
addModulesToBody :: StanzaTarget -> Text -> [Text] -> Either Text (Text, [Text])
addModulesToBody tgt body mods = case stSelector tgt of
  Nothing  -> Right (addWithinBody (stFieldName tgt) body mods)
  Just sel -> case Deps.sliceStanza sel (T.lines body) of
    Nothing -> Left ("stanza not found: " <> Deps.renderSelector sel)
    Just (pre, stanzaLns, post) ->
      let stanzaBody           = T.unlines stanzaLns
          (newStanzaBody, add) = addWithinBody (stFieldName tgt) stanzaBody mods
          newStanzaLns         = T.lines newStanzaBody
      in Right (T.unlines (pre <> newStanzaLns <> post), add)

-- | Single-stanza rewrite: add modules to the first occurrence of
-- @<fieldName>:@ in @body@; synthesise the field if absent.
addWithinBody :: Text -> Text -> [Text] -> (Text, [Text])
addWithinBody fieldName body mods =
  let lns = T.lines body
      (pre, headerAndRest) = break (isFieldHeader fieldName) lns
  in case headerAndRest of
       [] ->
         -- Field absent: synthesise at the start of the stanza
         -- body. Skips any leading blank lines so the field lands
         -- just under the stanza header.
         let (blanks, rest) = span (T.null . T.strip) lns
             indent         = "    "
             newField       = (indent <> fieldName <> ":") :
                              [ indent <> "    " <> m | m <- mods ]
         in if null mods
              then (body, [])
              else (T.unlines (blanks <> newField <> rest), mods)
       (h : rest) ->
         let (cont, tailLns) = break isNewField rest
             existing = existingModules fieldName (h : cont)
             toAdd    = [ m | m <- mods, m `notElem` existing ]
             indent   = continuationIndent fieldName h
             newCont  = cont <> [ indent <> m | m <- toAdd ]
             newBody  = T.unlines (pre <> (h : newCont) <> tailLns)
         in if null toAdd
              then (body, [])
              else (newBody, toAdd)

isFieldHeader :: Text -> Text -> Bool
isFieldHeader fieldName ln =
  let s = T.toLower (T.stripStart ln)
  in T.toLower (fieldName <> ":") `T.isPrefixOf` s

-- | A continuation line is indented more than the header; a new
-- field starts at column 0 or at the same indent as the header.
isNewField :: Text -> Bool
isNewField ln =
  let stripped = T.strip ln
  in T.null stripped   -- blank line ends the block
  || (T.null (T.takeWhile (== ' ') ln) && not (T.null stripped))
  || ":" `T.isInfixOf` T.takeWhile (/= ' ') stripped

-- | Parse existing module names from the target field's block.
existingModules :: Text -> [Text] -> [Text]
existingModules fieldName = concatMap extract
  where
    kw = T.toLower fieldName <> ":"
    extract ln =
      let stripped = T.strip ln
          afterKw  = T.stripStart (T.drop (T.length kw) (T.toLower ln))
      in if kw `T.isPrefixOf` T.toLower stripped
           then [T.strip (T.dropWhile (/= ' ') stripped) | not (T.null afterKw)]
           else [stripped]

-- | Indentation to use for inserted lines — mirrors the column the
-- first module after @<fieldName>:@ starts at on the header line.
continuationIndent :: Text -> Text -> Text
continuationIndent fieldName headerLine =
  let leading = T.takeWhile (== ' ') headerLine
      afterField =
        T.drop (T.length leading + T.length fieldName + 1) headerLine
      spacesBeforeValue = T.takeWhile (== ' ') afterField
      cols = T.length leading
           + T.length fieldName + 1
           + T.length spacesBeforeValue
  in T.replicate (max cols (T.length leading + 4)) " "

--------------------------------------------------------------------------------
-- cabal discovery (cheap reuse of the Deps layout)
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

successResult :: StanzaTarget -> [FilePath] -> [FilePath] -> [Text] -> ToolResult
successResult tgt created existed addedToCabal =
  let payload = object
        [ "stanza"          .= stLabel tgt
        , "field"           .= stFieldName tgt
        , "source_dir"      .= T.pack (stSourceDir tgt)
        , "created_files"   .= map T.pack created
        , "existing_files"  .= map T.pack existed
        , "cabal_added"     .= addedToCabal
        ]
  in Env.toolResponseToResult (Env.mkOk payload)

-- | Structured rejection payload (ISSUE-47 + #90). Issue #90 §4
-- maps invalid-module-name to 'Validation' kind; the structured
-- 'rejected' array is preserved inside 'result' for backward-compat.
rejectionResult :: [(Text, ModuleNameError)] -> ToolResult
rejectionResult entries =
  let n        = length entries
      summary  = "rejected " <> tshow n <> " invalid module name"
                              <> (if n == 1 then "" else "s")
                              <> "; see 'rejected' for details"
      rendered = [ object
                     [ "name"   .= name
                     , "reason" .= renderModuleNameError mnErr
                     ]
                 | (name, mnErr) <- entries
                 ]
      err = (Env.mkErrorEnvelope Env.Validation summary)
              { Env.eeField = Just "modules"
              , Env.eeHint  =
                  Just "Haskell module names follow conid('.'conid)*, where each conid starts with an uppercase ASCII letter and may contain A-Z, a-z, 0-9, underscore, and apostrophe. Reserved keywords (module, where, class, ...) are rejected."
              , Env.eeCause = Just (T.pack (show n) <> " names rejected")
              }
      response = (Env.mkFailed err)
                   { Env.reResult = Just (object [ "rejected" .= rendered ]) }
  in Env.toolResponseToResult response

tshow :: Show a => a -> Text
tshow = T.pack . show
