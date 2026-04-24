-- | @ghci_add_modules@ — register new modules in the project's
-- @.cabal@ exposed-modules list AND scaffold their empty @.hs@
-- files under @src/@. Idempotent (skips names already present).
module HaskellFlows.Tool.AddModules
  ( descriptor
  , handle
  , AddModulesArgs (..)
  , moduleToPath
  , parseModuleList
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_add_modules"
    , tdDescription =
        "Register new modules in the project's .cabal exposed-modules "
          <> "and scaffold their empty .hs stubs under src/. Idempotent."
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
              ]
          , "required"             .= ["modules" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype AddModulesArgs = AddModulesArgs { amModules :: [Text] }
  deriving stock (Show)

instance FromJSON AddModulesArgs where
  parseJSON = withObject "AddModulesArgs" $ \o -> do
    raw <- o .: "modules"
    AddModulesArgs <$> parseModuleList raw

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
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right (AddModulesArgs mods) -> do
    mCabal <- findCabalFile pd
    case mCabal of
      Nothing -> pure (errorResult "No .cabal file found in project root")
      Just file -> do
        (createdFiles, existingFiles) <- scaffoldFiles pd mods
        eCabal <- tryRewriteCabal file mods
        case eCabal of
          Left err -> pure (errorResult err)
          Right addedToCabal ->
            pure (successResult createdFiles existingFiles addedToCabal)

--------------------------------------------------------------------------------
-- scaffolding
--------------------------------------------------------------------------------

-- | Convert @Expr.Syntax@ to @src/Expr/Syntax.hs@.
moduleToPath :: Text -> FilePath
moduleToPath m =
  let parts = T.splitOn "." m
      joined = T.intercalate "/" parts
  in "src/" <> T.unpack joined <> ".hs"

scaffoldFiles :: ProjectDir -> [Text] -> IO ([FilePath], [FilePath])
scaffoldFiles pd = foldl' step (pure ([], []))
  where
    step ioAcc m = do
      (c, ex) <- ioAcc
      case mkModulePath pd (moduleToPath m) of
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

tryRewriteCabal :: FilePath -> [Text] -> IO (Either Text [Text])
tryRewriteCabal file mods = do
  res <- try (TIO.readFile file) :: IO (Either SomeException Text)
  case res of
    Left e     -> pure (Left (T.pack ("Could not read: " <> show e)))
    Right body -> do
      let (newBody, added) = addModulesToBody body mods
      if null added
        then pure (Right [])
        else do
          wres <- try (TIO.writeFile file newBody) :: IO (Either SomeException ())
          case wres of
            Left e  -> pure (Left (T.pack ("Could not write: " <> show e)))
            Right _ -> pure (Right added)

-- | Insert each not-already-present module into the first
-- @exposed-modules:@ field. Preserves indentation by copying the
-- leading whitespace of the header line.
addModulesToBody :: Text -> [Text] -> (Text, [Text])
addModulesToBody body mods =
  let lns   = T.lines body
      (pre, headerAndRest) = break isExposedHeader lns
  in case headerAndRest of
       []  -> (body, [])
       (h : rest) ->
         let (cont, tailLns) = break isNewField rest
             existing = existingModules (h : cont)
             toAdd    = [ m | m <- mods, m `notElem` existing ]
             indent   = continuationIndent h
             newCont  = cont <> [ indent <> m | m <- toAdd ]
             newBody  = T.unlines (pre <> (h : newCont) <> tailLns)
         in if null toAdd
              then (body, [])
              else (newBody, toAdd)

isExposedHeader :: Text -> Bool
isExposedHeader ln =
  let s = T.toLower (T.stripStart ln)
  in "exposed-modules:" `T.isPrefixOf` s

-- | A continuation line is indented more than the header; a new
-- field starts at column 0 or at the same indent as the header.
isNewField :: Text -> Bool
isNewField ln =
  let stripped = T.strip ln
  in T.null stripped   -- blank line ends the block
  || (T.null (T.takeWhile (== ' ') ln) && not (T.null stripped))
  || ":" `T.isInfixOf` T.takeWhile (/= ' ') stripped

-- | Parse existing module names from the exposed-modules block.
existingModules :: [Text] -> [Text]
existingModules = concatMap extract
  where
    extract ln =
      let stripped = T.strip ln
          afterKw  = T.stripStart (T.drop (T.length "exposed-modules:")
                                           (T.toLower ln))
      in if "exposed-modules:" `T.isPrefixOf` T.toLower stripped
           then [T.strip (T.dropWhile (/= ' ') stripped) | not (T.null afterKw)]
           else [stripped]

-- | Indentation to use for inserted lines — mirrors the column the
-- first module after "exposed-modules:" starts at.
continuationIndent :: Text -> Text
continuationIndent headerLine =
  let leading = T.takeWhile (== ' ') headerLine
      afterField =
        T.drop (T.length leading + T.length "exposed-modules:") headerLine
      spacesBeforeValue = T.takeWhile (== ' ') afterField
      cols = T.length leading
           + T.length ("exposed-modules:" :: Text)
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

successResult :: [FilePath] -> [FilePath] -> [Text] -> ToolResult
successResult created existed addedToCabal =
  let payload = object
        [ "success"         .= True
        , "created_files"   .= map T.pack created
        , "existing_files"  .= map T.pack existed
        , "cabal_added"     .= addedToCabal
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
