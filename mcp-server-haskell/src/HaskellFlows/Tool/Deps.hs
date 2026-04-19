-- | @ghci_deps@ — add / remove / list entries in the project's @.cabal@
-- file without the agent having to edit it by hand.
--
-- We deliberately keep the cabal-file parser line-oriented rather than
-- pulling @Cabal@'s full parser as a dependency: it adds ~15 MB of
-- transitive closure, and we only care about one field
-- (@build-depends@) of the @library@ stanza. A focused string parser
-- covers the common comma-leading and comma-trailing shapes that
-- @cabal init@ / @cabal-fmt@ produce.
--
-- Security posture:
--
-- * The target @.cabal@ is located by scanning 'ProjectDir' only; we
--   never accept a path from the agent. Traversal is impossible.
-- * The package name goes through a strict identifier check — Hackage
--   names are @[A-Za-z0-9-]+@ and we refuse anything else. No shell
--   metacharacter can leak into the edit.
-- * The version constraint is validated with a minimal parser that
--   only accepts the operator + literal shape cabal actually uses
--   (@>=@, @<@, @^>=@, @&&@, numeric versions, spaces). Anything else
--   is rejected.
module HaskellFlows.Tool.Deps
  ( descriptor
  , handle
  , DepsArgs (..)
  , Action (..)
  , validatePackageName
  , validateVersionConstraint
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_deps"
    , tdDescription =
        "Manage build-depends in the project's .cabal file. Actions: "
          <> "'list' (current deps), 'add' (insert pkg + optional "
          <> "version constraint), 'remove' (delete by name). After "
          <> "add/remove run ghci_session(action='restart') to reload."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action"  .= object
                  [ "type" .= ("string" :: Text)
                  , "enum" .= (["list", "add", "remove"] :: [Text])
                  ]
              , "package" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Hackage package name. Required for add/remove."
                       :: Text)
                  ]
              , "version" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional cabal version constraint. Example: \
                       \\">= 2.14\", \"^>= 1.4\". Only used on 'add'."
                       :: Text)
                  ]
              ]
          , "required"             .= ["action" :: Text]
          , "additionalProperties" .= False
          ]
    }

data Action = ActList | ActAdd | ActRemove
  deriving stock (Eq, Show)

data DepsArgs = DepsArgs
  { daAction  :: !Action
  , daPackage :: !(Maybe Text)
  , daVersion :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON DepsArgs where
  parseJSON = withObject "DepsArgs" $ \o -> do
    a <- o .:  "action"
    p <- o .:? "package"
    v <- o .:? "version"
    act <- case (a :: Text) of
      "list"   -> pure ActList
      "add"    -> pure ActAdd
      "remove" -> pure ActRemove
      other    -> fail ("unknown action: " <> T.unpack other)
    pure DepsArgs { daAction = act, daPackage = p, daVersion = v }

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> do
    mCabal <- findCabalFile pd
    case mCabal of
      Nothing ->
        pure (errorResult "No .cabal file found in project root")
      Just file -> handleAction file args

handleAction :: FilePath -> DepsArgs -> IO ToolResult
handleAction file args = case daAction args of
  ActList -> do
    res <- try (TIO.readFile file) :: IO (Either SomeException Text)
    case res of
      Left e     -> pure (errorResult (T.pack ("Could not read cabal file: " <> show e)))
      Right body -> pure (listResult file (parseBuildDepends body))
  ActAdd -> case daPackage args of
    Nothing  -> pure (errorResult "'package' is required for add")
    Just pkg -> case validatePackageName pkg of
      Left err -> pure (errorResult err)
      Right safePkg -> case traverse validateVersionConstraint (daVersion args) of
        Left err -> pure (errorResult err)
        Right safeVer -> runEdit file safePkg (addDep safeVer) "added"
  ActRemove -> case daPackage args of
    Nothing  -> pure (errorResult "'package' is required for remove")
    Just pkg -> case validatePackageName pkg of
      Left err       -> pure (errorResult err)
      Right safePkg  -> runEdit file safePkg removeDep "removed"

runEdit
  :: FilePath
  -> Text                              -- validated package name
  -> (Text -> Text -> Text)            -- (pkg -> body -> newBody)
  -> Text                              -- verb for the success message
  -> IO ToolResult
runEdit file pkg f verb = do
  res <- try (TIO.readFile file) :: IO (Either SomeException Text)
  case res of
    Left e -> pure (errorResult (T.pack ("Could not read cabal file: " <> show e)))
    Right body -> do
      let newBody = f pkg body
      if newBody == body
        then pure (errorResult ("No change: '" <> pkg <> "' not found or already at desired state."))
        else do
          wres <- try (TIO.writeFile file newBody) :: IO (Either SomeException ())
          case wres of
            Left e  -> pure (errorResult (T.pack ("Could not write cabal file: " <> show e)))
            Right _ -> pure (editResult file pkg verb)

--------------------------------------------------------------------------------
-- cabal file discovery
--------------------------------------------------------------------------------

-- | Find the single @.cabal@ file in the project root. Returns
-- 'Nothing' if zero or multiple are present (the latter is unusual and
-- the agent should resolve it by hand).
findCabalFile :: ProjectDir -> IO (Maybe FilePath)
findCabalFile pd = do
  let root = unProjectDir pd
  exists <- doesDirectoryExist root
  if not exists
    then pure Nothing
    else do
      entries <- listDirectory root
      let cabalFiles = [ root </> e | e <- entries, takeExtension e == ".cabal" ]
      case cabalFiles of
        [one] -> pure (Just one)
        _     -> pure Nothing

--------------------------------------------------------------------------------
-- boundary validation
--------------------------------------------------------------------------------

-- | A Hackage package name is @[A-Za-z][A-Za-z0-9-]*@. Any other
-- character (including whitespace and meta-characters) is rejected.
validatePackageName :: Text -> Either Text Text
validatePackageName raw
  | T.null raw                = Left "package name is empty"
  | not (T.all okChar raw)    = Left ("invalid character in package name: " <> raw)
  | not (firstIsLetter raw)   = Left "package name must start with a letter"
  | otherwise                 = Right raw
  where
    okChar c = isAlphaNum c || c == '-'
    firstIsLetter t = case T.uncons t of
      Just (c, _) -> isAlphaNum c && not (isDigit c)
      Nothing     -> False

-- | Cabal version constraints are a tiny language: @>=@, @<=@, @<@,
-- @>@, @==@, @^>=@, conjunctions via @&&@, numeric @X.Y.Z@ versions,
-- and whitespace. We accept only those characters — no shell
-- metacharacters, no identifiers.
validateVersionConstraint :: Text -> Either Text Text
validateVersionConstraint raw
  | T.null stripped          = Left "version constraint is empty"
  | T.any (not . okChar) raw = Left ("invalid character in version constraint: " <> raw)
  | otherwise                = Right stripped
  where
    stripped = T.strip raw
    okChar c = isDigit c
            || isSpace c
            || c `elem` (".<>=^&" :: String)

--------------------------------------------------------------------------------
-- cabal file edits
--------------------------------------------------------------------------------

-- | Extract the names currently present in the library's
-- @build-depends@. Robust to:
--
-- * @build-depends:    base, text, aeson@              (comma-trailing)
-- * @build-depends:    base@, newline, @, text@         (comma-leading)
-- * @build-depends:@, newline, indented continuation
parseBuildDepends :: Text -> [Text]
parseBuildDepends body =
  let raw  = extractBuildDependsBody body
      toks = map T.strip (splitOnCommas raw)
  in sortOn T.toLower
       [ packageHead t
       | t <- toks
       , not (T.null t)
       ]
  where
    packageHead t =
      let (name, _) = T.break (\c -> isSpace c || c == '<' || c == '>' || c == '=' || c == '^' || c == '&')
                               (T.strip t)
      in name

-- | Pull everything that belongs to the first @build-depends:@ block
-- in the @library@ stanza. Stops at the next top-level keyword or at
-- end-of-file. Conservative: if we don't recognise the structure, we
-- return an empty string (the caller reports no deps).
extractBuildDependsBody :: Text -> Text
extractBuildDependsBody body =
  let lns = T.lines body
      -- Find the line that starts a build-depends block.
      rest = dropWhile (not . isBuildDependsHeader) lns
  in case rest of
       []     -> ""
       (h:ls) ->
         let headTail = T.drop (T.length "build-depends:") (T.strip h)
             cont     = takeWhile isContinuation ls
             all_     = headTail : map T.strip cont
         in T.unwords all_

isBuildDependsHeader :: Text -> Bool
isBuildDependsHeader ln =
  let s = T.toLower (T.stripStart ln)
  in "build-depends:" `T.isPrefixOf` s

-- | A continuation of @build-depends:@ is any line that isn't a new
-- top-level stanza header or a new field name at column 0.
isContinuation :: Text -> Bool
isContinuation ln
  | T.null stripped              = False
  | isBuildDependsHeader ln      = False
  | looksLikeFieldHeader stripped = False
  | not (T.null (T.takeWhile isSpace ln)) = True   -- indented continuation
  | otherwise                    = T.take 1 stripped == "," -- leading comma
  where
    stripped = T.strip ln

looksLikeFieldHeader :: Text -> Bool
looksLikeFieldHeader t =
  case T.breakOn ":" t of
    (name, rest)
      | T.null rest      -> False
      | T.any isSpace name -> False
      | otherwise        -> True

-- | Comma-split at the top level, ignoring commas inside parentheses.
splitOnCommas :: Text -> [Text]
splitOnCommas = go 0 []
  where
    go :: Int -> String -> Text -> [Text]
    go _ acc t | T.null t    = [T.pack (reverse acc)]
    go d acc t =
      case T.uncons t of
        Just ('(', r) -> go (d + 1)       ('(':acc) r
        Just (')', r) -> go (max 0 (d-1)) (')':acc) r
        Just (',', r)
          | d == 0    -> T.pack (reverse acc) : go 0 [] r
        Just (c,   r) -> go d (c:acc) r
        Nothing       -> [T.pack (reverse acc)]

-- | Insert a new @, pkg [version]@ entry after the last dep of the
-- library's @build-depends:@ block. If the dep already exists we
-- return the original body untouched (caller reports \"no change\").
addDep :: Maybe Text -> Text -> Text -> Text
addDep mVer pkg body
  | pkg `elem` parseBuildDepends body = body
  | otherwise                          =
      let entry = case mVer of
            Nothing -> pkg
            Just v  -> pkg <> " " <> v
      in insertAfterBuildDepends entry body

-- | Remove the dep by deleting its comma-prefixed entry from every
-- continuation line and the header line. If the dep isn't present,
-- returns the original body unchanged.
removeDep :: Text -> Text -> Text
removeDep pkg body
  | pkg `notElem` parseBuildDepends body = body
  | otherwise = T.unlines (map dropFromLine (T.lines body))
  where
    dropFromLine ln =
      let toks     = map T.strip (splitOnCommas ln)
          filtered = filter (not . depMatches) toks
          rebuilt  = T.intercalate ", " filtered
          -- Preserve leading "," or "build-depends:" framing when
          -- there's no dep on the line. Conservative: if nothing was
          -- left, drop the line wholesale by returning empty.
      in if filtered == toks then ln else rebuilt

    depMatches tok =
      let (name, _) = T.break (\c -> isSpace c || c == '<' || c == '>' || c == '=' || c == '^' || c == '&') tok
      in T.strip name == pkg

-- | Append @, <entry>@ to the end of the library's build-depends
-- continuation. If no existing block is found we append a new line.
insertAfterBuildDepends :: Text -> Text -> Text
insertAfterBuildDepends entry body =
  let lns = T.lines body
  in case splitAtBuildDependsEnd lns of
       Just (pre, post) ->
         T.unlines (pre <> [indentLike (last pre) <> ", " <> entry] <> post)
       Nothing -> body <> "\n-- build-depends: " <> entry <> "\n"
  where
    indentLike = T.takeWhile isSpace

-- | Split the source at the boundary between the end of the
-- build-depends block and whatever follows. Returns @Nothing@ if no
-- block was found.
splitAtBuildDependsEnd :: [Text] -> Maybe ([Text], [Text])
splitAtBuildDependsEnd ls =
  case break isBuildDependsHeader ls of
    (_, [])          -> Nothing
    (pre, h : rest)  ->
      let contLines = takeWhile isContinuation rest
          tailLines = drop (length contLines) rest
      in Just (pre <> (h : contLines), tailLines)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

listResult :: FilePath -> [Text] -> ToolResult
listResult file deps =
  let payload =
        object
          [ "success"      .= True
          , "action"       .= ("list" :: Text)
          , "cabal_file"   .= T.pack file
          , "count"        .= length deps
          , "build_depends".= deps
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

editResult :: FilePath -> Text -> Text -> ToolResult
editResult file pkg verb =
  let payload =
        object
          [ "success"    .= True
          , "action"     .= verb
          , "cabal_file" .= T.pack file
          , "package"    .= pkg
          , "hint"       .= ( "Run ghci_session(action=\"restart\") to \
                             \reload GHCi with the new dependency set."
                            :: Text )
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
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
