-- | @ghci_create_project@ — scaffold a Haskell cabal project inside the
-- current 'ProjectDir'.
--
-- Writes a minimal but complete project:
--
-- > <project-name>.cabal     -- library + test-suite stanzas
-- > cabal.project            -- single-package
-- > src/<ModuleRoot>.hs      -- stub with one example function
-- > test/Spec.hs             -- runs the one example test
--
-- No @app\/Main.hs@ by default — most agents want a library first, and
-- the executable can be added later with 'ghci_deps'-style edits.
--
-- Security posture:
--
-- * The project name is validated as an identifier (letters, digits,
--   hyphen) — same rule as Hackage. Anything else is rejected before
--   any FS write.
-- * All files are written relative to 'ProjectDir'. There is no
--   path input from the agent, so path traversal is impossible by
--   construction.
-- * @overwrite=false@ is the safe default; an existing file in the
--   way makes the tool fail loudly rather than clobber work.
module HaskellFlows.Tool.CreateProject
  ( descriptor
  , handle
  , CreateArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum, isAsciiUpper, isDigit, toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  )
import System.FilePath ((</>), takeDirectory)

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_create_project"
    , tdDescription =
        "Scaffold a minimal cabal project (library + test-suite) in the "
          <> "current project directory. Creates <name>.cabal, "
          <> "cabal.project, src/<Module>.hs, and test/Spec.hs."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Package name (Hackage shape: letters + digits \
                       \+ hyphen, must start with a letter)." :: Text)
                  ]
              , "module" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Top-level module name. Default: derived from the \
                       \package name by PascalCase-ing the segments."
                       :: Text)
                  ]
              , "overwrite" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("If true, overwrite existing scaffolded files. \
                       \Default: false — fails if any target already exists."
                       :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

data CreateArgs = CreateArgs
  { caName      :: !Text
  , caModule    :: !(Maybe Text)
  , caOverwrite :: !Bool
  }
  deriving stock (Show)

instance FromJSON CreateArgs where
  parseJSON = withObject "CreateArgs" $ \o -> do
    n  <- o .:  "name"
    m  <- o .:? "module"
    ow <- o .:? "overwrite" .!= False
    pure CreateArgs { caName = n, caModule = m, caOverwrite = ow }

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> case validateName (caName args) of
    Left err -> pure (errorResult err)
    Right pkg -> do
      let modName = case caModule args of
            Just m  -> m
            Nothing -> packageToModule pkg
      case validateModule modName of
        Left err -> pure (errorResult err)
        Right m  -> scaffold pd pkg m (caOverwrite args)

--------------------------------------------------------------------------------
-- boundary validation
--------------------------------------------------------------------------------

validateName :: Text -> Either Text Text
validateName raw
  | T.null raw                      = Left "name is empty"
  | not (T.all okChar raw)          = Left ("invalid character in package name: " <> raw)
  | not (startsWithLetter raw)      = Left "package name must start with a letter"
  | otherwise                       = Right raw
  where
    okChar c = isAlphaNum c || c == '-'
    startsWithLetter t = case T.uncons t of
      Just (c, _) -> isAlphaNum c && not (isDigit c)
      Nothing     -> False

validateModule :: Text -> Either Text Text
validateModule raw
  | T.null raw                       = Left "module name is empty"
  | T.any (not . okChar) raw         = Left ("invalid character in module name: " <> raw)
  | not (startsWithUpper raw)        = Left "module name must start with an uppercase letter"
  | otherwise                        = Right raw
  where
    okChar c = isAlphaNum c || c == '.' || c == '_'
    startsWithUpper t = case T.uncons t of
      Just (c, _) -> isAsciiUpper c
      Nothing     -> False

-- | @haskell-flows-mcp@ → @HaskellFlowsMcp@.
packageToModule :: Text -> Text
packageToModule = T.concat . map capitalise . T.splitOn "-"
  where
    capitalise t = case T.uncons t of
      Nothing     -> t
      Just (c,cs) -> T.cons (toUpper c) cs

--------------------------------------------------------------------------------
-- scaffolding
--------------------------------------------------------------------------------

-- | Plan of files to write. Kept as data so we can check for collisions
-- before touching disk.
data FilePlan = FilePlan
  { fpRelPath :: !FilePath
  , fpContent :: !Text
  }
  deriving stock (Show)

scaffold :: ProjectDir -> Text -> Text -> Bool -> IO ToolResult
scaffold pd pkg modName overwrite = do
  let root  = unProjectDir pd
      plans =
        [ FilePlan (T.unpack pkg <> ".cabal")                   (cabalFile pkg modName)
        , FilePlan "cabal.project"                               cabalProject
        , FilePlan ("src" </> moduleToRelPath modName <> ".hs") (sourceFile modName)
        , FilePlan ("test" </> "Spec.hs")                       (testFile modName)
        ]
  if not overwrite
    then do
      clashes <- filterExistingM root plans
      case clashes of
        [] -> writeAll root plans pkg modName
        xs -> pure (errorResult
                ( "Target files already exist: "
               <> T.intercalate ", " (map (T.pack . fpRelPath) xs)
               <> ". Pass overwrite=true to replace." ))
    else writeAll root plans pkg modName

filterExistingM :: FilePath -> [FilePlan] -> IO [FilePlan]
filterExistingM root = filterM
  where
    filterM = \case
      []     -> pure []
      (x:xs) -> do
        e  <- doesFileExist (root </> fpRelPath x)
        rs <- filterM xs
        pure (if e then x : rs else rs)

writeAll :: FilePath -> [FilePlan] -> Text -> Text -> IO ToolResult
writeAll root plans pkg modName = do
  res <- try (mapM_ (writeOne root) plans) :: IO (Either SomeException ())
  case res of
    Left e  -> pure (errorResult (T.pack ("write failed: " <> show e)))
    Right _ -> pure (createdResult pkg modName plans)

writeOne :: FilePath -> FilePlan -> IO ()
writeOne root fp = do
  let full = root </> fpRelPath fp
  createDirectoryIfMissing True (takeDirectory full)
  TIO.writeFile full (fpContent fp)

moduleToRelPath :: Text -> FilePath
moduleToRelPath = T.unpack . T.replace "." "/"

--------------------------------------------------------------------------------
-- file contents
--------------------------------------------------------------------------------

cabalFile :: Text -> Text -> Text
cabalFile pkg modName = T.unlines
  [ "cabal-version:      3.0"
  , "name:               " <> pkg
  , "version:            0.1.0.0"
  , "synopsis:           (describe here)"
  , "license:            BSD-3-Clause"
  , "build-type:         Simple"
  , ""
  , "common shared"
  , "    default-language:   GHC2024"
  , "    ghc-options:        -Wall -Wcompat -Widentities"
  , "                        -Wincomplete-record-updates"
  , "                        -Wincomplete-uni-patterns"
  , "                        -Wpartial-fields"
  , "                        -Wredundant-constraints"
  , "                        -Wunused-packages"
  , "    default-extensions: OverloadedStrings"
  , "                        DerivingStrategies"
  , "                        LambdaCase"
  , ""
  , "library"
  , "    import:           shared"
  , "    hs-source-dirs:   src"
  , "    exposed-modules:  " <> modName
  , "    build-depends:    base >= 4.20 && < 5"
  , ""
  , "test-suite " <> pkg <> "-test"
  , "    import:           shared"
  , "    type:             exitcode-stdio-1.0"
  , "    hs-source-dirs:   test"
  , "    main-is:          Spec.hs"
  , "    build-depends:    base >= 4.20 && < 5"
  , "                    , " <> pkg
  ]

cabalProject :: Text
cabalProject = "packages: .\n"

sourceFile :: Text -> Text
sourceFile modName = T.unlines
  [ "-- | Stub module scaffolded by ghci_create_project."
  , "module " <> modName <> " (greet) where"
  , ""
  , "-- | Example function — replace with your own."
  , "greet :: String -> String"
  , "greet who = \"Hello, \" <> who <> \"!\""
  ]

testFile :: Text -> Text
testFile modName = T.unlines
  [ "-- | Scaffolded test suite. Prints PASS/FAIL per case, exits 1 on"
  , "-- any failure. Add tests here and extend with QuickCheck / hspec"
  , "-- as the project grows."
  , "module Main where"
  , ""
  , "import " <> modName <> " (greet)"
  , "import System.Exit (exitFailure, exitSuccess)"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  let ok = greet \"world\" == \"Hello, world!\""
  , "  putStrLn (if ok then \"PASS  greet world\" else \"FAIL  greet world\")"
  , "  if ok then exitSuccess else exitFailure"
  ]

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

createdResult :: Text -> Text -> [FilePlan] -> ToolResult
createdResult pkg modName plans =
  let payload =
        object
          [ "success"       .= True
          , "package"       .= pkg
          , "module"        .= modName
          , "files_written" .= map (T.pack . fpRelPath) plans
          , "hint"          .= ( "Run ghci_load(module_path=\"src/"
                              <> T.replace "." "/" modName
                              <> ".hs\") to verify the scaffold compiles."
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
