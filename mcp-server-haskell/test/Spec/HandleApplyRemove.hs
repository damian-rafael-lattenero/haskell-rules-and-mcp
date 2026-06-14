-- | Handle-level tests for RemoveModules happy path and ApplyExports
-- keyword/lowercase guards (integration, uses real cabal file).
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.HandleApplyRemove
  ( testHandleRemoveModulesHappyPath
  , testHandleApplyExportsRefusesKeyword
  , testHandleApplyExportsAcceptsLowercase
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Types (mkProjectDir)
import qualified HaskellFlows.Types
import qualified HaskellFlows.Tool.ApplyExports as ApplyExports
import qualified HaskellFlows.Tool.RemoveModules as RM

import Spec.Helpers (decodeToolResult, runToolEnvelope)
import Spec.ToolEnvFixture (pdEnv)

--------------------------------------------------------------------------------
-- local fixture helpers (not exported)
--------------------------------------------------------------------------------

fixtureCabal :: Text
fixtureCabal = T.unlines
  [ "cabal-version: 3.0"
  , "name:          fixture"
  , "version:       0.0.0"
  , "library"
  , "    default-language: GHC2024"
  , "    hs-source-dirs:   src"
  , "    exposed-modules:  Foo"
  , "    build-depends:    base"
  ]

getTestTimestamp :: IO Int
getTestTimestamp = do
  t <- getPOSIXTime
  pure (floor (t * 1_000_000))

withFixture :: (HaskellFlows.Types.ProjectDir -> FilePath -> IO a) -> IO a
withFixture k = do
  tmp <- getTemporaryDirectory
  ts  <- show <$> getTestTimestamp
  let dir       = tmp </> ("haskell-flows-mn-" <> ts)
      cabalFile = dir </> "fixture.cabal"
  createDirectoryIfMissing True dir
  TIO.writeFile cabalFile fixtureCabal
  res <- case mkProjectDir dir of
    Left _   -> error "fixture: mkProjectDir failed"
    Right pd -> k pd cabalFile
  removePathForcibly dir
  pure res

--------------------------------------------------------------------------------
-- tests
--------------------------------------------------------------------------------

testHandleRemoveModulesHappyPath :: IO Bool
testHandleRemoveModulesHappyPath = withFixture $ \pd cabalFile -> do
  let args = A.object [ "modules" A..= (["Foo"] :: [Text]) ]
  result <- RM.handle pd args
  after  <- TIO.readFile cabalFile
  -- The fixture starts with 'Foo' on the exposed-modules header
  -- line; after the call that line should no longer carry 'Foo'
  -- as a value (the bare 'exposed-modules:' header survives).
  let exposedLines =
        [ ln | ln <- T.lines after
             , "exposed-modules:" `T.isInfixOf` T.toLower (T.stripStart ln) ]
      headerStripped = case exposedLines of
        (ln:_) -> T.strip (T.drop (T.length "exposed-modules:")
                          (T.dropWhile (/= ':') ln))
        []     -> "no-exposed-modules-line"
  pure (not (trIsError result) && headerStripped /= "Foo")

-- | 'ghc_apply_exports' refuses a reserved keyword as an export.
-- The module file is NOT modified — same atomic-refusal contract.
testHandleApplyExportsRefusesKeyword :: IO Bool
testHandleApplyExportsRefusesKeyword = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "Widget.hs"
      original   = T.unlines
        [ "module Widget where"
        , "greet :: String"
        , "greet = \"hi\""
        ]
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath original
  let args = A.object
        [ "module_path" A..= ("src/Widget.hs" :: Text)
        , "exports"     A..= (["greet", "module"] :: [Text])
        ]
  result <- ApplyExports.handle (pdEnv pd) args
  bodyAfter <- TIO.readFile modulePath
  -- Issue #90 Phase C: 'rejected' moved under 'result' inside the
  -- envelope. The 'resultPayload' helper drills through.
  pure
    (  trIsError result
    && bodyAfter == original
    && hasField "rejected" (resultPayload result)
    )

-- | 'ghc_apply_exports' regression: lowercase function-name exports
-- are still legal (exports != module names).
testHandleApplyExportsAcceptsLowercase :: IO Bool
testHandleApplyExportsAcceptsLowercase = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "Widget.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath (T.unlines ["module Widget where", "greet = \"hi\""])
  let args = A.object
        [ "module_path" A..= ("src/Widget.hs" :: Text)
        , "exports"     A..= (["greet"] :: [Text])
        ]
  result <- ApplyExports.handle (pdEnv pd) args
  bodyAfter <- TIO.readFile modulePath
  pure (not (trIsError result) && "(greet) where" `T.isInfixOf` bodyAfter)

--------------------------------------------------------------------------------
-- helpers shared by the handler-boundary tests
--------------------------------------------------------------------------------

-- | Decode a 'ToolResult' content payload back into a JSON 'Value'
-- so the tests can pattern-match on field shape (success, error,
-- rejected[]). Mirrors what an MCP client would do.
extractPayload :: ToolResult -> A.Value
extractPayload tr = case trContent tr of
  (TextContent t : _) -> case A.eitherDecodeStrict (encodeUtf8Strict t) of
    Right v -> v
    Left _  -> A.Null
  _ -> A.Null
  where
    encodeUtf8Strict = BL.toStrict . TLE.encodeUtf8 . TL.fromStrict

-- | Issue #90: drill through the envelope to the inner @result@
-- payload. Most pre-envelope tests inspected fields at the top
-- level — those fields now live under @result@. Returns the
-- top-level Value when there's no @result@ field (graceful
-- back-compat for tools still emitting the pre-envelope shape).
resultPayload :: ToolResult -> A.Value
resultPayload tr = case extractPayload tr of
  A.Object o -> case AKM.lookup (AKey.fromText "result") o of
    Just inner -> inner
    Nothing    -> A.Object o
  v          -> v

hasField :: Text -> A.Value -> Bool
hasField k (A.Object o) = AKM.member (AKey.fromText k) o
hasField _ _            = False

lookupField :: Text -> A.Value -> Maybe A.Value
lookupField k (A.Object o) = AKM.lookup (AKey.fromText k) o
lookupField _ _            = Nothing

fieldEquals :: Text -> A.Value -> A.Value -> Bool
fieldEquals k expected v = lookupField k v == Just expected
