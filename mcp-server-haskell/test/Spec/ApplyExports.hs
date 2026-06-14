-- | Unit tests for 'Tool.ApplyExports': text manipulation (idempotent,
-- inject, replace, no-header) and handle-level applied/write flags.
--
-- Extracted from the Spec.hs monolith (#271) via the function-export shape.
module Spec.ApplyExports
  ( testApplyExportsIdempotent
  , testApplyExportsInjects
  , testApplyExportsReplacesExistingList
  , testApplyExportsNoHeader
  , testApplyExportsSuccessHasApplied
  , testApplyExportsNoChangeHasApplied
  , testApplyExportsHandleAppliedTrue
  , testApplyExportsHandleAppliedFalse
  , testApplyExportsWriteFalse
  ) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol (ToolContent (..), ToolResult (..))
import HaskellFlows.Types (mkProjectDir, ProjectDir)
import qualified HaskellFlows.Types
import qualified HaskellFlows.Tool.ApplyExports as ApplyExports

import Spec.Helpers (decodeToolResult, runToolEnvelope, getTestTimestamp)
import Spec.ToolEnvFixture (pdEnv)

-- | #173: idempotent case — when the requested list is IDENTICAL to
-- the existing one, must return Unchanged.
testApplyExportsIdempotent :: IO Bool
testApplyExportsIdempotent =
  let body = T.unlines
        [ "-- header"
        , "module Foo (a, b) where"
        , "a = 1"
        ]
  in pure (ApplyExports.rewriteHeader ["a", "b"] body == ApplyExports.Unchanged)

testApplyExportsInjects :: IO Bool
testApplyExportsInjects =
  let body = T.unlines
        [ "module Foo where"
        , "a = 1"
        ]
  in case ApplyExports.rewriteHeader ["a", "b"] body of
       ApplyExports.Rewritten newBody ->
         pure (T.isInfixOf "module Foo (a, b) where" newBody)
       _ -> pure False

-- | #173: when the header already has a DIFFERENT export list,
-- 'rewriteHeader' must return 'Rewritten' with the new list — not
-- 'Unchanged'.
testApplyExportsReplacesExistingList :: IO Bool
testApplyExportsReplacesExistingList =
  let body = T.unlines
        [ "module Foo (greet, reverseStr, double, Color, Tree (..)) where"
        , "greet = undefined"
        ]
  in case ApplyExports.rewriteHeader ["greet", "reverseStr"] body of
       ApplyExports.Rewritten newBody ->
         pure $ T.isInfixOf "module Foo (greet, reverseStr) where" newBody
              && not (T.isInfixOf "double" newBody)
              && not (T.isInfixOf "Tree" newBody)
       _ -> pure False

-- | #173: when the source has no @module Foo where@ line,
-- 'rewriteHeader' must return 'NoHeader', not 'Unchanged'.
testApplyExportsNoHeader :: IO Bool
testApplyExportsNoHeader =
  let body = T.unlines
        [ "-- No module declaration"
        , "main :: IO ()"
        , "main = pure ()"
        ]
  in pure (ApplyExports.rewriteHeader ["main"] body == ApplyExports.NoHeader)

-- | #133: successResult (file written) must include applied=true in
-- the result payload so callers can distinguish write from no-op.
testApplyExportsSuccessHasApplied :: IO Bool
testApplyExportsSuccessHasApplied = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "TestMod.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath (T.unlines ["module TestMod where", "x = 1"])
  let args = A.object
        [ "module_path" A..= ("src/TestMod.hs" :: T.Text)
        , "exports"     A..= (["x"] :: [T.Text])
        ]
  tr <- ApplyExports.handle (pdEnv pd) args
  let payload = resultPayload tr
  pure $ fieldEquals "applied" (A.Bool True) payload

-- | #133: noChangeResult (header already has exports) must include
-- applied=false in the result payload.
testApplyExportsNoChangeHasApplied :: IO Bool
testApplyExportsNoChangeHasApplied = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "TestMod2.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  -- Write a module that already has an export list → triggers noChangeResult.
  TIO.writeFile modulePath (T.unlines ["module TestMod2 (x) where", "x = 1"])
  let args = A.object
        [ "module_path" A..= ("src/TestMod2.hs" :: T.Text)
        , "exports"     A..= (["x"] :: [T.Text])
        ]
  tr <- ApplyExports.handle (pdEnv pd) args
  let payload = resultPayload tr
  pure $ fieldEquals "no_change" (A.Bool True) payload
      && fieldEquals "applied"   (A.Bool False) payload

-- | #133: handle write path (new export list inserted) returns applied=true.
-- Exercises the full handler integration.
testApplyExportsHandleAppliedTrue :: IO Bool
testApplyExportsHandleAppliedTrue = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "AppliedTrue.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath (T.unlines ["module AppliedTrue where", "foo = 42"])
  let args = A.object
        [ "module_path" A..= ("src/AppliedTrue.hs" :: T.Text)
        , "exports"     A..= (["foo"] :: [T.Text])
        ]
  tr <- ApplyExports.handle (pdEnv pd) args
  bodyAfter <- TIO.readFile modulePath
  let payload = resultPayload tr
  pure $ fieldEquals "applied" (A.Bool True) payload
      && "foo" `T.isInfixOf` bodyAfter

-- | #133: handle no-op path (header already has exports) returns applied=false.
testApplyExportsHandleAppliedFalse :: IO Bool
testApplyExportsHandleAppliedFalse = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "AppliedFalse.hs"
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath (T.unlines ["module AppliedFalse (bar) where", "bar = 0"])
  let args = A.object
        [ "module_path" A..= ("src/AppliedFalse.hs" :: T.Text)
        , "exports"     A..= (["bar"] :: [T.Text])
        ]
  tr <- ApplyExports.handle (pdEnv pd) args
  let payload = resultPayload tr
  pure $ fieldEquals "applied"   (A.Bool False) payload
      && fieldEquals "no_change" (A.Bool True)  payload

-- | #155: write=false (dry-run) must return applied=false and must NOT
-- write to disk. Before the fix the tool always wrote and returned
-- applied=true regardless of the write parameter.
testApplyExportsWriteFalse :: IO Bool
testApplyExportsWriteFalse = withFixture $ \pd _ -> do
  let projectDir = HaskellFlows.Types.unProjectDir pd
      modulePath = projectDir </> "src" </> "WriteFalse.hs"
      origBody   = T.unlines ["module WriteFalse where", "foo = 42"]
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile modulePath origBody
  let args = A.object
        [ "module_path" A..= ("src/WriteFalse.hs" :: T.Text)
        , "exports"     A..= (["foo"] :: [T.Text])
        , "write"       A..= False
        ]
  tr <- ApplyExports.handle (pdEnv pd) args
  bodyAfter <- TIO.readFile modulePath
  let payload = resultPayload tr
  pure $ fieldEquals "applied" (A.Bool False) payload
      && bodyAfter == origBody  -- file must be unchanged

--------------------------------------------------------------------------------
-- Local helpers (not exported)
--------------------------------------------------------------------------------

withFixture :: (ProjectDir -> FilePath -> IO a) -> IO a
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

fixtureCabal :: T.Text
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

resultPayload :: ToolResult -> A.Value
resultPayload tr = case extractPayload tr of
  A.Object o -> case AKM.lookup (AKey.fromText "result") o of
    Just inner -> inner
    Nothing    -> A.Object o
  v          -> v

extractPayload :: ToolResult -> A.Value
extractPayload tr = case trContent tr of
  (TextContent t : _) ->
    case A.eitherDecodeStrict (encodeUtf8Strict t) of
      Right v -> v
      Left _  -> A.Null
  _ -> A.Null
  where
    encodeUtf8Strict = BL.toStrict . TLE.encodeUtf8 . TL.fromStrict

fieldEquals :: T.Text -> A.Value -> A.Value -> Bool
fieldEquals k expected v = lookupField k v == Just expected

lookupField :: T.Text -> A.Value -> Maybe A.Value
lookupField k (A.Object o) = AKM.lookup (AKey.fromText k) o
lookupField _ _            = Nothing
