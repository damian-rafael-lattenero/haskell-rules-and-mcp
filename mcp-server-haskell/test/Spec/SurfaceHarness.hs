-- | Wave-2b harness — Capa 2 (golden) + Capa 3 (sweep + raw-string
-- guard).
--
-- The golden test freezes the @tools/list@ wire surface: every name,
-- schema key and action enum lives in 'test/golden/tools-list.json'.
-- A surface change shows up as ONE reviewable diff instead of
-- scattered runtime failures. Regenerate with
-- @HASKELL_FLOWS_REGEN_GOLDEN=1 cabal test@.
--
-- The sweep replaces hand-maintained whitelists: for every
-- 'ToolName' and a matrix of canonical payloads, any emitted
-- 'NextStep' must only reference registered tools — checked on the
-- typed 'ToolName' field, not on strings.
--
-- The raw-string guard reads @src/@ and fails CI if a nextStep or
-- schema enum is emitted from a string literal instead of going
-- through the typed helpers ('NextStep''s 'ToJSON', 'Action''s
-- 'actionEnumValues').
module Spec.SurfaceHarness
  ( testToolsListGolden
  , testNextStepSweep
  , testNoRawNextStepStrings
  , testActionSpecsTotal
  ) where

import Control.Monad (forM, forM_, unless)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty')
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Foldable (toList)
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , listDirectory
  )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)

import HaskellFlows.Mcp.Action
import HaskellFlows.Mcp.NextStep (ChainStep (..), NextStep (..), suggestNext)
import HaskellFlows.Mcp.ToolName
import HaskellFlows.Tool.Registry (allToolDescriptors)

--------------------------------------------------------------------------------
-- Capa 2 — golden freeze of tools/list
--------------------------------------------------------------------------------

goldenPath :: FilePath
goldenPath = "test/golden/tools-list.json"

prettyCfg :: Config
prettyCfg = defConfig { confCompare = compare }   -- keys in ABC order

renderToolsList :: BL.ByteString
renderToolsList = encodePretty' prettyCfg allToolDescriptors

testToolsListGolden :: IO Bool
testToolsListGolden = do
  regen <- lookupEnv "HASKELL_FLOWS_REGEN_GOLDEN"
  case regen of
    Just _ -> do
      BLC.writeFile goldenPath (renderToolsList <> "\n")
      putStrLn ("  golden regenerated: " <> goldenPath)
      pure True
    Nothing -> do
      exists <- doesFileExist goldenPath
      if not exists
        then do
          putStrLn "  test/golden/tools-list.json missing —\
                   \ run with HASKELL_FLOWS_REGEN_GOLDEN=1 once"
          pure False
        else do
          current <- BL.readFile goldenPath
          if current == renderToolsList <> "\n" || current == renderToolsList
            then pure True
            else do
              putStrLn "  tools/list drifted from test/golden/tools-list.json —"
              putStrLn "  intentional? regenerate with\
                       \ HASKELL_FLOWS_REGEN_GOLDEN=1 and review the diff."
              pure False

--------------------------------------------------------------------------------
-- Capa 3 — nextStep sweep (no whitelists)
--------------------------------------------------------------------------------

-- | Canonical payloads per tool: the generic success shape plus the
-- action-discriminated branches of the composites. Every tool gets
-- the full matrix — 'suggestNext' ignores irrelevant fields.
canonicalPayloads :: [Value]
canonicalPayloads =
  [ object []
  , object [ "success" .= True, "errors" .= ([] :: [Text])
           , "warnings" .= ([] :: [Text]) ]
  , object [ "action" .= ("load" :: Text) ]
  , object [ "action" .= ("module" :: Text), "module_path" .= ("src/M.hs" :: Text) ]
  , object [ "action" .= ("project" :: Text) ]
  , object [ "action" .= ("lint" :: Text), "path" .= ("src" :: Text) ]
  , object [ "action" .= ("check" :: Text), "state" .= ("passed" :: Text) ]
  , object [ "action" .= ("check" :: Text), "state" .= ("failed" :: Text)
           , "counterexample" .= ("[]" :: Text) ]
  , object [ "action" .= ("run" :: Text), "state" .= ("passed" :: Text) ]
  , object [ "action" .= ("arbitrary" :: Text) ]
  , object [ "action" .= ("status" :: Text) ]
  , object [ "action" .= ("toolchain" :: Text) ]
  , object [ "action" .= ("imports" :: Text), "imports" .= ([] :: [Text]) ]
  , object [ "action" .= ("type" :: Text), "type" .= ("Int -> Int" :: Text) ]
  , object [ "action" .= ("info" :: Text), "defined_in" .= ("base" :: Text) ]
  , object [ "action" .= ("browse" :: Text), "modules" .= ([] :: [Text]) ]
  , object [ "action" .= ("fix_warning" :: Text), "applied" .= True ]
  , object [ "action" .= ("add" :: Text) ]
  , object [ "action" .= ("write" :: Text), "id" .= ("s1" :: Text) ]
  ]

referencedTools :: NextStep -> [ToolName]
referencedTools ns =
  nsTool ns : map csTool (maybe [] toList (nsChain ns))

testNextStepSweep :: IO Bool
testNextStepSweep = do
  let registered = Set.fromList allToolNames
      cases = [ (n, p) | n <- allToolNames, p <- canonicalPayloads ]
      violations =
        [ (n, p, t)
        | (n, p) <- cases
        , Just ns <- [suggestNext n True p]
        , t <- referencedTools ns
        , t `Set.notMember` registered ]
  unless (null violations) $
    forM_ (take 5 violations) $ \(n, p, t) ->
      putStrLn ("  nextStep from " <> show (toolNameText n) <> " with "
                <> take 60 (show p) <> " references unregistered "
                <> show (toolNameText t))
  pure (null violations)

--------------------------------------------------------------------------------
-- Capa 3b — raw-string guard over src/
--------------------------------------------------------------------------------

-- | Every @.hs@ under @src/@, recursively.
haskellSources :: FilePath -> IO [FilePath]
haskellSources dir = do
  entries <- listDirectory dir
  fmap concat $ forM entries $ \e -> do
    let p = dir </> e
    isDir <- doesDirectoryExist p
    if isDir
      then haskellSources p
      else pure [ p | takeExtension p == ".hs" ]

-- | Fails when a @\"tool\" .=@ or @\"enum\" .=@ emitter builds the
-- value from a string / string-list literal instead of going through
-- the typed helpers. The 'ToJSON' instances render via
-- 'toolNameText' and the schemas via 'actionEnumValues', so a clean
-- tree passes.
testNoRawNextStepStrings :: IO Bool
testNoRawNextStepStrings = do
  cwd <- getCurrentDirectory
  files <- haskellSources (cwd </> "src")
  offenders <- fmap concat $ forM files $ \f -> do
    content <- readFile f
    let ls = zip [1 :: Int ..] (lines content)
    pure [ (f, n, unwords (words l))
         | (n, l) <- ls, isRawEmitter l ]
  unless (null offenders) $ do
    putStrLn "  raw string emitters (use NextStep ADT / actionEnumValues):"
    forM_ (take 8 offenders) $ \(f, n, l) ->
      putStrLn ("  " <> f <> ":" <> show n <> ": " <> take 70 l)
  pure (null offenders)
  where
    isRawEmitter l =
      let t = dropWhile (== ' ') l
          hasTool = "\"tool\"" `isPrefixOf` t
          hasEnum = "\"enum\"" `isPrefixOf` t
          rawVal = any (`isInfixOf` l) [ ".= (\"", ".= ([\"", ".= [ \"" ]
      in (hasTool || hasEnum) && rawVal

--------------------------------------------------------------------------------

-- | Every 'ActionSpec' is total: each inhabitant parses back from
-- its own rendered wire text.
testActionSpecsTotal :: IO Bool
testActionSpecsTotal = do
  let roundtrip :: (Eq a) => String -> ActionSpec a -> [String]
      roundtrip label spec =
        [ label <> " does not round-trip " <> T.unpack t
        | a <- asEnum spec
        , let t = asText spec a
        , parseActionText spec t /= Just a ]
      broken = concat
        [ roundtrip "check" checkSpec
        , roundtrip "property" propertySpec
        , roundtrip "session" sessionSpec
        , roundtrip "edit" editSpec
        , roundtrip "module" moduleSpec
        , roundtrip "inspect" inspectSpec ]
  unless (null broken) $
    forM_ broken (putStrLn . ("  " <>))
  pure (null broken)
