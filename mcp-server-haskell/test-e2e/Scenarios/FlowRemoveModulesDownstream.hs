-- | Flow: 'ghc_remove_modules' refuses (without force) when other
-- .hs files still import the modules being removed (#41).
--
-- Pre-fix behaviour
-- -----------------
-- The tool succeeded silently with 'success=true' even when
-- @test/Spec.hs@ still had @import Expr (greet)@. The next
-- @ghc_quickcheck@ in the test-suite failed with a confusing
-- \"Variable not in scope\" — the agent had no signal pointing
-- back to the removal.
--
-- New contract
-- ------------
--   * Default (force=false): if any remaining .hs file imports a
--     to-be-removed module, refuse with 'success=false' and a
--     'downstream_imports' array of @{file, line, module}@ tuples.
--     The .cabal stays untouched.
--   * Force (force=true): proceed but include
--     'warnings.downstream_imports' so the agent knows what to
--     repair next.
--   * No importers: the tool behaves exactly as before — silent
--     success, no warnings.
module Scenarios.FlowRemoveModulesDownstream
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (statusOk, errorKind, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + register Expr + plant a downstream
  -- importer in test/Spec.hs.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("rm-down-demo" :: Text) ])
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["Expr"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Expr.hs") $ T.unlines
    [ "module Expr where"
    , ""
    , "greet :: String -> String"
    , "greet name = \"hi \" <> name"
    ]
  createDirectoryIfMissing True (projectDir </> "test")
  TIO.writeFile (projectDir </> "test" </> "Spec.hs") $ T.unlines
    [ "module Main where"
    , ""
    , "import Expr (greet)"
    , ""
    , "main :: IO ()"
    , "main = putStrLn (greet \"world\")"
    ]

  -- Step 2 — default remove (no force) must REFUSE with a
  -- downstream_imports array. .cabal stays untouched.
  t0 <- stepHeader 1 "ghc_remove_modules refuses on importers (#41)"
  cabalBefore <- TIO.readFile =<< findCabal projectDir
  rRefused <- Client.callTool c GhcRemoveModules
                (object [ "modules" .= (["Expr"] :: [Text]) ])
  cabalAfter <- TIO.readFile =<< findCabal projectDir
  let success    = statusOk rRefused
      -- Issue #90: post-envelope, error.kind is the closed enum
      -- (Validation here). The legacy 'downstream_imports_present'
      -- string is preserved on the wire as error.cause for
      -- consumers that need the specific reason. Check both.
      kindIsValidation = errorKind rRefused == Just "validation"
      causeIsDown      = case lookupField "error" rRefused of
        Just (Object o) -> case KeyMap.lookup (Key.fromText "cause") o of
          Just (String s) -> s == "downstream_imports_present"
          _               -> False
        _ -> False
      hasDownArr = arrayLen "downstream_imports" rRefused >= 1
      cabalUntouched = cabalBefore == cabalAfter
  cRefuse <- liveCheck $ checkPure
    "default remove refused, .cabal untouched, downstream_imports present"
    (success == Just False
       && kindIsValidation
       && causeIsDown
       && hasDownArr
       && cabalUntouched)
    ( "Expected: success=false, error.kind=validation, \
      \error.cause=downstream_imports_present, array≥1, .cabal unchanged. \
      \Got: success=" <> T.pack (show success)
      <> ", kind=" <> T.pack (show (errorKind rRefused))
      <> ", n=" <> T.pack (show (arrayLen "downstream_imports" rRefused))
      <> ", cabalUntouched=" <> T.pack (show cabalUntouched) )
  stepFooter 1 t0

  -- Step 3 — force=true proceeds AND surfaces warnings.
  t1 <- stepHeader 2 "ghc_remove_modules force=true → warning (#41)"
  rForced <- Client.callTool c GhcRemoveModules
               (object
                 [ "modules" .= (["Expr"] :: [Text])
                 , "force"   .= True
                 ])
  let forcedOk    = statusOk rForced == Just True
      hasWarnings = arrayPathLen ["warnings", "downstream_imports"] rForced >= 1
  cForce <- liveCheck $ checkPure
    "force=true succeeds with warnings.downstream_imports populated"
    (forcedOk && hasWarnings)
    ( "Expected success=true with warnings.downstream_imports array. \
      \Got: success=" <> T.pack (show forcedOk)
      <> ", warnsLen=" <> T.pack (show (arrayPathLen ["warnings","downstream_imports"] rForced)) )
  stepFooter 2 t1

  pure [cRefuse, cForce]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

findCabal :: FilePath -> IO FilePath
findCabal root = pure (root </> "rm-down-demo.cabal")

arrayLen :: Text -> Value -> Int
arrayLen k v = case lookupField k v of
  Just (Array xs) -> V.length xs
  _               -> -1

arrayPathLen :: [Text] -> Value -> Int
arrayPathLen ks v = case foldl step (Just v) ks of
  Just (Array xs) -> V.length xs
  _               -> -1
  where
    step Nothing  _  = Nothing
    step (Just o) k  = lookupField k o

