-- | Flow: B-6 — a property over a type WITHOUT an Arbitrary instance must
-- be diagnosed honestly.
--
-- THE BUG (dogfooding lambda-hm-v3): @ghc_quickcheck@ over a type lacking
-- @Arbitrary@ returned @error_kind=compile_error@ with the message
-- "Project has compile errors — fix them with ghc_check_project", even
-- though the project compiles perfectly. The real failure is a missing
-- @Arbitrary@ instance, and the right next move is @ghc_arbitrary@, not
-- @ghc_check_project@.
--
-- This scenario plants a 'data Foo' with NO Arbitrary instance, runs a
-- property over it, and asserts the structured diagnosis:
--   * status = failed
--   * error_kind = missing_instance   (not compile_error)
--   * the message / nextStep steers to ghc_arbitrary
module Scenarios.FlowMissingArbitrary
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
import E2E.Envelope (errorKind, statusIs)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | A module with a data type that has NO Arbitrary instance.
calcSrc :: Text
calcSrc = T.unlines
  [ "module Calc where"
  , ""
  , "data Foo = Foo Int"
  , "  deriving (Eq, Show)"
  , ""
  , "idFoo :: Foo -> Foo"
  , "idFoo x = x"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- setup — scaffold + QuickCheck dep + load the no-Arbitrary module.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("missing-arb-demo" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Calc"] :: [Text]) ])
  _ <- Client.callTool c GhcDeps (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  _ <- Client.callTool c GhcLoad (object [ "module_path" .= ("src/Calc.hs" :: Text) ])

  -- The property quantifies over Foo, which has no Arbitrary instance.
  t0 <- stepHeader 1 "quickcheck over a no-Arbitrary type → honest diagnosis"
  qcR <- Client.callTool c GhcQuickCheck (object
    [ "property" .= ("\\(x :: Foo) -> idFoo x == x" :: Text)
    , "module"   .= ("src/Calc.hs" :: Text)
    ])
  let isFailed   = statusIs "failed" qcR
      kindOk     = errorKind qcR == Just "missing_instance"
      -- the message OR nextStep must mention ghc_arbitrary (not check_project)
      mentionsArb = "ghc_arbitrary" `T.isInfixOf` T.pack (show qcR)
      notMisleading = not ("ghc_check_project" `T.isInfixOf` T.pack (show qcR))
  cKind <- liveCheck $ checkPure
    "error_kind=missing_instance (not compile_error)"
    (isFailed && kindOk)
    ("status/ kind wrong — Got: " <> truncRender qcR)
  cSteer <- liveCheck $ checkPure
    "steers to ghc_arbitrary, not the misleading ghc_check_project"
    (mentionsArb && notMisleading)
    ("expected ghc_arbitrary mention and no ghc_check_project — Got: " <> truncRender qcR)
  stepFooter 1 t0

  pure [cKind, cSteer]

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 700
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
