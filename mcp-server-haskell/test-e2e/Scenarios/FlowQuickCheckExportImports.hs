-- | Flow: @ghc_quickcheck_export@ produces a self-contained,
-- compilable @test/Spec.hs@ even when persisted properties live
-- under the test scope (#40).
--
-- Pre-fix behaviour
-- -----------------
-- Properties persisted with @module="test/Spec.hs"@ (because the
-- author ran @ghc_quickcheck@ inside the test stanza's scope) made
-- the exporter emit a self-referential @import Spec@ in the
-- generated @module Main where@ file. Worse, the lambdas
-- referenced symbols from project library modules (@simplify@,
-- @eval@, …) that were never imported, so the file failed to
-- compile out-of-the-box. The contract advertised by the tool
-- (\"@cabal test@ replays your property set\") was effectively
-- broken — every export needed manual editing.
--
-- New contract
-- ------------
-- The exporter consults the project's library @exposed-modules@
-- and emits @import \<Module\>@ for every entry, so library
-- symbols are in scope. It also computes the would-be module
-- name of the output path and filters that out of the import
-- list, so the file never imports itself.
module Scenarios.FlowQuickCheckExportImports
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold a tiny library project. We don't need it to
  -- build the test-suite end-to-end here (cabal test has its own
  -- coverage in 'ExprEvaluator'); we only need a .cabal whose
  -- library stanza exposes a module the renderer can pick up.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("qcexp-imports" :: Text) ])

  -- Step 2 — register a library module so 'libraryExposedModules'
  -- has something non-empty to return. The renderer should union
  -- this into the import set.
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= ["Lib.Foo" :: Text] ])

  -- Step 3 — write a property to the on-disk store with
  -- 'module="test/Spec.hs"'. Pre-fix this triggered the broken
  -- 'import Spec' header.
  let storeDir = projectDir </> ".haskell-flows"
  createDirectoryIfMissing True storeDir
  TIO.writeFile (storeDir </> "properties.json")
    "[{\"expression\":\"\\\\x -> x == (x :: Int)\",\
    \\"module\":\"test/Spec.hs\",\"passed\":1,\"updated\":0}]"

  -- Step 4 — invoke the export. Defaults the output to
  -- 'test/Spec.hs', mirroring the bug's exact reproduction.
  t0 <- stepHeader 1 "ghc_quickcheck_export defaults to test/Spec.hs (#40)"
  r <- Client.callTool c GhcQuickCheckExport (object [])
  let exportSucceeded = fieldBool "success" r == Just True
  cExport <- liveCheck $ checkPure
    "export tool returns success=true"
    exportSucceeded
    ("expected success=true; raw: " <> truncRender r)
  stepFooter 1 t0

  -- Step 5 — read the generated file off disk and assert the
  -- post-#40 invariants directly.
  t1 <- stepHeader 2 "generated test/Spec.hs has no self-import (#40)"
  let specPath = projectDir </> "test" </> "Spec.hs"
  specExists <- doesFileExist specPath
  body <- if specExists then TIO.readFile specPath else pure ""
  let noSelf       = not ("import Spec" `T.isInfixOf` body)
      hasMainHdr   = "module Main where" `T.isInfixOf` body
      hasLibImport = "import Lib.Foo"    `T.isInfixOf` body
  cFile <- liveCheck $ checkPure
    "test/Spec.hs exists and has module Main with no 'import Spec'"
    (specExists && noSelf && hasMainHdr)
    ( "Expected: file exists, no 'import Spec', has 'module Main where'. \
      \Got: exists=" <> T.pack (show specExists)
      <> ", noSelf=" <> T.pack (show noSelf)
      <> ", hasMainHdr=" <> T.pack (show hasMainHdr)
      <> ". Body head: " <> T.take 400 body )
  stepFooter 2 t1

  -- Step 6 — the library's exposed-modules must be unioned into
  -- the import set so the property's library-symbol references
  -- resolve at compile time.
  t2 <- stepHeader 3 "generated file imports library exposed-modules (#40)"
  cLibImport <- liveCheck $ checkPure
    "generated test/Spec.hs has 'import Lib.Foo'"
    hasLibImport
    ( "Expected 'import Lib.Foo' (the library's only exposed-module). \
      \Body head: " <> T.take 400 body )
  stepFooter 3 t2

  pure [cExport, cFile, cLibImport]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldBool :: Text -> Value -> Maybe Bool
fieldBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
