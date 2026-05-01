-- | Flow: @ghc_switch_project@ reopens the property store
-- against the new project root (#39).
--
-- Pre-fix behaviour
-- -----------------
-- 'srvStore' was bound to the project that was active at server
-- boot. After @ghc_switch_project@ swapped 'srvProjectDir' the
-- store kept pointing at the previous project's
-- @.haskell-flows/properties.json@ — so @ghc_regression(list)@,
-- @ghc_check_module@, and @ghc_check_project@ on the NEW
-- project all leaked the OLD project's properties. Dogfood
-- session 2026-04-24 surfaced this as ghost regressions on
-- @src/Expr/Simplify.hs@ in a freshly scaffolded playground.
--
-- New contract
-- ------------
-- Switching projects atomically reopens the store at the new
-- root. Properties belong to their project, not to the server
-- process.
module Scenarios.FlowSwitchProjectStore
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
import E2E.Envelope (statusOk, lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

minimalCabal :: Text -> Text
minimalCabal pkg = T.unlines
  [ "cabal-version: 2.4"
  , "name: " <> pkg
  , "version: 0.1.0.0"
  , ""
  , "library"
  , "  hs-source-dirs:   src"
  , "  exposed-modules:"
  , "  build-depends:    base"
  , "  default-language: Haskell2010"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — Project A is the scenario's own dir. Scaffold it
  -- (so 'ghc_switch_project' can land back on it later) and plant
  -- a single property in its store. We bypass ghc_quickcheck
  -- (no source code to compile) and write the JSON directly
  -- with the same shape PropertyStore.save would produce.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("store-iso-a" :: Text) ])
  let storeA = projectDir </> ".haskell-flows" </> "properties.json"
  createDirectoryIfMissing True (projectDir </> ".haskell-flows")
  TIO.writeFile storeA
    "[{\"expression\":\"\\\\x -> x == (x :: Int)\",\
    \\"module\":\"src/Foo.hs\",\"passed\":1,\"updated\":0}]"

  -- Step 2 — confirm the planted property is visible via
  -- ghc_regression(list) BEFORE the switch.
  t0 <- stepHeader 1 "baseline · list sees A's property"
  baseline <- Client.callTool c GhcPropertyStore
                (object [ "action" .= ("list" :: Text) ])
  let baseN  = countN "count" baseline
  cBaseline <- liveCheck $ checkPure
    "ghc_regression(list) reports count=1 in project A"
    (baseN == 1)
    ("Expected count=1 (planted property in A); got count="
       <> T.pack (show baseN))
  stepFooter 1 t0

  -- Step 3 — sibling project B with NO property store.
  let projB = projectDir </> "store-iso-b"
  createDirectoryIfMissing True projB
  TIO.writeFile (projB </> "store-iso-b.cabal") (minimalCabal "store-iso-b")
  TIO.writeFile (projB </> "cabal.project") "packages: .\n"

  t1 <- stepHeader 2 "switch · A → B reopens store at B"
  switchAB <- Client.callTool c GhcProject
                (object [ "action" .= ("switch" :: Text), "path" .= T.pack projB ])
  cSwitch <- liveCheck $ checkPure
    "switch A→B succeeds"
    (statusOk switchAB == Just True)
    ("switch must succeed; got: " <> truncRender switchAB)

  -- Step 4 — pre-#39 this returned count=1 (A's property
  -- leaking through). Post-#39 the store was reopened against
  -- B, which has no .haskell-flows/ → count=0.
  inB <- Client.callTool c GhcPropertyStore
           (object [ "action" .= ("list" :: Text) ])
  let bN = countN "count" inB
  cIsolated <- liveCheck $ checkPure
    "after switch A→B, list sees 0 properties (no leak from A) (#39)"
    (bN == 0)
    ("Expected count=0 in B (clean store); got count=" <> T.pack (show bN)
       <> " — A's property is leaking through. Raw: " <> truncRender inB)
  stepFooter 2 t1

  -- Step 5 — plant a different property in B's store, then
  -- switch back to A. Each project must see its OWN entries
  -- and only its own.
  t2 <- stepHeader 3 "isolation · plant in B, switch back, verify both"
  let storeB = projB </> ".haskell-flows" </> "properties.json"
  createDirectoryIfMissing True (projB </> ".haskell-flows")
  TIO.writeFile storeB
    "[{\"expression\":\"\\\\y -> y + 0 == (y :: Int)\",\
    \\"module\":\"src/Bar.hs\",\"passed\":1,\"updated\":0}]"

  -- B sees its own property.
  bAgain <- Client.callTool c GhcPropertyStore
              (object [ "action" .= ("list" :: Text) ])
  let bAgainN = countN "count" bAgain
  cBSeesItsOwn <- liveCheck $ checkPure
    "after planting B's property, list reports count=1 in B"
    (bAgainN == 1)
    ("Expected count=1 in B post-plant; got count="
       <> T.pack (show bAgainN))

  -- Switch back to A — must see A's original property, NOT B's.
  switchBA <- Client.callTool c GhcProject
                (object [ "action" .= ("switch" :: Text), "path" .= T.pack projectDir ])
  let backOk = statusOk switchBA == Just True
  cSwitchBack <- liveCheck $ checkPure
    "switch B→A succeeds"
    backOk
    ("switch back must succeed; got: " <> truncRender switchBA)

  inA <- Client.callTool c GhcPropertyStore
           (object [ "action" .= ("list" :: Text) ])
  let aN = countN "count" inA
  cAUnchanged <- liveCheck $ checkPure
    "after switch B→A, list sees A's 1 property (not B's) (#39)"
    (aN == 1)
    ("Expected count=1 in A (original property); got count="
       <> T.pack (show aN) <> ". Raw: " <> truncRender inA)
  stepFooter 3 t2

  pure
    [ cBaseline
    , cSwitch, cIsolated
    , cBSeesItsOwn, cSwitchBack, cAUnchanged
    ]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

countN :: Text -> Value -> Int
countN k v = case lookupField k v of
  Just (Number n) -> truncate (toRational n)
  _               -> -1

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw

-- Suppress unused-import warning if V/Number aren't used downstream:
_useImports :: ()
_useImports = ()
  where _ = V.empty :: V.Vector ()
