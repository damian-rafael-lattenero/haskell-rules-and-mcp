-- | Flow: @ghc_property_lifecycle@ — inspect the property store.
--
-- After a successful @ghc_quickcheck@ auto-persists a property,
-- @ghc_property_lifecycle@ exposes the store's contents. Used
-- by agents to prune flaky / obsolete entries before a push.
module Scenarios.FlowPropertyLifecycle
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
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

-- | Tiny module so @ghc_quickcheck@ has something to test.
calcSrc :: Text
calcSrc = T.unlines
  [ "module Calc where"
  , ""
  , "double :: Int -> Int"
  , "double x = x * 2"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup — scaffold + add QuickCheck + load a tiny module
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + add QuickCheck + load Calc"
  _ <- Client.callTool c "ghc_create_project"
         (object [ "name" .= ("proplife-demo" :: Text) ])
  _ <- Client.callTool c "ghc_add_modules"
         (object [ "modules" .= (["Calc"] :: [Text]) ])
  _ <- Client.callTool c "ghc_deps" (object
         [ "action"  .= ("add" :: Text)
         , "package" .= ("QuickCheck" :: Text)
         , "stanza"  .= ("test-suite" :: Text)
         , "version" .= (">= 2.14" :: Text)
         ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  _ <- Client.callTool c "ghc_load"
         (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- seed: quickcheck a simple property so it persists.
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "quickcheck (auto-persist on pass)"
  _ <- Client.callTool c "ghc_quickcheck" (object
    [ "property" .= ("\\(x :: Int) -> double x == x + x" :: Text)
    , "module"   .= ("src/Calc.hs" :: Text)
    ])
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- ghc_property_lifecycle — inspect the store.
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_property_lifecycle (inspect store)"
  r <- Client.callTool c "ghc_property_lifecycle" (object [])
  c1 <- liveCheck $ checkJsonField "success" r "success" (Bool True)
  c2 <- liveCheck $ checkJsonFieldMatches
          "store has ≥ 1 property"
          r "properties" (arrayOfLenAtLeast 1)
          "expected at least one persisted property"
  c3 <- liveCheck $ checkJsonFieldMatches
          "each entry carries 'expression' + 'passed' fields"
          r "properties" entriesAreWellFormed
          "every property entry should have 'expression' and 'passed' \
          \keys — the minimum the regression runner needs"
  stepFooter 3 t2

  pure [c1, c2, c3]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

arrayOfLenAtLeast :: Int -> Value -> Bool
arrayOfLenAtLeast n (Array a) = V.length a >= n
arrayOfLenAtLeast _ _         = False

entriesAreWellFormed :: Value -> Bool
entriesAreWellFormed (Array a) =
  not (V.null a) && all oneEntry (V.toList a)
  where
    oneEntry (Object o) =
         hasKey "expression" o
      && hasKey "passed"     o
    oneEntry _ = False
    hasKey k = KeyMap.member (Key.fromText k)
entriesAreWellFormed _ = False
