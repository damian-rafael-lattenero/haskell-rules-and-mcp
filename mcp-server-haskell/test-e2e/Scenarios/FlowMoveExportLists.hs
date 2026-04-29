-- | Flow: 'ghc_move' end-to-end with explicit export lists on
-- both modules + Haddock boundary detection (#76).
--
-- Pre-#76 the move had two compounding bugs:
--
--   1. The slicer arrived past the body of the moved binding
--      and absorbed the Haddock '-- |' line that documented
--      the NEXT binding. The destination ended up with an
--      orphan Haddock comment, the source binding lost its
--      docstring.
--
--   2. The destination module's explicit export list was never
--      updated, so the moved symbol landed in the file but
--      stayed PRIVATE — every consumer that imported it would
--      fail.
--
-- This scenario sets up both conditions in a single project:
--
--   * Source has '-- |' before each binding so the slicer must
--     stop precisely at the boundary line of the next binding.
--   * Destination declares 'module Dest (placeholder) where' —
--     the explicit list shape that exposed bug 2.
--   * A consumer module imports the moved symbol from Dest,
--     verifying the post-move project loads green (which is
--     impossible if the symbol is private).
module Scenarios.FlowMoveExportLists
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
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
import E2E.Envelope (statusOk, fieldBool)
import HaskellFlows.Mcp.ToolName (ToolName (..))

sourceSrc :: Text
sourceSrc = T.unlines
  [ "module Src (alpha, beta) where"
  , ""
  , "-- | Alpha = 1."
  , "alpha :: Int"
  , "alpha = 1"
  , ""
  , "-- | Beta = 2."        -- ← critical: pre-#76 leaked into alpha's slice
  , "beta :: Int"
  , "beta = 2"
  ]

destSrc :: Text
destSrc = T.unlines
  [ "module Dst (placeholder) where"   -- ← explicit list, must gain 'alpha'
  , ""
  , "placeholder :: Int"
  , "placeholder = 0"
  ]

consumerSrc :: Text
consumerSrc = T.unlines
  [ "module Cnsmr where"
  , ""
  , "import Dst (alpha)            -- expects alpha exported by Dst"
  , ""
  , "useAlpha :: Int"
  , "useAlpha = alpha + 10"
  ]

cabalSrc :: Text
cabalSrc = T.unlines
  [ "cabal-version:    2.4"
  , "name:             move-exports-demo"
  , "version:          0.1.0.0"
  , "build-type:       Simple"
  , ""
  , "library"
  , "    exposed-modules:  Src, Dst, Cnsmr"
  , "    hs-source-dirs:   src"
  , "    build-depends:    base >= 4.14 && < 5"
  , "    default-language: Haskell2010"
  , ""
  , "test-suite t"
  , "    type:             exitcode-stdio-1.0"
  , "    main-is:          Spec.hs"
  , "    hs-source-dirs:   test"
  , "    build-depends:    base, move-exports-demo"
  , "    default-language: Haskell2010"
  ]

specSrc :: Text
specSrc = T.unlines
  [ "module Main where"
  , ""
  , "main :: IO ()"
  , "main = pure ()"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — plant a 4-file project on disk. We bypass
  -- 'create_project' because it scaffolds a single-module
  -- shape that won't exercise both bugs at once.
  let writeAt rel body = do
        let full = projectDir </> rel
        createDirectoryIfMissing True (dirOf full)
        TIO.writeFile full body
  writeAt "move-exports-demo.cabal" cabalSrc
  writeAt "src/Src.hs"               sourceSrc
  writeAt "src/Dst.hs"               destSrc
  writeAt "src/Cnsmr.hs"             consumerSrc
  writeAt "test/Spec.hs"             specSrc
  TIO.writeFile (projectDir </> "cabal.project") "packages: .\n"

  -- Step 2 — move 'alpha' from Src to Dst. With #76, this must:
  --   (a) slice ONLY alpha's signature + body + Haddock — never
  --       leak '-- | Beta = 2.' into the cut;
  --   (b) add 'alpha' to Dst's explicit export list;
  --   (c) drop 'alpha' from Src's export list;
  --   (d) leave the project loading green (Cnsmr's import works
  --       because alpha is now exported from Dst).
  t0 <- stepHeader 1 "ghc_move (alpha → Dst) with explicit export lists (#76)"
  rMove <- Client.callTool c GhcMove
             (object
                [ "symbol" .= ("alpha" :: Text)
                , "from"   .= ("Src"   :: Text)
                , "to"     .= ("Dst"   :: Text)
                ])
  let okMove = fieldBool "applied" rMove == Just True
            && statusOk rMove == Just True
  cMove <- liveCheck $ checkPure
    "move applies cleanly (no rollback)"
    okMove
    ("Got: " <> truncRender rMove)
  stepFooter 1 t0

  -- Step 3 — assert the destination header gained 'alpha'.
  t1 <- stepHeader 2 "destination Dst.hs export list contains 'alpha' (#76)"
  destAfter <- TIO.readFile (projectDir </> "src" </> "Dst.hs")
  let destHasAlpha = T.isInfixOf "module Dst (placeholder, alpha)" destAfter
                  || T.isInfixOf "module Dst (alpha, placeholder)" destAfter
  cDestExp <- liveCheck $ checkPure
    "Dst's export list now lists 'alpha'"
    destHasAlpha
    ("First 3 lines of Dst.hs: " <> firstLines 3 destAfter)
  stepFooter 2 t1

  -- Step 4 — assert the source kept Beta's Haddock intact.
  -- Pre-#76 the slicer ate 'beta's docstring; post-fix it stays.
  t2 <- stepHeader 3 "source Src.hs preserves beta's Haddock (#76)"
  srcAfter <- TIO.readFile (projectDir </> "src" </> "Src.hs")
  let stillHasBetaDoc = T.isInfixOf "-- | Beta = 2." srcAfter
                     && T.isInfixOf "beta :: Int"    srcAfter
      -- And the dest must NOT have the orphan Haddock.
      destNoOrphan   = not (T.isInfixOf "-- | Beta = 2." destAfter)
  cSlice <- liveCheck $ checkPure
    "Src kept beta's Haddock; Dst has no orphan '-- | Beta = 2.'"
    (stillHasBetaDoc && destNoOrphan)
    ("Src head: " <> firstLines 5 srcAfter
     <> " | Dst head: " <> firstLines 5 destAfter)
  stepFooter 3 t2

  pure [cMove, cDestExp, cSlice]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

dirOf :: FilePath -> FilePath
dirOf = reverse . dropWhile (/= '/') . reverse

firstLines :: Int -> Text -> Text
firstLines n = T.intercalate " ⏎ " . take n . T.lines

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
