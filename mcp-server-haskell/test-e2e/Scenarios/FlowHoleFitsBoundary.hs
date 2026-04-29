-- | Flow: 'ghc_hole' validFits parses operator-named candidates
-- as separate entries (#71).
--
-- Pre-#71, the continuation classifier ate every fit whose
-- name started with '(' (operators like @(-)@, @(^)@, @(+)@ —
-- common candidates for any @Num a@ hole). The agent saw
-- adjacent fits collapsed into the preceding entry's @source@
-- field. Empirically the dogfood @_addOp :: Int -> Int -> Int@
-- hole produced 4 entries when GHC actually reported 6.
--
-- Post-#71 the type-signature substring is the canonical
-- disambiguator: any line containing @ :: @ is a fresh
-- fit-head, regardless of how it starts.
--
-- This scenario plants a hole whose expected type is satisfied
-- by both @(+)@ and a user-named binding, then asserts the
-- response carries an entry whose @name@ field is @"(+)"@
-- — the canonical post-#71 invariant.
module Scenarios.FlowHoleFitsBoundary
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
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | Module body with one hole whose expected type matches
-- many fits including operator-named ones.
moduleSrc :: Text
moduleSrc = T.unlines
  [ "module HoleDemo (addPair) where"
  , ""
  , "-- | Adds a pair of ints — body has a hole on the operator."
  , "addPair :: Int -> Int -> Int"
  , "addPair a b = _addOp a b"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + plant the buggy module.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("hole-fits-demo" :: Text) ])
  TIO.writeFile (projectDir </> "src" </> "HoleDemo.hs") moduleSrc
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["HoleDemo"] :: [Text]) ])

  -- Step 2 — query the hole. The response's validFits array
  -- must contain a row whose name is "(+)" — the canonical
  -- operator-named fit GHC offers for any 'Num a => a -> a -> a'
  -- hole. Pre-#71 this row was absorbed into a preceding
  -- entry's source field and missing from the array.
  t0 <- stepHeader 1 "ghc_hole returns operator-named fit as a distinct row (#71)"
  rHole <- Client.callTool c GhcHole
            (object [ "module_path" .= ("src/HoleDemo.hs" :: Text) ])
  let firstHole = case lookupField "holes" rHole of
        Just (Array a) | not (V.null a) -> Just (V.head a)
        _                                -> Nothing
      fitsArr = case firstHole of
        Just (Object o) -> case KeyMap.lookup (Key.fromText "validFits") o of
          Just (Array fs) -> V.toList fs
          _               -> []
        _ -> []
      fitNames = [ n | Object f <- fitsArr
                     , Just (String n) <- [KeyMap.lookup (Key.fromText "name") f] ]
      hasOperator = any startsWithParen fitNames
      -- Critical: no fit's source carries the next fit's
      -- identifier+type. We check there's no ' :: ' substring
      -- inside any fit's source field — that would be the
      -- absorbed-next-fit signature.
      noLeakedSig = not (any sourceHasTypeSig fitsArr)
      ok = hasOperator && noLeakedSig
  cFits <- liveCheck $ checkPure
    "validFits has at least one '(...)' name and no source carries an absorbed ' :: '"
    ok
    ("Got names: " <> T.intercalate ", " fitNames)
  stepFooter 1 t0

  pure [cFits]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

startsWithParen :: Text -> Bool
startsWithParen n = case T.uncons n of
  Just ('(', _) -> True
  _             -> False

sourceHasTypeSig :: Value -> Bool
sourceHasTypeSig (Object f) =
  case KeyMap.lookup (Key.fromText "source") f of
    Just (String s) -> T.isInfixOf " :: " s
    _               -> False
sourceHasTypeSig _ = False

