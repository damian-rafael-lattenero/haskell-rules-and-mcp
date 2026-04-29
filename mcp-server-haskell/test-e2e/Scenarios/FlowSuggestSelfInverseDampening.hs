-- | Flow: 'ghc_suggest' applies normaliser dampening to the
-- list-shaped 'Self-inverse on lists' rule (#73).
--
-- Pre-#73, 'ghc_suggest(function_name="normalize")' on
-- @[a] -> [a]@ returned the involutive twin at Low (good)
-- AND the list-roundtrip at Medium (bad — same losing law,
-- different surface). An agent following confidence rankings
-- would test the Medium one first and burn a round-trip on a
-- counterexample search the dampening was specifically
-- designed to prevent.
--
-- Post-#73 both surfaces drop to Low with a name-aware
-- rationale. We verify by:
--
--   1. defining a 'normalize :: Ord a => [a] -> [a]' in a
--      test module and asking ghc_suggest;
--   2. asserting the 'Self-inverse on lists' suggestion (if
--      any) carries confidence: low and a normaliser-aware
--      rationale;
--   3. for sanity: a 'reverse'-shaped function keeps the
--      Medium ranking (we do that one in unit tests; the e2e
--      focuses on the live-MCP call path).
module Scenarios.FlowSuggestSelfInverseDampening
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
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

moduleSrc :: Text
moduleSrc = T.unlines
  [ "module DampDemo (normalize) where"
  , ""
  , "-- | Canonicalising sort — idempotent, NOT self-inverse."
  , "normalize :: Ord a => [a] -> [a]"
  , "normalize = go"
  , "  where"
  , "    go []     = []"
  , "    go (y:ys) = insert y (go ys)"
  , "    insert z [] = [z]"
  , "    insert z (w:ws)"
  , "      | z <= w    = z : w : ws"
  , "      | otherwise = w : insert z ws"
  ]

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  -- Step 1 — scaffold + plant the normalize function.
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("damp-demo" :: Text) ])
  TIO.writeFile (projectDir </> "src" </> "DampDemo.hs") moduleSrc
  _ <- Client.callTool c GhcAddModules
         (object [ "modules" .= (["DampDemo"] :: [Text]) ])
  _ <- Client.callTool c GhcLoad
         (object [ "module_path" .= ("src/DampDemo.hs" :: Text) ])

  -- Step 2 — ask suggest. The 'Self-inverse on lists'
  -- suggestion (if surfaced) MUST be Low, not Medium.
  t0 <- stepHeader 1 "ghc_suggest dampens Self-inverse for normalize (#73)"
  rSug <- Client.callTool c GhcSuggest
            (object [ "function_name" .= ("normalize" :: Text) ])
  let allSugs = case lookupField "suggestions" rSug of
        Just (Array a) -> V.toList a
        _              -> []
      selfInv = [ s | s@(Object _) <- allSugs
                    , fieldOf "law" s == Just "Self-inverse on lists" ]
      ok = case selfInv of
        [s] -> fieldOf "confidence" s == Just "low"
            && maybe False (T.isInfixOf "normaliser") (fieldOf "rationale" s)
        []  -> True   -- legitimately filtered by min_confidence
        _   -> False  -- shouldn't have multiple
  cSug <- liveCheck $ checkPure
    "Self-inverse on lists, when present, is Low + normaliser-aware"
    ok
    ("Got: " <> truncRender rSug)
  stepFooter 1 t0

  pure [cSug]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

fieldOf :: Text -> Value -> Maybe Text
fieldOf k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = KeyMap.lookup (Key.fromText k) o
lookupField _ _          = Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
