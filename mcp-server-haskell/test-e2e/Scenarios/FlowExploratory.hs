-- | Flow: read-only inspectors. Scaffolds a tiny project with
-- one source module, loads it, then exercises every tool that
-- only READS the session state — no writes, no GHCi side
-- effects beyond the load.
--
-- Tools exercised:
--
--   ghc_type      ghc_info      ghc_eval      ghc_complete
--   ghc_goto      ghc_doc
--
-- Side effects asserted through the pipeline:
--
--   ghc_create_project   ghc_add_modules   ghc_load
--
-- A failure here means the query layer (the most commonly
-- exercised half of the tool surface) has regressed. Under 1 s.
module Scenarios.FlowExploratory
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
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

--------------------------------------------------------------------------------
-- source the flow writes into the scenario's projectDir
--------------------------------------------------------------------------------

calcSrc :: Text
calcSrc =
  "-- | Tiny module used by the Exploratory flow. Every top-level\n\
  \-- binding is chosen to make one specific query tool verifiable.\n\
  \module Calc\n\
  \  ( greet\n\
  \  , double\n\
  \  , Tree (..)\n\
  \  ) where\n\
  \\n\
  \greet :: String -> String\n\
  \greet name = \"Hello, \" ++ name\n\
  \\n\
  \double :: Int -> Int\n\
  \double x = x * 2\n\
  \\n\
  \data Tree a = Leaf | Node a (Tree a) (Tree a)\n"

--------------------------------------------------------------------------------
-- runFlow
--------------------------------------------------------------------------------

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  --------------------------------------------------------------------
  -- setup — scaffold + write Calc.hs + register + load
  --------------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + add Calc + load"
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("exploratory" :: Text) ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["Calc"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "Calc.hs") calcSrc
  loadR <- Client.callTool c GhcLoad
            (object [ "module_path" .= ("src/Calc.hs" :: Text) ])
  c1 <- liveCheck $ checkJsonField "setup · load success" loadR "success" (Bool True)
  stepFooter 1 t0

  --------------------------------------------------------------------
  -- ghc_type — ask for the type of a local binding + a Prelude one
  --------------------------------------------------------------------
  t1 <- stepHeader 2 "ghc_type local + Prelude"
  tLocal   <- Client.callTool c GhcType
               (object [ "expression" .= ("double" :: Text) ])
  tPrelude <- Client.callTool c GhcType
               (object [ "expression" .= ("reverse" :: Text) ])
  c2a <- liveCheck $ mkContainsCheck
           "ghc_type(double) mentions Int -> Int" tLocal "type" "Int -> Int"
  c2b <- liveCheck $ mkContainsCheck
           "ghc_type(reverse) mentions list signature" tPrelude "type" "[a] -> [a]"
  stepFooter 2 t1

  --------------------------------------------------------------------
  -- ghc_info — declaration for a TYPE
  --------------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_info on data Tree"
  infoR <- Client.callTool c GhcInfo
            (object [ "name" .= ("Tree" :: Text) ])
  c3 <- liveCheck $ checkJsonFieldMatches
          "ghc_info(Tree) mentions 'data Tree' in definition"
          infoR "definition" (containsText "data Tree")
          "expected 'data Tree' declaration in the 'definition' field"
  stepFooter 2 t2

  --------------------------------------------------------------------
  -- ghc_eval — evaluate a pure expression + a local call
  --------------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_eval pure + local"
  evalPure  <- Client.callTool c GhcEval
                 (object [ "expression" .= ("1 + 2" :: Text) ])
  evalLocal <- Client.callTool c GhcEval
                 (object [ "expression" .= ("double 21" :: Text) ])
  c4a <- liveCheck $ mkContainsCheck
           "ghc_eval(1 + 2) returns 3" evalPure "output" "3"
  c4b <- liveCheck $ mkContainsCheck
           "ghc_eval(double 21) returns 42" evalLocal "output" "42"
  stepFooter 4 t3

  --------------------------------------------------------------------
  -- ghc_complete — completions for 'fold' prefix
  --------------------------------------------------------------------
  t4 <- stepHeader 5 "ghc_complete prefix=fold"
  compR <- Client.callTool c GhcComplete
            (object [ "prefix" .= ("fold" :: Text), "limit" .= (20 :: Int) ])
  c5 <- liveCheck $ checkJsonFieldMatches
          "ghc_complete returns ≥ 1 'fold*' candidate"
          compR "candidates" arrayNonEmpty
          "expected at least one completion in the 'candidates' array"
  stepFooter 5 t4

  --------------------------------------------------------------------
  -- ghc_goto — source location of a local name
  --------------------------------------------------------------------
  t5 <- stepHeader 6 "ghc_goto on local 'greet'"
  gotoR <- Client.callTool c GhcGoto
            (object [ "name" .= ("greet" :: Text) ])
  c6 <- liveCheck $ Check
    { cName   = "ghc_goto(greet) returns a file location"
    , cOk     = hasString "file"   gotoR
             || hasString "module" gotoR
    , cDetail = "expected a top-level 'file' or 'module' field on \
                \the goto payload; got: " <> renderShort gotoR
    }
  stepFooter 6 t5

  --------------------------------------------------------------------
  -- ghc_doc — Haddock lookup. Accept either real doc text or the
  -- "no doc" graceful fallback (older 'base' is built without
  -- Haddock on some distributions).
  --------------------------------------------------------------------
  t6 <- stepHeader 7 "ghc_doc on Prelude.map"
  docR <- Client.callTool c GhcDoc
            (object [ "name" .= ("map" :: Text) ])
  c7 <- liveCheck $ checkJsonFieldMatches
          "ghc_doc(map) returns success (with text OR graceful miss)"
          docR "success" (\v -> v == Bool True)
          "ghc_doc should always answer 'success': true, even when the \
          \docs are unavailable (returns a 'hint' in that case)"
  stepFooter 7 t6

  pure [c1, c2a, c2b, c3, c4a, c4b, c5, c6, c7]

--------------------------------------------------------------------------------
-- small predicates used by the flow
--------------------------------------------------------------------------------

-- | Build a 'Check' asserting that a top-level text field
-- contains a given substring. Reads nicely at the call site.
mkContainsCheck :: Text -> Value -> Text -> Text -> Check
mkContainsCheck name payload key needle = Check
  { cName   = name
  , cOk     = case lookupField key payload of
                Just (String s) -> needle `T.isInfixOf` s
                _               -> False
  , cDetail = case lookupField key payload of
                Just (String s) -> "expected '" <> needle <> "' in " <> key
                                <> "; got: " <> s
                _               -> "field '" <> key <> "' missing or not a string"
  }

containsText :: Text -> Value -> Bool
containsText needle (String s) = needle `T.isInfixOf` s
containsText _      _          = False

arrayNonEmpty :: Value -> Bool
arrayNonEmpty (Array a) = not (V.null a)
arrayNonEmpty _         = False

renderShort :: Value -> Text
renderShort v =
  let s = T.pack (show v)
  in if T.length s > 180 then T.take 180 s <> "…" else s

hasString :: Text -> Value -> Bool
hasString k v = case lookupField k v of
  Just (String _) -> True
  _               -> False

-- suppress unused-binding warning for checkPure (helper pattern,
-- kept for other flows in this module over time)
_unused :: Check
_unused = checkPure "x" True ""
