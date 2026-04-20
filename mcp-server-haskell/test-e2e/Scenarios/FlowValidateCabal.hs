-- | Flow: @ghci_validate_cabal@ — cabal check + heuristics.
--
-- Two passes:
--   (1) Clean scaffold → success, no issues flagged.
--   (2) Corrupt the cabal with a duplicate @base@ dep →
--       validator flags the duplicate.
module Scenarios.FlowValidateCabal
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
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  ----------------------------------------------------------------
  -- setup — fresh scaffold
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold validate-demo"
  _ <- Client.callTool c "ghci_create_project"
         (object [ "name" .= ("validate-demo" :: Text) ])
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- (1) clean validate
  ----------------------------------------------------------------
  t1 <- stepHeader 2 "ghci_validate_cabal on clean scaffold"
  r1 <- Client.callTool c "ghci_validate_cabal" (object [])
  c1 <- liveCheck $ checkJsonField "clean · success" r1 "success" (Bool True)
  c2 <- liveCheck $ checkJsonFieldMatches
          "clean · issues array (possibly empty)"
          r1 "issues" isArray
          "validate_cabal response should carry an 'issues' array"
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- (2) corrupt with duplicate dep
  ----------------------------------------------------------------
  t2 <- stepHeader 3 "inject duplicate 'base' dep + re-validate"
  let cabalPath = projectDir </> "validate-demo.cabal"
  body <- TIO.readFile cabalPath
  -- Append an obviously-duplicate dep inside the library stanza.
  -- The tool's heuristic looks for repeated package names within
  -- the same build-depends block.
  let body' =
        T.replace
          "build-depends:    base >= 4.20 && < 5"
          "build-depends:    base >= 4.20 && < 5\n                    , base"
          body
  TIO.writeFile cabalPath body'

  r2 <- Client.callTool c "ghci_validate_cabal" (object [])
  c3 <- liveCheck $ checkJsonFieldMatches
          "duplicate · issues array mentions 'base'"
          r2 "issues" (issuesMention "base")
          "the 'base' package appearing twice must surface as an issue"
  stepFooter 3 t2

  pure [c1, c2, c3]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

isArray :: Value -> Bool
isArray (Array _) = True
isArray _         = False

-- | True iff the 'issues' array contains at least one entry
-- whose string fields mention the needle (package name).
issuesMention :: Text -> Value -> Bool
issuesMention needle (Array a) =
  any contains (V.toList a)
  where
    contains (Object o) = any fieldContains (KeyMap.elems o)
    contains (String s) = needle `T.isInfixOf` s
    contains _          = False

    fieldContains (String s) = needle `T.isInfixOf` s
    fieldContains _          = False
issuesMention _ _ = False

_k :: Key.Key
_k = Key.fromText ""
