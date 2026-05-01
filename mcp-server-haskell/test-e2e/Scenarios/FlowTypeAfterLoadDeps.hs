-- | Flow: ghc_type after ghc_load with non-base deps (#80).
--
-- Regression anchor for issue #80
-- -------------------------------
-- The dogfood report at commit @e8fc3c7@ described a cascade of
-- @[GHC-87110] hidden package@ + @[GHC-61689] does not export@
-- errors when @ghc_type "id"@ was called after @ghc_load@ on a
-- project module that imported non-base packages (aeson, ghc,
-- regex-tdfa, ghc-paths). The most fundamental query the MCP
-- exposes — \"what is the type of @id@?\" — is meant to ALWAYS
-- succeed; the cascade silently disabled type-driven property
-- suggestion, hole-fit verification, and pre-refactor type
-- checks for any project depending on internal-API packages.
--
-- This scenario plants the same conditions in a throwaway
-- project: scaffolds a library that depends on @aeson@, writes
-- a module with @import Data.Aeson@, loads it, and then asks
-- for the type of a base symbol. The expected response is
-- @success=true@ with a polymorphic-type rendering — not a
-- hidden-package cascade.
--
-- Filter handle: substring @\"type_after_load\"@ in the
-- scenario label, so an inner-loop iteration can target it via
-- @HASKELL_FLOWS_E2E_ONLY=type_after_load@.
module Scenarios.FlowTypeAfterLoadDeps
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
  , checkJsonField
  , checkJsonFieldMatches
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import HaskellFlows.Mcp.ToolName (ToolName (..))

-- | Module that drags @aeson@ in at compile time. The presence
-- of @import Data.Aeson@ is what turns the bug on under the
-- pre-fix interactive-context handling — the parsed import gets
-- installed in the IC and stays there when ghc_type runs.
usesAesonSrc :: Text
usesAesonSrc =
  "module UsesAeson (toJ) where\n\
  \\n\
  \import Data.Aeson (ToJSON, toJSON, Value)\n\
  \\n\
  \toJ :: Int -> Value\n\
  \toJ = toJSON\n"

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c projectDir = do
  --------------------------------------------------------------------
  -- setup — scaffold + add aeson dep + register UsesAeson + load.
  --------------------------------------------------------------------
  t0 <- stepHeader 1 "scaffold + add aeson + register UsesAeson + load"
  _ <- Client.callTool c GhcCreateProject
         (object [ "name" .= ("type-after-load" :: Text) ])
  _ <- Client.callTool c GhcDeps
         (object
           [ "action"  .= ("add" :: Text)
           , "package" .= ("aeson" :: Text)
           , "stanza"  .= ("library" :: Text)
           ])
  _ <- Client.callTool c GhcModules
         (object [ "action" .= ("add" :: Text), "modules" .= (["UsesAeson"] :: [Text]) ])
  createDirectoryIfMissing True (projectDir </> "src")
  TIO.writeFile (projectDir </> "src" </> "UsesAeson.hs") usesAesonSrc
  loadR <- Client.callTool c GhcLoad
            (object [ "module_path" .= ("src/UsesAeson.hs" :: Text) ])
  c1 <- liveCheck $ checkJsonField
          "setup · ghc_load on UsesAeson succeeded (deps + import resolved)"
          loadR "success" (Bool True)
  stepFooter 1 t0

  --------------------------------------------------------------------
  -- ghc_type "id" — base symbol, must return forall a. a -> a.
  -- Pre-fix this returned a hundreds-of-lines hidden-package cascade
  -- with [GHC-87110] / [GHC-61689]. Asserting both success=true AND
  -- an actual polymorphic-shape rendering closes the original bug
  -- and any future regression where success is reported but the
  -- type is wrong (e.g. "()" or empty string).
  --------------------------------------------------------------------
  t1 <- stepHeader 2 "type_after_load · ghc_type \"id\" returns polymorphic type"
  tIdR <- Client.callTool c GhcType
           (object [ "expression" .= ("id" :: Text) ])
  c2a <- liveCheck $ checkJsonField
          "ghc_type(id) success=true (no hidden-package cascade · #80)"
          tIdR "success" (Bool True)
  c2b <- liveCheck $ checkJsonFieldMatches
          "ghc_type(id) renders polymorphic 'a -> a' shape"
          tIdR "type" containsArrowOverTypeVar
          ( "Expected the response 'type' field to render a polymorphic \
            \identity signature (e.g. 'a -> a' or 'forall a. a -> a'). A \
            \cascade of hidden-package errors here means the interactive \
            \context lost the project's package set after load. Raw: "
            <> truncRender tIdR )
  stepFooter 2 t1

  --------------------------------------------------------------------
  -- ghc_type "map" — second base-only oracle, on a function whose
  -- signature has two type variables. If the cascade only swallowed
  -- the simplest expression we'd miss it; this catches the case
  -- where 'id' happens to short-circuit but real polymorphic
  -- functions still fail.
  --------------------------------------------------------------------
  t2 <- stepHeader 3 "ghc_type \"map\" returns base list signature"
  tMapR <- Client.callTool c GhcType
            (object [ "expression" .= ("map" :: Text) ])
  c3 <- liveCheck $ checkJsonFieldMatches
          "ghc_type(map) renders '(a -> b) -> [a] -> [b]' shape"
          tMapR "type" mentionsListArrow
          ( "Expected 'type' field to mention a list-shaped arrow signature \
            \matching map :: (a -> b) -> [a] -> [b]. Raw: "
            <> truncRender tMapR )
  stepFooter 3 t2

  --------------------------------------------------------------------
  -- ghc_type "toJSON" — project-dep symbol. Distinguishes the
  -- "package set lost on context restore" failure mode (where a
  -- previously imported aeson symbol stops resolving even though
  -- ghc_load succeeded) from a base-only-works fix that left the
  -- real problem unsolved. The third query is the union-anchor
  -- between the base oracle (id, map) and the project-dep oracle
  -- (toJSON), so a future regression that only re-breaks one half
  -- shows up as a partial pass.
  --------------------------------------------------------------------
  t3 <- stepHeader 4 "ghc_type \"toJSON\" resolves under aeson dep (project-dep oracle)"
  tToJsonR <- Client.callTool c GhcType
               (object [ "expression" .= ("toJSON" :: Text) ])
  c4 <- liveCheck $ checkJsonField
          "ghc_type(toJSON) success=true (aeson exposed in IC after load)"
          tToJsonR "success" (Bool True)
  stepFooter 4 t3

  pure [c1, c2a, c2b, c3, c4]

--------------------------------------------------------------------------------
-- predicates
--------------------------------------------------------------------------------

-- | Accept any rendering that ends with a single type variable
-- arrowed to itself: \"a -> a\" or \"forall a. a -> a\" or
-- \"forall {a}. a -> a\". The check is intentionally tolerant
-- because GHC's pretty-printer has emitted all three shapes
-- across 9.10 / 9.12.
containsArrowOverTypeVar :: Value -> Bool
containsArrowOverTypeVar (String s) =
  any (`T.isInfixOf` s)
    [ "a -> a"
    , "a -> a."   -- with trailing punctuation
    ]
containsArrowOverTypeVar _ = False

-- | Accept any rendering that mentions @[a]@ → @[b]@ in a
-- recognisable form. Map\'s exact rendering varies by GHC
-- minor; the substring @"[a]\"@ + @"->\"@ is stable enough.
mentionsListArrow :: Value -> Bool
mentionsListArrow (String s) =
  T.isInfixOf "->" s && (T.isInfixOf "[a]" s || T.isInfixOf "[b]" s)
mentionsListArrow _ = False

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 400
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw

-- | Anchor the @KeyMap.lookup\/Key.fromText@ pair so the
-- 'mentionsListArrow' / 'containsArrowOverTypeVar' callers do
-- not need to import either qualifier site individually. (The
-- imports are still required to keep the module honest under
-- -Wunused-imports.)
_keyMapAnchor :: KeyMap.KeyMap Value -> Key.Key -> Maybe Value
_keyMapAnchor km k = KeyMap.lookup k km
