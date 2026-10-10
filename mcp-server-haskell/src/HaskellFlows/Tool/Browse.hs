-- | @ghc_inspect(action=browse)@ — pure query + payload layer.
--
-- Lists names exported by a loaded module and their types. The
-- session-bound legacy handler died with the ApiSession backend
-- (W6.8); 'HaskellFlows.Tool.IdeBacked' runs the graph/contextual/
-- fallback queries inside its ghcide interactive context and shapes
-- the payload with 'browsePayload'.
module HaskellFlows.Tool.Browse
  ( parseBrowseOutput
    -- * W6 — Ghc queries + payload shaping (shared with IdeBacked)
  , BrowseArgs (..)
  , queryBrowseGraph
  , queryBrowseContextual
  , queryBrowseFallback
  , browsePayload
  , moduleNotInGraphPayload
  , moduleNotInGraphNextStep
  , parseErrorKind
  ) where

import Data.Aeson
import Data.List (isPrefixOf, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T

import GHC
  ( Ghc
  , InteractiveImport (IIDecl)
  , Module
  , Name
  , TyThing (AnId)
  , getModuleGraph
  , getModuleInfo
  , getNamesInScope
  , lookupModule
  , lookupName
  , mgModSummaries
  , mkModuleName
  , modInfoExports
  , moduleName
  , ms_hspp_file
  , ms_mod
  , setContext
  , simpleImportDecl
  )
import GHC.Types.Name (nameModule, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Var (varType)
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ToolName (ToolName (GhcInspect))
import qualified HaskellFlows.Mcp.NextStep as NS


newtype BrowseArgs = BrowseArgs Text

instance FromJSON BrowseArgs where
  parseJSON = withObject "BrowseArgs" $ \o -> BrowseArgs <$> o .: "module"

-- | Discriminate the FromJSON failure shape — same heuristic as
-- 'HaskellFlows.Tool.Workflow.parseErrorKind'. A missing required
-- field maps to 'MissingArg'; everything else falls back to
-- 'TypeMismatch'.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Primary browse path: look for the module in the compile graph,
-- restricted to source files under the project root.  Filtering by
-- project root prevents browsing stray external-package modules that
-- some CI environments include in the graph (e.g. @Prelude@ from
-- @base@ in GHC-from-source builds).
queryBrowseGraph :: FilePath -> Text -> Ghc (Maybe [Text])
queryBrowseGraph projectRoot nm = do
  let wanted = mkModuleName (T.unpack nm)
  mg <- getModuleGraph
  let matches =
        [ ms_mod ms
        | ms <- mgModSummaries mg
        , moduleName (ms_mod ms) == wanted
        , projectRoot `isPrefixOf` ms_hspp_file ms
        ]
  case matches of
    []      -> pure Nothing
    (m : _) -> browseModuleInfo m

-- | W6 contextual browse (ghcide backend): put the target module's
-- import in the interactive context and read the exports from the
-- names in scope — the GHCi @:browse@ shape. 'getModuleInfo' returns
-- Nothing for home modules whose interfaces ghcide holds only in
-- memory (no .hi was ever written), so the graph path alone would
-- render @Just []@ for them.
queryBrowseContextual :: Text -> Ghc (Maybe [Text])
queryBrowseContextual m = do
  let wanted = mkModuleName (T.unpack m)
  setContext [IIDecl (simpleImportDecl wanted)]
  names <- getNamesInScope
  let fromMod = [ n | n <- names, moduleName (nameModule n) == wanted ]
  if null fromMod
    then pure Nothing
    else Just . sort . nub <$> traverse renderExport fromMod

-- | #168 fallback: try the session's loaded package environment via
-- 'lookupModule'.  Called only when 'queryBrowseGraph' returns
-- 'Nothing'. Covers session-preloaded modules (Prelude, Data.Map, …)
-- that exist in the GHC package environment but are not part of the
-- project's own compile graph.
--
-- 'lookupModule' throws a 'SourceError' when the module is completely
-- unknown; the caller catches that at the 'IO' level.
queryBrowseFallback :: Text -> Ghc (Maybe [Text])
queryBrowseFallback nm = do
  let wanted = mkModuleName (T.unpack nm)
  m <- lookupModule wanted Nothing
  browseModuleInfo m

browseModuleInfo :: Module -> Ghc (Maybe [Text])
browseModuleInfo m = do
  minfo <- getModuleInfo m
  case minfo of
    Nothing -> pure (Just [])
    Just mi -> do
      let exports = modInfoExports mi
      entries <- traverse renderExport exports
      pure (Just entries)

-- | Render a single exported 'Name' as @"name :: type"@ when the
-- underlying 'TyThing' carries a type (identifier bindings); fall
-- back to the bare name for datatype / class / etc. entries.
renderExport :: Name -> Ghc Text
renderExport n = do
  let nm = T.pack (occNameString (nameOccName n))
  mTy <- lookupName n
  case mTy of
    Just (AnId i) ->
      pure (nm <> " :: " <> T.pack (showPprUnsafe (varType i)))
    _ ->
      pure nm

--------------------------------------------------------------------------------
-- legacy parser (retained for existing unit tests)
--------------------------------------------------------------------------------

-- | Pre-migration parser kept for the unit-test scaffolding. The live
-- path no longer calls this — the GHC API returns exports as 'Name'
-- directly. Retained as a pure parser fixture so the unit tests can
-- pin the text-shape contract without a live session.
parseBrowseOutput :: Text -> [Text]
parseBrowseOutput = filter (not . T.null) . map T.strip . T.lines

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

-- | Browse-success payload. Issue #90 Phase B: status='ok' with
-- the same field names as before ('module', 'count', 'entries')
-- so consumers continue to function during the dual-shape window.
browsePayload :: Text -> [Text] -> Value
browsePayload m entries = object
  [ "module"  .= m
  , "count"   .= length entries
  , "entries" .= entries
  ]

-- | Issue #72 + #90: payload for the no-match path. Carries
-- 'module' echo + a 'remediation' string. The previous shape's
-- 'error' string is replaced by the structured envelope at the
-- top level.
moduleNotInGraphPayload :: Text -> Value
moduleNotInGraphPayload m = object
  [ "module"      .= m
  , "remediation" .= ("Browse only enumerates modules compiled by this project. \
                      \For modules in interactive scope (Prelude, base, external \
                      \deps), look up individual names with ghc_info or query \
                      \with hoogle_search." :: Text)
  ]

-- | NextStep pointer attached to the no-match path: per-name
-- inspection via 'ghc_info', or discovery via 'hoogle_search'.
moduleNotInGraphNextStep :: NS.NextStep
moduleNotInGraphNextStep = NS.simple GhcInspect
  "'ghc_inspect(action=browse)' only sees modules compiled into this project. \
  \Use ghc_inspect(action=info, name=\"<symbol>\") for per-name inspection of \
  \external/base modules, or your host's search to discover names."
  (Just (object [ "name" .= ("<symbol you're trying to inspect>" :: Text) ]))
