-- | @ghc_browse@ — Phase-2 tool (GHC-API migrated).
--
-- Lists names exported by a loaded module and their types. Pre-migration
-- parsed the raw line-per-entry output of @:browse Module@; post-migration
-- queries 'getModuleInfo' + 'modInfoExports' and renders each export's
-- type via 'TyThing'.
module HaskellFlows.Tool.Browse
  ( descriptor
  , handle
  , parseBrowseOutput
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import GHC
  ( Ghc
  , Name
  , TyThing (AnId)
  , getModuleGraph
  , getModuleInfo
  , lookupName
  , mgModSummaries
  , mkModuleName
  , modInfoExports
  , moduleName
  , ms_mod
  )
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Var (varType)
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcBrowse
    , tdDescription =
        "List names exported by a loaded module + their types. "
          <> "Resolves against the auto-loaded project module graph."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module" .= object [ "type" .= ("string" :: Text) ] ]
          , "required"             .= ["module" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype BrowseArgs = BrowseArgs { baModule :: Text }

instance FromJSON BrowseArgs where
  parseJSON = withObject "BrowseArgs" $ \o -> BrowseArgs <$> o .: "module"

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right (BrowseArgs m) -> do
    eRes <- try (withGhcSession ghcSess (queryBrowse m))
    pure $ case eRes of
      Left (se :: SomeException) ->
        errorResult (T.pack ("GHC API error: " <> show se))
      Right Nothing ->
        -- Issue #72: 'browse' can only enumerate modules from the
        -- project's compile graph, but 'ghc_imports' often lists
        -- 'Prelude' and other base modules that are in the
        -- *interactive scope* without being in the graph. The
        -- pre-#72 message ("Module … is not in the loaded module
        -- graph") was technically correct but actionable-blind.
        -- Surface a structured nextStep that points the agent at
        -- 'ghc_info' (per-name) or 'hoogle_search' so the
        -- discrepancy doesn't waste a round-trip.
        moduleNotInGraphResult m
      Right (Just entries) ->
        successResult m entries

-- | Look up the module in the current module graph, pull its exports,
-- render each as "name :: type" (or just the name for non-Id things).
queryBrowse :: Text -> Ghc (Maybe [Text])
queryBrowse nm = do
  let wanted = mkModuleName (T.unpack nm)
  mg <- getModuleGraph
  let matches =
        [ ms_mod ms
        | ms <- mgModSummaries mg
        , moduleName (ms_mod ms) == wanted
        ]
  case matches of
    []      -> pure Nothing
    (m : _) -> do
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

successResult :: Text -> [Text] -> ToolResult
successResult m entries =
  let payload = object
        [ "success" .= True
        , "module"  .= m
        , "count"   .= length entries
        , "entries" .= entries
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

errorResult :: Text -> ToolResult
errorResult msg = ToolResult
  { trContent = [ TextContent (encodeUtf8Text (object
      [ "success" .= False, "error" .= msg ])) ]
  , trIsError = True
  }

-- | Issue #72: structured failure when 'browse' can't find the
-- requested module in the project module graph. The agent gets
-- a pointer at the canonical fallback path (per-name via
-- 'ghc_info' or 'hoogle_search') instead of a dead-end string.
moduleNotInGraphResult :: Text -> ToolResult
moduleNotInGraphResult m =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success"     .= False
        , "error"       .= ("Module '" <> m <> "' is not in the project's module graph.")
        , "error_kind"  .= ("module_not_in_graph" :: Text)
        , "remediation" .= ("Browse only enumerates modules compiled by this project. \
                            \For modules in interactive scope (Prelude, base, external \
                            \deps), look up individual names with ghc_info or query \
                            \with hoogle_search." :: Text)
        , "nextStep"    .= object
            [ "tool"    .= ("ghc_info" :: Text)
            , "why"     .= ("'ghc_browse' only sees modules compiled into this project. \
                            \Use ghc_info(name=\"<symbol>\") for per-name inspection of \
                            \external/base modules, or hoogle_search to discover names." :: Text)
            , "example" .= object
                [ "name" .= ("<symbol from " <> m <> ">" :: Text) ]
            ]
        ]))
      ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
