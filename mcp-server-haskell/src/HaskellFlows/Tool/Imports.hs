-- | @ghc_imports@ — Phase-6 tool (GHC-API migrated).
--
-- List the imports currently in the interactive context via
-- 'GHC.getContext'. Pre-migration wrapped @:show imports@; post
-- migration the GhcSession's interactive context is authoritative.
module HaskellFlows.Tool.Imports
  ( descriptor
  , handle
  , parseImportsOutput
    -- * Exposed for unit tests
  , importsPayload
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.List (nubBy)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import GHC
  ( Ghc
  , InteractiveImport (IIDecl, IIModule)
  , getContext
  , ideclName
  , moduleNameString
  , unLoc
  )
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Tool.Eval (evalContextExtras)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcImports
    , tdDescription =
        "List the imports currently in the GHC session's interactive "
          <> "context. Useful for confirming which modules are already "
          <> "available before suggesting an ghc_add_import."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object []
          , "additionalProperties" .= False
          ]
    }

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess _rawArgs = do
  eRes <- try (withGhcSession ghcSess queryImports)
  pure $ Env.toolResponseToResult $ case eRes of
    Left (se :: SomeException) ->
      Env.mkFailed
        ((Env.mkErrorEnvelope Env.InternalError
            (T.pack ("GHC API error: " <> show se)))
              { Env.eeCause = Just (T.pack (show se)) })
    Right pair -> Env.mkOk (importsPayload pair)

-- | F-10 / #114: split interactive context into source imports and the
-- MCP's own session preloads (Prelude, System.IO, etc. injected by
-- 'evalContextExtras'). Agents should see only the source imports;
-- the preloads are reported separately so the distinction is clear.
--
-- Deduplication (#114): the context can accumulate duplicate entries
-- when multiple 'ghc_eval' calls or 'loadForTarget' passes each append
-- the same module under different 'InteractiveImport' representations.
-- We deduplicate by module-name key BEFORE splitting so both buckets
-- stay tidy.
queryImports :: Ghc ([Text], [Text])
queryImports = do
  ctx <- getContext
  let deduped = nubBy (\a b -> importKey a == importKey b) ctx
      extras = Set.fromList evalContextExtras
      (preloads, source) = foldr (splitOne extras) ([], []) deduped
  pure (map renderImport source, map renderImport preloads)
  where
    importKey (IIDecl decl) = moduleNameString (unLoc (ideclName decl))
    importKey (IIModule mn) = moduleNameString mn
    splitOne extras ii (ps, ss) =
      if isExtra extras ii then (ii : ps, ss) else (ps, ii : ss)
    isExtra extras (IIDecl decl) =
      moduleNameString (unLoc (ideclName decl)) `Set.member` extras
    isExtra extras (IIModule mn) =
      moduleNameString mn `Set.member` extras

renderImport :: InteractiveImport -> Text
renderImport = \case
  IIDecl decl  -> T.pack (showPprUnsafe decl)
  IIModule mn  -> "module " <> T.pack (showPprUnsafe mn)

-- | Result payload. Issue #90 Phase B: 'result.{count, imports}'.
-- F-10: 'session_preloads' lists the MCP's own injected modules so
-- agents can distinguish them from the source file's own imports.
importsPayload :: ([Text], [Text]) -> Value
importsPayload (sourceImports, preloads) = object
  [ "count"            .= length sourceImports
  , "imports"          .= sourceImports
  , "session_preloads" .= preloads
  ]

-- | Pre-migration parser kept for the unit-test scaffolding. Retained
-- as a pure parser fixture so the tests can pin the text-shape
-- contract without a live session.
parseImportsOutput :: Text -> [Text]
parseImportsOutput = filter keep . map T.strip . T.lines
  where
    keep ln =
      not (T.null ln)
      && not (T.isInfixOf "via the command line" ln)
