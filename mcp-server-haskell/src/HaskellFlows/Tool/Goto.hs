-- | @ghc_goto@ — Phase-2 tool (GHC-API migrated).
--
-- Returns the source location where a name is defined. Pre-migration
-- parsed "Defined at" / "Defined in" markers from @:info@ output;
-- post-migration queries the 'Name''s 'SrcSpan' directly.
--
-- Richer jump-to-definition (cross-module re-exports, macro-generated
-- names) still belongs to HLS — a future phase will wrap an
-- @ghc_hls@ tool once that lands.
module HaskellFlows.Tool.Goto
  ( descriptor
  , handle
  , GotoArgs (..)
  , parseDefinedAt
  , Location (..)
    -- * Issue #117 — exposed for unit tests
  , locationPayload
    -- * Issue #224 — exposed for unit tests
  , qualifiedPreloadPayload
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

import GHC
  ( Ghc
  , Name
  , getNamesInScope
  , moduleName
  , nameSrcSpan
  )
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name (nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc
  ( SrcSpan (RealSrcSpan, UnhelpfulSpan)
  , srcSpanFile
  , srcSpanStartCol
  , srcSpanStartLine
  )
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Tool.Env (ToolEnv (..))

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcGoto
    , tdDescription =
        "PURPOSE: Return the source location where a name is defined, "
          <> "via the GHC API's SrcSpan. "
          <> "WHEN: jumping from a usage to its definition; locating a "
          <> "binding before ghc_refactor scopes a rename. "
          <> "WHEN NOT: you also want the type/kind/instances — that is "
          <> "ghc_info (which already includes 'defined_at'); cross-"
          <> "module re-exports / macro names — future ghc_hls. "
          <> "PREREQUISITES: name must be in scope. File+line is only "
          <> "available for names loaded in interpreted (byte-code) mode. "
          <> "Most project modules are compiled to object code, so goto "
          <> "typically returns the defining module name only "
          <> "(has_location: false). Use grep or ghc_info as a fallback. "
          <> "OUTPUT: {name, has_location, location?:{file, line, column}, "
          <> "module?}. "
          <> "SEE ALSO: ghc_info, ghc_browse."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name to locate. Examples: \"greet\", \"Functor\"."
                       :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype GotoArgs = GotoArgs
  { gaName :: Text
  }
  deriving stock (Show)

instance FromJSON GotoArgs where
  parseJSON = withObject "GotoArgs" $ \o ->
    GotoArgs <$> o .: "name"

-- | A resolved source location. Either a concrete @file:line:col@
-- (project-defined names) or a bare module name (for names resolved
-- to an imported module without a local SrcSpan).
data Location
  = InFile !Text !Int !Int
  | InModule !Text
  deriving stock (Eq, Show)

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = do
  ghcSess <- teSession env
  runHandle ghcSess rawArgs

runHandle :: GhcSession -> Value -> IO ToolResponse
runHandle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) }))
  Right (GotoArgs nm) -> case sanitizeExpression nm of
    Left e ->
      pure (Env.mkRefused (Env.sanitizeRejection "name" e))
    Right safe -> do
      eRes <- try (withGhcSession ghcSess (queryLocation safe))
      case eRes of
        Left (se :: SomeException) ->
          pure $ Env.mkFailed
            ((Env.mkErrorEnvelope Env.InternalError
                (T.pack ("GHC API error: " <> show se)))
                  { Env.eeCause = Just (T.pack (show se)) })
        Right Nothing -> do
          -- #224: before emitting the generic remediation, check if the
          -- name is qualified (contains '.') and try the unqualified
          -- suffix. If that matches a session preload, give a targeted hint
          -- rather than "run ghc_load".
          let unqual = T.takeWhileEnd (/= '.') safe
          unqualRes <-
            if T.length unqual < T.length safe && not (T.null unqual)
              then try (withGhcSession ghcSess (queryLocation unqual))
                     :: IO (Either SomeException (Maybe Location))
              else pure (Right Nothing)
          pure $ case unqualRes of
            Right (Just loc) ->
              Env.mkNoMatch (qualifiedPreloadPayload safe unqual loc)
            _ ->
              Env.mkNoMatch (notInScopePayload safe)
        -- Issue #117: file locations → ok (can jump); library/unknown
        -- module locations → no_match (name found but no source to
        -- jump to). The payload still carries module + has_location so
        -- the agent knows *why* there is no file path.
        Right (Just loc) ->
          pure $ case loc of
            InFile {} -> Env.mkOk (locationPayload safe loc)
            InModule {} -> Env.mkNoMatch (locationPayload safe loc)

-- | Discriminate the FromJSON failure shape — same heuristic as
-- the other Phase-B migrations.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Match names in the interactive scope by exact occurrence name,
-- then promote the 'SrcSpan' to a structured 'Location'.
queryLocation :: Text -> Ghc (Maybe Location)
queryLocation nm = do
  names <- getNamesInScope
  let target = T.unpack nm
      matches =
        [ n
        | n <- names
        , occNameString (nameOccName n) == target
        ]
  case matches of
    []    -> pure Nothing
    (n:_) -> pure (Just (nameToLocation n))

nameToLocation :: Name -> Location
nameToLocation n = case nameSrcSpan n of
  RealSrcSpan rspan _ ->
    InFile
      (T.pack (unpackFS (srcSpanFile rspan)))
      (srcSpanStartLine rspan)
      (srcSpanStartCol rspan)
  UnhelpfulSpan _ ->
    case nameModule_maybe n of
      Just m  -> InModule (T.pack (showPprUnsafe (moduleName m)))
      Nothing -> InModule "<unknown>"

--------------------------------------------------------------------------------
-- legacy parser (retained for unit-test back-compat)
--------------------------------------------------------------------------------

-- | Kept for the existing unit tests that validate the pre-migration
-- parser. The live code path no longer calls this — the GHC API
-- returns 'SrcSpan' directly. Retained as a pure parser fixture so
-- the unit tests can pin the text-shape contract without a live
-- session.
parseDefinedAt :: Text -> Maybe Location
parseDefinedAt raw = firstJust tryLine (T.lines raw)
  where
    tryLine ln
      | Just rest <- findMarker "-- Defined at " ln = parseFileLoc rest
      | Just rest <- findMarker "-- Defined in " ln = parseModuleLoc rest
      | otherwise = Nothing

    findMarker marker ln =
      let (_, after) = T.breakOn marker ln
      in if T.null after
           then Nothing
           else Just (T.drop (T.length marker) after)

parseFileLoc :: Text -> Maybe Location
parseFileLoc t =
  case T.splitOn ":" (T.strip t) of
    (file : lnTxt : colTxt : _) -> do
      l <- readMaybe (T.unpack (T.filter (/= ' ') lnTxt))
      c <- readMaybe (T.unpack (T.filter (/= ' ') colTxt))
      pure (InFile file l c)
    _ -> Nothing

parseModuleLoc :: Text -> Maybe Location
parseModuleLoc t =
  let stripped = T.dropAround (`elem` (" '\x2018\x2019" :: String)) (T.strip t)
  in if T.null stripped then Nothing else Just (InModule stripped)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ []     = Nothing
firstJust f (x:xs) = case f x of
  Just y  -> Just y
  Nothing -> firstJust f xs

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

-- | Render the resolved location into the same shape the legacy
-- callers consumed (kind=file with file/line/column, OR kind=module
-- with module). Phase B keeps these field names; only the wrapping
-- 'success: bool' moves out of the payload (auto-derived from
-- 'status').
-- | Issue #117: 'InFile' results carry @has_location: true@ (agent can
-- jump to a source file); 'InModule' results carry @has_location: false@
-- plus a remediation hint so the agent understands *why* no file is
-- available (e.g. the name lives in a library with no local source).
locationPayload :: Text -> Location -> Value
locationPayload nm = \case
  InFile f l c ->
    object
      [ "name"         .= nm
      , "kind"         .= ("file" :: Text)
      , "file"         .= f
      , "line"         .= l
      , "column"       .= c
      , "has_location" .= True
      ]
  InModule m ->
    object
      [ "name"         .= nm
      , "kind"         .= ("module" :: Text)
      , "module"       .= m
      , "has_location" .= False
      , "remediation"  .= remediationFor m
      ]
  where
    -- Issue #214: the old message said "no local source file" which is
    -- factually wrong for project-local modules compiled as a library.
    -- The real reason is that compiled Names carry an UnhelpfulSpan —
    -- the SrcSpan is not preserved across compilation. The message now
    -- reflects the actual limitation without misleading the agent.
    remediationFor m
      | m == "<unknown>" =
          "Name has no SrcSpan — it may be a built-in or auto-derived \
          \binding. Use ghc_info for type information." :: Text
      | otherwise =
          "Name is defined in module '" <> m <> "' but was compiled — \
          \source location is not available in compiled mode. \
          \Use ghc_info for its type or ghc_doc for Haddock documentation."

-- | #224: payload when a qualified name fails but the unqualified suffix
-- IS in scope. Tells the agent the name is available without the qualifier.
qualifiedPreloadPayload :: Text -> Text -> Location -> Value
qualifiedPreloadPayload qualName unqualName loc =
  let modPart = T.dropEnd (T.length unqualName + 1) qualName
      locFields = case loc of
        InFile f l c ->
          [ "has_location" .= True
          , "file"         .= f
          , "line"         .= l
          , "column"       .= c
          ]
        InModule m ->
          [ "has_location" .= False
          , "module"       .= m
          ]
  in object $
       [ "name"        .= qualName
       , "searched_in" .= ("interactive scope" :: Text)
       , "remediation" .= ("'" <> unqualName <> "' is in scope via the '"
                           <> modPart <> "' preload (unqualified). "
                           <> "For a qualified import use "
                           <> "'import qualified " <> modPart <> "', "
                           <> "or query with the unqualified name '" <> unqualName
                           <> "' to get the source location." :: Text)
       ] <> locFields

-- | Result payload for the no-match (name-not-in-scope) path.
-- Carries the searched name + a remediation pointer so the agent
-- can choose to retry via a richer surface.
notInScopePayload :: Text -> Value
notInScopePayload nm = object
  [ "name"        .= nm
  , "searched_in" .= ("interactive scope" :: Text)
  , "remediation" .= ("Name not currently in scope. If it's defined in a \
                      \loaded module, run ghc_load on that module first. \
                      \For external/base names, ghc_info often resolves \
                      \what ghc_goto cannot." :: Text)
  ]
