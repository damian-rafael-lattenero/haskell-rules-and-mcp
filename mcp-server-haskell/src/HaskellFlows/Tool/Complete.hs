-- | @ghc_complete@ — Phase-2 tool (GHC-API migrated).
--
-- Returns in-scope identifiers that start with the given prefix.
-- Pre-migration this wrapped @:complete repl "prefix"@ and parsed
-- its framed count+list output; post-migration it queries
-- 'getNamesInScope' directly and filters in-process.
--
-- Boundary safety: prefix still routes through 'sanitizeExpression'
-- so the newline/sentinel/empty/too-large contract is identical.
module HaskellFlows.Tool.Complete
  ( descriptor
  , handle
  , CompleteArgs (..)
  , renderCompletions
    -- * #252 (exported for unit tests)
  , splitQualifiedPrefix
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import GHC
  ( Ghc
  , getModuleInfo
  , getNamesInScope
  , lookupModule
  , mkModuleName
  , modInfoExports
  , moduleName
  , moduleNameString
  )
import GHC.Types.Name (nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)

import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.PermissiveJSON (IntField (unIntField))
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Tool.Env (ToolEnv (..))

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcComplete
    , tdDescription =
        "PURPOSE: Return in-scope identifiers that start with the given "
          <> "prefix, via :complete / GHC API. "
          <> "WHEN: discovering names that match a prefix before drilling "
          <> "in with ghc_info or ghc_type; auto-completion-style lookup "
          <> "during exploration. Qualified prefixes (e.g. \"Data.Map.\") "
          <> "are supported and return fully-qualified candidates — the "
          <> "module must be loaded or in session imports. "
          <> "WHEN NOT: you want a specific module's full export surface — "
          <> "use ghc_browse; the symbol is off-graph (external lib not "
          <> "yet loaded) — use hoogle_search. "
          <> "PREREQUISITES: a session is loaded (preloads always make "
          <> "Prelude visible); for qualified completions the qualifying "
          <> "module must be in scope (use ghc_add_import first if not). "
          <> "OUTPUT: {prefix, count, candidates:[name]}; default limit "
          <> "25, hard-capped at 200. "
          <> "SEE ALSO: ghc_info, ghc_type, ghc_browse."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "prefix" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Prefix to complete. Example: \"fold\" returns \
                       \foldr, foldl, foldMap, ..." :: Text)
                  ]
              , "limit" .= object
                  [ "type"        .= ("integer" :: Text)
                  , "description" .=
                      ("Maximum candidates to return. Default 25, capped \
                       \at 200." :: Text)
                  ]
              ]
          , "required"             .= ["prefix" :: Text]
          , "additionalProperties" .= False
          ]
    }

data CompleteArgs = CompleteArgs
  { caPrefix :: !Text
  , caLimit  :: !Int
  }
  deriving stock (Show)

-- | Issue #88: 'limit' accepts a stringified number ("10") in
-- addition to a JSON number, mirroring the array-param widening
-- already in place for other tools.
instance FromJSON CompleteArgs where
  parseJSON = withObject "CompleteArgs" $ \o -> do
    p <- o .:  "prefix"
    l <- maybe 25 unIntField <$> o .:? "limit"
    pure CompleteArgs { caPrefix = p, caLimit = clampLimit l }

clampLimit :: Int -> Int
clampLimit n
  | n <= 0    = 1
  | n > 200   = 200
  | otherwise = n

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
  Right (CompleteArgs prefix limit) ->
    case sanitizeExpression prefix of
      Left e ->
        pure (Env.mkRefused (Env.sanitizeRejection "prefix" e))
      Right safe -> do
        eRes <- try (withGhcSession ghcSess (queryCompletions safe))
        case eRes of
          Left (se :: SomeException) ->
            pure $ Env.mkFailed
                ((Env.mkErrorEnvelope Env.InternalError
                    (T.pack ("GHC API error: " <> show se)))
                      { Env.eeCause = Just (T.pack (show se)) })
          Right cands
            -- #252: if the in-scope path produced no matches AND the
            -- prefix is qualified (e.g. "Data.Map.lookup"), the module
            -- almost certainly isn't currently imported. Fall back to
            -- 'lookupModule' + 'modInfoExports' so we can answer from
            -- the loaded module graph / package environment without
            -- forcing the caller to `import` the module first.
            | null cands
            , Just (qual, npfx) <- splitQualifiedPrefix safe -> do
                eFbk <- try (withGhcSession ghcSess
                               (queryQualifiedFallback qual npfx))
                          :: IO (Either SomeException [Text])
                let fallbackCands = case eFbk of
                      Right xs -> xs
                      Left _   -> []   -- module unknown → empty list
                pure $ renderCompletions prefix limit fallbackCands
            | otherwise ->
                pure $ renderCompletions prefix limit cands

-- | Discriminate the FromJSON failure shape — a missing required
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


-- | Scan every name currently in the interactive context, keep the
-- ones whose occurrence name starts with the prefix. Sort + dedupe
-- to match the shape the subprocess @:complete@ produced.
--
-- Issue #252: when the prefix is qualified (contains a dot, e.g.
-- @"Data.Map."@ or @"Data.Map.in"@), split into module qualifier +
-- name prefix, then filter names by their home module and construct
-- fully-qualified candidate strings.  Unqualified prefixes fall back
-- to the original unqualified scan.
queryCompletions :: Text -> Ghc [Text]
queryCompletions prefix = do
  names <- getNamesInScope
  let matches = case splitQualifiedPrefix prefix of
        Just (qual, npfx) ->
          [ T.pack (T.unpack qual <> "." <> occStr)
          | n <- names
          , let occStr = occNameString (nameOccName n)
          , T.unpack npfx `isPrefixOf` occStr
          , case nameModule_maybe n of
              Just m  -> moduleNameString (moduleName m) == T.unpack qual
              Nothing -> False
          ]
        Nothing ->
          [ T.pack s
          | n <- names
          , let s = occNameString (nameOccName n)
          , T.unpack prefix `isPrefixOf` s
          ]
  pure (sort (nub matches))

-- | #252: parse a qualified prefix string into (moduleQualifier, namePrefix).
-- Returns 'Nothing' for unqualified prefixes (no dot at all).
--
-- Examples:
--
-- >>> splitQualifiedPrefix "Data.Map."
-- Just ("Data.Map", "")
--
-- >>> splitQualifiedPrefix "Data.Map.lookup"
-- Just ("Data.Map", "lookup")
--
-- >>> splitQualifiedPrefix "fold"
-- Nothing
--
-- The split is at the LAST dot — everything before is the module
-- qualifier, everything after is the (possibly empty) name prefix.
-- Pure — exported for unit tests.
splitQualifiedPrefix :: Text -> Maybe (Text, Text)
splitQualifiedPrefix prefix
  | "." `T.isInfixOf` prefix =
      let beforeDot = T.dropWhileEnd (/= '.') prefix
          qual      = if T.null beforeDot then prefix else T.dropEnd 1 beforeDot
          npfx      = T.takeWhileEnd (/= '.') prefix
      in Just (qual, npfx)
  | otherwise = Nothing

-- | #252: lookup-based fallback for qualified prefixes whose module is
-- NOT currently imported into the interactive context. Resolves the
-- qualifier via 'lookupModule' (which consults the loaded module graph
-- + the package environment), enumerates the module's exports via
-- 'modInfoExports', and filters by the name prefix.
--
-- This mirrors the same path 'ghc_browse' uses to surface exports of
-- off-graph modules (see 'HaskellFlows.Tool.Browse.queryBrowseFallback').
--
-- Throws 'SourceError' when the module is completely unknown — caller
-- catches at the IO level via 'try'.
queryQualifiedFallback :: Text -> Text -> Ghc [Text]
queryQualifiedFallback qual npfx = do
  let modName = mkModuleName (T.unpack qual)
  m  <- lookupModule modName Nothing
  mi <- getModuleInfo m
  case mi of
    Nothing   -> pure []
    Just info ->
      let exports = modInfoExports info
          matches =
            [ T.pack (T.unpack qual <> "." <> occStr)
            | n <- exports
            , let occStr = occNameString (nameOccName n)
            , T.unpack npfx `isPrefixOf` occStr
            ]
      in pure (sort (nub matches))

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

-- | Map the candidate list into the right envelope: 'no_match'
-- when the list is empty (the question was well-formed; the
-- answer is the empty set), 'ok' otherwise. The legacy field
-- shape ('prefix', 'count', 'candidates', 'truncated') is
-- preserved inside 'result' for the dual-shape window.
--
-- #145: when 0 candidates and the prefix looks qualified (contains
-- a dot), add a remediation hint: GHCi only resolves qualified
-- completions when the module is imported into the interactive scope.
renderCompletions :: Text -> Int -> [Text] -> Env.ToolResponse
renderCompletions prefix limit candidates =
  let capped = take limit candidates
      isQualified = "." `T.isInfixOf` prefix
      basePayload = object
        [ "prefix"     .= prefix
        , "count"      .= length capped
        , "candidates" .= capped
        , "truncated"  .= (length candidates > limit)
        ]
      -- #225: extract the module portion of a qualified prefix for the
      -- remediation message (e.g. "Data.List." → "Data.List").
      qualModule = T.dropWhileEnd (/= '.') prefix
                   & T.dropEnd 1  -- drop trailing dot
        where (&) = flip ($)
      noMatchPayload
        | isQualified =
            object
              [ "prefix"      .= prefix
              , "count"       .= (0 :: Int)
              , "candidates"  .= ([] :: [Text])
              , "truncated"   .= False
              -- #225: session preloads are unqualified; mention that
              -- explicitly so the user understands why it fails even
              -- when the module IS imported, and give a concrete alternative.
              , "remediation" .=
                  ("Qualified completions require 'import qualified "
                   <> qualModule <> "'. "
                   <> "Session preloads use unqualified imports — try the bare "
                   <> "name prefix instead (e.g. drop the \""
                   <> qualModule <> ".\" prefix), or add a qualified import "
                   <> "via ghc_add_import(name=\"" <> qualModule
                   <> "\") then retry." :: Text)
              ]
        | otherwise = basePayload
  in case candidates of
       [] -> Env.mkNoMatch noMatchPayload
       _  -> Env.mkOk basePayload
