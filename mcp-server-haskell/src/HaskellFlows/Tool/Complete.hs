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
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import GHC (Ghc, getNamesInScope, moduleName, moduleNameString)
import GHC.Types.Name (nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.PermissiveJSON (IntField (unIntField))
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

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

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right (CompleteArgs prefix limit) ->
    case sanitizeExpression prefix of
      Left e ->
        pure (Env.toolResponseToResult (Env.mkRefused (Env.sanitizeRejection "prefix" e)))
      Right safe -> do
        eRes <- try (withGhcSession ghcSess (queryCompletions safe))
        pure $ Env.toolResponseToResult $ case eRes of
          Left (se :: SomeException) ->
            Env.mkFailed
              ((Env.mkErrorEnvelope Env.InternalError
                  (T.pack ("GHC API error: " <> show se)))
                    { Env.eeCause = Just (T.pack (show se)) })
          Right cands -> renderCompletions prefix limit cands

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
  let matches
        | "." `T.isInfixOf` prefix =
            -- Split at the LAST dot: everything before is the module
            -- qualifier, everything after is the (possibly empty) name
            -- prefix.  "Data.Map." → qual="Data.Map", npfx=""
            -- "Data.Map.in" → qual="Data.Map", npfx="in"
            let qual = T.unpack (T.dropWhileEnd (/= '.') prefix
                                   & \t -> if T.null t then prefix
                                           else T.dropEnd 1 t)
                npfx = T.unpack (T.takeWhileEnd (/= '.') prefix)
            in [ T.pack (qual <> "." <> occStr)
               | n <- names
               , let occStr = occNameString (nameOccName n)
               , npfx `isPrefixOf` occStr
               , case nameModule_maybe n of
                   Just m  -> moduleNameString (moduleName m) == qual
                   Nothing -> False
               ]
        | otherwise =
            [ T.pack s
            | n <- names
            , let s = occNameString (nameOccName n)
            , T.unpack prefix `isPrefixOf` s
            ]
  pure (sort (nub matches))
  where
    (&) = flip ($)

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
