-- | @ghc_inspect(action=complete)@ — pure query + payload layer.
--
-- Returns in-scope identifiers that start with the given prefix.
-- The session-bound legacy handler died with the ApiSession backend
-- (W6.8); 'HaskellFlows.Tool.IdeBacked' applies 'sanitizeExpression'
-- and runs 'queryCompletions' / 'queryQualifiedFallback' inside its
-- ghcide interactive context, shaping the reply with
-- 'renderCompletions'.
module HaskellFlows.Tool.Complete
  ( CompleteArgs (..)
  , renderCompletions
    -- * W6 — Ghc queries (shared with IdeBacked)
  , queryCompletions
  , queryQualifiedFallback
  , parseErrorKind
    -- * #252 (exported for unit tests)
  , splitQualifiedPrefix
  ) where

import Data.Aeson
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

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.PermissiveJSON (IntField (unIntField))


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
