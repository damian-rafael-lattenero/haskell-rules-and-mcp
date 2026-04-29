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
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import GHC (Ghc, getNamesInScope)
import GHC.Types.Name (nameOccName)
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
        "Return in-scope identifiers that start with the given prefix, "
          <> "via the GHC API. Useful before calling :info or :type on a "
          <> "candidate."
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
queryCompletions :: Text -> Ghc [Text]
queryCompletions prefix = do
  names <- getNamesInScope
  let pfxStr = T.unpack prefix
      matches =
        [ s
        | n <- names
        , let s = occNameString (nameOccName n)
        , pfxStr `isPrefixOf` s
        ]
  pure (map T.pack (sort (nub matches)))

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

-- | Map the candidate list into the right envelope: 'no_match'
-- when the list is empty (the question was well-formed; the
-- answer is the empty set), 'ok' otherwise. The legacy field
-- shape ('prefix', 'count', 'candidates', 'truncated') is
-- preserved inside 'result' for the dual-shape window.
renderCompletions :: Text -> Int -> [Text] -> Env.ToolResponse
renderCompletions prefix limit candidates =
  let capped = take limit candidates
      payload = object
        [ "prefix"     .= prefix
        , "count"      .= length capped
        , "candidates" .= capped
        , "truncated"  .= (length candidates > limit)
        ]
  in case candidates of
       [] -> Env.mkNoMatch payload
       _  -> Env.mkOk payload
