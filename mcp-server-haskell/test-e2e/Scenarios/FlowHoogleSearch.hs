-- | Flow: 'hoogle_search' — the off-graph symbol lookup tool.
--
-- Closes a coverage gap: hoogle_search had no dedicated E2E scenario
-- (only 'FlowAddImportNoHoogle' touched it, and only the missing-binary
-- path via ghc_add_import). This exercises hoogle_search directly with
-- honest oracles that hold whether or not hoogle is installed on the
-- runner:
--
--   1. Empty query → refused (deterministic; the query validator runs
--      before any binary lookup, so no hoogle needed).
--   2. A real type-signature query → when hoogle is available the result
--      must contain the EXPECTED hit (`map` for `(a->b)->[a]->[b]`), not
--      just "some" hit; when it isn't, a well-formed no_match or
--      unavailable envelope. Strong on the ok branch, honest on the rest.
--   3. PATH scrubbed of hoogle → unavailable with a hoogle-mentioning
--      error (deterministic; mirrors the FlowAddImportNoHoogle technique
--      but against hoogle_search itself).
module Scenarios.FlowHoogleSearch
  ( runFlow
  ) where

import Control.Exception (bracket_)
import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (findExecutable)
import qualified System.Environment as Env

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (errorMessage, lookupField, statusIs)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  ----------------------------------------------------------------
  -- Step 1 — empty query is refused before any binary lookup.
  ----------------------------------------------------------------
  t0 <- stepHeader 1 "hoogle_search empty query → refused"
  rEmpty <- Client.callTool c HoogleSearch (object [ "query" .= ("" :: Text) ])
  c1 <- liveCheck $ checkPure
          "empty query → status=refused"
          (statusIs "refused" rEmpty)
          ("expected refused for empty query; got: " <> truncRender rEmpty)
  stepFooter 1 t0

  ----------------------------------------------------------------
  -- Step 2 — a real type-signature query. Strong oracle on the ok
  -- branch (must find `map`), well-formed on no_match/unavailable.
  ----------------------------------------------------------------
  mHoogle <- findExecutable "hoogle"
  t1 <- stepHeader 2 "hoogle_search \"(a -> b) -> [a] -> [b]\" finds map"
  rMap <- Client.callTool c HoogleSearch
            (object [ "query" .= ("(a -> b) -> [a] -> [b]" :: Text)
                    , "limit" .= (5 :: Int)
                    ])
  c2 <- liveCheck $ checkPure
          (case mHoogle of
             Just _  -> "hoogle present → ok with a `map` hit for the signature"
             Nothing -> "hoogle absent → well-formed no_match/unavailable")
          (hoogleQueryOracle rMap)
          ( "hoogle "
              <> (if isNothing mHoogle then "absent" else "present")
              <> "; got: " <> truncRender rMap )
  stepFooter 2 t1

  ----------------------------------------------------------------
  -- Step 3 — PATH scrubbed of hoogle → unavailable, deterministically.
  ----------------------------------------------------------------
  origPath <- Env.lookupEnv "PATH"
  t2 <- stepHeader 3 "hoogle_search with no hoogle on PATH → unavailable"
  cMissing <- bracket_
    (Env.setEnv "PATH" "/var/empty:/tmp/no-hoogle-here-deadbeef")
    (case origPath of
       Just p  -> Env.setEnv "PATH" p
       Nothing -> Env.unsetEnv "PATH")
    (do
      r <- Client.callTool c HoogleSearch
             (object [ "query" .= ("filter" :: Text) ])
      let unavailable = statusIs "unavailable" r
          mentions    = case errorMessage r of
                          Just m  -> "hoogle" `T.isInfixOf` T.toLower m
                          Nothing -> False
      liveCheck $ checkPure
        "scrubbed PATH → status=unavailable, error mentions hoogle"
        (unavailable && mentions)
        ("expected unavailable + 'hoogle' in error; got: " <> truncRender r))
  stepFooter 3 t2

  pure [c1, c2, cMissing]

--------------------------------------------------------------------------------
-- oracles
--------------------------------------------------------------------------------

-- | The type-signature query oracle, adaptive to hoogle availability:
--
--   * status=ok        → there must be a hit named @map@ whose signature
--                        is the queried @(a -> b) -> [a] -> [b]@. (strong)
--   * status=no_match   → count=0 with an empty hits array. (hoogle present
--                        but no generated index — well-formed empty.)
--   * status=unavailable→ a graceful "no hoogle" envelope.
--   * anything else     → fail.
hoogleQueryOracle :: Value -> Bool
hoogleQueryOracle r
  | statusIs "ok" r          = hasMapHit r
  | statusIs "no_match" r    = emptyHits r
  | statusIs "unavailable" r = True
  | otherwise                = False

-- | True when the @hits@ array carries an entry with name @map@ and the
-- queried signature — an independent oracle (we know `map`'s type), not
-- a tautology on whatever the tool echoed back.
hasMapHit :: Value -> Bool
hasMapHit r = case lookupField "hits" r of
  Just (Array a) -> any isMap (V.toList a)
  _              -> False
  where
    isMap (Object o) =
      objText o "name" == Just "map"
        && objText o "signature" == Just "(a -> b) -> [a] -> [b]"
    isMap _ = False

emptyHits :: Value -> Bool
emptyHits r = case lookupField "hits" r of
  Just (Array a) -> V.null a
  _              -> False

objText :: KeyMap.KeyMap Value -> Text -> Maybe Text
objText o k = case KeyMap.lookup (Key.fromText k) o of
  Just (String s) -> Just s
  _               -> Nothing

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
   in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
