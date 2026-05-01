-- | Flow: 'ghc_info' surfaces data constructors that GHCi's
-- @:info@ shows on its first line (#54).
--
-- Pre-fix behaviour
-- -----------------
-- 'ghc_info(name="Maybe")' returned
-- @"definition": "data Maybe Type constructor 'Maybe'"@ — the
-- constructor list ('Nothing', 'Just a') was completely
-- omitted. Every algebraic type queried through the MCP lost
-- its most useful information: \"what are the constructors?\".
-- Agents had to read the source file or fall back to
-- 'ghc_browse' to recover the data.
--
-- New contract
-- ------------
-- For algebraic 'TyCon's (data + newtype, classes excluded), the
-- response now includes:
--
--   * @definition@  — \"@data Maybe a = Nothing | Just a@\".
--   * @constructors@ — structured array of @{name, args}@ pairs.
--
-- Type synonyms / classes / functions keep the legacy shape
-- (no @constructors@ field) so existing JSON consumers don't
-- break.
module Scenarios.FlowInfoConstructors
  ( runFlow
  ) where

import Data.Aeson (Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import E2E.Assert
  ( Check (..)
  , checkPure
  , liveCheck
  , stepFooter
  , stepHeader
  )
import qualified E2E.Client as Client
import E2E.Envelope (lookupField)
import HaskellFlows.Mcp.ToolName (ToolName (..))

runFlow :: Client.McpClient -> FilePath -> IO [Check]
runFlow c _projectDir = do
  -- Step 0 — scaffold so the in-process GHC API can resolve
  -- 'Maybe' / 'Either' against the standard Prelude.
  _ <- Client.callTool c GhcProject
         (object [ "action" .= ("create" :: Text), "name" .= ("info-ctor-demo" :: Text) ])

  -- Step 1 — Maybe is the canonical 2-constructor algebraic type
  -- (one nullary, one unary).
  t0 <- stepHeader 1 "ghc_info(Maybe) shows Nothing + Just (#54)"
  rMaybe <- Client.callTool c GhcInfo
              (object [ "name" .= ("Maybe" :: Text) ])
  let mDefMaybe = lookupString "definition" rMaybe
      ctorsMaybe = ctorNames rMaybe
  cMaybe <- liveCheck $ checkPure
    "Maybe definition mentions 'Nothing' and 'Just', constructors array has both"
    (defContains "Nothing" mDefMaybe
      && defContains "Just" mDefMaybe
      && "Nothing" `elem` ctorsMaybe
      && "Just"    `elem` ctorsMaybe)
    ( "Expected definition to contain 'Nothing'+'Just' AND \
      \constructors=[Nothing, Just …]. \
      \Got definition=" <> fromMaybe "<missing>" mDefMaybe
      <> ", constructors=" <> T.pack (show ctorsMaybe) )
  stepFooter 1 t0

  -- Step 2 — Either widens the test to two type vars / non-trivial
  -- constructor arities.
  t1 <- stepHeader 2 "ghc_info(Either) shows Left + Right (#54)"
  rEither <- Client.callTool c GhcInfo
               (object [ "name" .= ("Either" :: Text) ])
  let mDefEither = lookupString "definition" rEither
      ctorsEither = ctorNames rEither
  cEither <- liveCheck $ checkPure
    "Either definition mentions 'Left' and 'Right'"
    (defContains "Left" mDefEither
      && defContains "Right" mDefEither
      && "Left"  `elem` ctorsEither
      && "Right" `elem` ctorsEither)
    ( "Expected definition to contain 'Left'+'Right' AND \
      \constructors=[Left, Right]. \
      \Got definition=" <> fromMaybe "<missing>" mDefEither
      <> ", constructors=" <> T.pack (show ctorsEither) )
  stepFooter 2 t1

  -- Step 3 — A class is NOT algebraic in the user-facing sense.
  -- The response must NOT carry a 'constructors' field, preserving
  -- wire-format compat for consumers that branch on its presence.
  t2 <- stepHeader 3 "ghc_info(Functor) has no 'constructors' field (#54)"
  rFunctor <- Client.callTool c GhcInfo
                (object [ "name" .= ("Functor" :: Text) ])
  let hasCtorsKey = case lookupField "constructors" rFunctor of
        Just _  -> True
        Nothing -> False
  cFunctor <- liveCheck $ checkPure
    "Functor response has no 'constructors' field"
    (not hasCtorsKey)
    ( "Class queries must NOT carry a 'constructors' field. \
      \Got: " <> truncRender rFunctor )
  stepFooter 3 t2

  pure [cMaybe, cEither, cFunctor]

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

defContains :: Text -> Maybe Text -> Bool
defContains needle = maybe False (T.isInfixOf needle)

lookupString :: Text -> Value -> Maybe Text
lookupString k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

ctorNames :: Value -> [Text]
ctorNames v = case lookupField "constructors" v of
  Just (Array xs) ->
    [ n | Object o <- V.toList xs
        , Just (String n) <- [KeyMap.lookup (Key.fromText "name") o]
    ]
  _ -> []

truncRender :: Value -> Text
truncRender v =
  let raw = T.pack (show v)
      cap = 600
  in if T.length raw > cap then T.take cap raw <> "…(truncated)" else raw
