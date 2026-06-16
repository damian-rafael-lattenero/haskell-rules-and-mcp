-- | B-1: non-blocking detection of unknown tool arguments.
--
-- The MCP server does not reject extra fields in a @tools/call@
-- (the @FromJSON@ instances simply ignore them). That silence bit
-- the dogfooding: @ghc_project(create, base_dir=…)@ passed a field
-- whose real name is @path@, so it was dropped and the scaffold
-- landed in the active project instead.
--
-- This module computes — purely — the set of top-level argument keys
-- a caller passed that the tool's declared schema does not mention,
-- plus a Levenshtein \"did you mean\" suggestion. The server attaches
-- the result as a NON-blocking 'Env.Warning' (status is unchanged);
-- it never rejects, so a benign extra field can't break a call.
--
-- Every tool's @input_schema@ carries a single flat @properties@ map
-- (the Claude API forbids top-level @oneOf@, so 'discriminatedSchema'
-- already merges every action branch's properties into one map — see
-- 'HaskellFlows.Mcp.Schema'). So reading @properties@ keys is enough;
-- no @oneOf@ traversal is needed.
module HaskellFlows.Mcp.ArgCheck
  ( schemaPropertyNames
  , unknownArgKeys
  , didYouMean
  , unknownArgsWarning
  ) where

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env

-- | The declared property names from a tool's @input_schema@ value
-- (the @properties@ object's keys). Returns @[]@ for a schema with no
-- @properties@ object.
schemaPropertyNames :: Value -> [Text]
schemaPropertyNames (Object o) =
  case KM.lookup (Key.fromText "properties") o of
    Just (Object props) -> map Key.toText (KM.keys props)
    _                    -> []
schemaPropertyNames _ = []

-- | Keys present in the caller's argument object that are NOT declared
-- in the schema's property set. Order follows the argument object.
-- A non-object @args@ (or empty declared set we still compare against)
-- yields @[]@ for non-objects.
unknownArgKeys :: [Text] -> Value -> [Text]
unknownArgKeys declared (Object o) =
  [ k | k <- map Key.toText (KM.keys o), k `notElem` declared ]
unknownArgKeys _ _ = []

-- | The closest declared name to an unknown key, within a small edit
-- distance, or 'Nothing' when nothing is plausibly close. The cutoff
-- scales with the key length so short names don't match everything.
didYouMean :: [Text] -> Text -> Maybe Text
didYouMean declared key =
  case sortOn snd [ (d, levenshtein key d) | d <- declared ] of
    ((best, dist) : _)
      | dist <= cutoff -> Just best
    _                  -> Nothing
  where
    -- allow up to ~1/3 of the key length in edits, min 1, max 3
    cutoff = max 1 (min 3 (T.length key `div` 3))

-- | Build a non-blocking warning when the caller passed unknown keys.
-- 'Nothing' when every key is recognised (so the server attaches no
-- warning). The message names each ignored key and, where one is
-- close, a \"did you mean\" suggestion.
unknownArgsWarning :: [Text] -> Value -> Maybe Env.Warning
unknownArgsWarning declared args =
  case unknownArgKeys declared args of
    []      -> Nothing
    unknown ->
      Just Env.Warning
        { Env.wKind    = Env.SlowPath
        , Env.wMessage =
            "Ignored unknown argument(s): "
              <> T.intercalate ", " (map renderOne unknown)
              <> ". These fields are not in this tool's schema and had no \
                 \effect — check the spelling against the tool's parameters."
        , Env.wExtra   = Nothing
        }
  where
    renderOne k = case didYouMean declared k of
      Just suggestion -> "'" <> k <> "' (did you mean '" <> suggestion <> "'?)"
      Nothing         -> "'" <> k <> "'"

--------------------------------------------------------------------------------
-- Levenshtein (small, allocation-light; argument keys are short)
--------------------------------------------------------------------------------

-- Canonical DP Levenshtein over the row vectors. @transform@ builds
-- the next matrix row from the previous one; @calc@ fills one cell as
-- the min of insertion / deletion / substitution.
levenshtein :: Text -> Text -> Int
levenshtein sa sb = last (foldl transform [0 .. length s1] s2)
  where
    s1 = T.unpack sa
    s2 = T.unpack sb
    transform ns@(n : ns') c = scanl calc (n + 1) (zip3 s1 ns ns')
      where
        calc z (c', x, y) = minimum [y + 1, z + 1, x + fromEnum (c /= c')]
    transform [] _ = []
