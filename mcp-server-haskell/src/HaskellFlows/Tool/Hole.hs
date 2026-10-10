-- | @ghc_inspect(action=hole)@ — pure payload layer.
--
-- The session-bound legacy handler died with the ApiSession backend
-- (W6.8): 'HaskellFlows.Tool.IdeBacked' serves action=hole by reading
-- ghcide diagnostics for the file and feeding the parsed holes into
-- 'holesPayload'. What remains here is the wire-shaping shared by
-- that route.
module HaskellFlows.Tool.Hole
  ( HoleArgs (..)
    -- * Pure payload shaping — shared with the ghcide backend
    -- ('HaskellFlows.Tool.IdeBacked' serves action=hole since W6)
  , holesPayload
  , renderHole
  , formatPathError
  , parseErrorKind
  ) where

import Data.Aeson
import Data.Text (Text)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Parser.Hole
  ( HoleFit (..)
  , TypedHole (..)
  , RelevantBinding (..)
  )
import HaskellFlows.Types (PathError (..))


data HoleArgs = HoleArgs
  { haModulePath :: !Text
  , haHoleName   :: !(Maybe Text)
  }
  deriving stock (Show)

instance FromJSON HoleArgs where
  parseJSON = withObject "HoleArgs" $ \o -> do
    mp <- o .:  "module_path"
    hn <- o .:? "hole_name"
    pure HoleArgs { haModulePath = mp, haHoleName = hn }

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

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Holes payload (used by both ok and no_match paths). Issue #90
-- Phase B keeps the legacy field shape ('module_path',
-- 'hole_count', 'holes') inside 'result' for the dual-shape
-- window.
holesPayload :: Text -> [TypedHole] -> Value
holesPayload mp holes = object
  [ "module_path" .= mp
  , "hole_count"  .= length holes
  , "holes"       .= map renderHole holes
  ]

renderHole :: TypedHole -> Value
renderHole h =
  object
    [ "hole"              .= thHole h
    , "expectedType"      .= thExpectedType h
    , "location"          .= object
        [ "file"   .= thFile h
        , "line"   .= thLine h
        , "column" .= thColumn h
        ]
    , "relevantBindings"  .= map renderBinding (thRelevantBindings h)
    , "validFits"         .= map renderFit (thValidFits h)
    ]
  where
    renderBinding rb =
      object
        [ "name" .= rbName rb
        , "type" .= rbType rb
        ]
    renderFit hf =
      object
        [ "name"   .= hfName hf
        , "type"   .= hfType hf
        , "source" .= hfSource hf
        ]

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p ->
    "Project directory is not absolute: " <> p
  PathEscapesProject a p _ ->
    "module_path '" <> a <> "' escapes project directory " <> p
