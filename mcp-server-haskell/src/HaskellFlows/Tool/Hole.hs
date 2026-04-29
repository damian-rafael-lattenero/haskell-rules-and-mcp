-- | @ghc_hole@ — full GhcSession (Wave 2).
--
-- Loads the project via 'loadForTarget' with 'Deferred' flavour, then
-- renders the captured diagnostics in GHCi-style output so the
-- existing 'parseTypedHoles' parser (tuned for terminal output) works
-- unchanged.
module HaskellFlows.Tool.Hole
  ( descriptor
  , handle
  , HoleArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , loadForTarget
  , targetForPath
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Error (GhcError, renderGhciStyle)
import HaskellFlows.Parser.Hole
  ( HoleFit (..)
  , TypedHole (..)
  , parseTypedHoles
  , RelevantBinding (..)
  )
import HaskellFlows.Types

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcHole
    , tdDescription =
        "Load a module under -fdefer-typed-holes and return every typed "
          <> "hole with its expected type and relevant bindings. Use this "
          <> "before implementing a stub — the expected type tells you "
          <> "exactly what fits."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Path to the module to scan for holes, relative to \
                       \the project directory." :: Text)
                  ]
              , "hole_name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Optional: filter results to a specific hole \
                       \identifier (e.g. \"_x\")." :: Text)
                  ]
              ]
          , "required"             .= ["module_path" :: Text]
          , "additionalProperties" .= False
          ]
    }

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

handle :: GhcSession -> ProjectDir -> Value -> IO ToolResult
handle ghcSess pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right (HoleArgs rawPath filt) ->
    case mkModulePath pd (T.unpack rawPath) of
      Left err ->
        pure (Env.toolResponseToResult (Env.mkRefused
          ((Env.mkErrorEnvelope Env.PathTraversal (formatPathError err))
              { Env.eeField = Just "module_path" })))
      Right _ -> do
        tgt <- targetForPath ghcSess (T.unpack rawPath)
        eRes <- try (loadForTarget ghcSess tgt Deferred)
        case eRes :: Either SomeException (Bool, [GhcError]) of
          Left ex ->
            pure (Env.toolResponseToResult (Env.mkFailed
              ((Env.mkErrorEnvelope Env.InternalError
                  ("loadForTarget failed: " <> T.pack (show ex)))
                    { Env.eeCause = Just (T.pack (show ex)) })))
          Right (_ok, diags) -> do
            let rendered = renderGhciStyle diags
                allHoles = parseTypedHoles rendered
                holes    = case filt of
                  Nothing  -> allHoles
                  Just nm  -> filter ((== nm) . thHole) allHoles
                payload  = holesPayload rawPath holes
            -- Issue #90 §3 + §6: zero-holes case maps to
            -- 'no_match' (the question — "where are the typed
            -- holes?" — was well-formed; the answer is the empty
            -- set). Non-empty → 'ok'.
            pure $ Env.toolResponseToResult $ case holes of
              [] -> Env.mkNoMatch payload
              _  -> Env.mkOk payload

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
