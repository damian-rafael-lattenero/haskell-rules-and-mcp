-- | @ghci_hole@ — full GhcSession (Wave 2).
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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , loadForTarget
  , targetForPath
  )
import HaskellFlows.Mcp.Protocol
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
    { tdName        = "ghci_hole"
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
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (HoleArgs rawPath filt) ->
    case mkModulePath pd (T.unpack rawPath) of
      Left err -> pure (errorResult (formatPathError err))
      Right _ -> do
        tgt <- targetForPath ghcSess (T.unpack rawPath)
        eRes <- try (loadForTarget ghcSess tgt Deferred)
        case eRes :: Either SomeException (Bool, [GhcError]) of
          Left ex ->
            pure (errorResult ("loadForTarget failed: " <> T.pack (show ex)))
          Right (_ok, diags) -> do
            let rendered = renderGhciStyle diags
                allHoles = parseTypedHoles rendered
                holes    = case filt of
                  Nothing  -> allHoles
                  Just nm  -> filter ((== nm) . thHole) allHoles
            pure (successResult rawPath holes)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: Text -> [TypedHole] -> ToolResult
successResult mp holes =
  let payload =
        object
          [ "success"     .= True
          , "module_path" .= mp
          , "hole_count"  .= length holes
          , "holes"       .= map renderHole holes
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

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

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False
        , "error"   .= msg
        ]))
      ]
    , trIsError = True
    }

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p ->
    "Project directory is not absolute: " <> p
  PathEscapesProject a p _ ->
    "module_path '" <> a <> "' escapes project directory " <> p

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
