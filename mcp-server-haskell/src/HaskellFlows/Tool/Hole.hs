-- | @ghci_hole@ — interactive typed-hole exploration.
--
-- Loads the target module under the deferred-hole pass ('Deferred' mode)
-- so GHCi reports holes as warnings rather than failing compilation,
-- then parses those warnings into structured 'TypedHole' records.
--
-- Security: the @module_path@ argument goes through 'mkModulePath' — the
-- same traversal-safe smart constructor used by 'ghci_load' — so a
-- path-escape input is rejected at the boundary before any GHCi traffic.
module HaskellFlows.Tool.Hole
  ( descriptor
  , handle
  , HoleArgs (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol
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

handle :: Session -> ProjectDir -> Value -> IO ToolResult
handle sess pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (HoleArgs rawPath filt) ->
    case mkModulePath pd (T.unpack rawPath) of
      Left err -> pure (errorResult (formatPathError err))
      Right mp -> do
        gr <- loadModuleWith sess mp Deferred
        let allHoles = parseTypedHoles (grOutput gr)
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
