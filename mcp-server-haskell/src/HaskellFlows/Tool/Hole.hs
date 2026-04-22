-- | @ghci_hole@ — Phase-3 tool (GHC-API migrated).
--
-- Loads the project under 'Deferred' mode and returns structured
-- typed-hole information. Schema preserved — the MVP port captures
-- diagnostic messages via the log hook and runs them through the
-- pre-migration 'parseTypedHoles' parser. Relevant bindings + valid
-- fits parse when the logger's SDoc rendering matches the legacy
-- GHCi output shape; when the rendering diverges they come back as
-- empty lists, with the hole identifier + expected type still intact.
--
-- Security: the @module_path@ argument is still validated through
-- 'mkModulePath' so traversal refusal is preserved at the boundary.
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

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (Deferred)
  , loadAndCaptureDiagnostics
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error (GhcError (..))
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
      Right _  -> do
        (_, diags) <- loadAndCaptureDiagnostics ghcSess Deferred
        let bundled = T.unlines [ geMessage d | d <- diags, looksLikeHole d ]
            allHoles = parseTypedHoles bundled
            holes = case filt of
              Nothing -> allHoles
              Just nm -> filter ((== nm) . thHole) allHoles
        pure (successResult rawPath holes)
  where
    looksLikeHole d =
      "hole" `T.isInfixOf` T.toLower (geMessage d)

--------------------------------------------------------------------------------
-- response shaping (schema preserved)
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
