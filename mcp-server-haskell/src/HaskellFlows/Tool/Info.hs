-- | @ghci_info@ — Phase-2 tool (GHC-API migrated).
--
-- Given a name, returns a structured @:info@ view: kind classification
-- (class/data/newtype/function/…), rendered definition, and list of
-- class instances. Pre-migration parsed @:i@ stdout via regex;
-- post-migration queries 'GHC.getInfo' directly and builds the same
-- 'ParsedInfo' shape from the returned 'TyThing' + @[ClsInst]@.
--
-- Boundary safety: still routes through 'sanitizeExpression' so the
-- newline/sentinel/empty/too-large rejection contract is unchanged.
module HaskellFlows.Tool.Info
  ( descriptor
  , handle
  , InfoArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import GHC
  ( Ghc
  , TyThing (AConLike, ATyCon, AnId)
  , getInfo
  , getNamesInScope
  )
import GHC.Core.TyCon
  ( isClassTyCon
  , isDataTyCon
  , isNewTyCon
  , isTypeSynonymTyCon
  )
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghci.Session (CommandError (..), sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Type
  ( InfoKind (..)
  , ParsedInfo (..)
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_info"
    , tdDescription =
        "Get detailed information about a Haskell name (function, type, "
          <> "typeclass) via the GHC API. Shows the definition, kind, "
          <> "instances, and where it's defined."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("The name to look up. Examples: \"Functor\", \
                       \\"Map.Map\", \"Maybe\", \"(++)\"" :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype InfoArgs = InfoArgs
  { iaName :: Text
  }
  deriving stock (Show)

instance FromJSON InfoArgs where
  parseJSON = withObject "InfoArgs" $ \o ->
    InfoArgs <$> o .: "name"

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (InfoArgs nm) -> case sanitizeExpression nm of
    Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
    Right safe -> do
      eRes <- try (withGhcSession ghcSess (queryInfo safe))
      pure $ case eRes of
        Left (se :: SomeException) ->
          errorResult (T.pack ("GHC API error: " <> show se))
        Right Nothing ->
          errorResult ("Not in scope: " <> safe)
        Right (Just pinfo) ->
          successResult pinfo

-- | Resolve the name in scope, query 'getInfo' including instances,
-- and build the pre-migration 'ParsedInfo' shape from its return.
queryInfo :: Text -> Ghc (Maybe ParsedInfo)
queryInfo nm = do
  names <- getNamesInScope
  let target = T.unpack nm
      matches =
        [ n
        | n <- names
        , occNameString (nameOccName n) == target
        ]
  case matches of
    []    -> pure Nothing
    (n:_) -> do
      info <- getInfo True n
      pure $ case info of
        Nothing -> Nothing
        Just (thing, _fixity, clsInsts, famInsts, _doc) ->
          Just ParsedInfo
            { piName       = nm
            , piKind       = kindFromTyThing thing
            , piDefinition = T.pack (showPprUnsafe thing)
            , piInstances  = map (T.pack . showPprUnsafe) clsInsts
                          <> map (T.pack . showPprUnsafe) famInsts
            }

-- | Classify a 'TyThing' into our enum. Mirrors what the @:i@ parser
-- guessed from the first-line syntax.
kindFromTyThing :: TyThing -> InfoKind
kindFromTyThing = \case
  AnId _      -> IkFunction
  AConLike _  -> IkData  -- a data constructor (not the type)
  ATyCon tc
    | isClassTyCon tc       -> IkClass
    | isNewTyCon tc         -> IkNewtype
    | isTypeSynonymTyCon tc -> IkTypeSynonym
    | isDataTyCon tc        -> IkData
    | otherwise             -> IkUnknown
  _           -> IkUnknown

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

successResult :: ParsedInfo -> ToolResult
successResult parsed =
  let payload =
        object
          [ "success"    .= True
          , "name"       .= piName parsed
          , "kind"       .= kindToText (piKind parsed)
          , "definition" .= piDefinition parsed
          , "instances"  .= piInstances parsed
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

kindToText :: InfoKind -> Text
kindToText = \case
  IkClass       -> "class"
  IkData        -> "data"
  IkNewtype     -> "newtype"
  IkTypeSynonym -> "type-synonym"
  IkFunction    -> "function"
  IkUnknown     -> "unknown"

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

formatCommandError :: CommandError -> Text
formatCommandError = \case
  ContainsNewline  -> "name must be a single line (no newline characters)"
  ContainsSentinel -> "name contains the internal framing sentinel and was rejected"
  EmptyInput       -> "name is empty"
  InputTooLarge sz cap ->
    "name is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
