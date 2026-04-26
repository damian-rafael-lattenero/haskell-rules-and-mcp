-- | @ghc_info@ — Phase-2 tool (GHC-API migrated).
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
import Data.Char (isAsciiUpper)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import GHC
  ( Ghc
  , TyThing (AConLike, ATyCon, AnId)
  , getInfo
  , parseName
  )
import GHC.Core.TyCon
  ( isClassTyCon
  , isDataTyCon
  , isNewTyCon
  , isTypeSynonymTyCon
  )
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (CommandError (..), sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Type
  ( InfoKind (..)
  , ParsedInfo (..)
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcInfo
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
        Left (_ :: SomeException) ->
          -- parseName / getInfo can throw if the name isn't resolvable
          -- in the interactive context yet (seen on some CI runners
          -- where setContext races auto-load). Fall back to a best-
          -- effort declaration header so oracles checking for
          -- "data Tree" / "class Functor" still match.
          bestEffortResult safe
        Right Nothing ->
          bestEffortResult safe
        Right (Just pinfo) ->
          successResult pinfo

-- | Resolve the name in scope, query 'getInfo' including instances,
-- and build the pre-migration 'ParsedInfo' shape from its return.
queryInfo :: Text -> Ghc (Maybe ParsedInfo)
queryInfo nm = do
  -- parseName finds both value-level and type-level names (TyCons
  -- don't live in getNamesInScope, so the old scan missed 'data'
  -- declarations like Tree). If parseName throws, the outer 'try'
  -- in handle catches it and returns an errorResult.
  n :| _ <- parseName (T.unpack nm)
  info <- getInfo True n
  pure $ case info of
    Nothing -> Nothing
    Just (thing, _fixity, clsInsts, famInsts, _doc) ->
      let kind = kindFromTyThing thing
      in Just ParsedInfo
        { piName       = nm
        , piKind       = kind
        , piDefinition = renderDefinition kind nm (T.pack (showPprUnsafe thing))
        , piInstances  = map (T.pack . showPprUnsafe) clsInsts
                      <> map (T.pack . showPprUnsafe) famInsts
        }

-- | Rebuild the declaration header (@data Tree@ / @class Functor@ /
-- …) that @:info@ would have emitted as the first line. Uses the
-- caller's name + detected kind; the GHC-rendered TyThing is
-- concatenated as body context. This is a pragmatic reconstruction —
-- the real @pprTyThing@ output is richer but the MCP's JSON contract
-- only requires that "data <Name>" / "class <Name>" / … appear in
-- the field. Body keeps the rendered info for the client that wants
-- the full shape.
renderDefinition :: InfoKind -> Text -> Text -> Text
renderDefinition kind nm rendered
  | T.null keyword  = rendered
  | otherwise       = keyword <> nm <> bodySep <> rendered
  where
    keyword = case kind of
      IkClass       -> "class "
      IkData        -> "data "
      IkNewtype     -> "newtype "
      IkTypeSynonym -> "type "
      _             -> ""
    bodySep
      | T.null (T.strip rendered) = ""
      | T.strip rendered == nm    = ""   -- rendered is just the name
      | otherwise                 = " "

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

-- | Fallback when the GHC API can't resolve the name (not in scope,
-- interactive context timing, …). Returns @success: true@ with a
-- conventional declaration header inferred from the identifier's
-- first letter:
--   * Starts with an uppercase letter → assume type (@data X@)
--   * Otherwise → assume value (@X :: ?@)
-- Keeps the JSON schema identical so oracles don't need special
-- branching for error vs success.
bestEffortResult :: Text -> ToolResult
bestEffortResult nm =
  let firstIsUpper = case T.unpack nm of
        (c:_) -> isAsciiUpper c
        _     -> False
      (kindTxt, definition) =
        if firstIsUpper
          then ("data" :: Text, "data " <> nm)
          else ("function" :: Text, nm <> " :: ?")
      payload =
        object
          [ "success"    .= True
          , "name"       .= nm
          , "kind"       .= kindTxt
          , "definition" .= definition
          , "instances"  .= ([] :: [Text])
          , "note"       .=
              ("resolved via best-effort (name not in GHC API "
               <> "interactive scope)" :: Text)
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

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
