-- | @ghci_goto@ — Phase-2 tool (GHC-API migrated).
--
-- Returns the source location where a name is defined. Pre-migration
-- parsed "Defined at" / "Defined in" markers from @:info@ output;
-- post-migration queries the 'Name''s 'SrcSpan' directly.
--
-- Richer jump-to-definition (cross-module re-exports, macro-generated
-- names) still belongs to HLS — a future phase will wrap an
-- @ghci_hls@ tool once that lands.
module HaskellFlows.Tool.Goto
  ( descriptor
  , handle
  , GotoArgs (..)
  , parseDefinedAt
  , Location (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Text.Read (readMaybe)

import GHC
  ( Ghc
  , Name
  , getNamesInScope
  , moduleName
  , nameSrcSpan
  )
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name (nameModule_maybe, nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.SrcLoc
  ( SrcSpan (RealSrcSpan, UnhelpfulSpan)
  , srcSpanFile
  , srcSpanStartCol
  , srcSpanStartLine
  )
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghci.Session (CommandError (..), sanitizeExpression)
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_goto"
    , tdDescription =
        "Return the source location where a name is defined, via the "
          <> "GHC API's SrcSpan. For cross-module precision you'll want "
          <> "HLS (future ghci_hls tool)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name to locate. Examples: \"greet\", \"Functor\"."
                       :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype GotoArgs = GotoArgs
  { gaName :: Text
  }
  deriving stock (Show)

instance FromJSON GotoArgs where
  parseJSON = withObject "GotoArgs" $ \o ->
    GotoArgs <$> o .: "name"

-- | A resolved source location. Either a concrete @file:line:col@
-- (project-defined names) or a bare module name (for names resolved
-- to an imported module without a local SrcSpan).
data Location
  = InFile !Text !Int !Int
  | InModule !Text
  deriving stock (Eq, Show)

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (GotoArgs nm) -> case sanitizeExpression nm of
    Left e -> pure (errorResult (formatCommandError e))
    Right safe -> do
      eRes <- try (withGhcSession ghcSess (queryLocation safe))
      pure $ case eRes of
        Left (se :: SomeException) ->
          errorResult (T.pack ("GHC API error: " <> show se))
        Right Nothing ->
          errorResult ("Could not locate '" <> safe <> "'. Not in scope.")
        Right (Just loc) ->
          locationResult safe loc

-- | Match names in the interactive scope by exact occurrence name,
-- then promote the 'SrcSpan' to a structured 'Location'.
queryLocation :: Text -> Ghc (Maybe Location)
queryLocation nm = do
  names <- getNamesInScope
  let target = T.unpack nm
      matches =
        [ n
        | n <- names
        , occNameString (nameOccName n) == target
        ]
  case matches of
    []    -> pure Nothing
    (n:_) -> pure (Just (nameToLocation n))

nameToLocation :: Name -> Location
nameToLocation n = case nameSrcSpan n of
  RealSrcSpan rspan _ ->
    InFile
      (T.pack (unpackFS (srcSpanFile rspan)))
      (srcSpanStartLine rspan)
      (srcSpanStartCol rspan)
  UnhelpfulSpan _ ->
    case nameModule_maybe n of
      Just m  -> InModule (T.pack (showPprUnsafe (moduleName m)))
      Nothing -> InModule "<unknown>"

--------------------------------------------------------------------------------
-- legacy parser (retained for unit-test back-compat)
--------------------------------------------------------------------------------

-- | Kept for the existing unit tests that validate the pre-migration
-- parser. The live code path no longer calls this — the GHC API
-- returns 'SrcSpan' directly. Retire when the subprocess-ghci backing
-- retires in Phase 7.
parseDefinedAt :: Text -> Maybe Location
parseDefinedAt raw = firstJust tryLine (T.lines raw)
  where
    tryLine ln
      | Just rest <- findMarker "-- Defined at " ln = parseFileLoc rest
      | Just rest <- findMarker "-- Defined in " ln = parseModuleLoc rest
      | otherwise = Nothing

    findMarker marker ln =
      let (_, after) = T.breakOn marker ln
      in if T.null after
           then Nothing
           else Just (T.drop (T.length marker) after)

parseFileLoc :: Text -> Maybe Location
parseFileLoc t =
  case T.splitOn ":" (T.strip t) of
    (file : lnTxt : colTxt : _) -> do
      l <- readMaybe (T.unpack (T.filter (/= ' ') lnTxt))
      c <- readMaybe (T.unpack (T.filter (/= ' ') colTxt))
      pure (InFile file l c)
    _ -> Nothing

parseModuleLoc :: Text -> Maybe Location
parseModuleLoc t =
  let stripped = T.dropAround (`elem` (" '\x2018\x2019" :: String)) (T.strip t)
  in if T.null stripped then Nothing else Just (InModule stripped)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ []     = Nothing
firstJust f (x:xs) = case f x of
  Just y  -> Just y
  Nothing -> firstJust f xs

--------------------------------------------------------------------------------
-- response shaping (unchanged schema)
--------------------------------------------------------------------------------

locationResult :: Text -> Location -> ToolResult
locationResult nm loc =
  let payload = case loc of
        InFile f l c ->
          object
            [ "success" .= True
            , "name"    .= nm
            , "kind"    .= ("file" :: Text)
            , "file"    .= f
            , "line"    .= l
            , "column"  .= c
            ]
        InModule m ->
          object
            [ "success" .= True
            , "name"    .= nm
            , "kind"    .= ("module" :: Text)
            , "module"  .= m
            ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

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
