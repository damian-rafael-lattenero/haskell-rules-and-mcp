-- | @ghci_goto@ — jump-to-definition via GHCi's @:info@ output.
--
-- GHCi reports the source location of a name in a trailing
-- @-- Defined at \<file\>:\<line\>:\<col\>@ comment. We pull that out
-- and return it as a structured location. For names defined elsewhere
-- (e.g. @Prelude.map@) GHCi emits @-- Defined in 'Prelude'@; we
-- surface that too.
--
-- Richer jump-to-definition (cross-module, re-exports, macro-generated
-- names) belongs to HLS — a future phase can wrap @ghci_hls@ actions
-- once that tool is ported.
module HaskellFlows.Tool.Goto
  ( descriptor
  , handle
  , GotoArgs (..)
  , parseDefinedAt
  , Location (..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Text.Read (readMaybe)

import HaskellFlows.Ghci.Session
import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_goto"
    , tdDescription =
        "Return the source location where a name is defined. Parses "
          <> "the \"Defined at\" / \"Defined in\" marker from GHCi's "
          <> ":info output. For cross-module precision you'll want HLS "
          <> "(future ghci_hls tool)."
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

-- | A resolved source location. Either a concrete @file:line:col@ for
-- project-defined names, or a bare module name for imports.
data Location
  = InFile !Text !Int !Int       -- file, line, column
  | InModule !Text                -- qualified module name only
  deriving stock (Eq, Show)

handle :: Session -> Value -> IO ToolResult
handle sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (GotoArgs nm) -> case sanitizeExpression nm of
    Left e -> pure (errorResult (formatCommandError e))
    Right safe -> do
      gr <- infoOf sess safe >>= \case
        Left _    -> pure Nothing
        Right gr' -> pure (Just gr')
      pure $ case gr of
        Nothing ->
          errorResult "GHCi rejected the name (boundary sanitiser)"
        Just gr' ->
          if not (grSuccess gr')
            then errorResult (grOutput gr')
            else case parseDefinedAt (grOutput gr') of
              Just loc -> locationResult safe loc
              Nothing  -> errorResult
                ( "Could not extract a location for '" <> safe <> "'. "
               <> "Raw GHCi output was:\n" <> grOutput gr' )

--------------------------------------------------------------------------------
-- parser
--------------------------------------------------------------------------------

-- | Scan every line of @:info@ output looking for either a
-- @-- Defined at …@ or @-- Defined in '…'@ trailer.
parseDefinedAt :: Text -> Maybe Location
parseDefinedAt raw = firstJust tryLine (T.lines raw)
  where
    tryLine ln
      -- GHC 9.x commonly prints ':info' output like:
      --   simplify :: Expr -> Expr \t-- Defined at src/X.hs:9:1
      -- i.e. the marker lives AFTER the signature on the same
      -- line, not on a dedicated line. Previous code only
      -- stripped from line start; this 'splitAt ' search finds
      -- the marker anywhere.
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
  -- shape: "src/Foo.hs:12:5"
  case T.splitOn ":" (T.strip t) of
    (file : lnTxt : colTxt : _) -> do
      l <- readMaybe (T.unpack (T.filter (/= ' ') lnTxt))
      c <- readMaybe (T.unpack (T.filter (/= ' ') colTxt))
      pure (InFile file l c)
    _ -> Nothing

parseModuleLoc :: Text -> Maybe Location
parseModuleLoc t =
  -- shape: "'Prelude'" or similar with Unicode quotes.
  let stripped = T.dropAround (`elem` (" '\x2018\x2019" :: String)) (T.strip t)
  in if T.null stripped then Nothing else Just (InModule stripped)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ []     = Nothing
firstJust f (x:xs) = case f x of
  Just y  -> Just y
  Nothing -> firstJust f xs

--------------------------------------------------------------------------------
-- response shaping
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
