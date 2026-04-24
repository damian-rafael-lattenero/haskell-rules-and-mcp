-- | @ghc_add_import@ — for \"not in scope\" errors, search via
-- Hoogle and return candidate @import@ lines. Does NOT modify files
-- — the agent chooses which line to apply.
module HaskellFlows.Tool.AddImport
  ( descriptor
  , handle
  , AddImportArgs (..)
  , renderImportLine
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol
import qualified HaskellFlows.Tool.Hoogle as Hoogle

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghc_add_import"
    , tdDescription =
        "Suggest `import` lines for a name that is \"Not in scope\". "
          <> "Queries Hoogle for the name; returns candidate import "
          <> "lines ranked by Hoogle score. Does NOT modify files."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .= ("Name to look up. Examples: \"fromMaybe\", \"Map\"." :: Text)
                  ]
              , "qualified" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .= ("Render the line as `import qualified Foo as F`. Default: false." :: Text)
                  ]
              ]
          , "required"             .= ["name" :: Text]
          , "additionalProperties" .= False
          ]
    }

data AddImportArgs = AddImportArgs
  { aiName      :: !Text
  , aiQualified :: !Bool
  }
  deriving stock (Show)

instance FromJSON AddImportArgs where
  parseJSON = withObject "AddImportArgs" $ \o ->
    AddImportArgs
      <$> o .:  "name"
      <*> o .:? "qualified" .!= False

handle :: Value -> IO ToolResult
handle rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> do
    -- Delegate to Hoogle with a small query count; user wants suggestions,
    -- not an exhaustive listing.
    let hoogleArgs = object
          [ "query" .= aiName args
          , "count" .= (10 :: Int)
          ]
    hoogleRes <- Hoogle.handle hoogleArgs
    let candidates = extractModules hoogleRes
        imports = map (renderImportLine (aiQualified args)) (uniqueTop 5 candidates)
        payload = object
          [ "success" .= True
          , "name"    .= aiName args
          , "count"   .= length imports
          , "imports" .= imports
          , "hint"    .= ( "None of these are guaranteed correct — pick the \
                           \module whose context best fits your use case. \
                           \Then paste the line at the top of your .hs file \
                           \and reload with ghc_load." :: Text )
          ]
    pure ToolResult
           { trContent = [ TextContent (encodeUtf8Text payload) ]
           , trIsError = False
           }

-- | Build one @import@ line. Qualified form gets a single-letter
-- alias derived from the module's last component.
renderImportLine :: Bool -> Text -> Text
renderImportLine qualifiedMode modName
  | qualifiedMode =
      "import qualified " <> modName <> " as " <> shortAlias modName
  | otherwise =
      "import " <> modName

-- | Take the last dotted component's first letter. Falls back to
-- the module's first letter if somehow empty.
shortAlias :: Text -> Text
shortAlias m =
  let parts = T.splitOn "." m
      last_ = if null parts then m else last parts
  in T.take 1 (if T.null last_ then m else last_)

-- | Pull unique module names from a Hoogle JSON response's
-- `results[*].module` field. Best-effort — if the response shape
-- drifts we return [].
extractModules :: ToolResult -> [Text]
extractModules tr = case trContent tr of
  (TextContent t : _) ->
    case decode (TLE.encodeUtf8 (TL.fromStrict t)) of
      Just (Object o) -> case KeyMap.lookup "results" o of
        Just (Array xs) ->
          [ m | Object r <- F.toList xs
              , Just (String m) <- [KeyMap.lookup "module" r]
          ]
        _               -> []
      _ -> []
  _ -> []

uniqueTop :: Int -> [Text] -> [Text]
uniqueTop n = take n . dedupe
  where
    dedupe []       = []
    dedupe (x:xs)   = x : dedupe (filter (/= x) xs)

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
