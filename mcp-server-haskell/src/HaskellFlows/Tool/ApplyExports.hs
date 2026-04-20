-- | @ghci_apply_exports@ — rewrite a module's header to include an
-- explicit export list. Idempotent; if the header already has one,
-- returns @{no_change: true}@.
module HaskellFlows.Tool.ApplyExports
  ( descriptor
  , handle
  , ApplyExportsArgs (..)
  , rewriteHeader
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_apply_exports"
    , tdDescription =
        "Rewrite a module's header to declare an explicit export list. "
          <> "Idempotent: if an export list is already present, "
          <> "returns no_change=true."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type" .= ("string" :: Text) ]
              , "exports" .= object
                  [ "type"  .= ("array" :: Text)
                  , "items" .= object [ "type" .= ("string" :: Text) ]
                  ]
              ]
          , "required"             .= (["module_path", "exports"] :: [Text])
          , "additionalProperties" .= False
          ]
    }

data ApplyExportsArgs = ApplyExportsArgs
  { aeModulePath :: !Text
  , aeExports    :: ![Text]
  }
  deriving stock (Show)

instance FromJSON ApplyExportsArgs where
  parseJSON = withObject "ApplyExportsArgs" $ \o ->
    ApplyExportsArgs
      <$> o .: "module_path"
      <*> o .: "exports"

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> case mkModulePath pd (T.unpack (aeModulePath args)) of
    Left e -> pure (errorResult (T.pack (show e)))
    Right mp -> do
      let full = unModulePath mp
      eRead <- try (TIO.readFile full) :: IO (Either SomeException Text)
      case eRead of
        Left e -> pure (errorResult (T.pack ("Could not read: " <> show e)))
        Right body ->
          case rewriteHeader (aeExports args) body of
            Nothing -> pure (noChangeResult full)
            Just newBody -> do
              wres <- try (TIO.writeFile full newBody)
                :: IO (Either SomeException ())
              case wres of
                Left e  -> pure (errorResult (T.pack ("Could not write: " <> show e)))
                Right _ -> pure (successResult full (aeExports args))

-- | If the source has a @module Foo where@ header with no export
-- list, rewrite it to @module Foo (e1, e2, …) where@. Returns
-- 'Nothing' if an export list already exists OR if no header was
-- found — either way a no-change is the right answer.
rewriteHeader :: [Text] -> Text -> Maybe Text
rewriteHeader exports body =
  let lns = T.lines body
      (pre, rest) = break isModuleHeader lns
  in case rest of
       []      -> Nothing
       (h : tl)
         | "(" `T.isInfixOf` h -> Nothing   -- already has an export list
         | otherwise ->
             let new = injectExports exports h
             in Just (T.unlines (pre <> (new : tl)))

isModuleHeader :: Text -> Bool
isModuleHeader ln = "module " `T.isPrefixOf` T.stripStart ln

injectExports :: [Text] -> Text -> Text
injectExports exports headerLine =
  let stripped = T.stripStart headerLine
      leading = T.takeWhile (== ' ') headerLine
      -- "module Foo where" → ["module", "Foo", "where"]
      toks = T.words stripped
  in case toks of
       ("module" : name : "where" : _) ->
         leading <> "module " <> name <> " (" <> T.intercalate ", " exports
           <> ") where"
       _ -> headerLine

successResult :: FilePath -> [Text] -> ToolResult
successResult path exports =
  let payload = object
        [ "success"    .= True
        , "path"       .= T.pack path
        , "exports"    .= exports
        ]
  in ToolResult { trContent = [ TextContent (encodeUtf8Text payload) ], trIsError = False }

noChangeResult :: FilePath -> ToolResult
noChangeResult path =
  let payload = object
        [ "success"   .= True
        , "path"      .= T.pack path
        , "no_change" .= True
        , "reason"    .= ("The module header already has an export list, \
                          \or no `module Foo where` line was found." :: Text)
        ]
  in ToolResult { trContent = [ TextContent (encodeUtf8Text payload) ], trIsError = False }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
