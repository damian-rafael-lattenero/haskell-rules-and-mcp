-- | @ghc_apply_exports@ — rewrite a module's header to include an
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
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.ModuleName (isReservedKeyword)
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcApplyExports
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
  Right args ->
    -- ISSUE-47: refuse exports that contain reserved keywords. An
    -- export list of [\"module\"] would write @module Foo (module)
    -- where@ — a parse error. Exports lexically can be lowercase
    -- (function names), so we DON'T reuse 'validateModuleName'
    -- here; we only refuse the reserved-keyword subset that is
    -- unambiguously a typo / mistake.
    case rejectedExports (aeExports args) of
      bad@(_:_) -> pure (exportRejectionResult bad)
      [] -> case mkModulePath pd (T.unpack (aeModulePath args)) of
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

-- | Filter the export list down to entries that are unambiguously
-- invalid as Haskell exports — reserved keywords. We deliberately
-- DON'T validate the full identifier grammar here: exports can be
-- lowercase function names ('foo'), uppercase types ('Foo'),
-- constructor sub-lists ('Foo (..)'), re-exports ('module Foo'),
-- or operators ('(+)'). Most of those would round-trip; only the
-- reserved-keyword case produces a guaranteed parse error in the
-- rewritten module.
rejectedExports :: [Text] -> [Text]
rejectedExports = filter (isReservedKeyword . T.strip)

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

-- | ISSUE-47: structured rejection when at least one export is a
-- Haskell reserved keyword. The agent gets the offending names
-- back so it can fix and retry in one shot.
exportRejectionResult :: [Text] -> ToolResult
exportRejectionResult badNames =
  let n        = length badNames
      summary  = "rejected " <> tshow n <> " invalid export name"
                              <> (if n == 1 then "" else "s")
                              <> "; reserved Haskell keywords cannot \
                                 \appear in an export list"
      rendered = [ object
                     [ "name"   .= name
                     , "reason" .= ("'" <> name <> "' is a reserved \
                                    \Haskell keyword and would produce \
                                    \a parse error in the rewritten \
                                    \module header" :: Text)
                     ]
                 | name <- badNames
                 ]
      payload = object
        [ "success"  .= False
        , "error"    .= summary
        , "rejected" .= rendered
        , "hint"     .= ("Exports must be valid Haskell entities: \
                         \function names, types, constructor sub-lists \
                         \('Foo (..)'), or module re-exports \
                         \('module Foo'). Keywords (module, where, \
                         \class, ...) are not legal exports." :: Text)
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = True
       }

tshow :: Show a => a -> Text
tshow = T.pack . show

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
