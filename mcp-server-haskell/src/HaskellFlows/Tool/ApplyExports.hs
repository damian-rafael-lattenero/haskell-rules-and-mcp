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

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.ModuleName (isReservedKeyword)
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcApplyExports
    , tdDescription =
        "PURPOSE: Rewrite a module's header to declare an explicit "
          <> "export list. "
          <> "WHEN: tightening a module's surface after development; "
          <> "responding to a downstream API audit that demands explicit "
          <> "exports. "
          <> "WHEN NOT: you want to know what is currently exported — "
          <> "that is ghc_browse, not this tool; the export list already "
          <> "matches your intent — re-running is a no-op anyway. "
          <> "PREREQUISITES: decide the export list first via ghc_browse "
          <> "(see what is exported now) or by reading the module. "
          <> "OUTPUT: {applied, no_change?}; idempotent — if a list is "
          <> "already present and equal, returns no_change=true. "
          <> "Validates against reserved keywords before writing. "
          <> "SEE ALSO: ghc_browse, ghc_modules."
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
        Left e -> pure (pathTraversalResult (T.pack (show e)))
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

-- | Issue #90 Phase C: applied rewrite → status='ok', payload
-- carries the (path, exports) tuple so callers know what landed.
successResult :: FilePath -> [Text] -> ToolResult
successResult path exports =
  Env.toolResponseToResult (Env.mkOk (object
    [ "path"    .= T.pack path
    , "exports" .= exports
    ]))

-- | Issue #90 Phase C: idempotent no-op → status='ok' with
-- 'no_change=True' under 'result'. Same predicate as before
-- (callers branched on @no_change@), unchanged.
noChangeResult :: FilePath -> ToolResult
noChangeResult path =
  Env.toolResponseToResult (Env.mkOk (object
    [ "path"      .= T.pack path
    , "no_change" .= True
    , "reason"    .= ("The module header already has an export list, \
                      \or no `module Foo where` line was found." :: Text)
    ]))

-- | Issue #90 Phase C: bad-input / IO failure path → status='failed',
-- kind='validation' (input was structurally fine but failed a
-- domain check or filesystem operation). Path-traversal cases are
-- caught at 'mkModulePath'.
errorResult :: Text -> ToolResult
errorResult msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.Validation msg))

-- | Issue #100 Phase C: 'mkModulePath' rejected the path (escapes
-- project root) → status='refused', kind='path_traversal'.
pathTraversalResult :: Text -> ToolResult
pathTraversalResult msg =
  Env.toolResponseToResult
    (Env.mkRefused (Env.mkErrorEnvelope Env.PathTraversal msg))

-- | ISSUE-47: structured rejection when at least one export is a
-- Haskell reserved keyword. The agent gets the offending names
-- back so it can fix and retry in one shot.
--
-- Issue #90 Phase C: status='refused' (the input was rejected by
-- a hard pre-flight gate, like newline injection / oversized
-- input) with kind='validation'. The 'rejected' / 'hint' fields
-- stay under 'result' so consumers can iterate per-bad-export.
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
        [ "rejected" .= rendered
        , "hint"     .= ("Exports must be valid Haskell entities: \
                         \function names, types, constructor sub-lists \
                         \('Foo (..)'), or module re-exports \
                         \('module Foo'). Keywords (module, where, \
                         \class, ...) are not legal exports." :: Text)
        ]
      envErr   = Env.mkErrorEnvelope Env.Validation summary
      response = (Env.mkRefused envErr) { Env.reResult = Just payload }
  in Env.toolResponseToResult response

tshow :: Show a => a -> Text
tshow = T.pack . show
