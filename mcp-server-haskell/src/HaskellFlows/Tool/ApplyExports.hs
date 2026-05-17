-- | @ghc_apply_exports@ — rewrite a module's header to include an
-- explicit export list.
--
-- #173: if the header already has an export list and it DIFFERS from
-- the requested one, the list is REPLACED.  Only if it is already
-- identical does the tool return @{no_change: true}@.
module HaskellFlows.Tool.ApplyExports
  ( descriptor
  , handle
  , ApplyExportsArgs (..)
  , rewriteHeader
    -- * Internals exposed for unit tests
  , RewriteResult (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.PermissiveJSON (BoolField (unBoolField))
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
              , "write" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("When false (default true), preview the new header "
                       <> "without writing to disk. The response carries "
                       <> "applied=false and the would-be exports list." :: Text)
                  ]
              ]
          , "required"             .= (["module_path", "exports"] :: [Text])
          , "additionalProperties" .= False
          ]
    }

data ApplyExportsArgs = ApplyExportsArgs
  { aeModulePath :: !Text
  , aeExports    :: ![Text]
  , aeWrite      :: !Bool  -- ^ #155: default True; False = dry-run preview
  }
  deriving stock (Show)

instance FromJSON ApplyExportsArgs where
  parseJSON = withObject "ApplyExportsArgs" $ \o -> do
    mp <- o .:  "module_path"
    ex <- o .:  "exports"
    -- Default write=true (write to disk). BoolField accepts string "false".
    wr <- maybe True unBoolField <$> o .:? "write"
    pure ApplyExportsArgs
      { aeModulePath = mp
      , aeExports    = ex
      , aeWrite      = wr
      }

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
                Unchanged  -> pure (noChangeResult full)
                NoHeader   -> pure (noHeaderResult full)
                Rewritten newBody ->
                  -- #155: when write=false, return a preview with
                  -- applied=false — do NOT write to disk.
                  if not (aeWrite args)
                    then pure (previewResult full (aeExports args))
                    else do
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

-- | Outcome of 'rewriteHeader'.
data RewriteResult
  = Unchanged  -- ^ Export list already matches; nothing to do.
  | NoHeader   -- ^ No @module Foo where@ line found in the file.
  | Rewritten !Text  -- ^ New file content with the updated export list.
  deriving stock (Eq, Show)

-- | Rewrite the module header to include 'exports'.
--
-- * No header found → 'NoHeader'.
-- * Header found, no existing export list → inject it → 'Rewritten'.
-- * Header found, existing list identical to 'exports' → 'Unchanged'.
-- * Header found, existing list DIFFERS from 'exports' → replace it
--   → 'Rewritten'. (#173: was incorrectly returning 'Nothing' here.)
rewriteHeader :: [Text] -> Text -> RewriteResult
rewriteHeader exports body =
  let lns = T.lines body
      (pre, rest) = break isModuleHeader lns
  in case rest of
       []      -> NoHeader
       (h : tl)
         | "(" `T.isInfixOf` h ->
             -- #173: header already has a list. Replace it unless it's
             -- already identical to the requested list.
             let newH = replaceExportList exports h
             in if newH == h
                  then Unchanged
                  else Rewritten (T.unlines (pre <> (newH : tl)))
         | otherwise ->
             let newH = injectExports exports h
             in Rewritten (T.unlines (pre <> (newH : tl)))

-- | Replace the existing @(…)@ export list in a module header line
-- with the new 'exports'. Preserves the @module Name@ prefix and
-- the @where@ suffix.  Falls back to the original line on parse failure.
replaceExportList :: [Text] -> Text -> Text
replaceExportList exports headerLine =
  case T.breakOn "(" headerLine of
    (_, "")      -> headerLine  -- no "(" found, leave untouched
    (before, rest) ->
      -- rest = "(e1, e2, ...) where" — strip everything up to and
      -- including the last ')' to get the after-close text.
      let upToClose  = T.dropWhileEnd (/= ')') rest  -- "(e1, e2, ...)"
          afterClose = T.drop (T.length upToClose) rest  -- " where" or ""
      in if T.null upToClose
           then headerLine
           else before <> "(" <> T.intercalate ", " exports <> ")" <> afterClose

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
-- #133: include @applied=true@ so callers can distinguish a successful
-- write from the idempotent no-op path ('noChangeResult').
successResult :: FilePath -> [Text] -> ToolResult
successResult path exports =
  Env.toolResponseToResult (Env.mkOk (object
    [ "path"    .= T.pack path
    , "exports" .= exports
    , "applied" .= True
    ]))

-- | #155: dry-run preview → status='ok', applied=false.
-- The file is NOT written; the response shows the would-be export list
-- so the caller can confirm before committing with write=true.
previewResult :: FilePath -> [Text] -> ToolResult
previewResult path exports =
  Env.toolResponseToResult (Env.mkOk (object
    [ "path"    .= T.pack path
    , "exports" .= exports
    , "applied" .= False
    , "preview" .= True
    ]))

-- | Issue #90 Phase C + #173: idempotent no-op → status='ok' with
-- 'no_change=True'. Only emitted when the existing export list is
-- already identical to the requested one.
noChangeResult :: FilePath -> ToolResult
noChangeResult path =
  Env.toolResponseToResult (Env.mkOk (object
    [ "path"      .= T.pack path
    , "applied"   .= False
    , "no_change" .= True
    , "reason"    .= ("The module header already has this exact export \
                      \list — nothing to change." :: Text)
    ]))

-- | #173: no @module Foo where@ line found in the source file.
-- Distinct from the 'Unchanged' case so callers can tell the difference.
noHeaderResult :: FilePath -> ToolResult
noHeaderResult path =
  Env.toolResponseToResult (Env.mkOk (object
    [ "path"      .= T.pack path
    , "applied"   .= False
    , "no_change" .= True
    , "reason"    .= ("No 'module Foo where' declaration was found in \
                      \the file — cannot inject an export list." :: Text)
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
