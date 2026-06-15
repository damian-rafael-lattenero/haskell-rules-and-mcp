-- | @ghc_add_import@ — for \"not in scope\" errors, search via
-- Hoogle for candidate @import@ lines AND add the top hit to the
-- live GHCi interactive context so subsequent 'ghc_eval' calls can
-- use it immediately. Does NOT modify source files — the agent
-- chooses which candidate to persist to disk.
--
-- #146: previously the tool only returned candidates without touching
-- the session, making its name misleading. Now the top candidate
-- (if any) is injected into the interactive context via
-- 'parseImportDecl' + 'setContext'. The response carries
-- @session_updated@ (bool) and @added_import@ (the line that was
-- injected, or null) so callers can see what happened.
module HaskellFlows.Tool.AddImport
  ( descriptor
  , handle
  , AddImportArgs (..)
  , renderImportLine
  , extractModules
    -- * Pure helpers (exported for unit tests)
  , filterInternal         -- #204
  , prioritizeModuleMatch  -- #204
  , looksLikeModule        -- #242
    -- * Session helper (exported for unit tests)
  , addImportToSession
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum, isUpper)
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T

import GHC (InteractiveImport (IIDecl), getContext, parseImportDecl, setContext)
import System.Directory (findExecutable)

import HaskellFlows.Config (Limits)
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Mcp.Envelope (ToolResponse)
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Tool.Env (ToolEnv (..))
import qualified HaskellFlows.Tool.Hoogle as Hoogle

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcAddImport
    , tdDescription =
        "PURPOSE: Look up which module exports a name and add the top "
          <> "hit to the live GHCi session so ghc_eval can use it "
          <> "immediately. "
          <> "WHEN: a compile error reports a missing identifier and you "
          <> "need to know which module exports it; adding a transient "
          <> "import to the interactive session before evaluating an "
          <> "expression with ghc_eval. "
          <> "WHEN NOT: the name is already in scope — check via "
          <> "ghc_imports first; you want to discover names by type "
          <> "signature — use hoogle_search directly; you want to persist "
          <> "the import to a source file — use ghc_apply_exports or "
          <> "edit the file directly then ghc_load. "
          <> "PREREQUISITES: hoogle binary on PATH (ghc_toolchain "
          <> "action='status' confirms availability). "
          <> "OUTPUT: {name, count, imports, session_updated, "
          <> "added_import, hint}. The top candidate is injected into "
          <> "the GHCi session (session_updated=true) so ghc_eval works "
          <> "immediately. Does NOT modify source files. "
          <> "SEE ALSO: ghc_imports, hoogle_search, ghc_apply_exports."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .= ("Name to look up. Examples: \"fromMaybe\", \"Map.lookup\", \"Data.Map.Strict\"." :: Text)
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

handle :: ToolEnv -> Value -> IO ToolResponse
handle env rawArgs = do
  ghcSess <- teSession env
  runHandle (teLimits env) ghcSess rawArgs

runHandle :: Limits -> GhcSession -> Value -> IO ToolResponse
runHandle lim ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left err ->
    pure (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind err)
          (T.pack ("Invalid arguments: " <> err)))
            { Env.eeCause = Just (T.pack err) }))
  Right args -> do
    -- Issue #53 + #90: gate on hoogle availability up front; the
    -- 'unavailable' status is now distinct from 'failed' so the
    -- agent learns environment issues are not retryable without
    -- installing the binary.
    mPath <- findExecutable "hoogle"
    case mPath of
      Nothing -> pure unavailableHoogle
      Just _  -> do
        -- Issue #242: when the name looks like a fully-qualified module
        -- path (e.g. "Data.Map", "Data.Map.Strict"), bypass Hoogle
        -- entirely. Hoogle searches by *symbol*, so "Data.Map" returns
        -- the Map type-family from Data.List.NonEmpty.Singletons rather
        -- than the containers module. We can generate the import line
        -- directly from the module name.
        ranked <-
          if looksLikeModule (aiName args)
            then pure [aiName args]
            else do
              let hoogleArgs = object
                    [ "query" .= aiName args
                    , "count" .= (10 :: Int)
                    ]
              hoogleRes <- Hoogle.runHandle lim hoogleArgs
              let candidates = extractModules hoogleRes
              -- #204: strip .Internal modules then bring the closest
              -- name/module match to the front of the candidate list.
              pure (prioritizeModuleMatch (aiName args)
                      (filterInternal candidates))
        let imports = map (renderImportLine (aiQualified args))
                        (uniqueTop 5 ranked)
        -- #146: inject the top candidate into the live GHCi session so
        -- subsequent ghc_eval calls can use the imported name
        -- immediately, without requiring the user to reload or edit a
        -- source file.
        mSessionResult <- case imports of
          []    -> pure Nothing
          (i:_) -> Just <$> addImportToSession ghcSess i
        let sessionAdded = maybe False fst mSessionResult
            addedImport  = case mSessionResult of
              Just (True, importLine) -> toJSON importLine
              _                       -> Null
            hintText :: Text
            hintText
              | null imports =
                  "Hoogle returned no matches for '" <> aiName args
                  <> "'. Check spelling, try a fully-qualified search \
                     \(e.g. 'Map.lookup'), or look it up by type."
              | sessionAdded =
                  "Top candidate injected into the GHCi session — "
                  <> "ghc_eval can use it now. To persist it to a "
                  <> "source file, paste the import line at the top of "
                  <> "your .hs file and call ghc_load."
              | otherwise =
                  "None of these are guaranteed correct — pick the \
                  \module whose context best fits your use case. \
                  \Paste the line at the top of your .hs file and \
                  \reload with ghc_load."
            payload = object
              [ "name"            .= aiName args
              , "count"           .= length imports
              , "imports"         .= imports
              , "session_updated" .= sessionAdded
              , "added_import"    .= addedImport
              , "hint"            .= hintText
              ]
        -- Issue #90 §6: zero suggestions → status='no_match'.
        -- Hits → status='ok'. Same payload either way.
        pure $ case imports of
          [] -> Env.mkNoMatch payload
          _  -> Env.mkOk payload

-- | #146: inject @importLine@ into the live GHCi interactive context
-- by parsing it with 'parseImportDecl' and prepending the result to
-- 'getContext'. Returns @(True, importLine)@ on success,
-- @(False, errorMsg)@ when GHC rejects the line.
--
-- The change is in-memory only — source files are not touched. The
-- import persists until the next 'invalidateLoadCache' triggers a
-- fresh 'setContext' in 'withGhcSession'.
addImportToSession :: GhcSession -> Text -> IO (Bool, Text)
addImportToSession ghcSess importLine = do
  eRes <- try (withGhcSession ghcSess $ do
    ctx   <- getContext
    idecl <- parseImportDecl (T.unpack importLine)
    setContext (IIDecl idecl : ctx)
    ) :: IO (Either SomeException ())
  pure $ case eRes of
    Left  e -> (False, T.pack (show e))
    Right _ -> (True,  importLine)

-- | Discriminate the FromJSON failure shape — same heuristic as
-- the other Phase-B migrations.
parseErrorKind :: String -> Env.ErrorKind
parseErrorKind err
  | "key" `isInfixOfStr` err = Env.MissingArg
  | otherwise                = Env.TypeMismatch
  where
    isInfixOfStr needle haystack =
      let n = length needle
      in any (\i -> take n (drop i haystack) == needle)
             [0 .. length haystack - n]

-- | Issue #53 + #90: status='unavailable' (NOT 'failed') for the
-- environment-binary-missing case. Agents key on the cleaner
-- discriminator: an unavailable tool is not retryable without
-- installing the binary.
unavailableHoogle :: ToolResponse
unavailableHoogle =
  Env.mkUnavailable
    ((Env.mkErrorEnvelope Env.BinaryUnavailable
        "hoogle binary not found on PATH")
          { Env.eeRemediation =
              Just "Install hoogle (cabal install hoogle) and generate the index (hoogle generate), then retry. ghc_add_import cannot suggest imports without an indexed hoogle."
          })

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

-- | Pull unique module names from a Hoogle 'ToolResponse'.
--
-- 'Hoogle.handle' puts hits directly in reResult:
-- @{"hits":[{"module":"..."},...]}@.
-- With 'ToolResponse', reResult is already the parsed inner payload —
-- no text decoding or "result" wrapper peeling needed.
extractModules :: ToolResponse -> [Text]
extractModules tr = case Env.reResult tr of
  Just (Object o) ->
    case KeyMap.lookup "hits" o of
      Just (Array xs) ->
        [ m | Object r <- F.toList xs
            , Just (String m) <- [KeyMap.lookup "module" r]
        ]
      _ -> []
  _ -> []

-- | #204: Remove @.Internal@ modules from the candidate list.
--
-- Modules like @Data.Map.Internal@ and @Data.Sequence.Internal@ are
-- implementation-detail modules that are never part of the public API.
-- Surfacing them as the top hit (which Hoogle does when the user queries
-- by module path) gives wrong and confusing guidance.
filterInternal :: [Text] -> [Text]
filterInternal = filter (not . (".Internal" `T.isInfixOf`))

-- | #204: When the query looks like a qualified module path (contains
-- @\".\"@), reorder candidates so that exact matches come first, then
-- prefix matches, then everything else — without otherwise altering
-- Hoogle's ordering within each group.
--
-- This ensures that @ghc_add_import(name=\"Data.Map.Strict\")@ returns
-- @Data.Map.Strict@ as the top hit rather than @Data.Map.Lazy@ or
-- whatever Hoogle happens to rank highest.
prioritizeModuleMatch :: Text -> [Text] -> [Text]
prioritizeModuleMatch q mods
  | "." `T.isInfixOf` q =
      let exact  = filter (== q)                                     mods
          prefix = filter (\m -> q `T.isPrefixOf` m && m /= q)      mods
          other  = filter (\m -> not (q `T.isPrefixOf` m) && m /= q) mods
      in exact ++ prefix ++ other
  | otherwise = mods

uniqueTop :: Int -> [Text] -> [Text]
uniqueTop n = take n . dedupe
  where
    dedupe []       = []
    dedupe (x:xs)   = x : dedupe (filter (/= x) xs)

-- | Issue #242: return True when every dot-separated component of the
-- query starts with an uppercase letter — these are qualified module
-- paths like @Data.Map@, @Data.Map.Strict@, @Control.Monad@. Hoogle
-- treats them as symbol queries and returns type-family or constructor
-- hits from unrelated modules, so callers should bypass Hoogle and use
-- the name directly as the import target.
--
-- Returns False for bare names (@fromMaybe@), qualified function refs
-- (@Map.lookup@, @Data.Map.lookup@), and operator-only names (@(<>)@).
looksLikeModule :: Text -> Bool
looksLikeModule q =
  let parts = T.splitOn "." q
  in length parts >= 2
  && all isModuleComponent parts
  where
    isModuleComponent p =
      not (T.null p)
      && isUpper (T.head p)
      && T.all (\c -> isAlphaNum c || c == '_' || c == '\'') p

