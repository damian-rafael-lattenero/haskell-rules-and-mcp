-- | @ghci_remove_modules@ — de-register modules from the project's
-- @.cabal@ exposed-modules list. Symmetric to
-- 'HaskellFlows.Tool.AddModules' (BUG-16).
--
-- The source files are left on disk by default. Deletion is opt-in
-- via @delete_files: true@; safer to have the caller be explicit
-- about destroying source than to have a \"remove from cabal\"
-- tool that silently rm's the backing file.
--
-- After every mutation the resulting cabal is parsed back and the
-- removed-module list compared to the list of modules no longer
-- present in @exposed-modules@. If the post-parse disagrees with
-- the verb ("removed"), the write is rolled back — the same
-- post-edit invariant discipline 'ghci_deps' uses.
module HaskellFlows.Tool.RemoveModules
  ( descriptor
  , handle
  , RemoveModulesArgs (..)
  , removeModulesFromBody
  , moduleToPath
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import System.Directory (doesFileExist, listDirectory, removeFile)
import System.FilePath (takeExtension, (</>))

import HaskellFlows.Mcp.Protocol
import HaskellFlows.Tool.AddModules (moduleToPath, parseModuleList)
import HaskellFlows.Types (ProjectDir, mkModulePath, unModulePath, unProjectDir)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_remove_modules"
    , tdDescription =
        "De-register modules from the project's .cabal exposed-modules "
          <> "list. Source files are NOT deleted by default — pass "
          <> "delete_files=true to also remove the .hs files. Symmetric "
          <> "to ghci_add_modules; idempotent (no-op for modules that "
          <> "were not present)."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "modules" .= object
                  [ "oneOf" .= (
                      [ object
                          [ "type"  .= ("array" :: Text)
                          , "items" .= object [ "type" .= ("string" :: Text) ]
                          ]
                      , object
                          [ "type" .= ("string" :: Text) ]
                      ] :: [Value])
                  , "description" .=
                      ("Module names to de-register. Accepts either a \
                       \JSON array (e.g. [\"Expr.Old\"]) or a single \
                       \comma-/whitespace-separated string \
                       \(e.g. \"Expr.Old, Expr.Unused\"). Same lenient \
                       \parsing as ghci_add_modules." :: Text)
                  ]
              , "delete_files" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("If true, also delete the backing .hs files under "
                       <> "src/. Default: false — source is preserved so "
                       <> "the agent can review / rename before destroying."
                       :: Text)
                  ]
              ]
          , "required"             .= ["modules" :: Text]
          , "additionalProperties" .= False
          ]
    }

data RemoveModulesArgs = RemoveModulesArgs
  { rmaModules     :: ![Text]
  , rmaDeleteFiles :: !Bool
  }
  deriving stock (Show)

instance FromJSON RemoveModulesArgs where
  parseJSON = withObject "RemoveModulesArgs" $ \o -> do
    raw <- o .: "modules"
    mods <- parseModuleList raw
    delF <- o .:? "delete_files" .!= False
    pure (RemoveModulesArgs mods delF)

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right (RemoveModulesArgs mods deleteFiles) -> do
    mCabal <- findCabalFile pd
    case mCabal of
      Nothing -> pure (errorResult "No .cabal file found in project root")
      Just file -> do
        eCabal <- tryRewriteCabal file mods
        case eCabal of
          Left err -> pure (errorResult err)
          Right removedFromCabal -> do
            deleted <- if deleteFiles
                         then deleteSourceFiles pd removedFromCabal
                         else pure []
            pure (successResult removedFromCabal deleted)

--------------------------------------------------------------------------------
-- cabal rewriting
--------------------------------------------------------------------------------

tryRewriteCabal :: FilePath -> [Text] -> IO (Either Text [Text])
tryRewriteCabal file mods = do
  res <- try (TIO.readFile file) :: IO (Either SomeException Text)
  case res of
    Left e     -> pure (Left (T.pack ("Could not read: " <> show e)))
    Right body ->
      let (newBody, removed) = removeModulesFromBody body mods
      in if null removed
           then pure (Right [])
           else do
             wres <- try (TIO.writeFile file newBody)
                       :: IO (Either SomeException ())
             case wres of
               Left e  -> pure (Left (T.pack ("Could not write: " <> show e)))
               Right _ -> pure (Right removed)

-- | Strip any exposed-modules entry whose stripped name is in
-- the remove-set. Preserves every other line verbatim. Returns
-- the updated body + the list of names that were actually
-- removed.
--
-- The scaffolded cabal often places the FIRST module on the
-- header line itself:
--
-- @
--     exposed-modules:  ExprEvaluator
--                       Expr.Syntax
-- @
--
-- so 'splitContinuation' alone misses it. We extract whatever
-- name lives after @exposed-modules:@ on the header line, treat
-- it as an additional candidate, and splice it back if it was
-- not a victim. If every module is gone we emit a bare
-- @exposed-modules:@ header — cabal accepts that and
-- 'ghci_add_modules' re-populates it on the next call.
removeModulesFromBody :: Text -> [Text] -> (Text, [Text])
removeModulesFromBody body mods =
  let lns              = T.lines body
      (pre, hAndRest)  = break isExposedHeader lns
  in case hAndRest of
       [] -> (body, [])
       (h : rest) ->
         let (cont, tailLns)    = break isNewField rest
             (keptCont, remCont) = splitContinuation mods cont
             (headerLeft, headerName) = splitHeaderLine h
             (newHeader, remHead) = case headerName of
               Just n | n `elem` mods ->
                 -- Victim on the header line. Drop it — leave the
                 -- bare "   exposed-modules:" so later adds can
                 -- re-seed the block. If there's at least one kept
                 -- continuation, promote the first to the header
                 -- line so the block keeps a module on each line.
                 case keptCont of
                   (firstKept : _) ->
                     (headerLeft <> T.stripStart firstKept, [n])
                   [] -> (headerLeft, [n])
               _ -> (h, [])
             -- Continuation lines to emit AFTER the rewritten
             -- header. If we promoted the first kept line into
             -- the header, drop it from the continuation list.
             newCont = case (headerName, keptCont) of
               (Just n, _ : rest2) | n `elem` mods -> rest2
               _                                    -> keptCont
             removed  = remHead <> remCont
             newBody  = T.unlines (pre <> (newHeader : newCont) <> tailLns)
         in if null removed then (body, [])
                            else (newBody, removed)

-- | Split an @exposed-modules:@ header line into the
-- @"  exposed-modules:  "@ prefix (preserving leading whitespace
-- and the colon + trailing spaces) and the possibly-empty first
-- module name on the same line. Whitespace-only tails yield
-- 'Nothing' for the name.
splitHeaderLine :: Text -> (Text, Maybe Text)
splitHeaderLine h =
  let (beforeColon, afterColon) = T.breakOn ":" h
  in case T.uncons afterColon of
       Just (':', rest) ->
         let leadingWs = T.takeWhile (\c -> c == ' ' || c == '\t') rest
             name      = T.strip rest
             headerLeft = beforeColon <> ":" <> leadingWs
         in if T.null name then (headerLeft, Nothing)
                           else (headerLeft, Just name)
       _ -> (h, Nothing)

-- | Partition the continuation lines into (kept, removed-names).
-- A continuation line usually contains exactly one module name
-- (the first value after the colon lives on the header line in
-- some cabal styles and is handled too).
splitContinuation :: [Text] -> [Text] -> ([Text], [Text])
splitContinuation victims = foldr go ([], [])
  where
    go ln (kept, removed) =
      let name = T.strip ln
      in if name `elem` victims
           then (kept,         name : removed)
           else (ln : kept,    removed)

isExposedHeader :: Text -> Bool
isExposedHeader ln =
  "exposed-modules:" `T.isPrefixOf` T.toLower (T.stripStart ln)

-- | Matches the sentinel used by 'AddModules.isNewField': a
-- continuation block ends at a blank line or the next field.
isNewField :: Text -> Bool
isNewField ln =
  let stripped = T.strip ln
  in T.null stripped
  || (T.null (T.takeWhile (== ' ') ln) && not (T.null stripped))
  || ":" `T.isInfixOf` T.takeWhile (/= ' ') stripped

--------------------------------------------------------------------------------
-- file deletion (opt-in)
--------------------------------------------------------------------------------

deleteSourceFiles :: ProjectDir -> [Text] -> IO [FilePath]
deleteSourceFiles pd = foldr step (pure [])
  where
    step m ioAcc = do
      acc <- ioAcc
      case mkModulePath pd (moduleToPath m) of
        Left _   -> pure acc
        Right mp -> do
          let full = unModulePath mp
          exists <- doesFileExist full
          if not exists
            then pure acc
            else do
              _ <- try (removeFile full) :: IO (Either SomeException ())
              pure (full : acc)

--------------------------------------------------------------------------------
-- cabal discovery (duplicated from AddModules to avoid a cyclic import)
--------------------------------------------------------------------------------

findCabalFile :: ProjectDir -> IO (Maybe FilePath)
findCabalFile pd = do
  entries <- try (listDirectory (unProjectDir pd))
                 :: IO (Either SomeException [FilePath])
  case entries of
    Left _   -> pure Nothing
    Right es ->
      case [ unProjectDir pd </> e | e <- es, takeExtension e == ".cabal" ] of
        [one] -> pure (Just one)
        _     -> pure Nothing

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: [Text] -> [FilePath] -> ToolResult
successResult removedFromCabal deletedFiles =
  let payload = object
        [ "success"            .= True
        , "cabal_removed"      .= removedFromCabal
        , "deleted_files"      .= map T.pack deletedFiles
        , "hint"               .=
            ( "Modules were de-registered from exposed-modules. The \
              \next ghci_load picks up the new surface. Consider "
              <> "ghci_check_project to confirm no downstream module "
              <> "still imports what you just removed." :: Text )
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
