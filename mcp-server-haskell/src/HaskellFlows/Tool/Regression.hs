-- | @ghci_regression@ — replay every persisted QuickCheck property as a
-- regression suite.
--
-- Properties are auto-saved by 'HaskellFlows.Tool.QuickCheck' the first
-- time they pass. This tool lets the agent re-verify the whole suite in
-- one call — essential at the start of a session to confirm nothing
-- drifted, or after a large refactor.
--
-- Actions:
--
-- * @list@ — return the stored property inventory without running
--   anything. Useful for introspection.
-- * @run@ (default) — execute every stored property. Aggregates the
--   per-property outcomes into a summary + list of regressions.
--
-- No new property is ever persisted by this tool — it's a
-- /verification/ layer, not a capture layer.
module HaskellFlows.Tool.Regression
  ( descriptor
  , handle
  , RegressionArgs (..)
  , Action (..)
    -- * Reusable runners for other tools (e.g. Tool.Gate)
  , Replay (..)
  , runOne
    -- * Pure helpers exposed for unit tests
  , parseShowModulesPaths
  ) where

import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Data.PropertyStore
  ( Store
  , StoredProperty (..)
  , loadAll
  )
import HaskellFlows.Ghci.Session
  ( Session
  , GhciResult (..)
  , execute
  , runProperty
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_regression"
    , tdDescription =
        "Replay every persisted QuickCheck property as a regression "
          <> "suite. Actions: 'list' (inspect the store without running), "
          <> "'run' (execute all). Properties are auto-persisted by "
          <> "ghci_quickcheck on first pass."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "action" .= object
                  [ "type"        .= ("string" :: Text)
                  , "enum"        .= (["list", "run"] :: [Text])
                  , "description" .= ("Default: 'run'." :: Text)
                  ]
              ]
          , "additionalProperties" .= False
          ]
    }

data Action = ActList | ActRun
  deriving stock (Eq, Show)

newtype RegressionArgs = RegressionArgs
  { raAction :: Action
  }
  deriving stock (Show)

instance FromJSON RegressionArgs where
  parseJSON = withObject "RegressionArgs" $ \o -> do
    mAct <- o .:? "action"
    a <- case mAct :: Maybe Text of
      Nothing     -> pure ActRun
      Just "list" -> pure ActList
      Just "run"  -> pure ActRun
      Just other  -> fail ("unknown action: " <> T.unpack other)
    pure (RegressionArgs a)

handle :: Store -> Session -> Value -> IO ToolResult
handle store sess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (RegressionArgs a) -> do
    props <- loadAll store
    case a of
      ActList -> pure (listResult props)
      ActRun  -> do
        -- Snapshot the caller's scope before we start re-loading
        -- modules per property. Without this, a run that touches
        -- several test-suite modules leaves GHCi pointing at the
        -- last one, and the next @ghci_eval@ from the caller
        -- fails with "Variable not in scope: main" / similar.
        scopeBefore <- snapshotLoadedModules sess
        results     <- mapM (runOne sess) props
        restoreLoadedModules sess scopeBefore
        pure (runResult results)

--------------------------------------------------------------------------------
-- running
--------------------------------------------------------------------------------

-- | One stored property's replay result, ready for JSON shaping.
data Replay = Replay
  { rpStored :: !StoredProperty
  , rpResult :: !QuickCheckResult
  }

runOne :: Session -> StoredProperty -> IO Replay
runOne sess sp = do
  -- If the store knows which file defines the property, re-load
  -- that file first so the identifier is guaranteed in scope. A
  -- missing or invalid path is a silent no-op — 'runProperty' will
  -- surface the resulting "Variable not in scope" as an unparsed
  -- regression. (The path was validated when persisted.)
  case spModule sp of
    Just m | not (T.null m) ->
      void (execute sess (":load " <> m))
    _ -> pure ()
  res <- runProperty sess (spExpression sp)
  let qr = case res of
        Left _   -> QcUnparsed (spExpression sp)
                     "boundary-sanitiser rejected the stored expression"
        Right gr -> parseQuickCheckOutput (spExpression sp) (grOutput gr)
  pure Replay { rpStored = sp, rpResult = qr }

--------------------------------------------------------------------------------
-- scope snapshot / restore
--
-- We pay one extra @:show modules@ + one @:load <paths>@ at the end
-- of every regression run so callers aren't surprised by their
-- previously-loaded test-suite module having been knocked out of
-- scope by the replay loop. Both primitives are best-effort: if
-- @:show modules@ cannot be parsed we just don't restore anything.
--------------------------------------------------------------------------------

snapshotLoadedModules :: Session -> IO [Text]
snapshotLoadedModules sess = do
  GhciResult raw ok <- execute sess ":show modules"
  pure $ if ok then parseShowModulesPaths raw else []

restoreLoadedModules :: Session -> [Text] -> IO ()
restoreLoadedModules _sess []    = pure ()    -- nothing to restore
restoreLoadedModules sess  paths =
  -- Pass the LAST path only. GHCi's ':show modules' emits
  -- dependents after their dependencies, so the last entry is
  -- (with overwhelming probability) the target module the caller
  -- had set with their earlier ':load'. A single-argument ':load'
  -- on that file pulls in the transitive imports automatically
  -- and unambiguously sets the target module — which in turn
  -- governs what's in scope for the next ':eval' or ':info'.
  --
  -- Multi-argument ':load A.hs B.hs C.hs' was observed to leave
  -- the target-module scope in an inconsistent state on GHC 9.12
  -- for multi-module test-suites (the subsequent ':eval' saw
  -- neither the Main-binding-level exports nor the qualified
  -- imports from the would-be target). Use the one-file form.
  void (execute sess (":load " <> last paths))

-- | Parse the output of GHCi's @:show modules@ into the list of file
-- paths it references. Output lines look like:
--
-- >   Foo              ( src/Foo.hs, interpreted )
-- >   Main             ( test/Spec.hs, interpreted )
--
-- Anything that doesn't match the @Name ( path, kind )@ shape is
-- silently skipped. Exposed for unit tests so the parser's
-- tolerance to GHC-version drift can be pinned without a live
-- session.
parseShowModulesPaths :: Text -> [Text]
parseShowModulesPaths raw =
  [ path
  | ln <- T.lines raw
  , Just path <- [pathFromLine ln]
  ]
  where
    pathFromLine ln =
      let (_, afterOpen) = T.breakOn "(" ln
      in if T.null afterOpen
           then Nothing
           else
             let inside          = T.strip (T.drop 1 afterOpen)
                 (rawPath, _)    = T.breakOn "," inside
                 pathStripped    = T.strip rawPath
             in if T.null pathStripped
                  then Nothing
                  else Just pathStripped

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

listResult :: [StoredProperty] -> ToolResult
listResult props =
  let payload =
        object
          [ "success"    .= True
          , "action"     .= ("list" :: Text)
          , "count"      .= length props
          , "properties" .= map renderStored props
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

runResult :: [Replay] -> ToolResult
runResult replays =
  let total        = length replays
      regressions  = filter (not . isPass . rpResult) replays
      regressed    = length regressions
      payload =
        object
          [ "success"     .= (regressed == 0)
          , "action"      .= ("run" :: Text)
          , "total"       .= total
          , "passed"      .= (total - regressed)
          , "regressions" .= map renderRegression regressions
          , "summary"     .= summarise total regressed
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = regressed > 0
       }

isPass :: QuickCheckResult -> Bool
isPass (QcPassed _ _) = True
isPass _              = False

summarise :: Int -> Int -> Text
summarise 0 _ =
  "No stored properties. Run ghci_quickcheck and it'll auto-persist on pass."
summarise total 0 =
  T.pack (show total) <> " / " <> T.pack (show total) <> " stored properties pass."
summarise total regressed =
  T.pack (show regressed) <> " of " <> T.pack (show total) <> " stored \
  \properties regressed. Details in 'regressions'."

renderStored :: StoredProperty -> Value
renderStored sp =
  object
    [ "expression" .= spExpression sp
    , "module"     .= spModule sp
    , "passed"     .= spPassed sp
    , "updated"    .= spUpdated sp
    ]

renderRegression :: Replay -> Value
renderRegression r =
  object
    [ "expression" .= spExpression (rpStored r)
    , "module"     .= spModule (rpStored r)
    , "outcome"    .= renderOutcome (rpResult r)
    ]

renderOutcome :: QuickCheckResult -> Value
renderOutcome = \case
  QcPassed _ n          -> object [ "state" .= ("passed" :: Text), "passed" .= n ]
  QcFailed _ n shr cex  -> object
                             [ "state"          .= ("failed" :: Text)
                             , "passed"         .= n
                             , "shrinks"        .= shr
                             , "counterexample" .= cex
                             ]
  QcException _ err     -> object [ "state" .= ("exception" :: Text), "error" .= err ]
  QcGaveUp _ n disc     -> object
                             [ "state"     .= ("gave_up" :: Text)
                             , "passed"    .= n
                             , "discarded" .= disc
                             ]
  QcUnparsed _ raw      -> object [ "state" .= ("unparsed" :: Text), "raw" .= raw ]

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

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
