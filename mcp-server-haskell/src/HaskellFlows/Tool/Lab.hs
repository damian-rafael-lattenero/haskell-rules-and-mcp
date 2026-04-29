-- | @ghc_lab@ — module-wide property-first audit (#60).
--
-- Phase 1 (MVP, total scope estimated at 1 week): orchestrate the
-- existing primitives into a single call so the agent stops paying
-- the 12-18-round-trip tax for a module audit.
--
-- Pipeline per binding:
--   1. Walk the module's top-level signatures (regex over the
--      source — the GHC API already loaded the module via the
--      caller's preceding 'ghc_load').
--   2. Run 'Suggest.Rules.applyRules' filtered by 'min_confidence'.
--   3. For each suggestion, route through 'Tool.QuickCheck' so
--      passing properties auto-persist via the same code path as
--      the standalone tool (no duplicate code; idempotent on
--      retry).
--   4. Aggregate per-function reports.
--
-- Phase 1 deferrals (documented in the response so the agent
-- knows what to expect):
--
--   * 'generate_missing_arbitrary' — return Arbitrary suggestions.
--     Phase 1 reports an empty array; the agent runs 'ghc_arbitrary'
--     manually for now.
--   * 'determinism_runs' — Phase 1 ignores it. Phase 2 wires
--     'ghc_determinism' into the per-property loop.
--   * Coverage delta vs the project's PropertyStore.
module HaskellFlows.Tool.Lab
  ( descriptor
  , handle
  , LabArgs (..)
    -- * Pure helpers (exported for unit tests)
  , Binding (..)
  , listTopLevelBindings
  , confidenceAtLeast
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKM
import Data.Aeson.Types (parseEither)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time.Clock.POSIX (getPOSIXTime)

import HaskellFlows.Data.PropertyStore (Store)
import HaskellFlows.Ghc.ApiSession (GhcSession)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.TypeSignature (parseSignature)
import HaskellFlows.Suggest.Rules
  ( Confidence (..)
  , Suggestion (..)
  , applyRules
  )
import qualified HaskellFlows.Tool.QuickCheck as Qc
import HaskellFlows.Types
  ( ProjectDir
  , mkModulePath
  , unModulePath
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcLab
    , tdDescription =
        "Module-wide property-first audit. For every top-level "
          <> "binding in the supplied module, propose candidate "
          <> "QuickCheck laws via the same engine 'ghc_suggest' uses, "
          <> "filter by min_confidence, and run each via "
          <> "'ghc_quickcheck'. Passing properties auto-persist to "
          <> "the regression store. Phase 1 returns a per-function "
          <> "report; arbitrary-template generation and determinism "
          <> "integration are deferred to Phase 2."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path"    .= obj "string"
              , "min_confidence" .= obj "string"
              ]
          , "required"             .= (["module_path"] :: [Text])
          , "additionalProperties" .= False
          ]
    }
  where
    obj :: Text -> Value
    obj t = object [ "type" .= t ]

data LabArgs = LabArgs
  { laModulePath    :: !Text
  , laMinConfidence :: !Confidence
  }
  deriving stock (Show)

instance FromJSON LabArgs where
  parseJSON = withObject "LabArgs" $ \o -> do
    mp <- o .:  "module_path"
    mc <- o .:? "min_confidence" .!= "medium"
    pure LabArgs
      { laModulePath    = mp
      , laMinConfidence = parseConfidence mc
      }

parseConfidence :: Text -> Confidence
parseConfidence raw = case T.toLower raw of
  "low"    -> Low
  "medium" -> Medium
  "high"   -> High
  _        -> Medium

confidenceAtLeast :: Confidence -> Confidence -> Bool
confidenceAtLeast threshold candidate =
  rank candidate >= rank threshold
  where
    rank :: Confidence -> Int
    rank Low    = 0
    rank Medium = 1
    rank High   = 2

handle :: GhcSession -> Store -> ProjectDir -> Value -> IO ToolResult
handle ghcSess store pd rawArgs = case parseEither parseJSON rawArgs of
  Left err -> pure (errorResult (T.pack ("Invalid arguments: " <> err)))
  Right args -> case mkModulePath pd (T.unpack (laModulePath args)) of
    Left e   -> pure (errorResult (T.pack (show e)))
    Right mp -> do
      let full = unModulePath mp
      eBody <- try (TIO.readFile full)
                 :: IO (Either SomeException Text)
      case eBody of
        Left e -> pure (errorResult
          (T.pack ("Could not read module: " <> show e)))
        Right body -> runLab ghcSess store pd args (laModulePath args) body

runLab
  :: GhcSession -> Store -> ProjectDir
  -> LabArgs -> Text -> Text -> IO ToolResult
runLab ghcSess store pd args modulePath body = do
  t0 <- realToFrac <$> getPOSIXTime :: IO Double
  let bindings = listTopLevelBindings body
  perFn <- mapM (auditOne ghcSess store pd args modulePath) bindings
  t1 <- realToFrac <$> getPOSIXTime :: IO Double
  pure (renderReport modulePath perFn (truncate ((t1 - t0) * 1000)))

--------------------------------------------------------------------------------
-- top-level binding extraction
--------------------------------------------------------------------------------

-- | A top-level binding discovered in a module body.
data Binding = Binding
  { bName      :: !Text  -- ^ identifier
  , bSignature :: !Text  -- ^ raw signature text (after \"::\")
  }
  deriving stock (Eq, Show)

-- | Phase 1: the listing is a regex-style line-walk over the
-- module body. We pick up @name :: <sig>@ lines starting at
-- column 0. Pattern-bound declarations and class-method
-- defaults are deferred to Phase 2.
--
-- Multi-line signatures are joined: any continuation line that
-- starts with whitespace AND immediately follows a recognised
-- signature line is appended to the previous binding's
-- signature.
listTopLevelBindings :: Text -> [Binding]
listTopLevelBindings body = walk (T.lines body) Nothing []
  where
    walk [] (Just b) acc = reverse (b : acc)
    walk [] Nothing  acc = reverse acc
    walk (ln : rest) curr acc =
      case parseSignatureLine ln of
        Just b ->
          walk rest (Just b) (close curr acc)
        Nothing ->
          -- Phase 1 multi-line shape:
          --   concatPairs
          --     :: (Eq a, Show b) => [(a, b)] -> [b]
          --     concatPairs = undefined
          -- A column-0 identifier alone, followed by a
          -- whitespace-leading line that begins with @::@, is
          -- still a signature.
          case (curr, parseBareNameLine ln, looksLikeColonStart rest) of
            (_, Just nm, Just sig) ->
              walk (drop 1 rest) (Just (Binding nm sig)) (close curr acc)
            _ -> case curr of
              Just b
                | isContinuation ln ->
                    walk rest (Just b { bSignature =
                                          bSignature b <> " "
                                           <> T.strip ln }) acc
              _ -> walk rest Nothing (close curr acc)

    close Nothing  acc = acc
    close (Just b) acc = b : acc

    isContinuation ln =
      not (T.null ln)
        && not (T.null (T.takeWhile (== ' ') ln))
        && not (T.null (T.strip ln))

    -- A line that is JUST a lowercase identifier at column 0
    -- (no spaces, no symbols).
    parseBareNameLine ln =
      let stripped = T.strip ln
      in if T.takeWhile (== ' ') ln /= ""
           then Nothing
           else case T.uncons stripped of
                  Just (c, _)
                    | isAsciiLower c
                    , T.all isIdent stripped
                    -> Just stripped
                  _ -> Nothing
      where
        isIdent c = isAsciiLower c
                 || isAsciiUpper c
                 || isDigit c
                 || c == '_' || c == '\''

    -- Does the next line look like an indented '::' continuation?
    -- If yes, return the joined signature (consuming this and
    -- subsequent indented lines as one signature).
    looksLikeColonStart [] = Nothing
    looksLikeColonStart (next : afterNext) =
      let stripped = T.stripStart next
      in case T.stripPrefix ":: " stripped of
           Just rhs ->
             let extras = takeWhile isContinuation afterNext
             in Just (T.strip rhs <> " "
                       <> T.unwords (map T.strip extras))
           Nothing -> Nothing

-- | Parse a single signature line of shape @name :: <sig>@.
-- Returns 'Nothing' on anything that isn't a top-level
-- signature.
parseSignatureLine :: Text -> Maybe Binding
parseSignatureLine ln =
  let stripped = T.strip ln
  in if T.null stripped || T.takeWhile (== ' ') ln /= ""
       then Nothing
       else case T.breakOn " :: " stripped of
              (lhs, rhs)
                | not (T.null rhs)
                , isIdent lhs
                -> Just Binding
                     { bName      = lhs
                     , bSignature = T.drop 4 rhs
                     }
              _ -> Nothing
  where
    isIdent t = case T.uncons t of
      Just (c, _) -> isAsciiLower c
      Nothing     -> False

--------------------------------------------------------------------------------
-- per-binding audit
--------------------------------------------------------------------------------

data PropertyOutcome = PropertyOutcome
  { poLaw        :: !Text
  , poCategory   :: !Text
  , poConfidence :: !Confidence
  , poExpression :: !Text
  , poStatus     :: !Text   -- "passed" | "failed" | "skipped"
  , poDetail     :: !Text   -- extra info from quickcheck
  }
  deriving stock (Show)

data FunctionReport = FunctionReport
  { frName       :: !Text
  , frSignature  :: !Text
  , frProperties :: ![PropertyOutcome]
  , frReason     :: !Text   -- "" or e.g. "no-laws-matched"
  }
  deriving stock (Show)

auditOne
  :: GhcSession -> Store -> ProjectDir -> LabArgs -> Text -> Binding
  -> IO FunctionReport
auditOne ghcSess store pd args modulePath bind =
  case parseSignature (bSignature bind) of
    Nothing -> pure FunctionReport
      { frName       = bName bind
      , frSignature  = bSignature bind
      , frProperties = []
      , frReason     = "signature-parse-failed"
      }
    Just sig ->
      let suggestions = filter
            (confidenceAtLeast (laMinConfidence args) . sConfidence)
            (applyRules (bName bind) sig)
      in if null suggestions
           then pure FunctionReport
             { frName       = bName bind
             , frSignature  = bSignature bind
             , frProperties = []
             , frReason     = "no-laws-matched"
             }
           else do
             outs <- mapM (runProperty ghcSess store pd modulePath) suggestions
             pure FunctionReport
               { frName       = bName bind
               , frSignature  = bSignature bind
               , frProperties = outs
               , frReason     = ""
               }

-- | Drive 'Tool.QuickCheck.handle' once and translate its JSON
-- payload into our compact 'PropertyOutcome'. Phase 1 keys on the
-- @success@ + @state@ fields the existing tool emits — no need
-- to reach into the QC parser internals.
runProperty
  :: GhcSession -> Store -> ProjectDir -> Text -> Suggestion
  -> IO PropertyOutcome
runProperty ghcSess store pd modulePath sug = do
  let qcArgs = object
        [ "property" .= sProperty sug
        , "module"   .= modulePath
        ]
  res <- Qc.handle store ghcSess qcArgs
  let payload = decodeFirst (trContent res)
      status  = decideStatus payload
      detail  = fromMaybe "" (lookupString "raw" payload)
  pure PropertyOutcome
    { poLaw        = sLaw sug
    , poCategory   = sCategory sug
    , poConfidence = sConfidence sug
    , poExpression = sProperty sug
    , poStatus     = status
    , poDetail     = T.take 400 detail
    }
  where
    _ = pd  -- pd kept in signature for cohesion; unused in Phase 1
    decodeFirst (TextContent t : _) =
      fromMaybe Null (decode (TLE.encodeUtf8 (TL.fromStrict t)))
    decodeFirst _ = Null

decideStatus :: Value -> Text
decideStatus payload = case lookupString "state" payload of
  Just s  -> s   -- usually "passed" / "failed" / "exception" / "gave_up"
  Nothing -> case lookupBool "success" payload of
    Just True  -> "passed"
    Just False -> "failed"
    Nothing    -> "unknown"

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderReport :: Text -> [FunctionReport] -> Int -> ToolResult
renderReport modulePath fns wallMs =
  let totalProps = sum (map (length . frProperties) fns)
      passedProps = sum
        [ 1 | f <- fns, p <- frProperties f, poStatus p == "passed" ]
      coveredFns = length
        [ () | f <- fns, any ((== "passed") . poStatus) (frProperties f) ]
      uncovered  = length fns - coveredFns
      payload = object
        [ "success"            .= True
        , "module_path"        .= modulePath
        , "audited_bindings"   .= length fns
        , "covered"            .= coveredFns
        , "uncovered"          .= uncovered
        , "properties_total"   .= totalProps
        , "properties_passed"  .= passedProps
        , "wall_time_ms"       .= wallMs
        , "functions"          .= map renderFn fns
        , "arbitrary_suggestions" .= ([] :: [Value])  -- Phase 2
        , "summary"            .= summarise totalProps passedProps
                                            (length fns) coveredFns
        ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

renderFn :: FunctionReport -> Value
renderFn f = object $
  [ "name"      .= frName f
  , "signature" .= frSignature f
  ] <> case frReason f of
         "" -> [ "properties" .= map renderProp (frProperties f) ]
         r  -> [ "status" .= ("skipped" :: Text)
               , "reason" .= r
               ]

renderProp :: PropertyOutcome -> Value
renderProp p = object
  [ "law"        .= poLaw p
  , "category"   .= poCategory p
  , "confidence" .= confidenceText (poConfidence p)
  , "expression" .= poExpression p
  , "status"     .= poStatus p
  , "detail"     .= poDetail p
  ]

confidenceText :: Confidence -> Text
confidenceText Low    = "low"
confidenceText Medium = "medium"
confidenceText High   = "high"

summarise :: Int -> Int -> Int -> Int -> Text
summarise total passed nFns covered =
  T.pack (show passed) <> "/" <> T.pack (show total)
    <> " properties passed across " <> T.pack (show covered) <> "/"
    <> T.pack (show nFns) <> " functions."

errorResult :: Text -> ToolResult
errorResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success" .= False, "error" .= msg ])) ]
    , trIsError = True
    }

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode

--------------------------------------------------------------------------------
-- JSON walk helpers
--------------------------------------------------------------------------------

lookupString :: Text -> Value -> Maybe Text
lookupString k v = case lookupField k v of
  Just (String s) -> Just s
  _               -> Nothing

lookupBool :: Text -> Value -> Maybe Bool
lookupBool k v = case lookupField k v of
  Just (Bool b) -> Just b
  _             -> Nothing

lookupField :: Text -> Value -> Maybe Value
lookupField k (Object o) = AKM.lookup (AKey.fromText k) o
lookupField _ _          = Nothing
