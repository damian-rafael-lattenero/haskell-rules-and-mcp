-- | @hoogle_search@ — query the local @hoogle@ binary for functions that
-- match a given name or type signature.
--
-- This is the first tool in the Haskell port that spawns an /external/
-- process unrelated to GHCi, so it's also the first one exercising a
-- different concurrency shape: short-lived @hoogle@ invocations with a
-- hard timeout (prevents a hung or missing index from blocking the
-- agent), availability detection on first use, and structured
-- line-parsing of Hoogle's plain-text output.
--
-- Security posture:
--
-- * The query is passed to 'proc' in argv form — no shell, no injection.
-- * Queries are length-capped before spawn so an abusive agent can't
--   ship a 10 MB argv.
-- * Hoogle binary resolution goes through 'findExecutable' — we don't
--   accept an arbitrary path from the caller, only the @PATH@-resolved
--   one. A caller who wants a bundled hoogle must put it on @PATH@.
module HaskellFlows.Tool.Hoogle
  ( descriptor
  , handle
  , HoogleArgs (..)
  , parseHoogleLine
  , HoogleHit (..)
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (nubBy)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (findExecutable)
import System.Exit (ExitCode (..))
import System.IO (hClose, hGetContents)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText HoogleSearch
    , tdDescription =
        "Search the local Hoogle index for functions, types, and classes. "
          <> "Accepts either a name (\"filter\") or a type signature "
          <> "(\"(a -> Bool) -> [a] -> [a]\"). Requires the `hoogle` binary "
          <> "to be installed and present on PATH; reports availability "
          <> "explicitly when it isn't."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "query" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Hoogle query. Examples: \"filter\", \
                       \\"(a -> Bool) -> [a] -> [a]\"" :: Text)
                  ]
              , "limit" .= object
                  [ "type"        .= ("integer" :: Text)
                  , "description" .=
                      ("Maximum number of hits to return. Default 10, \
                       \hard-capped at 50." :: Text)
                  ]
              ]
          , "required"             .= ["query" :: Text]
          , "additionalProperties" .= False
          ]
    }

data HoogleArgs = HoogleArgs
  { haQuery :: !Text
  , haLimit :: !Int
  }
  deriving stock (Show)

instance FromJSON HoogleArgs where
  parseJSON = withObject "HoogleArgs" $ \o -> do
    q <- o .:  "query"
    l <- o .:? "limit" .!= 10
    pure HoogleArgs { haQuery = q, haLimit = clampLimit l }

clampLimit :: Int -> Int
clampLimit n
  | n <= 0    = 1
  | n > 50    = 50
  | otherwise = n

-- | Upper bound on the query length; anything longer is rejected at the
-- boundary rather than shipped to the child.
maxQueryChars :: Int
maxQueryChars = 512

-- | Hard timeout on the 'hoogle' subprocess (microseconds).
hoogleTimeoutMicros :: Int
hoogleTimeoutMicros = 10_000000  -- 10 s

handle :: Value -> IO ToolResult
handle rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right args ->
    case validateQuery (haQuery args) of
      Left (kind, msg) ->
        pure (Env.toolResponseToResult (Env.mkRefused
          ((Env.mkErrorEnvelope kind msg) { Env.eeField = Just "query" })))
      Right q  -> do
        mPath <- findExecutable "hoogle"
        case mPath of
          Nothing ->
            pure (Env.toolResponseToResult (Env.mkUnavailable
              ((Env.mkErrorEnvelope Env.BinaryUnavailable
                  "hoogle binary not found on PATH")
                    { Env.eeRemediation =
                        Just "Install hoogle (cabal install hoogle) and generate the index (hoogle generate), then retry."
                    })))
          Just _ -> do
            res <- runHoogle q (haLimit args)
            pure (renderResult q res)

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

validateQuery :: Text -> Either (Env.ErrorKind, Text) Text
validateQuery q
  | T.null stripped =
      Left (Env.EmptyInput, "query is empty")
  | T.length stripped > maxQueryChars =
      Left ( Env.OversizedInput
           , "query exceeds " <> T.pack (show maxQueryChars) <> " characters"
           )
  | otherwise = Right stripped
  where
    stripped = T.strip q

--------------------------------------------------------------------------------
-- subprocess runner
--------------------------------------------------------------------------------

data HoogleOutcome
  = HoSuccess [HoogleHit]
  | HoTimeout
  | HoFailure !Int !Text
  deriving stock (Eq, Show)

-- | Run @hoogle --count=N "<query>"@ with a hard timeout and capture
-- stdout. Argv-form only; the query is passed as a single argument.
runHoogle :: Text -> Int -> IO HoogleOutcome
runHoogle q limit = do
  let cp = (proc "hoogle"
              [ "--count=" <> show limit
              , T.unpack q
              ])
             { std_in  = NoStream
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  -- Pull the streams on worker threads so a slow child doesn't block
  -- the waiter. We want hard-cap both the wall time AND the bytes.
  outVar <- newEmptyMVar
  errVar <- newEmptyMVar
  _ <- forkIO (hGetContents hOut >>= \s -> putMVar outVar s)
  _ <- forkIO (hGetContents hErr >>= \s -> putMVar errVar s)
  exited <- timeout hoogleTimeoutMicros (waitForProcess ph)
  case exited of
    Nothing -> do
      terminateProcess ph
      hClose hOut
      hClose hErr
      pure HoTimeout
    Just ExitSuccess -> do
      out <- takeMVar outVar
      pure (HoSuccess (parseHoogleOutput (T.pack out)))
    Just (ExitFailure code) -> do
      err <- takeMVar errVar
      pure (HoFailure code (T.pack err))

--------------------------------------------------------------------------------
-- plain-text hoogle output parser
--------------------------------------------------------------------------------

-- | One Hoogle hit. Module and name are optional because some hits
-- (e.g. keyword matches, packages) don't carry a module path.
data HoogleHit = HoogleHit
  { hhModule    :: !(Maybe Text)
  , hhName      :: !(Maybe Text)   -- ^ function / type name (F-20)
  , hhSignature :: !Text
  }
  deriving stock (Eq, Show)

-- | Hoogle plain-text format per line:
--
-- > Prelude filter :: (a -> Bool) -> [a] -> [a]
--
-- or for a package-scoped hit:
--
-- > base Prelude filter :: (a -> Bool) -> [a] -> [a]
--
-- or a \"No results\" sentinel when nothing matched.
parseHoogleOutput :: Text -> [HoogleHit]
parseHoogleOutput = mapMaybe parseHoogleLine . T.lines

parseHoogleLine :: Text -> Maybe HoogleHit
parseHoogleLine raw
  | T.null stripped                 = Nothing
  | "No results found" `T.isInfixOf` stripped = Nothing
  | otherwise =
      case T.breakOn " :: " stripped of
        (_, rest) | T.null rest -> Nothing
        (lhs, rest) ->
          let sig = T.strip (T.drop 4 rest)  -- drop " :: "
              (modTxt, nm) = splitModule lhs
              nameField = if T.null nm then Nothing else Just nm
          in Just HoogleHit { hhModule = modTxt, hhName = nameField, hhSignature = sig }
  where
    stripped = T.strip raw

-- | Pull the module prefix from an LHS like @Prelude filter@.
-- Hoogle uses whitespace so we split on the last run of spaces.
splitModule :: Text -> (Maybe Text, Text)
splitModule lhs =
  let ws = T.words lhs
  in case ws of
       []     -> (Nothing, "")
       [_]    -> (Nothing, lhs)
       xs     -> (Just (T.unwords (init xs)), last xs)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Map subprocess outcomes to the unified envelope. Issue #90 §6:
-- zero hits → 'no_match' (well-formed query, empty answer);
-- non-empty hits → 'ok'; subprocess timeout → 'timeout' with
-- 'inner_timeout'; non-zero exit code → 'failed' with
-- 'subprocess_error'.
renderResult :: Text -> HoogleOutcome -> ToolResult
renderResult q outcome = Env.toolResponseToResult $ case outcome of
  HoSuccess [] ->
    Env.mkNoMatch (hitsPayload q [])
  HoSuccess hits ->
    Env.mkOk (hitsPayload q hits)
  HoTimeout ->
    Env.mkTimeout
      ((Env.mkErrorEnvelope Env.InnerTimeout
          ("hoogle timed out after 10s for query: " <> q))
            { Env.eeCause = Just "10s subprocess budget" })
  HoFailure code err ->
    Env.mkFailed
      ((Env.mkErrorEnvelope Env.SubprocessError
          ("hoogle exited with code " <> T.pack (show code)
           <> " for query '" <> q <> "'"))
            { Env.eeCause = Just (T.strip err) })

hitsPayload :: Text -> [HoogleHit] -> Value
hitsPayload q hits =
  let unique = nubBy sameHit hits  -- F-19: dedup by (module, signature)
  in object
       [ "query" .= q
       , "count" .= length unique
       , "hits"  .= map renderHit unique
       ]
  where
    sameHit a b = hhModule a == hhModule b && hhSignature a == hhSignature b

renderHit :: HoogleHit -> Value
renderHit h = object
  [ "module"    .= hhModule h
  , "name"      .= hhName h       -- F-20: function/type name
  , "signature" .= hhSignature h
  ]
