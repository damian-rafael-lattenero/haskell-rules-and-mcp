-- | @ghci_toolchain_status@ — single-call availability inventory for
-- every external binary the MCP can delegate to.
--
-- The TS port solves this ad-hoc per tool (each one has its own
-- \"tool unavailable\" branch). That's fine until an agent asks
-- \"can I use hoogle right now?\" without actually wanting to run
-- one — the only way to know is to try and parse the failure.
-- This tool gives a structured answer in one call.
--
-- Returns, for every probed binary:
--
-- * @available@ — boolean, from 'findExecutable'
-- * @path@ — absolute resolved path, when available
-- * @version@ — best-effort @tool --version@ first line (nil on
--   timeout / parse failure — not treated as \"unavailable\", just
--   \"version unknown\")
-- * @category@ — @gate@ (blocks CI), @workflow@ (optional), @query@
--
-- Pure query; does not mutate session state.
module HaskellFlows.Tool.ToolchainStatus
  ( descriptor
  , handle
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
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

import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_toolchain_status"
    , tdDescription =
        "Report availability + version of every external binary the "
          <> "MCP can delegate to (cabal, ghc, hlint, fourmolu, "
          <> "ormolu, hoogle, hls). Read-only; no session mutation."
    , tdInputSchema =
        object
          [ "type"                 .= ("object" :: Text)
          , "properties"           .= object []
          , "additionalProperties" .= False
          ]
    }

-- | (binary name, version-flag, category).
--
-- Category convention:
--
-- * @gate@ — required for module-complete gates (cabal, ghc, hlint).
-- * @workflow@ — enables a named workflow (fourmolu/ormolu for
--   format, hls for refactor, hoogle for search).
-- * @query@ — optional improvement but not blocking.
probeTargets :: [(Text, String, Text)]
probeTargets =
  [ ("cabal",    "--numeric-version", "gate")
  , ("ghc",      "--numeric-version", "gate")
  , ("hlint",    "--version",         "gate")
  , ("fourmolu", "--version",         "workflow")
  , ("ormolu",   "--version",         "workflow")
  , ("hoogle",   "--version",         "query")
  , ("hls",      "--numeric-version", "workflow")
  , ("haskell-language-server", "--numeric-version", "workflow")
  ]

versionTimeoutMicros :: Int
versionTimeoutMicros = 3_000_000  -- 3s per binary

handle :: Value -> IO ToolResult
handle rawArgs = case parseEither parseJSON rawArgs :: Either String Value of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right _ -> do
    entries <- mapM probeOne probeTargets
    pure (renderResult entries)

--------------------------------------------------------------------------------
-- probe
--------------------------------------------------------------------------------

data Entry = Entry
  { eName      :: !Text
  , eCategory  :: !Text
  , eAvailable :: !Bool
  , ePath      :: !(Maybe FilePath)
  , eVersion   :: !(Maybe Text)
  }

probeOne :: (Text, String, Text) -> IO Entry
probeOne (name, verFlag, category) = do
  mPath <- findExecutable (T.unpack name)
  case mPath of
    Nothing ->
      pure Entry
        { eName = name, eCategory = category
        , eAvailable = False, ePath = Nothing, eVersion = Nothing
        }
    Just p -> do
      mVer <- getVersion p verFlag
      pure Entry
        { eName = name, eCategory = category
        , eAvailable = True, ePath = Just p, eVersion = mVer
        }

-- | Capture a best-effort first line of @tool --version@ with a hard
-- timeout. Failure to parse returns 'Nothing' — the tool is still
-- available, we just didn't manage to extract a version string.
getVersion :: FilePath -> String -> IO (Maybe Text)
getVersion bin verFlag = do
  let cp = (proc bin [verFlag])
             { std_in  = NoStream
             , std_out = CreatePipe
             , std_err = CreatePipe
             }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  outVar <- newEmptyMVar
  _ <- forkIO (hGetContents hOut >>= putMVar outVar)
  _ <- forkIO (void (hGetContents hErr))
  exited <- timeout versionTimeoutMicros (waitForProcess ph)
  case exited of
    Nothing -> do
      terminateProcess ph
      hClose hOut
      hClose hErr
      pure Nothing
    Just ExitSuccess -> do
      o <- takeMVar outVar
      pure (firstLine (T.pack o))
    Just _ ->
      pure Nothing

firstLine :: Text -> Maybe Text
firstLine t = case T.lines (T.strip t) of
  (l:_) | not (T.null l) -> Just l
  _                      -> Nothing

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult :: [Entry] -> ToolResult
renderResult entries =
  let blocking = filter (\e -> eCategory e == "gate" && not (eAvailable e)) entries
      payload =
        object
          [ "success"         .= null blocking
          , "tools"           .= map renderEntry entries
          , "blocking_gates"  .= map eName blocking
          , "summary"         .= summarise entries blocking
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False    -- missing tools are info, not server error
       }

renderEntry :: Entry -> Value
renderEntry e =
  object
    [ "name"      .= eName e
    , "category"  .= eCategory e
    , "available" .= eAvailable e
    , "path"      .= ePath e
    , "version"   .= eVersion e
    ]

summarise :: [Entry] -> [Entry] -> Text
summarise entries blocking =
  let n    = length entries
      avail = length (filter eAvailable entries)
  in T.pack (show avail) <> " of " <> T.pack (show n)
     <> " tools available" <> case blocking of
          [] -> "."
          bs -> "; blocking gates: "
             <> T.intercalate ", " (map eName bs)

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
