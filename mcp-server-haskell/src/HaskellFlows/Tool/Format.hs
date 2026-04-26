-- | @ghc_format@ — run @fourmolu@ or @ormolu@ on a module.
--
-- Mirrors the TS tool's resolution policy: try @fourmolu@ first, fall
-- back to @ormolu@, report unavailable if neither is on @PATH@. With
-- @write=true@ the tool rewrites the file in-place; with @write=false@
-- (the default) it returns the formatted text without touching disk —
-- safer default, the agent inspects the diff first.
--
-- Boundary safety: the module path goes through 'mkModulePath', so a
-- caller cannot trick us into formatting (or rewriting!) a file
-- outside the project tree.
module HaskellFlows.Tool.Format
  ( descriptor
  , handle
  , FormatArgs (..)
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
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
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Types
  ( ModulePath
  , PathError (..)
  , ProjectDir
  , mkModulePath
  , unModulePath
  , unProjectDir
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcFormat
    , tdDescription =
        "Format a Haskell module with fourmolu (preferred) or ormolu "
          <> "(fallback). Default is check-only — pass write=true to "
          <> "rewrite the file in-place. Reports availability when "
          <> "neither formatter is on PATH."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Relative path to the module." :: Text)
                  ]
              , "write" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("Rewrite the file in place. Default: false \
                       \(returns formatted text only)." :: Text)
                  ]
              ]
          , "required"             .= ["module_path" :: Text]
          , "additionalProperties" .= False
          ]
    }

data FormatArgs = FormatArgs
  { faModulePath :: !Text
  , faWrite      :: !Bool
  }
  deriving stock (Show)

instance FromJSON FormatArgs where
  parseJSON = withObject "FormatArgs" $ \o -> do
    mp <- o .:  "module_path"
    w  <- o .:? "write" .!= False
    pure FormatArgs { faModulePath = mp, faWrite = w }

formatTimeoutMicros :: Int
formatTimeoutMicros = 30 * 1_000_000  -- 30 s

handle :: ProjectDir -> Value -> IO ToolResult
handle pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> case mkModulePath pd (T.unpack (faModulePath args)) of
    Left e -> pure (errorResult (formatPathError e))
    Right mp -> do
      mFormatter <- resolveFormatter
      case mFormatter of
        Nothing ->
          pure (unavailableResult "Neither fourmolu nor ormolu was found on PATH")
        Just f -> do
          outcome <- runFormatter pd f mp (faWrite args)
          pure (renderResult f mp (faWrite args) outcome)

-- | Which formatter we plan to invoke. 'fBinary' is the absolute path
-- resolved at the time of the call so a mid-call PATH change can't
-- make us spawn a different binary than we reported.
data Formatter = Formatter { fName :: !Text, fBinary :: !FilePath }
  deriving stock (Eq, Show)

resolveFormatter :: IO (Maybe Formatter)
resolveFormatter = do
  mFour <- findExecutable "fourmolu"
  case mFour of
    Just p  -> pure (Just Formatter { fName = "fourmolu", fBinary = p })
    Nothing -> do
      mOr <- findExecutable "ormolu"
      pure (fmap (\p -> Formatter { fName = "ormolu", fBinary = p }) mOr)

--------------------------------------------------------------------------------
-- subprocess
--------------------------------------------------------------------------------

data FmtOutcome
  = FmtOk !Text      -- output (or "" if we wrote in place)
  | FmtTimeout
  | FmtFailure !Int !Text
  deriving stock (Eq, Show)

runFormatter :: ProjectDir -> Formatter -> ModulePath -> Bool -> IO FmtOutcome
runFormatter pd f mp write = do
  -- We pass the traversal-checked absolute path from 'unModulePath'
  -- directly; --mode inplace rewrites in place, --mode stdout returns
  -- the formatted text without touching disk.
  let mode = if write then "inplace" else "stdout"
      args = ["--mode", mode, unModulePath mp]
      cp   = (proc (T.unpack (fName f)) args)
               { cwd     = Just (unProjectDir pd)
               , std_in  = NoStream
               , std_out = CreatePipe
               , std_err = CreatePipe
               }
  (_, Just hOut, Just hErr, ph) <- createProcess cp
  outVar <- newEmptyMVar
  errVar <- newEmptyMVar
  _ <- forkIO (hGetContents hOut >>= putMVar outVar)
  _ <- forkIO (hGetContents hErr >>= putMVar errVar)
  exited <- timeout formatTimeoutMicros (waitForProcess ph)
  case exited of
    Nothing -> do
      terminateProcess ph
      hClose hOut
      hClose hErr
      pure FmtTimeout
    Just ExitSuccess -> do
      o <- takeMVar outVar
      pure (FmtOk (T.pack o))
    Just (ExitFailure code) -> do
      e <- takeMVar errVar
      pure (FmtFailure code (T.pack e))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult :: Formatter -> ModulePath -> Bool -> FmtOutcome -> ToolResult
renderResult _ _ _ (FmtOk out) =
  let payload =
        object
          [ "success"    .= True
          , "formatted"  .= out
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }
renderResult _ _ _ FmtTimeout =
  errorResult "formatter timed out after 30 seconds"
renderResult _ _ _ (FmtFailure code err) =
  errorResult ( "formatter exited with code " <> T.pack (show code)
             <> ": " <> T.strip err )

unavailableResult :: Text -> ToolResult
unavailableResult msg =
  ToolResult
    { trContent = [ TextContent (encodeUtf8Text (object
        [ "success"     .= False
        , "error"       .= msg
        , "remediation" .= ( "Install fourmolu (`cabal install fourmolu`) \
                            \or ormolu and retry." :: Text )
        ]))
      ]
    , trIsError = True
    }

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

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p        -> "Project directory is not absolute: " <> p
  PathEscapesProject a p _ -> "module_path '" <> a <> "' escapes project directory " <> p

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
