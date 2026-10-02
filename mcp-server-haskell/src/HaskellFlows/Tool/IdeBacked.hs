-- | ghcide-backend routing for the F1 pilot tools.
--
-- 'routeIde' is the strangler seam: when
-- @HASKELL_FLOWS_BACKEND=ghcide@ is set, @ghc_check_module@,
-- @ghc_eval@ and @ghc_type@ are served by 'IdeSession' instead of
-- 'ApiSession'. Every other tool keeps its legacy handler untouched —
-- the flag is additive and defaults to the legacy backend.
module HaskellFlows.Tool.IdeBacked
  ( routeIde
  , warmupIdeSession
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Monad (void)
import Data.Aeson (Value, object, withObject, (.=), (.:))
import Data.Aeson.Key (Key)
import Data.Aeson.Types (parseEither)
import Data.IORef (IORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>))

import HaskellFlows.Ghc.IdeSession
  ( IdeSession
  , anchorModuleIn
  , bootIdeSession
  , ideEvalExprIn
  , ideTypeOfExprIn
  , ideTypecheckFile
  )
import HaskellFlows.Mcp.Envelope
  ( ErrorKind (..)
  , ToolResponse
  , mkErrorEnvelope
  , mkFailed
  , mkOk
  )
import HaskellFlows.Mcp.ToolName (ToolName (..))
import HaskellFlows.Types (ProjectDir, unProjectDir)

-- | Get-or-boot the ghcide session under the MVar (first caller boots,
-- everyone else reuses — same single-writer shape as 'srvGhcSession').
-- The handler itself runs OUTSIDE the MVar: a slow tool call must never
-- hold the boot lock (that would serialize every ghcide-backed call).
withIdeSession
  :: MVar (Maybe IdeSession)
  -> IORef ProjectDir
  -> (IdeSession -> IO ToolResponse)
  -> IO ToolResponse
withIdeSession ref pdRef k = do
  s <- modifyMVar ref $ \case
    Just s -> pure (Just s, s)
    Nothing -> do
      s <- bootIdeSession =<< readIORef pdRef
      pure (Just s, s)
  k s

-- | Boot the ghcide session in the background (idempotent — shares the
-- MVar singleton). Used by the transport warmup so the first tool call
-- on the ghcide backend arrives warm.
warmupIdeSession :: MVar (Maybe IdeSession) -> IORef ProjectDir -> IO ()
warmupIdeSession ref pdRef =
  void (withIdeSession ref pdRef (\_ -> pure mkWarmupAck))
  where
    mkWarmupAck = mkOk (object ["warmup" .= ("ghcide" :: Text)])

-- | Nothing for tools the ghcide backend does not serve (yet).
routeIde
  :: MVar (Maybe IdeSession)
  -> IORef ProjectDir
  -> ToolName
  -> Value
  -> Maybe (IO ToolResponse)
routeIde ref pdRef tn args = case tn of
  GhcCheckModule -> Just (withIdeSession ref pdRef (handleCheck pdRef args))
  GhcEval -> Just (withIdeSession ref pdRef (handleEval args))
  GhcType -> Just (withIdeSession ref pdRef (handleType args))
  _ -> Nothing

argField :: Key -> Value -> Either String Text
argField field = parseEither (withObject "args" (.: field))

handleCheck :: IORef ProjectDir -> Value -> IdeSession -> IO ToolResponse
handleCheck pdRef raw s = case argField "module_path" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right mp -> do
    pd <- readIORef pdRef
    errs <- ideTypecheckFile s (unProjectDir pd </> T.unpack mp)
    if null errs
      then
        pure
          ( mkOk
              ( object
                  [ "module_path" .= mp
                  , "backend" .= ("ghcide" :: Text)
                  , "errors" .= ([] :: [Value])
                  , "summary" .= ("No type errors." :: Text)
                  ]
              )
          )
      else
        pure
          ( mkFailed
              ( mkErrorEnvelope
                  CompileError
                  (T.pack (show (length errs)) <> " error(s) — ghcide backend")
              )
          )

-- | Distinguish timeouts from compile failures in the eval/type
-- envelopes — the budget tripping is not a compile error.
budgetErrorKind :: Text -> ErrorKind
budgetErrorKind err
  | "timeout" `T.isInfixOf` T.toLower err = InnerTimeout
  | otherwise = CompileError

handleEval :: Value -> IdeSession -> IO ToolResponse
handleEval raw s = case argField "expression" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right expr -> do
    anchor <- anchorModuleIn s
    r <- ideEvalExprIn s anchor [] ("show (" <> expr <> ")")
    case r of
      Left err -> pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) err))
      Right out ->
        pure
          ( mkOk
              ( object
                  [ "output" .= out
                  , "truncated" .= False
                  , "backend" .= ("ghcide" :: Text)
                  ]
              )
          )

handleType :: Value -> IdeSession -> IO ToolResponse
handleType raw s = case argField "expression" raw of
  Left err -> pure (mkFailed (mkErrorEnvelope MissingArg (T.pack err)))
  Right expr -> do
    anchor <- anchorModuleIn s
    r <- ideTypeOfExprIn s anchor expr
    case r of
      Left err -> pure (mkFailed (mkErrorEnvelope (budgetErrorKind err) err))
      Right ty -> pure (mkOk (object ["type" .= ty, "backend" .= ("ghcide" :: Text)]))
