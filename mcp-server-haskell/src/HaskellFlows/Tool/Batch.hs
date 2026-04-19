-- | @ghci_batch@ — run multiple tool invocations in a single request.
--
-- Motivation documented in `docs/ts-mcp-retrospective.md` § A1/G1. The
-- common dev loop is load → lint → format → check_module on a single
-- module; 4 round-trips where 1 would do. This tool folds them into
-- one request with a list of action objects, returning a parallel
-- list of results.
--
-- Semantics:
--
-- * Actions run sequentially, in list order.
-- * @fail_fast=true@ (default): stop on the first action whose
--   @trIsError@ is true. The remaining actions return a structured
--   @skipped@ entry.
-- * @fail_fast=false@: every action runs regardless; the response is
--   the complete array.
-- * Any exception inside an action is caught and converted to an
--   error-shape result — batch never tears down the server loop.
--
-- Security / safety:
--
-- * The inner dispatcher is passed in as a function so this module is
--   oblivious to the concrete 'Server' type and the tool table. Keeps
--   the import graph DAG-shaped.
-- * Every inner action reuses its own tool's boundary validation
--   (sanitizeExpression, mkModulePath, etc.) — batch adds no new
--   trust boundary, just composes existing ones.
-- * Arbitrary recursion guard: a batch inside a batch is refused at
--   the boundary (a nested 'ghci_batch' action returns a structured
--   error). Prevents pathological stack growth.
module HaskellFlows.Tool.Batch
  ( descriptor
  , handle
  , BatchArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import HaskellFlows.Mcp.Protocol

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_batch"
    , tdDescription =
        "Run a list of tool invocations sequentially in one request. "
          <> "Each action is `{tool: string, args: object}`. Returns "
          <> "an array of results in the same order. With fail_fast=true "
          <> "(default) stops on the first error; with fail_fast=false "
          <> "runs every action. Nesting ghci_batch inside itself is "
          <> "refused."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "actions" .= object
                  [ "type"        .= ("array" :: Text)
                  , "description" .=
                      ("List of `{tool, args}` tool invocations." :: Text)
                  , "items"       .= object
                      [ "type"       .= ("object" :: Text)
                      , "required"   .= (["tool", "args"] :: [Text])
                      , "properties" .= object
                          [ "tool" .= object [ "type" .= ("string" :: Text) ]
                          , "args" .= object [ "type" .= ("object" :: Text) ]
                          ]
                      ]
                  ]
              , "fail_fast" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("Stop on first error. Default: true." :: Text)
                  ]
              ]
          , "required"             .= ["actions" :: Text]
          , "additionalProperties" .= False
          ]
    }

data BatchArgs = BatchArgs
  { baActions  :: ![ToolCall]
  , baFailFast :: !Bool
  }
  deriving stock (Show)

instance FromJSON BatchArgs where
  parseJSON = withObject "BatchArgs" $ \o -> do
    acts <- o .:  "actions"
    ff   <- o .:? "fail_fast" .!= True
    pure BatchArgs { baActions = acts, baFailFast = ff }

-- | The dispatcher is passed as a parameter so this module doesn't
-- depend on 'HaskellFlows.Mcp.Server' directly — clean DAG, easy to
-- test in isolation.
handle :: (ToolCall -> IO ToolResult) -> Value -> IO ToolResult
handle dispatch rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right args -> do
    results <- runActions dispatch (baFailFast args) (baActions args)
    pure (renderResult (baFailFast args) results)

--------------------------------------------------------------------------------
-- execution
--------------------------------------------------------------------------------

data ActionOutcome
  = AoOk      !Text !ToolResult   -- tool name + result
  | AoSkipped !Text                -- skipped because fail_fast tripped
  | AoNested  !Text                -- refused (ghci_batch in ghci_batch)
  | AoThrew   !Text !Text          -- tool name + exception text
  deriving stock (Show)

runActions
  :: (ToolCall -> IO ToolResult)
  -> Bool                              -- fail_fast
  -> [ToolCall]
  -> IO [ActionOutcome]
runActions _ _ []          = pure []
runActions dispatch ff (c:cs)
  | tcName c == "ghci_batch" = do
      -- Refuse nested batch. Continue (or stop) per fail_fast.
      rest <- if ff
                then pure (map (AoSkipped . tcName) cs)
                else runActions dispatch ff cs
      pure (AoNested (tcName c) : rest)
  | otherwise = do
      eTr <- try (dispatch c) :: IO (Either SomeException ToolResult)
      let this = case eTr of
            Right tr -> AoOk (tcName c) tr
            Left  ex -> AoThrew (tcName c) (T.pack (show ex))
          stop = ff && outcomeIsError this
      rest <-
        if stop
          then pure (map (AoSkipped . tcName) cs)
          else runActions dispatch ff cs
      pure (this : rest)

outcomeIsError :: ActionOutcome -> Bool
outcomeIsError = \case
  AoOk      _ tr  -> trIsError tr
  AoSkipped _     -> False    -- hasn't run yet
  AoNested  _     -> True     -- refused = error
  AoThrew   _ _   -> True

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult :: Bool -> [ActionOutcome] -> ToolResult
renderResult ff outcomes =
  let errCount  = length (filter outcomeIsError outcomes)
      skipCount = length [ () | AoSkipped _ <- outcomes ]
      okCount   = length outcomes - errCount - skipCount
      payload =
        object
          [ "success"   .= (errCount == 0)
          , "fail_fast" .= ff
          , "total"     .= length outcomes
          , "ok"        .= okCount
          , "failed"    .= errCount
          , "skipped"   .= skipCount
          , "results"   .= map renderOutcome outcomes
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = errCount > 0
       }

renderOutcome :: ActionOutcome -> Value
renderOutcome (AoOk nm tr) =
  object
    [ "tool"     .= nm
    , "status"   .= (if trIsError tr then "failed" :: Text else "ok")
    , "result"   .= toJSON tr
    ]
renderOutcome (AoSkipped nm) =
  object
    [ "tool"   .= nm
    , "status" .= ("skipped" :: Text)
    , "reason" .= ("fail_fast tripped on an earlier action" :: Text)
    ]
renderOutcome (AoNested nm) =
  object
    [ "tool"   .= nm
    , "status" .= ("refused" :: Text)
    , "reason" .= ("ghci_batch cannot be nested inside ghci_batch" :: Text)
    ]
renderOutcome (AoThrew nm msg) =
  object
    [ "tool"   .= nm
    , "status" .= ("exception" :: Text)
    , "error"  .= msg
    ]

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
