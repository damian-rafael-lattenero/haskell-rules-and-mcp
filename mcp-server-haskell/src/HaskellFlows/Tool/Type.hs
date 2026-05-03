-- | @ghc_type@ — Phase-2 tool (GHC-API migrated).
--
-- Mirrors @mcp-server/src/tools/type-check.ts@: accepts a Haskell
-- expression, returns a parsed @{ expression, type }@ JSON payload.
--
-- Post-migration it calls 'GHC.exprType' via 'withGhcSession' instead
-- of shelling @:t@ into a subprocess ghci. The response schema is
-- unchanged — scenarios that check the JSON shape (@FlowExploratory@,
-- @FlowOversizedInput@, …) still see the same wire format.
--
-- Boundary safety: the expression argument still routes through
-- 'sanitizeExpression', so a newline or the legacy framing sentinel
-- is rejected up-front. In-process GHC wouldn't desync on a newline
-- the way subprocess-ghci did, but preserving the contract keeps the
-- rejection-surface identical to the ghci-backed version.
module HaskellFlows.Tool.Type
  ( descriptor
  , handle
  , queryExprType
  , TypeArgs (..)
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import GHC (Ghc, TcRnExprMode (TM_Inst), exprType)
import GHC.Utils.Outputable (showPprUnsafe)

import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Ghc.ApiSession (GhcSession, withGhcSession)
import HaskellFlows.Ghc.Sanitize (sanitizeExpression)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)

-- | The schema surfaced to clients via @tools/list@.
descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcType
    , tdDescription =
        "PURPOSE: Get the type of a Haskell expression via :t / GHC API. "
          <> "WHEN: verifying types of subexpressions before composing; "
          <> "understanding what a function expects or returns; sanity-"
          <> "checking before ghc_quickcheck. "
          <> "WHEN NOT: you need the full kind/instances/definition site "
          <> "— that is ghc_info; you want to evaluate the expression — "
          <> "ghc_eval. "
          <> "PREREQUISITES: imports for symbols in the expression must "
          <> "be in scope (see ghc_imports). "
          <> "OUTPUT: {expression, type}; type is the GHC-rendered "
          <> "monomorphic or polymorphic signature. "
          <> "SEE ALSO: ghc_info, ghc_eval, ghc_suggest."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "expression" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("The Haskell expression to type-check. Examples: \
                       \\"map (+1)\", \"foldr\", \"Just . show\"" :: Text)
                  ]
              ]
          , "required"             .= ["expression" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype TypeArgs = TypeArgs
  { taExpression :: Text
  }
  deriving stock (Show)

instance FromJSON TypeArgs where
  parseJSON = withObject "TypeArgs" $ \o ->
    TypeArgs <$> o .: "expression"

-- | Handle a @tools/call@ for @ghc_type@.
handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (Env.toolResponseToResult (Env.mkFailed
      ((Env.mkErrorEnvelope (parseErrorKind parseError)
          (T.pack ("Invalid arguments: " <> parseError)))
            { Env.eeCause = Just (T.pack parseError) })))
  Right (TypeArgs expr) ->
    case sanitizeExpression expr of
      Left cmdErr ->
        pure (Env.toolResponseToResult (Env.mkRefused
          (Env.sanitizeRejection "expression" cmdErr)))
      Right safe -> do
        eRes <- try (withGhcSession ghcSess (queryExprType safe))
        pure $ Env.toolResponseToResult $ case eRes of
          Left (se :: SomeException) ->
            -- Issue #90 §4: type-checker failure (expression does
            -- not type-check) maps to status='failed' with
            -- kind='type_error'. The user-facing message stays
            -- short ("expression did not type-check"); the full
            -- GHC SDoc lives in error.cause for debugging.
            Env.mkFailed
              ((Env.mkErrorEnvelope Env.TypeError
                  ("expression '" <> expr <> "' did not type-check"))
                    { Env.eeCause = Just (T.pack (show se)) })
          Right tyText -> Env.mkOk (typePayload expr tyText)

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

-- | Single in-process query: ask GHC for the type, render it.
-- Runs inside a 'withGhcSession' call so the auto-loaded interactive
-- context (Prelude + every module in the module graph) is live.
queryExprType :: Text -> Ghc Text
queryExprType safe = do
  ty <- exprType TM_Inst (T.unpack safe)
  pure (T.pack (showPprUnsafe ty))

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

-- | Type-found payload. Issue #90 Phase B: status='ok' with the
-- same field names as before ('expression', 'type') so consumers
-- continue to function during the dual-shape window.
typePayload :: Text -> Text -> Value
typePayload originalExpr tyRendered = object
  [ "expression" .= originalExpr
  , "type"       .= tyRendered
  ]
