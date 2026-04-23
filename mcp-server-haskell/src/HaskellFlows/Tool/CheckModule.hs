-- | @ghci_check_module@ — Wave-5 full GhcSession.
--
-- All four gates (compile / warnings / holes / regression) run
-- in-process: 'loadForTarget' Strict → errors + warnings;
-- 'loadForTarget' Deferred → hole warnings; property replay via
-- 'Regression.runOne' (which itself is in-process Wave-3).
module HaskellFlows.Tool.CheckModule
  ( descriptor
  , handle
  , CheckArgs (..)
  ) where

import Control.Exception (SomeException, try)
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
import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , invalidateLoadCache
  , loadForTarget
  , targetForPath
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , renderGhciStyle
  )
import HaskellFlows.Parser.Hole (parseTypedHoles)
import HaskellFlows.Parser.QuickCheck (QuickCheckResult (..))
import qualified HaskellFlows.Tool.Regression as RegTool
import HaskellFlows.Types
  ( ProjectDir
  , PathError (..)
  , mkModulePath
  )

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_check_module"
    , tdDescription =
        "Aggregate module-complete gates into one report: compiles? "
          <> "no errors? no warnings? no holes? stored properties still "
          <> "pass? Returns pass/fail per gate plus a single 'overall' "
          <> "summary."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "module_path" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Path to the module to check, relative to the \
                       \project directory." :: Text)
                  ]
              ]
          , "required"             .= ["module_path" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype CheckArgs = CheckArgs
  { caModulePath :: Text
  }
  deriving stock (Show)

instance FromJSON CheckArgs where
  parseJSON = withObject "CheckArgs" $ \o ->
    CheckArgs <$> o .: "module_path"

handle :: GhcSession -> Store -> ProjectDir -> Value -> IO ToolResult
handle ghcSess store pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (CheckArgs raw) -> case mkModulePath pd (T.unpack raw) of
    Left e -> pure (errorResult (formatPathError e))
    Right _ -> do
      invalidateLoadCache ghcSess
      tgt <- targetForPath ghcSess (T.unpack raw)
      eStrict <- try (loadForTarget ghcSess tgt Strict)
      case eStrict :: Either SomeException (Bool, [GhcError]) of
        Left ex ->
          pure (errorResult ("loadForTarget failed: " <> T.pack (show ex)))
        Right (strictOk, strictDiags) -> do
          let errors    = filter ((== SevError)   . geSeverity) strictDiags
              warnings  = filter ((== SevWarning) . geSeverity) strictDiags
              compileOk = strictOk && null errors
          holes <- if compileOk
                     then do
                       eDef <- try (loadForTarget ghcSess tgt Deferred)
                       pure $ case eDef :: Either SomeException (Bool, [GhcError]) of
                         Left _           -> []
                         Right (_, diags) ->
                           parseTypedHoles (renderGhciStyle diags)
                     else pure []
          allProps <- loadAll store
          let relevant = filter (\p -> spModule p == Just raw) allProps
          -- Reuse the Wave-3 Regression.runOne — it's already
          -- in-process via evalIOString.
          replays <- mapM (RegTool.runOne ghcSess) relevant
          let regressions =
                [ (RegTool.rpStored r, RegTool.rpResult r)
                | r <- replays
                , case RegTool.rpResult r of
                    QcPassed _ _ -> False
                    _            -> True
                ]
          pure $ renderResult
            raw compileOk errors warnings holes regressions (length relevant)

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult
  :: Text
  -> Bool
  -> [GhcError]
  -> [GhcError]
  -> [a]
  -> [(StoredProperty, QuickCheckResult)]
  -> Int
  -> ToolResult
renderResult mp compileOk errs warns holes regressions totalProps =
  let gateCompile    = gate compileOk     "module compiles strictly"
      gateNoWarnings = gate (null warns)  "no warnings (-Wall clean)"
      gateNoHoles    = gate (null holes)  "no deferred typed holes"
      gateProps      = gate (null regressions) $
        case totalProps of
          0 -> "no stored properties for this module (nothing to regress)"
          _ -> T.pack (show totalProps) <> " stored properties pass"
      overall = compileOk && null warns && null holes && null regressions
      payload =
        object
          [ "success"    .= overall
          , "module"     .= mp
          , "overall"    .= overall
          , "gates"      .= object
              [ "compile"    .= gateCompile
              , "warnings"   .= gateNoWarnings
              , "holes"      .= gateNoHoles
              , "properties" .= gateProps
              ]
          , "diagnostics" .= object
              [ "errors"   .= errs
              , "warnings" .= warns
              ]
          , "summary" .= summarise overall errs warns holes regressions
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = not overall
       }

gate :: Bool -> Text -> Value
gate ok reason =
  object
    [ "ok"     .= ok
    , "reason" .= reason
    ]

summarise
  :: Bool
  -> [GhcError]
  -> [GhcError]
  -> [a]
  -> [(StoredProperty, QuickCheckResult)]
  -> Text
summarise True _ _ _ _ =
  "All gates green. Module is complete."
summarise False errs warns holes regs =
  T.intercalate "; " $ filter (not . T.null)
    [ if null errs  then "" else T.pack (show (length errs))  <> " error(s)"
    , if null warns then "" else T.pack (show (length warns)) <> " warning(s)"
    , if null holes then "" else T.pack (show (length holes)) <> " hole(s)"
    , if null regs  then "" else T.pack (show (length regs))  <> " property regression(s)"
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

formatPathError :: PathError -> Text
formatPathError = \case
  PathNotAbsolute p        -> "Project directory is not absolute: " <> p
  PathEscapesProject a p _ -> "module_path '" <> a <> "' escapes project directory " <> p

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
