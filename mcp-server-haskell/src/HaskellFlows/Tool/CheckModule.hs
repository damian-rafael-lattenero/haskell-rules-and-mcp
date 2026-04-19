-- | @ghci_check_module@ — run every module-complete gate in a single
-- call and return a consolidated report.
--
-- The gates ('compile', 'warnings', 'holes', 'regression') are the
-- subset that can be answered from the Haskell MCP's current toolchain
-- without requiring external binaries. Format + HLint checks are
-- deferred to 'ghci_format' / 'ghci_lint' when those tools are ported
-- — this tool will pick them up automatically the day they become
-- available on the server side.
--
-- Each gate is reported with its pass/fail status and a short message.
-- A single @overall@ flag is the AND of every gate — this is what an
-- agent should check to decide whether the module is \"done\".
module HaskellFlows.Tool.CheckModule
  ( descriptor
  , handle
  , CheckArgs (..)
  ) where

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
  , LoadMode (..)
  , loadModuleWith
  , runProperty
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error
  ( GhcError (..)
  , Severity (..)
  , parseGhcErrors
  )
import HaskellFlows.Parser.Hole (parseTypedHoles)
import HaskellFlows.Parser.QuickCheck
  ( QuickCheckResult (..)
  , parseQuickCheckOutput
  )
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

handle :: Session -> Store -> ProjectDir -> Value -> IO ToolResult
handle sess store pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (CheckArgs raw) -> case mkModulePath pd (T.unpack raw) of
    Left e -> pure (errorResult (formatPathError e))
    Right mp -> do
      -- Single dual-pass load gives us compile + warnings + holes.
      strict <- loadModuleWith sess mp Strict
      let diags      = parseGhcErrors (grOutput strict)
          errors     = filter ((== SevError)   . geSeverity) diags
          warnings   = filter ((== SevWarning) . geSeverity) diags
          compileOk  = grSuccess strict && null errors
      -- Deferred pass only if strict compiled — otherwise holes are
      -- irrelevant, errors are blocking.
      holes <- if compileOk
                 then do
                   deferred <- loadModuleWith sess mp Deferred
                   pure (parseTypedHoles (grOutput deferred))
                 else pure []
      -- Regression: run only properties associated with this module.
      allProps <- loadAll store
      let relevant = filter (\p -> spModule p == Just raw) allProps
      regs <- mapM (runOne sess) relevant
      let regressions = filter (not . isPass . snd) regs
      pure $ renderResult
        raw compileOk errors warnings holes regressions (length relevant)

--------------------------------------------------------------------------------
-- gates
--------------------------------------------------------------------------------

runOne
  :: Session
  -> StoredProperty
  -> IO (StoredProperty, QuickCheckResult)
runOne sess sp = do
  res <- runProperty sess (spExpression sp)
  let qr = case res of
        Left _   -> QcUnparsed (spExpression sp) "boundary sanitiser rejected"
        Right gr -> parseQuickCheckOutput (spExpression sp) (grOutput gr)
  pure (sp, qr)

isPass :: QuickCheckResult -> Bool
isPass (QcPassed _ _) = True
isPass _              = False

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

renderResult
  :: Text                      -- module_path
  -> Bool                      -- compileOk
  -> [GhcError]                -- errors
  -> [GhcError]                -- warnings
  -> [a]                       -- holes (opaque; we only count)
  -> [(StoredProperty, QuickCheckResult)]   -- regressions
  -> Int                       -- total relevant stored props
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
