-- | @ghc_check_module@ — Wave-5 full GhcSession.
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
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
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
    { tdName        = toolNameText GhcCheckModule
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
              , "warnings_block" .= object
                  [ "type"        .= ("boolean" :: Text)
                  , "description" .=
                      ("When true (default), '-Wall' warnings count \
                       \against 'overall' — the strict pre-push gate. \
                       \Set false during early iteration to keep \
                       \warnings informational; they still appear in \
                       \'diagnostics.warnings' but don't fail the \
                       \gate. Errors and hole/regression gates are \
                       \always blocking." :: Text)
                  ]
              ]
          , "required"             .= ["module_path" :: Text]
          , "additionalProperties" .= False
          ]
    }

data CheckArgs = CheckArgs
  { caModulePath    :: !Text
  , caWarningsBlock :: !Bool
  }
  deriving stock (Show)

instance FromJSON CheckArgs where
  parseJSON = withObject "CheckArgs" $ \o -> do
    mp <- o .:  "module_path"
    wb <- o .:? "warnings_block" .!= True
    pure CheckArgs { caModulePath = mp, caWarningsBlock = wb }

handle :: GhcSession -> Store -> ProjectDir -> Value -> IO ToolResult
handle ghcSess store pd rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (CheckArgs raw warnBlock) -> case mkModulePath pd (T.unpack raw) of
    Left e -> pure (errorResult (formatPathError e))
    Right _ -> do
      invalidateLoadCache ghcSess
      tgt <- targetForPath ghcSess (T.unpack raw)
      eStrict <- try (loadForTarget ghcSess tgt Strict)
      case eStrict :: Either SomeException (Bool, [GhcError]) of
        Left ex ->
          pure (errorResult ("loadForTarget failed: " <> T.pack (show ex)))
        Right (strictOk, strictDiags) -> do
          -- 'loadForTarget' loads the whole target (library or
          -- test-suite), so 'strictDiags' is the UNION of warnings
          -- across every module in that target. Filter to this
          -- module's file only: without the filter, a warning in
          -- 'Expr.Pretty' would red-gate 'Expr.Syntax' too, and
          -- 'check_project' would show the same warnings attributed
          -- to N modules (one per module it iterated).
          -- Diagnostic attribution: GHC reports absolute paths in
          -- 'geFile' (e.g. @/tmp/proj/src/Foo.hs@); the user passed
          -- a project-relative path (e.g. @src/Foo.hs@). A suffix
          -- match on the relative path is enough to own/disown a
          -- diag — the absolute path will always end with the
          -- relative one when GHC is pointed at this project root.
          let ownDiag d = raw `T.isSuffixOf` geFile d
              ownDiags  = filter ownDiag strictDiags
              errors    = filter ((== SevError)   . geSeverity) ownDiags
              warnings  = filter ((== SevWarning) . geSeverity) ownDiags
              -- 'compileOk' still takes the PROJECT-wide strictOk
              -- flag: if any module failed to compile the whole
              -- load reported Failed, and this module can't
              -- legitimately be called green even if the specific
              -- diag happens to have landed on a sibling. Errors
              -- in OTHER modules still surface downstream via
              -- their own check_module call.
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
            raw compileOk errors warnings holes regressions
            (length relevant) warnBlock

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
  -> Bool    -- ^ warnings_block — True (default) keeps warnings blocking.
  -> ToolResult
renderResult mp compileOk errs warns holes regressions totalProps warnBlock =
  let gateCompile    = gate compileOk     "module compiles strictly"
      gateNoWarnings = gate (null warns || not warnBlock) $
        if null warns
          then "no warnings (-Wall clean)"
          else if warnBlock
            then T.pack (show (length warns)) <> " warning(s) (blocking — "
               <> "pass warnings_block=false to keep iterating)"
            else T.pack (show (length warns))
              <> " warning(s) (informational; warnings_block=false)"
      gateNoHoles    = gate (null holes)  "no deferred typed holes"
      gateProps      = gate (null regressions) $
        case totalProps of
          0 -> "no stored properties for this module (nothing to regress)"
          _ -> T.pack (show totalProps) <> " stored properties pass"
      overall = compileOk
             && (null warns || not warnBlock)
             && null holes
             && null regressions
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
