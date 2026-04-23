-- | @ghci_arbitrary@ — Wave-4 full GhcSession.
--
-- Given a type name, the tool:
--
-- 1. loads the user's project via 'loadForTarget' so user types resolve,
-- 2. looks the name up via 'parseName' + 'getInfo',
-- 3. renders the resulting 'TyThing' with 'showPprUnsafe' so the
--    existing text parser ('parseConstructors' / 'parseTypeParams')
--    still applies unchanged,
-- 4. emits an @instance Arbitrary T where arbitrary = oneof [...]@
--    template that the agent can paste.
--
-- Deliberately does NOT write to disk — letting the agent review +
-- paste preserves the auditing loop.
module HaskellFlows.Tool.Arbitrary
  ( descriptor
  , handle
  , ArbitraryArgs (..)
  , renderTemplate
  , parseConstructors
  , parseTypeParams
  , Constructor (..)
  , isRecursiveArg
  , isRecursiveConstructor
  , hasRecursiveConstructor
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import GHC
  ( Ghc
  , TyThing (ATyCon)
  , getInfo
  , parseName
  )
import GHC.Core.DataCon
  ( DataCon
  , dataConName
  , dataConOrigArgTys
  )
import GHC.Core.TyCon
  ( TyCon
  , tyConDataCons
  , tyConName
  , tyConTyVars
  )
import GHC.Types.Var (tyVarName)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Utils.Outputable (showPprUnsafe)

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , firstLibraryOrTestSuite
  , loadForTarget
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize
  ( CommandError (..)
  , sanitizeExpression
  )
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Parser.Error (GhcError)
import HaskellFlows.Parser.Type (isOutOfScope)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = "ghci_arbitrary"
    , tdDescription =
        "Generate a QuickCheck Arbitrary instance template for a user-defined "
          <> "data type. Returns the instance text for the agent to paste — does "
          <> "not modify files. Polymorphic types are handled: the template "
          <> "includes an Arbitrary constraint for every type variable. Complex "
          <> "types (GADTs, existentials, constrained constructors) may still "
          <> "need hand-editing after paste."
    , tdInputSchema =
        object
          [ "type"       .= ("object" :: Text)
          , "properties" .= object
              [ "type_name" .= object
                  [ "type"        .= ("string" :: Text)
                  , "description" .=
                      ("Name of the data/newtype to derive Arbitrary for. \
                       \Example: \"Expr\", \"Command\", \"Status\"." :: Text)
                  ]
              ]
          , "required"             .= ["type_name" :: Text]
          , "additionalProperties" .= False
          ]
    }

newtype ArbitraryArgs = ArbitraryArgs
  { aaTypeName :: Text
  }
  deriving stock (Show)

instance FromJSON ArbitraryArgs where
  parseJSON = withObject "ArbitraryArgs" $ \o ->
    ArbitraryArgs <$> o .: "type_name"

handle :: GhcSession -> Value -> IO ToolResult
handle ghcSess rawArgs = case parseEither parseJSON rawArgs of
  Left parseError ->
    pure (errorResult (T.pack ("Invalid arguments: " <> parseError)))
  Right (ArbitraryArgs tname) -> case sanitizeExpression tname of
    Left cmdErr -> pure (errorResult (formatCommandError cmdErr))
    Right safe -> do
      tgt <- firstLibraryOrTestSuite ghcSess
      eLoad <- try (loadForTarget ghcSess tgt Strict)
      case eLoad :: Either SomeException (Bool, [GhcError]) of
        Left ex ->
          pure (errorResult ("loadForTarget failed: " <> T.pack (show ex)))
        Right _ -> do
          -- loadForTarget already primed the session with the
          -- correct stanza flags + setContext. Don't wrap in
          -- withStanzaFlags here — re-applying setSessionDynFlags
          -- would reset the interactive context established above,
          -- leaving parseName unable to resolve user types.
          eRes <- try (withGhcSession ghcSess (renderTyThing safe))
          case eRes :: Either SomeException (Maybe Text) of
            Left ex ->
              pure (errorResult ("'" <> safe <> "' not in scope: " <> T.pack (show ex)))
            Right Nothing ->
              pure (errorResult ("'" <> safe <> "' not in scope (getInfo=Nothing)"))
            Right (Just rendered)
              | isOutOfScope rendered -> pure (errorResult rendered)
              | otherwise -> do
                  let params = parseTypeParams rendered
                  case parseConstructors rendered of
                    []    -> pure (errorResult
                              ( "No constructors parsed for '" <> safe
                              <> "'. It may be a GADT, typeclass, or type synonym — "
                              <> "those need a hand-written Arbitrary." ))
                    ctors -> pure (successResult safe ctors
                                    (renderTemplate safe params ctors))

-- | Resolve the name and render the resulting 'TyThing' in the
-- exact @data T = A | B Int | ...@ shape @:info@ would print.
--
-- 'showPprUnsafe' on a bare 'TyThing' only renders @"Type
-- constructor \`T\'"@ — useless for parseConstructors. Instead we
-- walk the 'TyCon' directly: tyConTyVars for the header, then
-- tyConDataCons + dataConOrigArgTys for each constructor.
renderTyThing :: Text -> Ghc (Maybe Text)
renderTyThing nm = do
  n :| _ <- parseName (T.unpack nm)
  info <- getInfo True n
  pure $ case info of
    Just (ATyCon tc, _fixity, _clsInsts, _famInsts, _doc) ->
      Just (renderTyConAsDataDecl tc)
    _ -> Nothing

-- | Render a 'TyCon' as a GHCi-style @data T a b = C1 Int | C2 Bool a@
-- declaration. Covers newtypes identically (single-constructor data).
-- Returns "data T" header only when the TyCon has no data constructors
-- (class / type synonym); the tool layer then reports "No constructors
-- parsed" which is the pre-existing behaviour.
renderTyConAsDataDecl :: TyCon -> Text
renderTyConAsDataDecl tc =
  let tyName    = T.pack (occNameString (nameOccName (tyConName tc)))
      -- Print just the user-facing OccName for each tyvar —
      -- 'showPprUnsafe' on a TyVar prefixes an internal unique tag
      -- ("a_ig1m"), which then breaks the "Arbitrary a =>" context
      -- emitted by renderTemplate.
      tvs       = map (T.pack . occNameString . nameOccName . tyVarName)
                      (tyConTyVars tc)
      header    = "data " <> tyName
                    <> (if null tvs then "" else " " <> T.intercalate " " tvs)
      dcs       = tyConDataCons tc
      rhs       = T.intercalate " | " (map renderDataCon dcs)
  in if null dcs
       then header
       else header <> " = " <> rhs

renderDataCon :: DataCon -> Text
renderDataCon dc =
  let cn   = T.pack (occNameString (nameOccName (dataConName dc)))
      args = map (parenArg . T.pack . showPprUnsafe . scaledThing)
                 (dataConOrigArgTys dc)
  in if null args then cn else cn <> " " <> T.intercalate " " args
  where
    -- Wrap multi-word argument types in parens so parseConstructors'
    -- space-split doesn't chop them (e.g. "Maybe Int" → "(Maybe Int)").
    parenArg t
      | T.any (== ' ') (T.strip t) && not (isAlreadyWrapped t) = "(" <> t <> ")"
      | otherwise                                              = t
    isAlreadyWrapped t = case T.uncons (T.strip t) of
      Just ('(', _) -> T.last (T.strip t) == ')'
      Just ('[', _) -> T.last (T.strip t) == ']'
      _             -> False

--------------------------------------------------------------------------------
-- constructor parsing (kept identical to the legacy version; the
-- GHC-rendered TyThing prints constructors in the same shape)
--------------------------------------------------------------------------------

data Constructor = Constructor
  { cName :: !Text
  , cArgs :: ![Text]
  }
  deriving stock (Eq, Show)

parseConstructors :: Text -> [Constructor]
parseConstructors out =
  let allLns   = T.lines out
      declLns  = dropWhile (not . isDataDeclLine) allLns
      trimmed  = T.unlines (takeWhile (not . isDefinedComment) declLns)
      one      = T.strip (T.replace "\n" " " trimmed)
  in if hasCtorHeader one
       then mapMaybe parseCtorText (splitOnPipe (dropDataHeader one))
       else []

isDataDeclLine :: Text -> Bool
isDataDeclLine ln =
  let s = T.stripStart ln
  in "data "    `T.isPrefixOf` s
  || "newtype " `T.isPrefixOf` s

hasCtorHeader :: Text -> Bool
hasCtorHeader t = "data "    `T.isPrefixOf` t
               || "newtype " `T.isPrefixOf` t

isDefinedComment :: Text -> Bool
isDefinedComment l = "-- Defined" `T.isInfixOf` l || "\t-- Defined" `T.isInfixOf` l

dropDataHeader :: Text -> Text
dropDataHeader t =
  case T.breakOn "=" t of
    (_, rest) | T.null rest -> ""
    (_, rest)               -> T.strip (T.drop 1 rest)

splitOnPipe :: Text -> [Text]
splitOnPipe = go 0 []
  where
    go :: Int -> String -> Text -> [Text]
    go depth acc t = case T.uncons t of
      Nothing      -> [T.pack (reverse acc)]
      Just ('(', rest) -> go (depth + 1) ('(':acc) rest
      Just (')', rest) -> go (max 0 (depth - 1)) (')':acc) rest
      Just ('|', rest)
        | depth == 0 -> T.pack (reverse acc) : go 0 [] rest
      Just (c, rest) -> go depth (c:acc) rest

parseCtorText :: Text -> Maybe Constructor
parseCtorText raw =
  case groupTokens (T.strip raw) of
    []     -> Nothing
    (n:xs)
      | T.null n  -> Nothing
      | otherwise ->
          Just Constructor
            { cName = n
            , cArgs = normaliseArgs xs
            }
  where
    normaliseArgs :: [Text] -> [Text]
    normaliseArgs [single]
      | Just n' <- recordFieldCount single
          = replicate n' "arbitrary"
    normaliseArgs xs = xs

recordFieldCount :: Text -> Maybe Int
recordFieldCount t
  | "{" `T.isPrefixOf` t && "}" `T.isSuffixOf` t =
      let inner = T.init (T.tail t)
      in if T.null (T.strip inner)
           then Just 0
           else Just (length (splitTopLevelCommas inner))
  | otherwise = Nothing

groupTokens :: Text -> [Text]
groupTokens = go 0 [] []
  where
    go :: Int -> String -> [Text] -> Text -> [Text]
    go depth curr acc t = case T.uncons t of
      Nothing -> reverse (flush curr acc)
      Just (c, rest)
        | c == '(' || c == '{' -> go (depth + 1) (c:curr) acc rest
        | c == ')' || c == '}' -> go (max 0 (depth - 1)) (c:curr) acc rest
        | c == ' ' && depth == 0 ->
            go depth [] (flush curr acc) rest
        | otherwise -> go depth (c:curr) acc rest

    flush [] acc = acc
    flush xs acc = T.pack (reverse xs) : acc

splitTopLevelCommas :: Text -> [Text]
splitTopLevelCommas = go 0 []
  where
    go :: Int -> String -> Text -> [Text]
    go depth acc t = case T.uncons t of
      Nothing -> [T.pack (reverse acc)]
      Just (c, rest)
        | c == '(' || c == '{' -> go (depth + 1) (c:acc) rest
        | c == ')' || c == '}' -> go (max 0 (depth - 1)) (c:acc) rest
        | c == ',' && depth == 0 ->
            T.pack (reverse acc) : go 0 [] rest
        | otherwise -> go depth (c:acc) rest

--------------------------------------------------------------------------------
-- template rendering (unchanged)
--------------------------------------------------------------------------------

parseTypeParams :: Text -> [Text]
parseTypeParams out =
  let allLns  = T.lines out
      declLns = dropWhile (not . isDataDeclLine) allLns
  in case declLns of
       []    -> []
       (h:_) ->
         let afterKw = stripKw (T.stripStart h)
             headPart = T.takeWhile (/= '=') afterKw
             tokens   = T.words (T.strip headPart)
         in case tokens of
              []          -> []
              (_name:ps)  -> filter (not . T.null) ps
  where
    stripKw t
      | "data "    `T.isPrefixOf` t = T.drop 5 t
      | "newtype " `T.isPrefixOf` t = T.drop 8 t
      | otherwise                   = t

isRecursiveArg :: Text -> Text -> Bool
isRecursiveArg typeName arg = typeName `elem` tokensOf arg
  where
    tokensOf = filter (not . T.null)
             . T.split (\c -> not (isAlphaNum c || c == '_' || c == '\''))

isRecursiveConstructor :: Text -> Constructor -> Bool
isRecursiveConstructor typeName c = any (isRecursiveArg typeName) (cArgs c)

hasRecursiveConstructor :: Text -> [Constructor] -> Bool
hasRecursiveConstructor typeName = any (isRecursiveConstructor typeName)

renderTemplate :: Text -> [Text] -> [Constructor] -> Text
renderTemplate typeName params ctors =
  let typeExpr = case params of
        [] -> typeName
        ps -> "(" <> typeName <> " " <> T.unwords ps <> ")"
      context = case params of
        []  -> ""
        [p] -> "Arbitrary " <> p <> " => "
        ps  -> "(" <> T.intercalate ", " [ "Arbitrary " <> p | p <- ps ]
            <> ") => "
      header   = "instance " <> context <> "Arbitrary " <> typeExpr <> " where"
      recursive = hasRecursiveConstructor typeName ctors
  in T.unlines $
       if recursive
         then renderSizedBody typeName ctors header
         else renderFlatBody              ctors header

renderFlatBody :: [Constructor] -> Text -> [Text]
renderFlatBody ctors header =
     [ header
     , "  arbitrary = oneof"
     ]
     <> zipWith renderBranch [0 :: Int ..] ctors
     <> [ "    ]" ]
  where
    renderBranch i c =
      let prefix = if i == 0 then "    [ " else "    , "
      in prefix <> renderRhsFlat c

renderSizedBody :: Text -> [Constructor] -> Text -> [Text]
renderSizedBody typeName ctors header =
  let leaves = filter (not . isRecursiveConstructor typeName) ctors
      base0  = if null leaves then ctors else leaves
  in [ header
     , "  arbitrary = sized go"
     , "    where"
     , "      go 0 = oneof"
     ]
     <> zipWith (baseBranch "        ") [0 :: Int ..] base0
     <> [ "        ]"
        , "      go n = frequency"
        ]
     <> zipWith (freqBranch "        " typeName) [0 :: Int ..] ctors
     <> [ "        ]" ]
  where
    baseBranch indent i c =
      let prefix = indent <> (if i == 0 then "[ " else ", ")
      in prefix <> renderRhsFlat c

    freqBranch indent tn i c =
      let prefix = indent <> (if i == 0 then "[ " else ", ")
          w      = if isRecursiveConstructor tn c then 1 else 2
      in prefix <> "(" <> T.pack (show (w :: Int)) <> ", "
              <> renderRhsSized tn c <> ")"

renderRhsFlat :: Constructor -> Text
renderRhsFlat c = case length (cArgs c) of
  0 -> "pure " <> cName c
  1 -> cName c <> " <$> arbitrary"
  n -> cName c
       <> " <$> arbitrary"
       <> T.concat (replicate (n - 1) " <*> arbitrary")

renderRhsSized :: Text -> Constructor -> Text
renderRhsSized typeName c = case cArgs c of
  []     -> "pure " <> cName c
  (a:as) ->
       cName c
    <> " <$> " <> slot a
    <> T.concat [ " <*> " <> slot a' | a' <- as ]
  where
    slot arg
      | isRecursiveArg typeName arg = "go (n `div` 2)"
      | otherwise                   = "arbitrary"

--------------------------------------------------------------------------------
-- response shaping
--------------------------------------------------------------------------------

successResult :: Text -> [Constructor] -> Text -> ToolResult
successResult typeName ctors tmpl =
  let payload =
        object
          [ "success"      .= True
          , "type_name"    .= typeName
          , "constructors" .= map renderCtor ctors
          , "template"     .= tmpl
          , "hint"         .= ( "Paste the template into the module that \
                               \defines '" <> typeName <> "'. If the type is \
                               \polymorphic, add an Arbitrary constraint on \
                               \each type variable." :: Text )
          ]
  in ToolResult
       { trContent = [ TextContent (encodeUtf8Text payload) ]
       , trIsError = False
       }

renderCtor :: Constructor -> Value
renderCtor c =
  object
    [ "name"  .= cName c
    , "arity" .= length (cArgs c)
    , "args"  .= cArgs c
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

formatCommandError :: CommandError -> Text
formatCommandError = \case
  ContainsNewline  -> "type_name must be a single line (no newline characters)"
  ContainsSentinel -> "type_name contains the internal framing sentinel and was rejected"
  EmptyInput       -> "type_name is empty"
  InputTooLarge sz cap ->
    "type_name is too large (" <> T.pack (show sz) <> " chars, cap is "
      <> T.pack (show cap) <> ")"

encodeUtf8Text :: Value -> Text
encodeUtf8Text = TL.toStrict . TLE.decodeUtf8 . encode
