-- | @ghc_arbitrary@ — Wave-4 full GhcSession.
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
    -- * Issue #210 — compile-failure response (exported for tests)
  , compileFailedErr
    -- * Issue #219 — unboxed-constructor detection (exported for tests)
  , hasUnboxedConstructor
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isAlphaNum)
import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

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
import GHC.Utils.Outputable
  ( Outputable
  , defaultSDocContext
  , ppr
  , renderWithContext
  , sdocSuppressUniques
  )

import HaskellFlows.Ghc.ApiSession
  ( GhcSession
  , LoadFlavour (..)
  , firstLibraryOrTestSuite
  , loadForTarget
  , withGhcSession
  )
import HaskellFlows.Ghc.Sanitize
  ( sanitizeExpression
  )
import qualified HaskellFlows.Mcp.Envelope as Env
import HaskellFlows.Mcp.ParseError (formatParseError)
import HaskellFlows.Mcp.Protocol
import HaskellFlows.Mcp.ToolName (ToolName (..), toolNameText)
import HaskellFlows.Parser.Error (GhcError)
import HaskellFlows.Parser.Type (isOutOfScope)

descriptor :: ToolDescriptor
descriptor =
  ToolDescriptor
    { tdName        = toolNameText GhcArbitrary
    , tdDescription =
        "PURPOSE: Generate a QuickCheck Arbitrary instance template for a "
          <> "user-defined data type. "
          <> "WHEN: a new data/newtype needs an Arbitrary before property "
          <> "testing; polymorphic types get an Arbitrary constraint per "
          <> "type variable. "
          <> "WHEN NOT: ghc_quickcheck once the instance exists; hand-edit "
          <> "GADTs / existentials / constrained constructors the template "
          <> "cannot fully express. "
          <> "PREREQUISITES: the type's module loaded so its constructors "
          <> "resolve. "
          <> "OUTPUT: {instance} text for the agent to paste — does not "
          <> "modify files. "
          <> "SEE ALSO: ghc_quickcheck, ghc_suggest."
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
    pure (formatParseError parseError)
  Right (ArbitraryArgs tname) -> case sanitizeExpression tname of
    Left cmdErr ->
      pure (Env.toolResponseToResult
              (Env.mkRefused (Env.sanitizeRejection "type_name" cmdErr)))
    Right safe -> do
      tgt <- firstLibraryOrTestSuite ghcSess
      eLoad <- try (loadForTarget ghcSess tgt Strict)
      case eLoad :: Either SomeException (Bool, [GhcError]) of
        Left ex ->
          pure (subprocessErr
                  ("loadForTarget failed: " <> T.pack (show ex)))
        -- Issue #210: check the Bool — False means the module has
        -- compile errors. Proceeding to renderTyThing would then
        -- fail with a confusing not_in_scope on a type that IS
        -- defined but wasn't loaded. Return a clear validation
        -- error instead.
        Right (False, loadErrs) ->
          pure (compileFailedErr (length loadErrs))
        Right (True, _) -> do
          -- loadForTarget already primed the session with the
          -- correct stanza flags + setContext. Don't wrap in
          -- withStanzaFlags here — re-applying setSessionDynFlags
          -- would reset the interactive context established above,
          -- leaving parseName unable to resolve user types.
          eRes <- try (withGhcSession ghcSess (renderTyThing safe))
          case eRes :: Either SomeException (Maybe Text) of
            Left ex ->
              pure (notInScopeErr
                      ("'" <> safe <> "' not in scope: " <> T.pack (show ex)))
            -- Issue #218: getInfo returned Nothing after a successful
            -- parseName + loadForTarget. This happens for GHC wired-in
            -- primitives (Bool lives in ghc-prim which is not explicitly
            -- listed in the stanza's -package-id flags; after
            -- -hide-all-packages the lookup fails silently). Report a
            -- clear validation error instead of the misleading "not in
            -- scope" message.
            Right Nothing ->
              pure (validationErr
                      ( "ghc_arbitrary cannot introspect '" <> safe <> "'. "
                      <> "This happens for GHC wired-in primitives (Bool, "
                      <> "Char, Int, Word, …) and type classes or type "
                      <> "synonyms. These types already have Arbitrary "
                      <> "instances in Test.QuickCheck — no template is "
                      <> "needed." ))
            Right (Just rendered)
              | isOutOfScope rendered -> pure (notInScopeErr rendered)
              | otherwise -> do
                  let params = parseTypeParams rendered
                  case parseConstructors rendered of
                    []    -> pure (validationErr
                              ( "No constructors parsed for '" <> safe
                              <> "'. It may be a GADT, typeclass, or type synonym — "
                              <> "those need a hand-written Arbitrary." ))
                    ctors
                      -- Issue #219: primitive-wrapping types (Int, Char,
                      -- Word, …) expose unboxed constructors (I#, C#, W#)
                      -- whose args are unboxed types (Int#, Char#). There
                      -- is no Arbitrary Int# in QuickCheck, so the
                      -- generated template would not compile. Detect and
                      -- report instead of emitting broken code.
                      | any hasUnboxedConstructor ctors ->
                          pure (validationErr
                                  ( "'" <> safe <> "' has unboxed-primop "
                                  <> "constructors (e.g. I#, C#, W#). "
                                  <> "ghc_arbitrary cannot generate a valid "
                                  <> "template for these types. Use "
                                  <> "'arbitraryBoundedIntegral' for integral "
                                  <> "types or 'arbitraryUnicodeChar' for Char." ))
                      | otherwise ->
                          pure (successResult safe ctors
                                  (renderTemplate safe params ctors))

-- | Issue #90 Phase C: caller-side parse failure → status='failed'
-- with kind='missing_arg' or 'type_mismatch' (Aeson FromJSON
-- failure messages tell us which).

-- | Issue #90 Phase C: lookup miss (parseName/getInfo failed
-- because the type isn't in scope) → status='no_match' with
-- kind='not_in_scope'. Distinct from a validation failure: the
-- input was syntactically fine, just absent.
notInScopeErr :: Text -> ToolResult
notInScopeErr msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.NotInScope msg))

-- | Issue #90 Phase C: structural rejection of types we can't
-- template (GADTs, typeclasses, type synonyms) → kind='validation'.
-- The input is syntactically a type name and is in scope; we just
-- don't have a template generator for its shape.
validationErr :: Text -> ToolResult
validationErr msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.Validation msg))

-- | Issue #90 Phase C: unexpected GHC API exception
-- (loadForTarget threw) → kind='subprocess_error'. The exception
-- text is preserved verbatim in the message body.
subprocessErr :: Text -> ToolResult
subprocessErr msg =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.SubprocessError msg))

-- | Issue #210: module compile failure — loadForTarget returned
-- (False, errors). The user needs to fix compile errors before
-- ghc_arbitrary can resolve the type. Status='failed',
-- kind='validation'.
compileFailedErr :: Int -> ToolResult
compileFailedErr n =
  Env.toolResponseToResult
    (Env.mkFailed (Env.mkErrorEnvelope Env.Validation
      ( "Module has " <> T.pack (show n) <> " compile error(s); "
      <> "fix them first (use ghc_check_module or ghc_explain_error), "
      <> "then retry ghc_arbitrary." )))

-- | Resolve the name and render the resulting 'TyThing' in the
-- exact @data T = A | B Int | ...@ shape @:info@ would print.
--
-- 'showPprUnsafe' on a bare 'TyThing' only renders @"Type
-- constructor \`T\'"@ — useless for parseConstructors. Instead we
-- walk the 'TyCon' directly: tyConTyVars for the header, then
-- tyConDataCons + dataConOrigArgTys for each constructor.
--
-- #226: 'parseName' can return multiple names when an external-package
-- type shares the same unqualified name as the user's home-module type.
-- Taking only the first name (which is often the external one) causes
-- 'getInfo' to return Nothing for it, triggering the misleading
-- "wired-in primitive" error.  We now try each name in order and
-- return the first ATyCon result we find.
renderTyThing :: Text -> Ghc (Maybe Text)
renderTyThing nm = do
  names <- parseName (T.unpack nm)
  let tryName n = do
        info <- getInfo True n
        pure $ case info of
          Just (ATyCon tc, _, _, _, _) -> Just (renderTyConAsDataDecl tc)
          _                            -> Nothing
  results <- mapM tryName (toList names)
  pure (firstJust results)
  where
    firstJust [] = Nothing
    firstJust (x:xs) = case x of
      Just _  -> x
      Nothing -> firstJust xs

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

-- | #170: Render an 'Outputable' value without GHC's internal unique
-- suffixes (e.g. @a_ig1m@ → @a@). 'showPprUnsafe' uses the default
-- 'SDocContext' which has 'sdocSuppressUniques = False', causing type
-- variable names in constructor argument positions to leak GHC-
-- internal identifiers into the response's @constructors.args@ field.
showPprNoUniques :: Outputable a => a -> String
showPprNoUniques =
  renderWithContext (defaultSDocContext { sdocSuppressUniques = True }) . ppr

renderDataCon :: DataCon -> Text
renderDataCon dc =
  let cn   = T.pack (occNameString (nameOccName (dataConName dc)))
      -- #170: use showPprNoUniques so argument types containing type
      -- variables print "a b" not "a_ig1m b_xyz99".
      args = map (parenArg . T.pack . showPprNoUniques . scaledThing)
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

-- | Issue #219: detect constructors whose name ends with @#@ (GHC
-- unboxed-primop wrappers: I#, C#, W#, …). The args of these
-- constructors are unboxed types (Int#, Char#, …) for which
-- 'Arbitrary' does not exist, so any template we'd emit would fail
-- to compile.
hasUnboxedConstructor :: Constructor -> Bool
hasUnboxedConstructor c = "#" `T.isSuffixOf` cName c

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

-- | Issue #90 Phase C: rendered template → status='ok'. All the
-- caller-facing fields ('type_name', 'constructors', 'template',
-- 'hint') stay under 'result' so existing consumers keep working.
successResult :: Text -> [Constructor] -> Text -> ToolResult
successResult typeName ctors tmpl =
  Env.toolResponseToResult (Env.mkOk (object
    [ "type_name"    .= typeName
    , "constructors" .= map renderCtor ctors
    , "template"     .= tmpl
    , "hint"         .= ( "Paste the template into the module that \
                         \defines '" <> typeName <> "'. If the type is \
                         \polymorphic, add an Arbitrary constraint on \
                         \each type variable." :: Text )
    ]))

renderCtor :: Constructor -> Value
renderCtor c =
  object
    [ "name"  .= cName c
    , "arity" .= length (cArgs c)
    , "args"  .= cArgs c
    ]
