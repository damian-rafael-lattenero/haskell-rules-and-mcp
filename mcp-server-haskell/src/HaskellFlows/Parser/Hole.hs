-- | Parser for GHC typed-hole warnings [GHC-88464].
--
-- Phase 4 scope: extract the location, the hole identifier (@_@ or
-- @_name@), the expected type, and the list of \"Relevant bindings
-- include:\" entries. Valid-fit parsing is deferred to Phase 5 — the
-- structure of the GHC output is noisier there and not strictly needed
-- to drive the property-first workflow.
--
-- Input shape we care about looks roughly like:
--
-- > src/Foo.hs:12:5: warning: [GHC-88464] [-Wtyped-holes]
-- >     • Found hole: _ :: Int -> Int
-- >     • In the expression: _
-- >       In an equation for ‘bar’: bar = _
-- >     • Relevant bindings include
-- >         x :: Int (bound at src/Foo.hs:12:1)
-- >         bar :: Int -> Int (bound at src/Foo.hs:12:1)
module HaskellFlows.Parser.Hole
  ( TypedHole (..)
  , RelevantBinding (..)
  , HoleFit (..)
  , parseTypedHoles
  , splitDiagnosticBlocks
  , extractValidFits
  , isContinuationFitLine    -- #71: exported for unit tests
  , parseFitLine             -- #71: exported for unit tests
  ) where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

data RelevantBinding = RelevantBinding
  { rbName :: !Text
  , rbType :: !Text
  }
  deriving stock (Eq, Show)

data TypedHole = TypedHole
  { thHole             :: !Text           -- ^ @_@ or @_name@
  , thExpectedType     :: !Text
  , thFile             :: !Text
  , thLine             :: !Int
  , thColumn           :: !Int
  , thRelevantBindings :: ![RelevantBinding]
  , thValidFits        :: ![HoleFit]      -- ^ parsed from "Valid hole fits include:"
  }
  deriving stock (Eq, Show)

-- | One candidate from a @Valid hole fits include:@ section.
--
-- Format we observe from GHC:
--
-- > Valid hole fits include
-- >   foo :: Int -> Int (bound at src/Foo.hs:12:1)
-- >   bar :: Int -> Int
-- >     with bar @Int
-- >     (imported from 'Foo')
--
-- We keep this as flat as possible — the tool layer gets @name@,
-- @type@, and an optional @source@ block (everything after the type
-- up to the next fit).
data HoleFit = HoleFit
  { hfName   :: !Text
  , hfType   :: !Text
  , hfSource :: !(Maybe Text)
  }
  deriving stock (Eq, Show)

-- | Extract every hole from raw GHC diagnostic output.
parseTypedHoles :: Text -> [TypedHole]
parseTypedHoles = mapMaybe parseOne . splitDiagnosticBlocks
  where
    parseOne block
      | isHoleBlock block = parseHoleBlock block
      | otherwise         = Nothing

-- | Split the raw output into diagnostic blocks. A block begins at any
-- line matching @\<file\>:\<line\>:\<col\>@. The 'parseTypedHoles' pass
-- keeps only blocks tagged with typed-hole markers.
splitDiagnosticBlocks :: Text -> [Text]
splitDiagnosticBlocks out =
  let ls = T.lines out
  in go [] [] ls
  where
    go acc []   [] = reverse acc
    go acc cur  [] = reverse (T.intercalate "\n" (reverse cur) : acc)
    go acc cur  (l:rest)
      | looksLikeHeader l && not (null cur) =
          go (T.intercalate "\n" (reverse cur) : acc) [l] rest
      | otherwise =
          go acc (l : cur) rest

--------------------------------------------------------------------------------
-- internals
--------------------------------------------------------------------------------

-- | Does a line start with @<file>:<line>:<col>@?
--
-- Implemented as a small FSM rather than a regex so we stay ReDoS-safe
-- and avoid pulling regex-tdfa into the parser.
looksLikeHeader :: Text -> Bool
looksLikeHeader ln =
  case T.break (== ':') ln of
    (pre, rest)
      | T.null pre       -> False
      | T.null rest      -> False
      | T.any (== ' ') pre -> False  -- "Some hole fits include"
      | otherwise        -> hasColonNumberColonNumber (T.drop 1 rest)
  where
    hasColonNumberColonNumber t =
      let (n1, r1) = T.span isDigit t
          hasN1    = not (T.null n1) && not (T.null r1) && T.head r1 == ':'
      in hasN1 && let (n2, _) = T.span isDigit (T.drop 1 r1) in not (T.null n2)

isHoleBlock :: Text -> Bool
isHoleBlock b = "GHC-88464" `T.isInfixOf` b || "Wtyped-holes" `T.isInfixOf` b

parseHoleBlock :: Text -> Maybe TypedHole
parseHoleBlock block = do
  let ls = T.lines block
  hdr           <- headMay ls
  (f, l, c)     <- parseHeader hdr
  (holeId, ty)  <- extractHoleAndType ls
  let bindings  = extractBindings ls
      fits      = extractValidFits ls
  pure TypedHole
    { thHole             = holeId
    , thExpectedType     = ty
    , thFile             = f
    , thLine             = l
    , thColumn           = c
    , thRelevantBindings = bindings
    , thValidFits        = fits
    }

parseHeader :: Text -> Maybe (Text, Int, Int)
parseHeader ln =
  case T.splitOn ":" ln of
    (f : ls : cs : _) -> do
      l <- readDecimal ls
      c <- readDecimal cs
      pure (f, l, c)
    _ -> Nothing

-- | Pull the @Found hole: _name :: Type@ line and split hole/type at the
-- @::@ separator.
extractHoleAndType :: [Text] -> Maybe (Text, Text)
extractHoleAndType ls = do
  line <- firstMatching ("Found hole:" `T.isInfixOf`) ls
  let afterMarker = T.strip (snd (T.breakOn "Found hole:" line))
      body        = T.strip (T.drop (T.length "Found hole:") afterMarker)
  case T.breakOn "::" body of
    (_, rest) | T.null rest -> Nothing
    (hole, rest) ->
      Just (T.strip hole, T.strip (T.drop 2 rest))

-- | Extract the lines that sit under a @Relevant bindings include@ header
-- and that look like @name :: type (bound at …)@.
extractBindings :: [Text] -> [RelevantBinding]
extractBindings ls =
  let afterMarker = dropWhile (not . ("Relevant bindings include" `T.isInfixOf`)) ls
  in case afterMarker of
       []     -> []
       (_:bs) -> mapMaybe bindingLine (takeWhile isBindingLine bs)
  where
    isBindingLine l =
      let s = T.strip l
      in "::" `T.isInfixOf` s && not ("•" `T.isPrefixOf` s)

-- | Extract the @Valid hole fits include:@ section. The header line
-- opens the block; each candidate lives on a line indented to
-- exactly two levels below the header. Continuation lines (three or
-- more levels) are sub-details (with clauses, import annotations)
-- and attached to the preceding fit's @source@ field.
extractValidFits :: [Text] -> [HoleFit]
extractValidFits ls =
  let rest = dropWhile (not . ("Valid hole fits include" `T.isInfixOf`)) ls
  in case rest of
       []     -> []
       (_:rs) -> collapseFits (takeWhile isFitRegion rs)
  where
    isFitRegion l =
      let s = T.strip l
      in not (T.null s)
         && not ("•" `T.isPrefixOf` s)   -- next GHC bullet breaks the section

-- | Merge continuation lines (deeper indent than the candidate head)
-- into the preceding fit's @source@ field.
collapseFits :: [Text] -> [HoleFit]
collapseFits = go Nothing []
  where
    go mCurrent acc [] =
      reverse (maybeCons mCurrent acc)
    go mCurrent acc (l:ls')
      | T.null (T.strip l) = go mCurrent acc ls'
      | isContinuationFitLine l, Just cur <- mCurrent =
          let extra = T.strip l
              newSource = case hfSource cur of
                Nothing -> Just extra
                Just s  -> Just (s <> " " <> extra)
              updated = cur { hfSource = newSource }
          in go (Just updated) acc ls'
      | otherwise =
          let updatedAcc = maybeCons mCurrent acc
          in case parseFitLine l of
               Just fit -> go (Just fit) updatedAcc ls'
               Nothing  -> go Nothing updatedAcc ls'

    maybeCons Nothing  acc = acc
    maybeCons (Just x) acc = x : acc

-- | Issue #71: a continuation line lives BELOW a fit-head and
-- carries the @(bound at …)@ / @with … (imported from …)@
-- annotations. The pre-#71 predicate matched on a leading @(@,
-- which made it confuse an operator-named fit head like
-- @(-) :: forall a. Num a => a -> a -> a@ for a continuation
-- of the previous fit — its 'source' field then absorbed the
-- whole next entry's name + type + provenance.
--
-- The robust disambiguator is the type-signature substring
-- @\" :: \"@: GHC never emits @ :: @ inside a continuation
-- block. A line containing it is a fresh candidate, regardless
-- of how it starts. We keep the indent guard so blank-trailing
-- garbage at column 0 still drops out of the section.
isContinuationFitLine :: Text -> Bool
isContinuationFitLine l =
  let indent     = T.length (T.takeWhile (== ' ') l)
      stripped   = T.stripStart l
      hasTypeSig = " :: " `T.isInfixOf` stripped
  in indent >= 6 && not hasTypeSig

parseFitLine :: Text -> Maybe HoleFit
parseFitLine l =
  let stripped = T.strip l
  in case T.breakOn "::" stripped of
       (_, rest) | T.null rest -> Nothing
       (nm, rest) ->
         let tyFull = T.strip (T.drop 2 rest)
             -- Any "(bound at" / "(imported from" annotation on the
             -- same line is kept as source; the type half is
             -- everything before that.
             (ty, srcRaw) = T.breakOn "(" tyFull
             src = if T.null srcRaw then Nothing else Just (T.strip srcRaw)
         in if T.null (T.strip nm) || T.null (T.strip ty)
              then Nothing
              else Just HoleFit
                     { hfName   = T.strip nm
                     , hfType   = T.strip ty
                     , hfSource = src
                     }

bindingLine :: Text -> Maybe RelevantBinding
bindingLine raw =
  let stripped = T.strip raw
  in case T.breakOn "::" stripped of
       (_, rest) | T.null rest -> Nothing
       (nm, rest) ->
         let tyFull = T.strip (T.drop 2 rest)
             ty     = T.strip (fst (T.breakOn "(bound at" tyFull))
         in if T.null (T.strip nm) || T.null ty
              then Nothing
              else Just RelevantBinding
                     { rbName = T.strip nm
                     , rbType = ty
                     }

--------------------------------------------------------------------------------
-- list micro-helpers
--------------------------------------------------------------------------------

headMay :: [a] -> Maybe a
headMay []    = Nothing
headMay (x:_) = Just x

firstMatching :: (a -> Bool) -> [a] -> Maybe a
firstMatching _ []     = Nothing
firstMatching p (x:xs) = if p x then Just x else firstMatching p xs

readDecimal :: Text -> Maybe Int
readDecimal t =
  let digits = T.takeWhile (`elem` ("0123456789" :: String)) t
  in if T.null digits
       then Nothing
       else Just (T.foldl' step 0 digits)
  where
    step acc c = acc * 10 + (fromEnum c - fromEnum '0')
