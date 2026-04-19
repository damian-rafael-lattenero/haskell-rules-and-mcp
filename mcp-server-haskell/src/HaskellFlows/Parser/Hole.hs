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
  , parseTypedHoles
  , splitDiagnosticBlocks
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
  { thHole            :: !Text           -- ^ @_@ or @_name@
  , thExpectedType    :: !Text
  , thFile            :: !Text
  , thLine            :: !Int
  , thColumn          :: !Int
  , thRelevantBindings :: ![RelevantBinding]
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
  let bindings = extractBindings ls
  pure TypedHole
    { thHole             = holeId
    , thExpectedType     = ty
    , thFile             = f
    , thLine             = l
    , thColumn           = c
    , thRelevantBindings = bindings
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
