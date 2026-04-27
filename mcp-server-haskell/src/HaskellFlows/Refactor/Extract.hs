-- | Pure rewrite engine for \"extract binding\".
--
-- Given a contiguous line range inside a module, produce a new version
-- of the file where those lines are replaced by a reference to a
-- freshly-introduced @where@ clause at the end of the enclosing top-
-- level declaration. The extracted body becomes the right-hand side of
-- the new binding.
--
-- Limitations (deliberate, since we're textual):
--
-- * Assumes the extracted range is an /expression/, not a pattern or
--   declaration. We reject ranges that sit at column 0 up-front (see
--   below); for everything else the compile-verify step in the tool
--   layer catches missuses — if the rewrite produces something that
--   doesn't parse, the snapshot is restored.
-- * The @where@ clause is appended at end-of-file. This is safe: GHC
--   treats it as attaching to the nearest top-level binding via layout
--   rules so long as it's at column 0. If the file already ends with a
--   @where@ on the nearest binding, the agent should prefer 'rename'
--   over 'extract' to add a local let — covered in the @hint@ we
--   return.
--
-- Top-level guard (issue #46):
--
-- Prior to this guard, an agent that pointed @extract_binding@ at a
-- whole equation line like @doubledSum xs = foldr ... xs@ would land
-- the textual cut at column 0. The cut produced (a) a dangling type
-- signature with no body and (b) a new top-level binding whose RHS
-- contained a second @=@ — i.e., GHC parse error. We can't fix that
-- without an AST, but we /can/ refuse it cheaply: in well-formed
-- Haskell, body expressions are always indented inside their enclosing
-- binding (RHS of @=@, body of a lambda, body of a @do@/@let@/
-- @where@). Anything at column 0 is a declaration, signature, import,
-- module header, or pragma — never an expression. So @commonIndent
-- body == 0@ is a robust canary for \"this is the wrong shape; ask
-- the agent to narrow scope to the body\".
--
-- This is a minimum-viable extract. A proper AST-aware version lives
-- behind a future @ghc_refactor@ powered by @ghc-lib-parser@; for now
-- this lets an agent factor a repeated sub-expression out with the
-- compiler enforcing correctness.
module HaskellFlows.Refactor.Extract
  ( extractBinding
  , ExtractResult (..)
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

-- | What the tool layer needs to render a response or revert.
data ExtractResult = ExtractResult
  { erNewContent :: !Text
  , erBindingTxt :: !Text       -- the new binding we appended
  , erIndent     :: !Int        -- indent of the extracted region
  }
  deriving stock (Eq, Show)

-- | Replace @[startLine, endLine]@ with a call to @newName@, and
-- append a @newName = <original block>@ binding to the file.
extractBinding
  :: Text                 -- ^ new binding name (validated)
  -> Int                  -- ^ start line (1-based, inclusive)
  -> Int                  -- ^ end line   (1-based, inclusive)
  -> Text                 -- ^ original file content
  -> Either Text ExtractResult
extractBinding newName startLine endLine raw
  | startLine < 1 =
      Left "line_start must be >= 1"
  | endLine < startLine =
      Left "line_end must be >= line_start"
  | otherwise = do
      let ls      = T.lines raw
          totalN  = length ls
      if endLine > totalN
        then Left ("line_end " <> tshow endLine
                <> " is past end of file (" <> tshow totalN <> " lines)")
        else do
          let (before, rest) = splitAt (startLine - 1) ls
              (body,  after) = splitAt (endLine - startLine + 1) rest
          if null body
            then Left "extracted range is empty"
            else do
              let indent = commonIndent body
              -- Issue #46: refuse top-level ranges. A range at column 0
              -- is never an expression; it's an equation, signature,
              -- import, or module-level decl. Lifting any of those
              -- would produce broken Haskell (dangling head + nested
              -- '='). Tell the agent how to recover.
              if indent == 0 && hasNonBlank body
                then Left (topLevelRefusal startLine endLine body)
                else do
                  let spaces     = T.replicate indent " "
                      callLine   = spaces <> newName
                      bindingTxt = renderBinding newName indent body
                      hadTrailing = T.isSuffixOf "\n" raw
                      newBody =
                        T.intercalate "\n"
                          (before <> [callLine] <> after <> [bindingTxt])
                        <> (if hadTrailing then "\n" else "")
                  pure ExtractResult
                    { erNewContent = newBody
                    , erBindingTxt = bindingTxt
                    , erIndent     = indent
                    }

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Common leading-whitespace length across a group of lines. Blank
-- lines are ignored so a vertical gap in the extracted block doesn't
-- collapse the indent to zero.
commonIndent :: [Text] -> Int
commonIndent ls =
  let nonBlank = filter (not . T.null . T.strip) ls
  in case nonBlank of
       []     -> 0
       (l:rest) ->
         foldr (min . T.length . T.takeWhile isSpace)
               (T.length (T.takeWhile isSpace l))
               rest

-- | Render @newName = <body>@ as a top-level binding that can be
-- appended to the file. Body lines are dedented by the common indent
-- so the right-hand side reads naturally at column 2, then bumped back
-- to column 0 for the @newName =@ header.
renderBinding :: Text -> Int -> [Text] -> Text
renderBinding newName indent body =
  let dedent ln =
        let n = min indent (T.length (T.takeWhile isSpace ln))
        in T.drop n ln
      -- Preserve relative indentation within the extracted block.
      reindent = map (\ln -> "  " <> dedent ln) body
      header   = newName <> " ="
  in T.intercalate "\n" (header : reindent)

-- | True if @body@ has at least one non-blank line. We use this to
-- distinguish \"top-level cut\" (at least one line at column 0) from
-- \"all-blank range\" (handled separately as the empty case).
--
-- HLint prefers the hoisted form @not . all (T.null . T.strip)@ over
-- @any (not . T.null . T.strip)@ (\"Hoist not\"). Both are pointwise
-- equivalent by De Morgan; we use the hoisted form to keep CI hint-free.
hasNonBlank :: [Text] -> Bool
hasNonBlank = not . all (T.null . T.strip)

-- | Refusal message for a column-0 (top-level) range. Names the first
-- non-blank line so the agent can see exactly which line tripped the
-- check, and tells them how to recover.
topLevelRefusal :: Int -> Int -> [Text] -> Text
topLevelRefusal startLine endLine body =
  let firstLine = case dropWhile (T.null . T.strip) body of
        (l:_) -> T.strip l
        []    -> ""
      preview = if T.length firstLine > 80
                  then T.take 80 firstLine <> "..."
                  else firstLine
  in "extract_binding requires an expression range, not a top-level "
     <> "declaration. The selected lines "
     <> tshow startLine <> "-" <> tshow endLine
     <> " start at column 0 (e.g. \"" <> preview <> "\"), which is a "
     <> "whole equation, type signature, import, or other top-level "
     <> "form — lifting it would produce invalid Haskell "
     <> "(\"<new_name>\" with no \"=\" at the call site, plus a nested "
     <> "\"=\" inside the appended binding). Narrow scope_line_start/"
     <> "scope_line_end to the indented body expression you actually "
     <> "want to lift (the right-hand side of the equation, a sub-"
     <> "expression of a let/where/do, etc.)."

tshow :: Show a => a -> Text
tshow = T.pack . show
