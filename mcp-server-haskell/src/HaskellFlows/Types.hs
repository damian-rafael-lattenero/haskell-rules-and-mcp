-- | Core shared types with security invariants enforced by construction.
--
-- The TypeScript version of this server exposes a CWE-22 path-traversal
-- surface in several tools (refactor, rename, create-project): callers pass
-- a @module_path@ string, the server does @path.resolve(projectDir, p)@, and
-- the result may escape the project directory. The fix in TS is a runtime
-- guard every caller must remember.
--
-- Here we make the invariant static. The only way to obtain a 'ModulePath'
-- is through 'mkModulePath', which refuses to construct one that escapes
-- its associated 'ProjectDir'. A function that accepts @ModulePath@ is
-- therefore proven-safe at compile time — no runtime check, no caller
-- discipline, no TODO to forget.
module HaskellFlows.Types
  ( -- * Project directories
    ProjectDir
  , unProjectDir
  , mkProjectDir
    -- * Module paths (traversal-safe)
  , ModulePath
  , unModulePath
  , modulePathRelative
  , modulePathProject
  , mkModulePath
    -- * Errors
  , PathError (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath
  ( (</>)
  , equalFilePath
  , isAbsolute
  , normalise
  , pathSeparator
  , splitDirectories
  )

-- | An absolute, canonical path to a Haskell project root directory.
--
-- Invariant: 'unProjectDir' is always absolute and normalized.
newtype ProjectDir = ProjectDir FilePath
  deriving stock (Eq, Show)

unProjectDir :: ProjectDir -> FilePath
unProjectDir (ProjectDir p) = p

-- | Smart constructor for 'ProjectDir'. Rejects relative paths so callers
-- cannot accidentally mix working-directory state into the validation.
mkProjectDir :: FilePath -> Either PathError ProjectDir
mkProjectDir raw
  | not (isAbsolute raw) = Left (PathNotAbsolute (T.pack raw))
  | otherwise            = Right (ProjectDir (normalise raw))

-- | A module-file path that is guaranteed to live inside its 'ProjectDir'.
--
-- Carries both the relative form (what the caller passed — used for
-- display) and the resolved absolute form (what we actually open).
-- Exposing both avoids a re-resolution on every file operation.
data ModulePath = ModulePath
  { _mpProject  :: !ProjectDir
  , _mpRelative :: !FilePath
  , _mpAbsolute :: !FilePath
  }
  deriving stock (Eq, Show)

unModulePath :: ModulePath -> FilePath
unModulePath = _mpAbsolute

modulePathRelative :: ModulePath -> FilePath
modulePathRelative = _mpRelative

modulePathProject :: ModulePath -> ProjectDir
modulePathProject = _mpProject

-- | Smart constructor for 'ModulePath'.
--
-- Given a project directory and a (typically relative) raw path, resolves
-- the path and verifies the result lives inside the project. Rejects
-- anything that would escape via @..@, absolute roots, or symlink-like
-- tricks that survive 'normalise'.
--
-- Implementation note: 'System.FilePath.normalise' deliberately does
-- not collapse @..@ segments — they are treated as opaque path
-- components. Relying only on a @startsWith@ check against the project
-- root therefore admits traversal (confirmed by the failing
-- @mkModulePath rejects traversal@ test in the first build). The fix
-- here is segment-based: after splitting the fully-joined path into
-- directory components, any remaining @..@ means the path would leave
-- the project tree, so we reject. This makes the invariant hold without
-- needing @canonicalizePath@ (which would require @IO@).
--
-- This is the load-bearing function for the traversal invariant. Any
-- tool that accepts a user-supplied path must route through here.
mkModulePath :: ProjectDir -> FilePath -> Either PathError ModulePath
mkModulePath pd raw =
  let root      = unProjectDir pd
      joined    = normalise (root </> raw)
      rootN     = normalise root
      prefix    = rootN <> [pathSeparator]
      segments  = splitDirectories joined
      hasDotDot = ".." `elem` segments
      insidePrefix =
        joined `equalFilePath` rootN
          || take (length prefix) joined == prefix
  in if not hasDotDot && insidePrefix
       then Right (ModulePath pd raw joined)
       else Left
              (PathEscapesProject
                 (T.pack raw)
                 (T.pack root)
                 (T.pack joined))

-- | Path-validation failures. Constructor fields are positional to avoid
-- @-Wpartial-fields@ on named accessors that would only project out of
-- one of the sum variants.
data PathError
  = PathNotAbsolute !Text
    -- ^ Raw path argument that was not absolute.
  | PathEscapesProject !Text !Text !Text
    -- ^ Attempted path, project root, resolved path (all for diagnostics).
  deriving stock (Eq, Show)
