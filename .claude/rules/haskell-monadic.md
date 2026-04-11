# Monadic Code Patterns

## Do-Notation Essentials

### `<-` vs `let` in do blocks
```haskell
-- <- unwraps the monad
do
  line <- getLine          -- line :: String (unwrapped from IO String)
  let upper = map toUpper line  -- pure binding, no unwrapping
  putStrLn upper

-- WRONG: let binds the monadic value itself, doesn't run it
do
  let line = getLine       -- line :: IO String (NOT String)
```

**Rule**: Use `<-` for monadic actions, `let` for pure computations. When a type error says "Expected `a` but got `m a`", you probably need `<-` instead of `let`. When it says "Expected `m a` but got `a`", you probably need `pure`/`return`.

### Last expression in do
The last expression in a `do` block IS the return value. No `return` needed unless wrapping a pure value:
```haskell
-- WRONG: redundant return
getAndUpper :: IO String
getAndUpper = do
  line <- getLine
  return (map toUpper line)  -- OK but idiomatic to use pure

-- BETTER:
getAndUpper = do
  line <- getLine
  pure (map toUpper line)

-- WRONG: missing pure/return for a pure value
getLength :: IO Int
getLength = do
  line <- getLine
  length line  -- ERROR: Int is not IO Int
-- RIGHT:
getLength = do
  line <- getLine
  pure (length line)
```

### Sequencing with `>>` vs `>>=`
```haskell
-- >> discards the result of the first action
main = putStrLn "hello" >> putStrLn "world"

-- >>= passes the result to the next function
main = getLine >>= putStrLn

-- In do notation, _ <- is equivalent to >>
do
  _ <- putStrLn "hello"  -- same as putStrLn "hello" >> ...
  putStrLn "world"
```

### void for discarding results
When an action returns a value you don't need and `-Wall` warns about it:
```haskell
import Control.Monad (void)

-- WRONG: warning about unused result
do
  swapMVar ref newVal  -- returns old value, warning if unused

-- RIGHT:
do
  void $ swapMVar ref newVal
  -- or: _ <- swapMVar ref newVal
```

---

## Monad Transformers

### The transformer stack pattern
Transformers wrap monads to combine effects. Read the stack inside-out:
```haskell
type App a = ReaderT Config (ExceptT AppError IO) a
-- IO is the base (can do I/O)
-- ExceptT adds error handling (can throw AppError)
-- ReaderT adds read-only environment (can read Config)
```

### lift vs liftIO
```haskell
-- lift: go up ONE layer in the stack
-- liftIO: go from IO directly to any MonadIO stack (any depth)

type App a = ReaderT Config IO a

example :: App ()
example = do
  cfg <- ask                    -- ReaderT operation (no lift needed)
  liftIO $ putStrLn "hello"     -- IO operation (liftIO from IO to ReaderT _ IO)

-- For deeper stacks, liftIO is almost always preferred over chained lifts:
type Deep a = StateT Int (ReaderT Config IO) a

-- WRONG: fragile, breaks if you add/remove layers
bad :: Deep ()
bad = lift (lift (putStrLn "hello"))

-- RIGHT: works regardless of stack depth
good :: Deep ()
good = liftIO (putStrLn "hello")
```

### Common transformer patterns

**ReaderT for configuration / environment:**
```haskell
type App a = ReaderT AppEnv IO a

runApp :: AppEnv -> App a -> IO a
runApp env app = runReaderT app env

-- Inside App:
getDbConn :: App Connection
getDbConn = asks dbConnection  -- asks applies a function to the environment
```

**ExceptT for typed errors:**
```haskell
type App a = ExceptT AppError IO a

runApp :: App a -> IO (Either AppError a)
runApp = runExceptT

-- Throwing errors:
validate :: Input -> App ValidInput
validate input
  | isValid input = pure (mkValid input)
  | otherwise     = throwError (InvalidInput input)

-- Catching errors:
safeRun :: App a -> App (Either AppError a)
safeRun action = catchError (Right <$> action) (pure . Left)
```

**StateT for mutable state:**
```haskell
type Counter a = StateT Int IO a

increment :: Counter ()
increment = modify' (+1)  -- Use modify' (strict) not modify (lazy) to avoid space leaks

getCount :: Counter Int
getCount = get

runCounter :: Counter a -> IO (a, Int)
runCounter c = runStateT c 0
```

### Running transformer stacks
Run from the outside in — the order of `run*` calls matters:
```haskell
type App a = StateT AppState (ReaderT Config IO) a

runApp :: Config -> AppState -> App a -> IO (a, AppState)
runApp cfg st app = runReaderT (runStateT app st) cfg
--                  ^outermost   ^innermost
```

---

## MTL-style constraints vs concrete stacks

### Prefer constraints for library code
```haskell
-- CONCRETE (ties you to a specific stack):
fetchUser :: Int -> ReaderT Config (ExceptT DbError IO) User

-- MTL-STYLE (works with any stack that has these capabilities):
fetchUser :: (MonadReader Config m, MonadError DbError m, MonadIO m) => Int -> m User
```

**When to use which:**
- Application code with a fixed stack → concrete type (simpler, better errors)
- Library/reusable code → MTL constraints (more flexible)
- When in doubt → start concrete, generalize if needed

### Common MTL classes
| Class | Provides | Key operations |
|-------|----------|----------------|
| `MonadReader r` | Read-only env | `ask`, `asks`, `local` |
| `MonadState s` | Mutable state | `get`, `put`, `modify'`, `gets` |
| `MonadError e` | Typed errors | `throwError`, `catchError` |
| `MonadIO` | IO access | `liftIO` |
| `MonadWriter w` | Append-only log | `tell`, `listen`, `pass` |

---

## Common Monadic Anti-patterns

### Unnecessary nesting
```haskell
-- WRONG: pointless nesting
do
  x <- action1
  do
    y <- action2
    pure (x + y)

-- RIGHT: flat do block
do
  x <- action1
  y <- action2
  pure (x + y)
```

### return/pure at wrong position
```haskell
-- WRONG: return wraps a value, it doesn't exit early like in imperative languages
doStuff :: IO Int
doStuff = do
  putStrLn "hello"
  return 42         -- This IS the return value, but only because it's last
  putStrLn "world"  -- This line DOES execute (return doesn't short-circuit)
  return 0          -- THIS is the actual return value
```

### Mixing monads in one do block
```haskell
-- WRONG: can't mix IO and Maybe in one do block
bad :: IO (Maybe Int)
bad = do
  x <- getLine          -- IO monad
  y <- readMaybe x      -- Maybe monad — ERROR: wrong monad

-- RIGHT: handle the Maybe explicitly
good :: IO (Maybe Int)
good = do
  x <- getLine
  case readMaybe x of   -- pattern match on the Maybe
    Nothing -> pure Nothing
    Just n  -> pure (Just n)

-- OR use MaybeT to combine them:
better :: MaybeT IO Int
better = do
  x <- liftIO getLine
  MaybeT (pure (readMaybe x))
```

### mapM vs traverse vs for_
```haskell
-- mapM and traverse are the same since GHC 7.10 (traverse is more general)
-- Use traverse for Traversable, mapM for legacy compatibility

-- When you DON'T need the results, use for_ or mapM_ (avoids building a list):
do
  for_ [1..100] $ \i ->
    putStrLn (show i)

-- WRONG: builds a list of () that gets thrown away
do
  results <- mapM putStrLn strings  -- results :: [()]
```

### when/unless for conditional effects
```haskell
import Control.Monad (when, unless)

-- WRONG: verbose
do
  if verbose
    then putStrLn "debug info"
    else pure ()

-- RIGHT:
do
  when verbose $ putStrLn "debug info"

  unless quiet $ putStrLn "output"
```

---

## Debugging Monadic Type Errors

When a monadic type error is confusing:

1. **Annotate the do block's monad explicitly:**
```haskell
example :: IO ()  -- Make sure the do block knows which monad
example = do
  ...
```

2. **Annotate each `<-` binding:**
```haskell
do
  (x :: String) <- getLine
  (n :: Int) <- pure (length x)
```

3. **Check which monad each sub-expression lives in:**
```
-- Use :t in GHCi:
:t getLine        -- IO String
:t readMaybe      -- Read a => String -> Maybe a
:t throwError     -- MonadError e m => e -> m a
```

4. **When the error mentions `m0` or `m1` (ambiguous monad):**
   Add a type annotation to the first `<-` or to the `do` block's result type.

5. **When the error mentions `No instance for (MonadState s IO)`:**
   You're trying to use a transformer operation in the wrong monad. Check your stack.
