# `Has` constraints

- [`Has` constraints](#has-constraints)
  - [Constraints with `ReaderT`](#constraints-with-readert)
    - [Exercises (Constraints with `ReaderT`)](#exercises-constraints-with-readert)

## Constraints with `ReaderT`

When we combined implicit arguments and `IO` we could start using `IO` in our functions, which
allowed us to add very basic logging to a file in our functions, while also not having to mention
the file name or file handle that we are logging with:

```haskell
import RIO
import System.IO (hPutStrLn, openFile, print)

-- type AppMonad a = ReaderT ApplicationState IO a
type AppMonad = ReaderT ApplicationState IO

data ApplicationState = ApplicationState
  { string :: String,
    logHandle :: Handle
  }

runMain :: IO ()
runMain = do
  logHandle <- openFile "./run-log.txt" AppendMode
  hSetBuffering logHandle LineBuffering
  let initialState = ApplicationState {string = "", logHandle}
  x <- runReaderT (canReadString 5) initialState {string = "Quanterall"}
  y <- runReaderT (canReadString 5) initialState {string = "Quanteral"}
  print x
  print y

notPassingArguments :: AppMonad Int
notPassingArguments = do
  ApplicationState {string} <- ask
  -- We can use `logToFile` here and not be concerned with the file handle because we know it's in
  -- the environment we're executing inside of already.
  logToFile $ "We got '" <> string <> "' from the environment"
  pure $ length string

canReadString :: Int -> AppMonad Int
canReadString added = do
  logToFile "We're about to call `notPassingArguments`"
  result <- notPassingArguments
  pure $ added + result

logToFile :: String -> AppMonad ()
logToFile logString = do
  -- We can use the `asks` function to automatically apply a function to the environment
  fileHandle <- asks logHandle -- `logHandle :: ApplicationState -> Handle`
  liftIO $ hPutStrLn fileHandle logString
```

If we wanted to generalize `logToFile`, we would have to modify the return value somewhat. If we
wanted to use it in another project, it's unlikely that we would have our `AppMonad` show up in that
project.

The solution to this is what could colloquially be called `Has` typeclasses. Let's examine the use
of one to generalize `logToFile`:

```haskell
import RIO
import System.IO (hPutStrLn, openFile, print)

-- type AppMonad a = ReaderT ApplicationState IO a
type AppMonad = ReaderT ApplicationState IO

data ApplicationState = ApplicationState
  { string :: String,
    logHandle :: Handle
  }

runMain :: IO ()
runMain = do
  logHandle <- openFile "./run-log.txt" AppendMode
  hSetBuffering logHandle LineBuffering
  let initialState = ApplicationState {string = "", logHandle}
  x <- runReaderT (canReadString 5) initialState {string = "Quanterall"}
  y <- runReaderT (canReadString 5) initialState {string = "Quanteral"}
  print x
  print y

notPassingArguments :: AppMonad Int
notPassingArguments = do
  ApplicationState {string} <- ask
  -- We can use `logToFile` here and not be concerned with the file handle because we know it's in
  -- the environment we're executing inside of already.
  logToFile $ "We got '" <> string <> "' from the environment"
  pure $ length string

canReadString :: Int -> AppMonad Int
canReadString added = do
  logToFile "We're about to call `notPassingArguments`"
  result <- notPassingArguments
  pure $ added + result

-- `HasLogHandle` is now a constraint on a type, meaning we can guarantee that whatever generic type
-- is used has an implementation of it.
class HasLogHandle e where
  getLogHandle :: e -> Handle

-- We implement `HasLogHandle` here for `ApplicationState`, meaning that while we've genericized it,
-- we still keep it as usable as it was before in our application.
instance HasLogHandle ApplicationState where
  getLogHandle = logHandle

-- This now works with `ReaderT` and any `e` that we have implemented `HasLogHandle` for.
logToFile :: (HasLogHandle e) => String -> ReaderT e IO ()
logToFile logString = do
  fileHandle <- asks getLogHandle
  liftIO $ hPutStrLn fileHandle logString
```

We've now made `HasLogHandle` generic over the environment, meaning we can use it in applications
that use `ReaderT` over `IO` with some environment we've implemented `HasLogHandle` for. If we want
to support any monad transformer stack, however, we can go further:

```haskell
import RIO
import System.IO (hPutStrLn, openFile, print)

-- type AppMonad a = ReaderT ApplicationState IO a
type AppMonad = ReaderT ApplicationState IO

data ApplicationState = ApplicationState
  { string :: String,
    logHandle :: Handle
  }

runMain :: IO ()
runMain = do
  logHandle <- openFile "./run-log.txt" AppendMode
  hSetBuffering logHandle LineBuffering
  let initialState = ApplicationState {string = "", logHandle}
  x <- runReaderT (canReadString 5) initialState {string = "Quanterall"}
  y <- runReaderT (canReadString 5) initialState {string = "Quanteral"}
  print x
  print y

notPassingArguments :: AppMonad Int
notPassingArguments = do
  ApplicationState {string} <- ask
  -- We can use `logToFile` here and not be concerned with the file handle because we know it's in
  -- the environment we're executing inside of already.
  logToFile $ "We got '" <> string <> "' from the environment"
  pure $ length string

canReadString :: Int -> AppMonad Int
canReadString added = do
  logToFile "We're about to call `notPassingArguments`"
  result <- notPassingArguments
  pure $ added + result

class HasLogHandle e where
  getLogHandle :: e -> Handle

instance HasLogHandle ApplicationState where
  getLogHandle = logHandle

-- This now works for any monad stack that has access to `Reader` functionality as well as `IO`.
logToFile :: (HasLogHandle e, MonadReader e m, MonadIO m) => String -> m ()
logToFile logString = do
  fileHandle <- asks getLogHandle
  liftIO $ hPutStrLn fileHandle logString
```

We've added two new constraints to our function; we can now guarantee that this works with any stack
that has `Reader` capabilities as well as `IO` regardless of any other transformers they may have.

This is a perfectly fine stopping point for a program in terms of abstraction and is very useful in
a library. We've made it generic enough where it can be used in many different contexts but are not
mentioning any superfluous expectations; we're effectively saying in our type signature that we need
the capability to read an implicit value of type `e` and that type needs to have `HasLogEnv`
implemented for it. We are also saying that we will be doing `IO`.

### Exercises (Constraints with `ReaderT`)

1. Go back to the first exercise you did in [ReaderT](./08-readert.md) and add `Has` constraints
   where you think they are applicable.
