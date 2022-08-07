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
import System.IO (hPutStrLn, print)

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
import System.IO (hPutStrLn, print)

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
import System.IO (hPutStrLn, print)

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
that can read an implicit argument of type `e`, where that `e` also has an implementation of
`HasLogHandle`. On top of that we are also saying that the `m` needs to be able to execute `IO`
actions.

This would be a perfectly fine stopping point for our function: It's generic enough where it can
work in many different stacks and with many different environment types, as long as they support
the stated things. The type signature itself also very reliably signals something about what is
going on in the function.

## `view`

When we've generalized this concept of requiring values in an environment, it can be helpful to provide
more tools for interacting with those requirements. One of the tools that is very common in the
ecosystem is `view`:

```haskell
view :: MonadReader env m => Getting a env a -> m a
```

This function will take a lens and automatically use that lens to get the thing that is being sought
after:

```haskell
import RIO
import System.IO (hPutStrLn)

data ApplicationState = ApplicationState
  { string :: String,
    loggingState :: LoggingState
  }

newtype LoggingState = LoggingState
  { logHandle :: Handle
  }

class HasLogHandle e where
  logHandleL :: Lens' e Handle

instance HasLogHandle ApplicationState where
  logHandleL =
    lens
      -- How to get our handle, we zoom in first on `loggingState` and then `loggingState`
      (logHandle . loggingState)
      -- How to set the same, given a value of `ApplicationState` and a new value for the log
      -- handle
      (\state a -> state {loggingState = (loggingState state) {logHandle = a}})

logToFile :: (MonadReader e m, MonadIO m, HasLogHandle e) => String -> m ()
logToFile logString = do
  fileHandle <- view logHandleL
  liftIO $ hPutStrLn fileHandle logString
```

### Exercises (Constraints with `ReaderT`)

1. Go back to the first exercise you did in [ReaderT](./08-readert.md) and add `Has` constraints
   where you think they are applicable.

2. Create a `HasSqlConnection` type class that describes the ability to get a `SqlConnection` from
   an environment. Implement it for the application state environment you would create for a web
   application that should be able to use a database as well as log to a file handle.

If you want to have an empty `SqlConnection` type you can define it as follows:

```haskell
data SqlConnection
```

3. Read the extra chapter on [optics](./extras/optics.md) and make your `HasSqlConnection` type
   class use a lens for accessing the SQL connection instead. Try using the `view` function for a
   function that needs to get the SQL connection.
