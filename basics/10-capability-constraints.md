# Capability constraints

- [Capability constraints](#capability-constraints)
  - [Tagless final / `MonadX` constraints](#tagless-final--monadx-constraints)
  - [Where we left off](#where-we-left-off)
  - [More constraints on what we can do in functions](#more-constraints-on-what-we-can-do-in-functions)
  - [Where do we go with this?](#where-do-we-go-with-this)
    - [Exercises (Capability constraints)](#exercises-capability-constraints)

## Tagless final / `MonadX` constraints

Sometimes we'd like to be very specific about what can happen in a function. This can be
accomplished in many ways, one of which is called "Tagless final". We've seen glimpses of this in
previous chapters, with both `MonadReader` and `MonadIO` showing up. When we see these in the type
signature of a function we know that the function has a certain capability and can confidently say
the function also works with any monad transformer stack that has the capability in question.

In this chapter we'll take a look at defining our own type classes for signalling behavior, to stand
in as a general example of how we can be more specific about what can happen in functions.

## Where we left off

We saw in our chapter on `Has` constraints that we could both generalize and constrain our functions
such that they can be used with any transformer stack but also only be usable if we have access to
a "log handle":

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

logToFile :: (HasLogHandle e, MonadReader e m, MonadIO m) => String -> m ()
logToFile logString = do
  fileHandle <- asks getLogHandle
  liftIO $ hPutStrLn fileHandle logString
```

## More constraints on what we can do in functions

We'd like to be even clearer about what is happening in our functions by way of one of these type
classes:

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

class HasLogHandle environment where
  getLogHandle :: environment -> Handle

instance HasLogHandle ApplicationState where
  getLogHandle = logHandle

class LogToHandle m where
  logToHandle :: Handle -> String -> m ()

instance LogToHandle AppMonad where
  logToHandle h = hPutStrLn h >>> liftIO

logToFile :: (HasLogHandle e, MonadReader e m, LogToHandle m) => String -> m ()
logToFile logString = do
  fileHandle <- asks getLogHandle
  logToHandle fileHandle logString
```

We could also imagine another capability type class, `LogToDefault` that does not take a handle to
write to:

```haskell
class LogToDefault m where
  outputToLog :: String -> m ()

-- Since `outputToLog` doesn't take a handle but we have one in our environment, we use it together
-- with `logToHandle` to make the default logging go to the file handle.
instance LogToDefault AppMonad where
  outputToLog s = do
    h <- asks getLogHandle
    logToHandle h s

-- `LogToDefault` is trivially implementable for just `IO` as well, meaning it would transparently
-- work in that context as well.
instance LogToDefault IO where
  outputToLog = putStrLn
```

With this type class we are free to implement entirely different behavior depending on the context
we are executing in. As an added example of that, let's imagine we had a custom testing context set
up and we wanted to implement `LogToDefault` for it in a way that let us capture the output:

```haskell
type TestMonad = ReaderT TestingState IO

newtype TestingState = TestingState
  { output :: IORef [String]
  }

instance LogToDefault TestMonad where
  outputToLog s = do
    outputReference <- asks output
    modifyIORef' outputReference (s :)
```

Note that since we are not mentioning something as specific as `Handle` in `outputToLog` the
implementation space actually grows considerably and different monads can implement it wildly
differently in comparison to `logToHandle :: Handle -> String -> m ()`. Sometimes, of course, it's
useful to be very specific about our requirements so that we can make more/stronger guarantees about
the behavior of them, however.

## Where do we go with this?

It's hard to say where to draw the line with these type classes. In the end it ought to be up to the
team that is working on things to say whether or not their constraints make sense. If your
application deals a lot with sending and receiving from AWS SQS, it could be useful to have
`MonadSQS` or even `MonadReadSQS`/`MonadWriteSQS` constraints to make it clear where this particular
effect is actually needed and wanted. When people are modifying the code as per new requirements,
they'll naturally think twice about having to add these capabilities in certain functions and will
also have to deal with these constraints being put on the calling functions as well. This can lead
to more thoughtful use of effects and much clearer signalling of capabilities.

It's also important to note that with a certain level of granularity these kinds of constraints can
become overly tedious. A mostly reasonable way to remedy this is to make type classes that
themselves group up other capabilities:

```haskell
newtype QueueName = QueueName {unQueueName :: Text}

class MonadReadSQS m where
  readFromQueue :: QueueName -> m (Maybe [Message])

class MonadWriteSQS m where
  writeToQueue :: QueueName -> Text -> m ()

class (MonadReadSQS m, MonadWriteSQS m) => MonadSQS m
```

Since we have these superclass requirements for `MonadSQS` we are guaranteeing that if `MonadSQS m`
shows up in a type signature, it means the `m` in question has implementations of `MonadReadSQS` and
`MonadWriteSQS`. This type of consolidation of capabilities can make sense depending on your
particular needs in terms of code clarity.

### Exercises (Capability constraints)

1. Go back to the first exercise you did in [Has constraints](./09-has-constraints.md) and add type
   classes for the different effectful things you are doing in your program. As an example, make a
   constraint that describes the ability to download a file via HTTP.
