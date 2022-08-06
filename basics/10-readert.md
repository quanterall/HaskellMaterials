# The ReaderT Monad Transformer

- [The ReaderT Monad Transformer](#the-readert-monad-transformer)
  - [Monad transformers](#monad-transformers)
  - [ReaderT](#readert)
    - [The `ReaderT` type](#the-readert-type)
  - [Our `Reader` example, extended](#our-reader-example-extended)
  - [Adding very basic logging to our example](#adding-very-basic-logging-to-our-example)
  - [Creating a custom type signature](#creating-a-custom-type-signature)
    - [Exercises (ReaderT)](#exercises-readert)
      - [Exercise notes (ReaderT)](#exercise-notes-readert)

## Monad transformers

A monad transformer is a data type that allows us to wrap one monad in another. What this means is
that it enables a context wherein we get the capabilities of both the transformer and the monad it
is wrapping. Monad transformers are themselves monads, which means we can create multiple layers to
get an arbitrary amount of capabilities. It's relatively rare in normal applications that you'd
create big stacks of monad transformers, however, as most things can be done with `ReaderT` wrapping
the `IO` monad.

## ReaderT

`ReaderT` is the transformer version of the `Reader` monad and gives us the capability to read an
environment that is passed around implicitly in an entire call graph. Together with `IO` this means
that we get access to a whole host of effectful things that we couldn't get with just `Reader`:

- Running `STM` actions
- Modifying and reading `IORef`s
- Interacting with database connection handles
- ...

The list of things we can do with `ReaderT` on top of `IO` is very long and this is why it's such a
powerful base structure for most of our applications.

### The `ReaderT` type

```haskell
newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}
```

The definition here itself isn't as important initially as the type signature and what the different
parts represent. We can restate the definition as follows:

```haskell
newtype ReaderT environment wrappedMonad returnValue =
  ReaderT {runReaderT :: environment -> wrappedMonad returnValue}
```

The `wrappedMonad` type will decide what we can **additionally** do, on top of having implicit
values we can read from an environment. It's very common to create applications that execute in the
`ReaderT appState IO a` context, where `appState` is the application's application state. This
allows for implicit environment reading as well as `IO` actions, which means we can read from
`IORef`s, use transactional variables and so on. This makes it so that we get access to a very
tightly controlled usage of mutable variables with a clear initialization stage, as the application
state has to be initialized with `runReaderT` at the start of the application.

## Our `Reader` example, extended

We will continue with our `Reader` example and extend it to work with `ReaderT`, allowing us to
retain the functionality of using implicit values but also bake in `IO`:

```haskell
module Library where

import RIO -- requires `rio` in `package.yaml`
import System.IO (print, putStrLn)

main :: IO ()
main = do
  -- Note how we have to use `bind`/`<-` here because `runReaderT` returns a value of type `m a`,
  -- in this case `IO a`. This is different from `runReader`, which returns just `a`.
  x <- runReaderT (canReadString 5) "Quanterall"
  y <- runReaderT (canReadString 5) "Quanteral"
  print x -- 15
  print y -- 14

notPassingArguments :: ReaderT String IO Int
notPassingArguments = do
  -- `ask` retrieves the environment, in this case a `String`.
  stringFromEnvironment <- ask
  -- Note how we use `liftIO` here to execute IO actions in a monad that contains `IO`.
  liftIO $ putStrLn $ "We got '" <> stringFromEnvironment <> "' from the environment"
  pure $ length stringFromEnvironment

canReadString :: Int -> ReaderT String IO Int
canReadString added = do
  -- We don't need to pass any arguments here; `notPassingArguments` will read the environment.
  liftIO $ putStrLn "We're about to call `notPassingArguments`"
  result <- notPassingArguments
  pure $ added + result
```

## Adding very basic logging to our example

We'd like to add something like logging to our example. Let's switch out our environment type with
a composite type that holds several things:

```haskell
import RIO
import System.IO (hPutStrLn, print)

data ApplicationState = ApplicationState
  { string :: String,
    logHandle :: Handle
  }

main :: IO ()
main = do
  logHandle <- openFile "./run-log.txt" AppendMode
  hSetBuffering logHandle LineBuffering
  let initialState = ApplicationState {string = "", logHandle}
  x <- runReaderT (canReadString 5) initialState {string = "Quanterall"}
  y <- runReaderT (canReadString 5) initialState {string = "Quanteral"}
  print x
  print y

notPassingArguments :: ReaderT ApplicationState IO Int
notPassingArguments = do
  ApplicationState {string} <- ask
  -- We can use `logToFile` here and not be concerned with the file handle because we know it's in
  -- the environment we're executing inside of already.
  logToFile $ "We got '" <> string <> "' from the environment"
  pure $ length string

canReadString :: Int -> ReaderT ApplicationState IO Int
canReadString added = do
  logToFile "We're about to call `notPassingArguments`"
  result <- notPassingArguments
  pure $ added + result

logToFile :: String -> ReaderT ApplicationState IO ()
logToFile logString = do
  -- We can use the `asks` function to automatically apply a function to the environment
  fileHandle <- asks logHandle -- `logHandle :: ApplicationState -> Handle`
  liftIO $ hPutStrLn fileHandle logString
```

We've now added a `logToFile` function that automatically gets the file handle it requires from the
environment. Despite very clearly writing via a file handle, which would ordinarily require us to
pass a handle and a string, we've been able to abstract this away into what is essentially a custom
`putStrLn` that writes to a log file we've set up at the beginning of our program.

## Creating a custom type signature

A lot of programs will be written with a certain transformer stack (collection of monads) in mind
and will mention this stack in many places. In this case we can very easily use a type alias to name
that stack and not be forced to write it out everywhere we use it:

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

We've now baked in the idea of having access to this state and this functionality in our `AppMonad`
type alias, which means we can just write functions with this return type and get access to the
context that it provides.

### Exercises (ReaderT)

1. Create a program that downloads the last three releases from a given repository on GitHub. You
   can find the [API specification here](https://docs.github.com/en/rest/reference/repos#releases).

   Initialize the program with a `Manager` from the `http-client-tls` package and make it an
   implicit argument via `ReaderT`. Feel free to define an abstraction that uses this `Manager` to
   download files, such that it can be used without even mentioning a `Manager`.

   Also add logging to this application to a given file based on the time[0] of the execution of the
   program. Suggested logging events would be when the program was doing things like fetching the
   list of releases as well as the downloading of files and perhaps even when a file is **not**
   downloaded because it already exists.

2. When the program in exercise 1 starts, start a thread via `async` that handles the logging in the
   program. Make it listen to a `TBMQueue` that takes some kind of logging event that you decide the
   structure of. When another thread wants to log something, it uses a utility function to put a
   message on the queue. Remember that `TBMQueue`s can be shared via your environment.

#### Exercise notes (ReaderT)

0. [getCurrentTime](https://hackage.haskell.org/package/rio-0.1.21.0/docs/RIO-Time.html#v:getCurrentTime)
   can be used for getting the current time.

   The following code snippet can be used for formatting the time in a reasonable fashion:

```haskell
import RIO.Time (UTCTime)
import qualified RIO.Time as Time

getFormattedTime :: IO String
getFormattedTime = formatStandard <$> Time.getCurrentTime

formatStandard :: UTCTime -> String
formatStandard = Time.formatTime Time.defaultTimeLocale (Time.iso8601DateFormat (Just "%H-%M-%S"))
```
