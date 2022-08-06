# The Reader Monad and The ReaderT Monad Transformer

- [The Reader Monad and The ReaderT Monad Transformer](#the-reader-monad-and-the-readert-monad-transformer)
  - [Reader](#reader)
    - [`ask` & `asks`](#ask--asks)
      - [`ask :: Reader r r`](#ask--reader-r-r)
      - [`asks :: (r -> a) -> Reader r a`](#asks--r---a---reader-r-a)
  - [The purpose of `Reader`](#the-purpose-of-reader)
  - [Monad transformers](#monad-transformers)
  - [ReaderT](#readert)
    - [The `ReaderT` type](#the-readert-type)
    - [`ReaderT env IO a`](#readert-env-io-a)
  - [Our `Reader` example, extended](#our-reader-example-extended)
  - [Adding very basic logging to our example](#adding-very-basic-logging-to-our-example)
  - [Creating our own monad type alias](#creating-our-own-monad-type-alias)
    - [Exercises (ReaderT)](#exercises-readert)
      - [Exercise notes (ReaderT)](#exercise-notes-readert)

## Reader

The `Reader` monad allows us to make an environment available to an entire part of our call graph.
What this means is that we can, at the start of executing a `Reader` context, bake in a value that
we won't have to pass explicitly to any of the `Reader` functions in that part of the call graph:

```haskell
import Control.Monad.Reader (Reader, ask, runReader)
import Prelude

main :: IO ()
main = do
  let x = runReader (canReadString 5) "Quanterall" -- This is where we pass the initial environment
      y = runReader (canReadString 5) "Quanteral" -- And here a different one
  print x -- 15
  print y -- 14

canReadString :: Int -> Reader String Int
canReadString added = do
  -- We don't need to pass any arguments here; `notPassingArguments` will read the environment
  result <- notPassingArguments
  pure $ added + result

notPassingArguments :: Reader String Int
notPassingArguments = do
  -- `ask` retrieves the environment, in this case a `String`
  stringFromEnvironment <- ask
  pure $ length stringFromEnvironment
```

As you can see above, `Reader` has two type parameters: `Reader environmentType returnValue`

The return value being in the right-most position is usual and is something we'll see more of.

### `ask` & `asks`

`ask` and `asks` are two ways we can retrieve values from the environment we are suppling.

#### `ask :: Reader r r`

`ask` retrieves the entire environment. With this we could, for example, do the following:

```haskell
notPassingArguments :: Reader String Int
notPassingArguments = do
  stringFromEnvironment <- ask
  pure $ length stringFromEnvironment
```

#### `asks :: (r -> a) -> Reader r a`

`asks` takes a function from our environment type to a resulting type, meaning we can pass a
function that runs on our environment to give us the result we need, leading to uses where we can
even perform on-the-fly transformations of our values.

With `asks` our previous example might look as something like the following two alternatives:

```haskell
notPassingArguments' :: Reader String Int
notPassingArguments' = do
  stringLength <- asks length
  pure stringLength

notPassingArguments'' :: Reader String Int
notPassingArguments'' = asks length
```

The function is immediately applied to the environment and returned, which means we can be more
direct about what we are actually getting from our code. This also means that we could immediately
reach into a value we might provide as part of a bigger structure as our environment, via record
getter functions.

## The purpose of `Reader`

In the grand scheme of things `Reader` is useful when we have a deep call stack that might need a
value in arbitrary depths of it, where passing a value down through every function might not be
useful, even if it could be considered "cleaner".

The most useful form of `Reader`, however, comes in the form of `ReaderT`. But first, let's talk
briefly about monad transformers.

## Monad transformers

A monad transformer is a data type that allows us to wrap one monad in another. What this means is
that it enables a context wherein we get the capabilities of both the transformer and the monad it
is wrapping. Monad transformers are themselves monads, which means we can create multiple layers to
get an arbitrary amount of capabilities. It's relatively rare in normal applications that you'd
create big stacks of monad transformers, however, as most things can be done with `ReaderT` wrapping
the `IO` monad.

## ReaderT

`ReaderT` is the transformer version of the `Reader` monad and gives us the capability to read an
environment that is passed around implicitly in an entire call graph.

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
values we can read from an environment.

### `ReaderT env IO a`

It's very common to create applications that execute in the `ReaderT env IO a` context, where `env`
is the application's environment/application state. This allows for implicit environment reading as
well as `IO` actions. Any resources that we need access to are established in the initialization of
our application and are then put in the `App` value, which we pass to `runReaderT` in order to run
our `ReaderT env IO` functions.

Putting `ReaderT` "on top of" `IO` like this means that we get access to a whole host of effectful
things that we couldn't get as comfortably with just `Reader` or just `IO`:

- Running `STM` actions on values that we don't need to pass around everywhere all the time
- Modifying and reading `IORef`s that we don't pass around
- Interacting with database connection handles that we don't pass around
- ...

The list of things we can do with `ReaderT` on top of `IO` is very long and this is why it's such a
powerful base structure for most of our applications.

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
  string' <- asks string
  -- We can use `logToFile` here and not be concerned with the file handle because we know it's in
  -- the environment we're executing inside of already.
  logToFile $ "We got '" <> string' <> "' from the environment"
  pure $ length string'

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

## Creating our own monad type alias

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

1. Create a function that takes a string that it writes to a file handle. The file handle should be
   retrieved from the environment with `ask` or `asks`. Run your function with `runReaderT` in your
   REPL; what do you have to do in order to execute it?

2. Create a program that downloads the last three releases from a given repository on GitHub. You
   can find the [API specification here](https://docs.github.com/en/rest/reference/repos#releases).

   Initialize the program with a `Manager` from the `http-client-tls` package and make it an
   implicit argument via `ReaderT`. Create an environment type (oftentimes called just `App`) that
   stores your application's resources (in this case a `Manager`) and whatever else your application
   needs in order to do its job.

3. Also add logging to this application to a given file based on the time[0] of the execution of the
   program. Suggested logging events would be when the program was doing things like fetching the
   list of releases as well as the downloading of files and perhaps even when a file is **not**
   downloaded because it already exists.

4. When the program in exercise 1 starts, start a thread via `async` that handles the logging in the
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
