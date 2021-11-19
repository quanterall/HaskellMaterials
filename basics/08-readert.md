# The ReaderT Monad Transformer

- [The ReaderT Monad Transformer](#the-readert-monad-transformer)
  - [Monad transformers](#monad-transformers)
  - [ReaderT](#readert)
  - [Our `Reader` example, extended](#our-reader-example-extended)

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
that we get access to a whole host of effectful things that we couldn't get with just `Reader`;
transactional variables, `IORef`s, database connection handles, and so on.

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

main :: IO ()
main = do
  -- Note how we have to use `bind`/`<-` here because `runReaderT` returns a value of type `m a`,
  -- in this case `IO a`. This is different from `runReader`, which returns just `a`.
  x <- runReaderT (canReadString 5) "Quanterall"
  y <- runReaderT (canReadString 5) "Quanteral"
  print x -- 15
  print y -- 14
```
