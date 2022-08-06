# The Reader Monad

- [The Reader Monad](#the-reader-monad)
  - [The purpose of `Reader`](#the-purpose-of-reader)

The `Reader` monad allows us to make an environment available to an entire part of our call graph.
What this means is that we can, at the start of executing a `Reader` context, bake in a value that
we won't have to pass explicitly to any of the `Reader` functions in that part of the call graph:

```haskell
import Control.Monad.Reader (Reader, ask, runReader)
import Prelude

notPassingArguments :: Reader String Int
notPassingArguments = do
  -- `ask` retrieves the environment, in this case a `String`
  stringFromEnvironment <- ask
  pure $ length stringFromEnvironment

canReadString :: Int -> Reader String Int
canReadString added = do
  -- We don't need to pass any arguments here; `notPassingArguments` will read the environment
  result <- notPassingArguments
  pure $ added + result

main :: IO ()
main = do
  let x = runReader (canReadString 5) "Quanterall" -- This is where we pass the initial environment
      y = runReader (canReadString 5) "Quanteral" -- And here a different one
  print x -- 15
  print y -- 14
```

As you can see above, `Reader` has two type parameters: `Reader environmentType returnValue`

The return value being in the right-most position is usual and is something we'll see more of.

## The purpose of `Reader`

In the grand scheme of things `Reader` is useful when we have a deep call stack that might need a
value in arbitrary depths of it, where passing a value down through every function might not be
useful, even if it could be considered "cleaner".

The most useful form of `Reader`, however, comes in the form of `ReaderT`, which you can read more
about in the next chapter: [ReaderT](./10-readert.md).
