# The Reader Monad

The `Reader` monad allows us to make an environment available to an entire part of our call graph.
What this means is that we can, at the start of executing a `Reader` context, bake in a value that
we won't have to pass explicitly to any of the `Reader` functions in that part of the call graph:

```haskell
import Control.Monad.Reader (Reader, ask, runReader)

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

The above example is of course contrived, but it's enough to know that if one wants to have a shared
environment for a call graph in ones program, the `Reader` monad can be useful. One thing to note is
that a related type, `ReaderT`, forms the basis with which many Haskell users build applications,
where they combine `Reader` with `IO` and thus get access to both `IO` and implicit values.
