# HaskellMaterials

## Installing `stack`

[How to install Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) is a
reasonable place to start.

Why `stack`? It's likely the easiest way for new people to start and (probably) the most common
choice in industry, though other tools are catching up.

## Making your first example project

`stack` has an analog for `mix new`:

```bash
$ stack new MyExampleProject simple
$ cd MyExampleProject
$ stack build
```

The `simple` part is which template we want to use. The `simple` one is enough for what we need,
but entire web development templates are available and as you would expect you can make your own
ones.

When we execute the above stack will automatically download an appropriate `ghc` version (GHC is the
Haskell compiler that is most commonly used).

## Example of Haskell syntax and use of common libraries

The syntax of Haskell can't really be summarized neatly in one go because there's been so much new
syntax added over the years via extensions that it'd be too much to take in. However,
[here](./misc/syntax.md) is an attempt at introducing most of the basic syntax that a simple project
could use.

## "Effectful"

[Here](./misc/effectful.md) is an explanation of what "effectful" means, practically speaking, as
well as a short introduction to `IO`.

## "Help! I want to print a value but I can't because Haskell is mean!"

[`Debug.Trace`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html) is your
friend. It will even put compiler warnings in there automatically so you don't leave your debug
logging statements in there.

```haskell
import qualified Debug.Trace as Trace

pureFunction :: Int -> Int
pureFunction parameter =
  -- This will show the first argument, i.e. turn it into a string and print it, then return the
  -- second argument; `parameter`
  Trace.traceShow (parameter + 42) parameter

main :: IO ()
main = do
    -- When this is run we will print `84`, but still get the passed in value bound to `x`.
    let x = pureFunction 42
    
    -- Here we will be printing 'Done: 42'
    putStrLn $ "Done: " <> show x
```

## Next steps

[Interacting with the terminal](./steps/01-interacting-with-the-terminal.md) has introductory
material about executing effectful things and an intro to OS interaction in Haskell code.