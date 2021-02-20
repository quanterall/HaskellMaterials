# Interacting with the terminal

## What does our starter template do?

When we've created our initial project we see a file called `Main.hs`. This is the file that is
automatically created as the entry point for our application, specifically the `main` function:

```haskell
module Main where

main :: IO ()
main = do
  putStrLn "hello world"
```

As is probably obvious, this is what "hello world" looks like in Haskell. The `IO ()` means that we
are doing something effectful and we are returning `()` from the function. `()` is the "unit" type
and without going too deep into it you can view `IO ()` as `void` in terms of function type
signatures.

## Helpful resources when programming in Haskell

### Hoogle

[`hoogle`](https://hoogle.haskell.org/) is a website and tool that allows one to search for type
signatures. If we, for example, search for the type signature `IO [String]`, that is a procedure
that returns a list of `String`, but does so in an effectful manner, meaning it can differ each time
we call it. We know that it is doing something **interesting** because it returns an `IO` action.

It should be noted that Hoogle is available as a "bang pattern" in the search engine
[DuckDuckGo](https://duckduckgo.com/), which means you can type `!h <search-term>` and go to Hoogle
immediately, allowing you to search for type signatures, function names, module names, etc. very
conveniently and quickly. This is usable as your browsers default search engine and you can use
Google via the "!g" bang pattern when you want Google results, giving you more flexibility.

If we go to Hoogle and type `IO [String]` the first example that comes up is
[`getArgs`](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:getArgs).

#### Brief intro to `IO` and what it means for functions

`getArgs` the epitome of an effectful function in that this depends entirely on what has been passed
to our executable when we run it.

Further along in the material we will go through what this means in more detail, but initially we
only need to know that a function that, given the same arguments, can return different results, is
effectful and returns `IO <something>`. If a function uses a function that returns `IO <something>`
it also must return `IO <something>`. Knowing and relying on this property is how we are able to
say that one function can be relied on to always return the same results given the same arguments and
another one is not. This has implications in terms of testability, possible reliability and which
functions are less likely to behave in adventurous ways.

Since `main` returns `IO ()` and thus executes "in the `IO` monad" anything that branches off from
it will be able to execute things in the `IO` monad in case it is type annotated as such. This means
that there is no limitation at all on a Haskell program and what it can do; functions simply need to
be decorated with this "property", if you will.

#### Using `getArgs`

If we want to use `getArgs` we can do so as follows:

```haskell
module Main where

import qualified System.Environment as Environment

main :: IO ()
main = do
  arguments <- Environment.getArgs
  print arguments
```

If we now run `stack run -- hello world` in our project it will compile and give us a result:

```bash
❯ stack run -- hello world
MyExampleProject> configure (exe)
Configuring MyExampleProject-0.1.0.0...
MyExampleProject> build (exe)
Preprocessing executable 'MyExampleProject' for MyExampleProject-0.1.0.0..
Building executable 'MyExampleProject' for MyExampleProject-0.1.0.0..
[1 of 1] Compiling Main
Linking .stack-work\\dist\\29cc6475\\build\\MyExampleProject\\MyExampleProject.exe ...
MyExampleProject> copy/register
Installing executable MyExampleProject in C:\\Users\\ricka\\code\\quanterall\\MyExampleProject\\.stack-work\\install\\31aae583\\bin
["hello","world"]
```

#### What does `<-` mean?

It is fairly common to use what's called "do notation" in Haskell. In this case our `main` function
has the keyword `do` at the start and what this means in practice is that it makes writing this type
of effectful code a bit nicer. You can view it basically like `async`/`await` in JavaScript in that
it serves the same purpose of keeping the flow of the code more straight forward.

`<-` in this analogy corresponds to `await`, so the type that `arguments` has is now just `[String]`
and not `IO [String]`. This means that we can pass it to functions that deal with lists of things.

```haskell
module Main where

import qualified System.Environment as Environment

main :: IO ()
main = do
  arguments <- Environment.getArgs
  print (length arguments)
```

Executing `stack run -- hello world` again will give us the result `2`, because there are two
arguments passed to the executable. `length` does not know or care that it is working with data
that originally came from something executed in the `IO` monad. It is a pure function and nothing
limits us from writing most parts of our program in this non-effectful fashion and then gluing the
parts together with effectful functions.

#### Taking input

Sometimes we need to take user input. This is obviously an effectful thing but we just need to know
the correct function to call:

```haskell
module Main where

import qualified System.Environment as Environment

main :: IO ()
main = do
  arguments <- Environment.getArgs
  putStrLn "How many arguments are there? "
  howMany <- readLn
  print (length arguments == howMany)
```

This program will print the question and then compare the answer to however many arguments we have
passed to the program, so if we run `stack exec -- hello world` and answer `2` we should see that it
prints `True`.