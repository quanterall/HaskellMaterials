# HaskellMaterials

## Learning Haskell

Learning Haskell is a challenge, much like learning any programming challenge. I've also found that
many people seem to make it an unnecessarily hard one because they imagine there is more theoretical
background that you have to learn in order to use Haskell. This is for the most part a fiction that
people convince themselves of. Haskell is first and foremost a practical language meant to solve
problems and there is very little that you can get out of theoretical (math or programming
language theory) knowledge when it comes to getting things done in it.

There are some things I would like to stress to everyone who wants to learn Haskell:

### Resist the urge to try to learn exactly how everything works immediately

I've seen a lot of people try to learn too much of how Haskell works immediately and I can pretty
much guarantee that they never treated learning any other language like this. With the exception of
the surface syntax of a language like C, it's near impossible. Getting comfortable with a language
takes time and effort and Haskell is no exception.

Sometimes you have to use features that you aren't intimately familiar with in order to do things.
Once you practice using them enough your intuition will catch up.

**Don't read `Monad` tutorials, use things that implement the `Monad` type class and understand how
it's used in practice, that'll be more useful than wasting your time reading theoretical (and
usually misleading/wrong) explanations of what monads are.**

### Keep in mind that learning Haskell is about getting things done

Learning Haskell is not a badge of honor. Learning more advanced things is only useful if it helps
you get things done or helps you understand more. There is lots of machinery that's created in the
Haskell world that is basically not applicable to almost any application. Learning is fun, but you
know what Haskell has too little of? People showing cool projects they created that actually do
things.

It would be a failure for this material to have you walking way saying things like:

> Haskell made me think differently. I don't use it often but it shaped the way I think.

### Category theory is a branch of mathematics

While stimulating to learn it's not useful knowledge for getting things done in Haskell. It is my
experience that people who don't use Haskell will often misrepresent the usefulness of theory. This
applies to more things than category theory.

## Installing `stack`

[How to install Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) is a
reasonable place to start.

Why `stack`? It's likely the easiest way for new people to start and (probably) the most common
choice in industry, though other tools are catching up.

## Making your first example project

`stack` has an analog for `mix new`:

```bash
stack new MyExampleProject simple
cd MyExampleProject
stack build
```

The `simple` part is which template we want to use. The `simple` one is enough for what we need,
but entire web development templates are available and as you would expect you can make your own
ones.

When we execute the above stack will automatically download an appropriate `ghc` version (GHC is the
Haskell compiler that is most commonly used).

## Basics and syntax

- [x] [Modules](./basics/00-modules.md)
- [x] [Values and functions, basic types](./basics/01-values-and-functions.md)
- [x] [Composite datatypes and working with them](./basics/02-composite-datatypes.md)
- [ ] [Miscellaneous syntax, to be split out](./misc/syntax.md)

## "Effectful"

[Here](./misc/effectful.md) is an explanation of what "effectful" means, practically speaking, as
well as a short introduction to `IO`.

## "Help! I want to print a value but I can't print without IO!"

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

## Type classes

I've written a small document
[on type classes and an intuition for how to view their utility](./misc/type-classes.md) that could
be useful for people who struggle with how to relate to them in practice. It also contains a short
presentation of a few very commonly used ones that could be good to know about.

## Lessons / Next steps

[Interacting with the terminal](./steps/01-interacting-with-the-terminal.md) has introductory
material about executing effectful things and an intro to OS interaction in Haskell code.

## I'm comfortable enough with Haskell basics but I don't know how to structure apps

For readers who might be using these documents as references and feel comfortable enough with their
basics, the following list of articles and suggestions might be useful:

(Note: Some of this will likely just be put in separate steps/lessons where we actually go through
how they work, and we most definitely should/will do presentations on them.)

- Learn about the `Reader` monad and that it allows one to read values in any code path extending
  from one with the type `Reader dataType`.
- Learn how `ReaderT dataType monad` allows one to layer `Reader` on top of another monad, giving
  **both** capabilities; `ReaderT dataType IO` will therefore allow us to read the `dataType` value
  everywhere **and** use `IO`.
- Read
  ["The ReaderT design pattern"](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) by
  Michael Snoyman. There is an overview of application "architecture" in there that is very useful,
  as well as motivations for choices made in the service of better Haskell applications. It's a very
  insightful article and the ideas in it should serve basically 90% of all production Haskell
  applications, easily.

In the article, these points are being stressed in different parts and I want to reiterate them and
point them out as things to do and experiment with:

- Experiment with programs keeping state in `Env` and executing in `ReaderT YourEnvironment IO`,
  use both `IO.IORef.IORef` and `Control.Concurrent.STM.TVar`, recognize how `STM` is for
  transactional memory modifications and should be used when memory could be modified at the same
  time in different places.
- Experiment with `(MonadReader env m, HasSpecificThing env) => ... ReaderT env m` to limit what a
  function can access from your environment, making it impossible that it touches things it
  shouldn't.
- Experiment with adding monad constraints like `CanModifyUsers m => User -> m ()` to your functions
  in order to limit them from being able to do anything else effectful; giving you ultimate control
  of exactly what effectful thing a function can do.
