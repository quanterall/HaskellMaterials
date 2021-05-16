# HaskellMaterials

- [HaskellMaterials](#haskellmaterials)
  - [Work In Progress](#work-in-progress)
  - [Real World Haskell](#real-world-haskell)
  - [Learning Haskell](#learning-haskell)
    - [Resist the urge to try to learn exactly how everything works immediately](#resist-the-urge-to-try-to-learn-exactly-how-everything-works-immediately)
    - [Keep in mind that learning Haskell is about getting things done](#keep-in-mind-that-learning-haskell-is-about-getting-things-done)
    - [Category theory is a branch of mathematics](#category-theory-is-a-branch-of-mathematics)
    - [Asking questions is good](#asking-questions-is-good)
  - [Installing the build- and project management-tool `stack`](#installing-the-build--and-project-management-tool-stack)
  - [Running the examples in this repository](#running-the-examples-in-this-repository)
  - [Making your first example project](#making-your-first-example-project)
    - [Using development containers in VSCode](#using-development-containers-in-vscode)
  - [Using the REPL/GHCi](#using-the-replghci)
  - [Basic reading materials](#basic-reading-materials)
  - ["Help! I want to print a value but I can't print without IO!"](#help!-i-want-to-print-a-value-but-i-cant-print-without-io!)
  - ["I'm comfortable enough with Haskell basics but I don't know how to structure apps."](#im-comfortable-enough-with-haskell-basics-but-i-dont-know-how-to-structure-apps.)

## Work In Progress

This repository is (and likely will be for quite some time) a work in progress. Suggestions for
articles on concepts and themes are welcome, as well as corrections/clarifications on already
available material.

## Real World Haskell

The purpose of these documents is to at some point be able to teach Haskell, but as a supplement to
them it can be useful to look at the book
[*Real World Haskell*](http://book.realworldhaskell.org/read/) as it is freely available online.
The need for this may lessen in time but since learning is such a difficult enterprise it's useful
to simply look at many resources and absorb as much as possible.

*Real World Haskell* is not super up-to-date but it teaches the basics of Haskell reasonably and
also has in mind that Haskell is a language for writing actual solutions in, not code for its own
sake. Additionally, it does not teach relatively modern Haskell application architecture, but the
resources for this have been included in these documents and will be taught separately.

## Learning Haskell

Learning Haskell is hard, much like learning any programming language. I've found that many people
seem to make it an unnecessarily hard one because they imagine there is more theoretical background
that you have to learn in order to use Haskell. This is for the most part a fiction that people
convince themselves of. Haskell is first and foremost a practical language meant to solve problems
and there is very little that you can get out of theoretical (math or programming language theory)
knowledge when it comes to getting things done in it.

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

### Asking questions is good

We learn when we bump up against things. We might not understand the behavior of some code or we
might not be able to express ourselves clearly yet. We might just wonder if maybe there's a better
way to do the thing we're doing because it feels awkward. Talking to other people about our code and
solutions is a very effective way to stimulate thinking deeply about these things.

If you have any questions/suggestions on the material or things not covered by the material (yet?)
you can pick any way to contact me (RocketChat, Discord, rickard.andersson@quanterall.com) and I'll
do my very best to answer, correct or otherwise handle it. Another avenue would be to create an issue
or submit a pull request with the changes and/or questions.

## Installing the build- and project management-tool `stack`

[How to install Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) is a
reasonable place to start.

Why `stack`? It's likely the easiest way for new people to start and (probably) the most common
choice in industry, though other tools are catching up.

## Running the examples in this repository

Most of the examples are created to work well with the `quanterall/basic` template referenced below
in this document. You can choose to run this in a development container or not, but the bigger point
is that this template sets up a default set of extensions. This means that it effectively
assumes/establishes a standard language subset to learn and use.

If you're having issues running the examples, it might be wise to create a project using
`stack new my-project-name quanterall/basic` and paste the example code in the generated
`Library.hs` file. This has been tested with many of the examples and that is in general how it's
supposed to work. If you find examples that do not, please make an issue or pull request about it.

## Making your first example project

I've set up templates for project generation via the `stack` tool. These come pre-configured with
appropriate settings (including which language features to use by default) and are very useful for
setting up projects, both experimental one-offs, serious libraries and applications.

The most basic of these is called `basic`:

```bash
stack new my-project-name quanterall/basic
cd my-project-name
stack run
```

The above command will result in a running example that prints to the screen and in the `Library.hs`
file you can edit the behavior of the program (in the `main` function).

[This repository](https://github.com/quanterall/stack-templates) has more templates and a description
of their use cases.

**Note: See below for a better, more streamlined workflow enabled by `docker`, `docker-compose` and
VSCode.**

### Using development containers in VSCode

I have created a template in the Quanterall organization called "basic" that will automatically
create an empty project that also includes a
[development container specification](https://code.visualstudio.com/docs/remote/create-dev-container)
that one can use to create simple Haskell applications.

To use it, issue the following command after having installed both `docker` and `stack`:

```bash
$ stack new project-name quanterall/basic
<... project creation text ...>
```

When you enter the directory created by this template, you'll notice that VSCode suggests re-opening
the directory in a container. When you do so, it will download the basic Haskell quanterall
development image (if you don't already have it) and create a container using it. This image has the
tools necessary for you to be able to have proper assistance in VSCode (auto-complete, linting, type
hints for the thing your cursor is on, error reporting and more) and the development container
specifies a set of extensions that work well with those tools.

Once this is done and you open the terminal execute the following to run the app (as per usual):

```bash
$ stack run
<... build output ...>
Hello, World!
```

I have created a video demostrating how to use the templates:

[![Watch](https://img.youtube.com/vi/fkR0DsKwIO4/0.jpg)](https://youtu.be/fkR0DsKwIO4)

## Using the REPL/GHCi

It is quite common to use the REPL/interactive environment to write snippets of code and also to
query your code base for information. I've made a video on some of the basic things you can do in
the REPL, some of the things you might want to keep in mind as well as some of the things I added to
make the experience a bit better in the development container REPLs:

[![Watch](https://img.youtube.com/vi/JuaqOirNF8g/0.jpg)](https://youtu.be/JuaqOirNF8g)

## Basic reading materials

The below documents contain information and examples about different topics. Generally speaking they
are readable from top to bottom in terms of the assumptions they make about knowledge level, though
the "Modules" document is somewhat of a standalone affair and might be removed entirely.

- [Modules](./basics/00-modules.md)
- [Values and functions, basic types](./basics/01-values-and-functions.md)
- [Composite datatypes and working with them](./basics/02-composite-datatypes.md)
- ["Effectful" and `IO`](./basics/03-effectful.md)
- [Type classes](./basics/04-type-classes.md)
- [JSON in Haskell](./basics/05-json-data.md)
- [Mutable variables in Haskell](./basics/06-mutable-variables.md)
- [The Reader Monad](./basics/07-reader.md)
- [The ReaderT Monad Transformer](./basics/08-readert.md)

## "Help! I want to print a value but I can't print without IO!"

[`Debug.Trace`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Debug-Trace.html) is your
friend. It will even put compiler warnings in there automatically so you don't leave your debug
logging statements in there to run in production.

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

## "I'm comfortable enough with Haskell basics but I don't know how to structure apps."

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

- Experiment with programs keeping state in `Env` and executing in `ReaderT Env IO`, use both
  `IO.IORef.IORef` and `Control.Concurrent.STM.TVar`, recognize how `STM` is for transactional
  memory modifications and should be used when memory could be modified at the same time in
  different places.
- Experiment with `(MonadReader env m, HasSpecificThing env) => ... ReaderT env m` to limit what a
  function can access from your environment, making it impossible that it touches things it
  shouldn't.
- Experiment with adding monad constraints like `CanModifyUsers m => User -> m ()` to your functions
  in order to limit them from being able to do anything else effectful; giving you ultimate control
  of exactly what effectful thing a function can do.
